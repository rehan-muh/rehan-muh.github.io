library(INLA)
library(fmesher)
library(mgcv)
library(spdep)
library(spatialreg)
library(sf)
library(rnaturalearth)
library(ggplot2)

base <- "https://rehan-muh.github.io/tutorials/data/"
d    <- read.csv(paste0(base, "languages.csv"))
d$elev_km <- d$elevation / 1000
d$sp_id   <- seq_len(nrow(d))

m0 <- glm(ejectives ~ elev_km, family = binomial, data = d)

# the neighbour graph and the INLA priors of the earlier chapters
coords <- as.matrix(d[, c("lon", "lat")])
nb <- make.sym.nb(knn2nb(knearneigh(coords, k = 5, longlat = TRUE)))
W  <- nb2mat(nb, style = "B")

fixed <- list(mean = 0, prec = 1)
slope_of <- function(fit) {
  cols <- c("mean", "sd", "0.025quant", "0.975quant")
  round(unlist(fit$summary.fixed["elev_km", cols]), 2)
}

pc_bym <- list(prec = list(prior = "pc.prec", param = c(3, 0.05)),
               phi  = list(prior = "pc", param = c(0.5, 0.5)))

i_bym <- inla(ejectives ~ elev_km +
                f(sp_id, model = "bym2", graph = W, scale.model = TRUE,
                  hyper = pc_bym),
              family = "binomial", data = d, control.fixed = fixed)

slope_of(i_bym)

classic_of <- function(fit) {
  args <- fit$.args
  args$inla.mode <- "classic"
  do.call(inla, args)
}
i_bym_classic <- classic_of(i_bym)

rbind(default = slope_of(i_bym), classic = slope_of(i_bym_classic))

to_sphere <- function(lon, lat) {
  rad <- pi / 180
  cbind(cos(lat * rad) * cos(lon * rad),
        cos(lat * rad) * sin(lon * rad),
        sin(lat * rad))
}
xyz <- to_sphere(d$lon, d$lat)

mesh <- fm_rcdt_2d(globe = 12)
mesh$n

spde <- inla.spde2.pcmatern(mesh,
                            prior.range = c(0.05, 0.05),
                            prior.sigma = c(3, 0.05))

loc_weights <- fm_basis(mesh, loc = xyz)

stack <- inla.stack(
  data    = list(y = d$ejectives),
  A       = list(loc_weights, 1),
  effects = list(field = seq_len(mesh$n),
                 data.frame(intercept = 1, elev_km = d$elev_km)))

i_spde <- inla(y ~ 0 + intercept + elev_km + f(field, model = spde),
               family = "binomial", data = inla.stack.data(stack),
               control.predictor = list(A = inla.stack.A(stack)),
               control.fixed = fixed)

ci <- c("mean", "0.025quant", "0.975quant")
rbind(default = slope_of(i_spde), classic = slope_of(classic_of(i_spde)))
round(i_spde$summary.hyperpar[, ci], 2)



world <- ne_countries(scale = "small", returnclass = "sf")
sf_use_s2(FALSE)

grid <- expand.grid(lon = seq(-179, 179, 2), lat = seq(-55, 75, 2), elev_km = 0)
grid <- st_as_sf(grid, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
grid <- grid[lengths(st_intersects(grid, world)) > 0, ]

grid$field <- as.vector(fm_basis(mesh, loc = to_sphere(grid$lon, grid$lat)) %*%
                          i_spde$summary.random$field$mean)

ggplot() +
  geom_sf(data = world, fill = "grey94", colour = NA) +
  geom_sf(data = grid, aes(colour = field), size = 1.5, shape = 15) +
  scale_colour_gradient2(low = "#2A78D6", mid = "#F0EFEC", high = "#E34948",
                         name = "spatial effect") +
  coord_sf(crs = "+proj=eqearth")

g_sos <- gam(ejectives ~ elev_km + s(lat, lon, bs = "sos", k = 60),
             family = binomial, data = d, method = "REML")

summary(g_sos)$p.table
summary(g_sos)$s.table
k.check(g_sos)

grid$surface <- predict(g_sos, newdata = grid, type = "terms",
                        terms = "s(lat,lon)")[, 1]

ggplot() +
  geom_sf(data = world, fill = "grey94", colour = NA) +
  geom_sf(data = grid, aes(colour = surface), size = 1.5, shape = 15) +
  scale_colour_gradient2(low = "#2A78D6", mid = "#F0EFEC", high = "#E34948",
                         name = "spatial effect") +
  coord_sf(crs = "+proj=eqearth")

d$log_cons <- log(d$n_consonants)
lw  <- nb2listw(nb, style = "W")
ols <- lm(log_cons ~ elev_km, data = d)

lm.morantest(ols, lw)

sar_err <- errorsarlm(log_cons ~ elev_km, data = d, listw = lw)
sar_lag <- lagsarlm(log_cons ~ elev_km, data = d, listw = lw)

rbind("OLS"       = summary(ols)$coefficients["elev_km", 1:2],
      "SAR error" = summary(sar_err)$Coef["elev_km", 1:2],
      "SAR lag"   = summary(sar_lag)$Coef["elev_km", 1:2]) |> round(3)
c(lambda = unname(sar_err$lambda), rho = unname(sar_lag$rho))

impacts(sar_lag, listw = lw)

lines <- data.frame(model = c("OLS", "SAR error"),
                    intercept = c(coef(ols)[1], sar_err$coefficients[1]),
                    slope     = c(coef(ols)[2], sar_err$coefficients[2]))

ggplot(d, aes(elev_km, log_cons)) +
  geom_point(colour = "grey75", size = 0.9) +
  geom_abline(data = lines, linewidth = 0.9,
              aes(intercept = intercept, slope = slope, colour = model)) +
  scale_colour_manual(values = c("grey35", "#2A78D6"), name = NULL) +
  labs(x = "elevation (km)", y = "log number of consonants")

library(brms)
options(mc.cores = 4, brms.backend = "cmdstanr")
dir.create("fits", showWarnings = FALSE)

pts <- st_as_sf(d, coords = c("lon", "lat"), crs = 4326)
xy  <- st_coordinates(st_transform(pts, "+proj=eqearth")) / 1e6
d$x <- xy[, 1]
d$y <- xy[, 2]
dimnames(W) <- list(d$glottocode, d$glottocode)

b_gp  <- brm(ejectives ~ elev_km + gp(x, y, k = 20, c = 5/4),
             data = d, family = bernoulli(),
             prior = prior(normal(0, 1), class = b) +
                     prior(exponential(1), class = sdgp),
             control = list(adapt_delta = 0.95),
             seed = 1, file = "fits/m_gp")
b_bym <- brm(ejectives ~ elev_km + car(W, gr = glottocode, type = "bym2"),
             data = d, data2 = list(W = W), family = bernoulli(),
             prior = prior(normal(0, 1), class = b) +
                     prior(exponential(1), class = sdcar),
             seed = 1, file = "fits/m_bym")

rbind("brms, Gaussian process"  = fixef(b_gp)["elev_km", 1:2],
      "brms, BYM2"              = fixef(b_bym)["elev_km", 1:2],
      "INLA, BYM2 (classic)"    = slope_of(i_bym_classic)[1:2],
      "INLA, SPDE on the globe" = slope_of(i_spde)[1:2],
      "mgcv, spline on sphere"  = summary(g_sos)$p.table["elev_km", 1:2]) |>
  round(2)

x <- seq(0, 4.5, by = 0.1)
averaged <- function(eta, b) {
  at_sea <- eta - b * d$elev_km           # every language moved to 0 km
  sapply(x, function(xi) mean(plogis(at_sea + b * xi)))
}
from_brms <- function(fit) {
  averaged(colMeans(posterior_linpred(fit)), fixef(fit)["elev_km", "Estimate"])
}
from_inla <- function(fit) {
  averaged(fit$summary.linear.predictor$mean[seq_len(nrow(d))],
           fit$summary.fixed["elev_km", "mean"])
}

lines <- rbind(
  data.frame(model = "brms GP",   view = "surface", p = from_brms(b_gp)),
  data.frame(model = "INLA SPDE", view = "surface", p = from_inla(i_spde)),
  data.frame(model = "mgcv spline", view = "surface",
             p = averaged(predict(g_sos), coef(g_sos)[["elev_km"]])),
  data.frame(model = "brms BYM2", view = "neighbours", p = from_brms(b_bym)),
  data.frame(model = "INLA BYM2", view = "neighbours",
             p = from_inla(i_bym_classic)))
lines$elev_km <- x
naive <- data.frame(elev_km = x)
naive$p <- predict(m0, newdata = naive, type = "response")

ggplot(lines, aes(elev_km, p)) +
  geom_line(aes(group = model, linetype = view), colour = "#2A78D6",
            linewidth = 0.8) +
  geom_line(data = naive, linetype = "22", colour = "grey35") +
  scale_linetype_manual(values = c(neighbours = "42", surface = "solid"),
                        name = NULL) +
  labs(x = "elevation (km)", y = "probability of ejectives")
