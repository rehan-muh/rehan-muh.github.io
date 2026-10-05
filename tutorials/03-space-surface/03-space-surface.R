library(ape)
library(brms)
library(sf)
library(spdep)
library(rnaturalearth)
library(ggplot2)

base <- "https://rehan-muh.github.io/tutorials/data/"
d    <- read.csv(paste0(base, "languages.csv"))
d$elev_km <- d$elevation / 1000

options(mc.cores = 4, brms.backend = "cmdstanr")
dir.create("fits", showWarnings = FALSE)

pts <- st_as_sf(d, coords = c("lon", "lat"), crs = 4326)
xy  <- st_coordinates(st_transform(pts, "+proj=eqearth")) / 1e6
d$x <- xy[, 1]
d$y <- xy[, 2]

priors <- prior(normal(0, 1), class = b) +
          prior(exponential(1), class = sdgp)

m_gp <- brm(ejectives ~ elev_km + gp(x, y, k = 20, c = 5/4),
            data = d, family = bernoulli(), prior = priors,
            control = list(adapt_delta = 0.95),
            seed = 1, file = "fits/m_gp")

summary(m_gp)

max_dist <- max(dist(xy)) * 1000        # in km
ls <- posterior_summary(m_gp, variable = "lscale_gpxy")
round(ls[, c("Estimate", "Q2.5", "Q97.5")] * max_dist)

m_gp12 <- brm(ejectives ~ elev_km + gp(x, y, k = 12, c = 5/4),
              data = d, family = bernoulli(), prior = priors,
              control = list(adapt_delta = 0.95),
              seed = 1, file = "fits/m_gp12")
m_gp28 <- brm(ejectives ~ elev_km + gp(x, y, k = 28, c = 5/4),
              data = d, family = bernoulli(), prior = priors,
              control = list(adapt_delta = 0.95),
              seed = 1, file = "fits/m_gp28")

check <- function(m) {
  c(fixef(m)["elev_km", c("Estimate", "Est.Error")],
    lscale = posterior_summary(m, variable = "lscale_gpxy")[, "Estimate"])
}
round(rbind("k = 12" = check(m_gp12),
            "k = 20" = check(m_gp),
            "k = 28" = check(m_gp28)), 2)

sea_level <- transform(d, elev_km = 0)
d$baseline <- fitted(m_gp, newdata = sea_level)[, "Estimate"]

world <- ne_countries(scale = "small", returnclass = "sf")
pts$baseline <- d$baseline

ggplot() +
  geom_sf(data = world, fill = "grey94", colour = NA) +
  geom_sf(data = pts[order(pts$baseline), ], aes(colour = baseline),
          size = 1.6) +
  scale_colour_viridis_c(name = "baseline probability") +
  coord_sf(crs = "+proj=eqearth")

nb <- knn2nb(knearneigh(as.matrix(d[, c("lon", "lat")]), k = 5, longlat = TRUE))
lw <- nb2listw(make.sym.nb(nb), style = "W")

p   <- fitted(m_gp)[, "Estimate"]
res <- (d$ejectives - p) / sqrt(p * (1 - p))        # Pearson residuals
after <- moran.mc(res, lw, nsim = 999)
after

round(fixef(m_gp)["elev_km", ], 2)

m0 <- glm(ejectives ~ elev_km, family = binomial, data = d)
x  <- seq(0, 4.5, by = 0.1)
pr <- predict(m0, newdata = data.frame(elev_km = x), se.fit = TRUE)
naive <- data.frame(elev_km = x, model = "naive glm", p = plogis(pr$fit),
                    lo = plogis(pr$fit - 1.96 * pr$se.fit),
                    hi = plogis(pr$fit + 1.96 * pr$se.fit))

b      <- as_draws_df(m_gp)$b_elev_km
eta    <- posterior_linpred(m_gp)        # draws by languages, logit scale
at_sea <- eta - outer(b, d$elev_km)      # every language moved to 0 km
p      <- sapply(x, function(xi) rowMeans(plogis(at_sea + b * xi)))
model  <- data.frame(elev_km = x, model = "spatial surface (GP)",
                     p  = colMeans(p),
                     lo = apply(p, 2, quantile, 0.025),
                     hi = apply(p, 2, quantile, 0.975))

ggplot(rbind(naive, model), aes(elev_km, p, colour = model, fill = model)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.15, colour = NA) +
  geom_line(linewidth = 0.9) +
  scale_colour_manual(values = c("grey40", "#2A78D6"),
                      aesthetics = c("colour", "fill"), name = NULL) +
  labs(x = "elevation (km)", y = "probability of ejectives")
