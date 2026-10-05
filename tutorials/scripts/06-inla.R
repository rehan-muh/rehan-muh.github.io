library(INLA)
library(fmesher)
library(ape)
library(spdep)
library(sf)
library(rnaturalearth)
library(ggplot2)

base <- "https://rehan-muh.github.io/tutorials/data/"
d    <- read.csv(paste0(base, "languages.csv"))
tree <- read.tree(paste0(base, "glottolog_tree.nwk"))
d$elev_km <- d$elevation / 1000

d$fam_id <- as.integer(factor(d$family))
d$phy_id <- seq_len(nrow(d))        # row i of the phylogenetic matrix
d$sp_id  <- seq_len(nrow(d))        # row i of the neighbour matrix

pc_sd <- list(prec = list(prior = "pc.prec", param = c(3, 0.05)))
fixed <- list(mean = 0, prec = 1)

sd_of <- function(fit, name) {
  m <- inla.tmarginal(function(p) 1 / sqrt(p), fit$marginals.hyperpar[[name]])
  z <- inla.zmarginal(m, silent = TRUE)
  round(c(mean = z$mean, lower = z$quant0.025, upper = z$quant0.975), 2)
}
slope_of <- function(fit) {
  cols <- c("mean", "sd", "0.025quant", "0.975quant")
  round(unlist(fit$summary.fixed["elev_km", cols]), 2)
}

i_fam <- inla(ejectives ~ elev_km + f(fam_id, model = "iid", hyper = pc_sd),
              family = "binomial", data = d, control.fixed = fixed)

slope_of(i_fam)
sd_of(i_fam, "Precision for fam_id")

A <- vcv(tree, corr = TRUE)[d$glottocode, d$glottocode]
Q <- solve(A)

i_phy <- inla(ejectives ~ elev_km +
                f(phy_id, model = "generic0", Cmatrix = Q, hyper = pc_sd),
              family = "binomial", data = d, control.fixed = fixed)

slope_of(i_phy)
sd_of(i_phy, "Precision for phy_id")

coords <- as.matrix(d[, c("lon", "lat")])
nb <- make.sym.nb(knn2nb(knearneigh(coords, k = 5, longlat = TRUE)))
W  <- nb2mat(nb, style = "B")

pc_bym <- list(prec = list(prior = "pc.prec", param = c(3, 0.05)),
               phi  = list(prior = "pc", param = c(0.5, 0.5)))

i_bym <- inla(ejectives ~ elev_km +
                f(sp_id, model = "bym2", graph = W, scale.model = TRUE,
                  hyper = pc_bym),
              family = "binomial", data = d, control.fixed = fixed)

slope_of(i_bym)
sd_of(i_bym, "Precision for sp_id")
ci <- c("mean", "0.025quant", "0.975quant")
round(i_bym$summary.hyperpar["Phi for sp_id", ci], 2)

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
                 data.frame(intercept = 1, elev_km = d$elev_km,
                            phy_id = d$phy_id)))

i_spde <- inla(y ~ 0 + intercept + elev_km + f(field, model = spde),
               family = "binomial", data = inla.stack.data(stack),
               control.predictor = list(A = inla.stack.A(stack)),
               control.fixed = fixed)

slope_of(i_spde)
round(i_spde$summary.hyperpar[, ci], 2)



world <- ne_countries(scale = "small", returnclass = "sf")
sf_use_s2(FALSE)

grid <- expand.grid(lon = seq(-179, 179, 2), lat = seq(-55, 75, 2))
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

i_joint <- inla(y ~ 0 + intercept + elev_km + f(field, model = spde) +
                  f(phy_id, model = "generic0", Cmatrix = Q, hyper = pc_sd),
                family = "binomial", data = inla.stack.data(stack),
                control.predictor = list(A = inla.stack.A(stack)),
                control.fixed = fixed)

slope_of(i_joint)
sd_of(i_joint, "Precision for phy_id")
round(i_joint$summary.hyperpar[c("Range for field", "Stdev for field"), ci], 2)

fits <- list("family intercept" = i_fam, "phylogeny" = i_phy,
             "space: BYM2" = i_bym, "space: SPDE" = i_spde,
             "phylogeny + space" = i_joint)

classic <- lapply(fits, function(fit) {
  args <- fit$.args
  args$inla.mode <- "classic"
  do.call(inla, args)
})

data.frame(default = sapply(fits, function(f) slope_of(f)[["mean"]]),
           classic = sapply(classic, function(f) slope_of(f)[["mean"]]))

fits[["space: BYM2"]] <- classic[["space: BYM2"]]

out <- as.data.frame(t(sapply(fits, slope_of)))
out$seconds <- round(sapply(fits, function(f) f$cpu.used[["Total"]]), 1)
out

x  <- seq(0, 4.5, by = 0.1)
m0 <- glm(ejectives ~ elev_km, family = binomial, data = d)

line_of <- function(fit, label) {
  eta    <- fit$summary.linear.predictor$mean[seq_len(nrow(d))]
  b      <- fit$summary.fixed["elev_km", "mean"]
  at_sea <- eta - b * d$elev_km           # every language moved to 0 km
  data.frame(elev_km = x, model = label,
             p = sapply(x, function(xi) mean(plogis(at_sea + b * xi))))
}
lines <- do.call(rbind, Map(line_of, fits, names(fits)))
lines$model <- factor(lines$model, levels = names(fits))

kind_of <- function(model) {
  ifelse(grepl("+", model, fixed = TRUE), "both",
         ifelse(grepl("space", model), "space", "ancestry"))
}
hues <- c(naive = "grey45", ancestry = "#1BAF7A", space = "#2A78D6",
          both = "grey10")
lines$kind <- kind_of(lines$model)

naive <- data.frame(elev_km = x)
naive$p <- predict(m0, newdata = naive, type = "response")

ggplot(lines, aes(elev_km, p)) +
  geom_line(aes(colour = kind), linewidth = 0.9) +
  geom_line(data = naive, linetype = "22", colour = "grey35") +
  scale_colour_manual(values = hues, name = NULL) +
  facet_wrap(~model, nrow = 2) +
  labs(x = "elevation (km)", y = "probability of ejectives")
