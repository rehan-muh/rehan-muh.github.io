library(mgcv)
library(ape)
library(sf)
library(rnaturalearth)
library(ggplot2)

base <- "https://rehan-muh.github.io/tutorials/data/"
d    <- read.csv(paste0(base, "languages.csv"))
tree <- read.tree(paste0(base, "glottolog_tree.nwk"))
d$elev_km <- d$elevation / 1000

d$family     <- factor(d$family)
d$glottocode <- factor(d$glottocode, levels = tree$tip.label)

g_fam <- gam(ejectives ~ elev_km + s(family, bs = "re"),
             family = binomial, data = d, method = "REML")

summary(g_fam)$p.table
gam.vcomp(g_fam)

A <- vcv(tree, corr = TRUE)
Q <- solve(A)
dimnames(Q) <- dimnames(A)

g_phy <- gam(ejectives ~ elev_km +
               s(glottocode, bs = "mrf", xt = list(penalty = Q)),
             family = binomial, data = d, method = "REML")

summary(g_phy)$p.table
gam.vcomp(g_phy)

g_sos <- gam(ejectives ~ elev_km + s(lat, lon, bs = "sos", k = 60),
             family = binomial, data = d, method = "REML")

summary(g_sos)$p.table
summary(g_sos)$s.table

k.check(g_sos)

world <- ne_countries(scale = "small", returnclass = "sf")
sf_use_s2(FALSE)

grid <- expand.grid(lon = seq(-179, 179, 2), lat = seq(-55, 75, 2), elev_km = 0)
grid <- st_as_sf(grid, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
grid <- grid[lengths(st_intersects(grid, world)) > 0, ]

grid$surface <- predict(g_sos, newdata = grid, type = "terms",
                        terms = "s(lat,lon)")[, 1]

ggplot() +
  geom_sf(data = world, fill = "grey94", colour = NA) +
  geom_sf(data = grid, aes(colour = surface), size = 1.5, shape = 15) +
  scale_colour_gradient2(low = "#2A78D6", mid = "#F0EFEC", high = "#E34948",
                         name = "spatial effect") +
  coord_sf(crs = "+proj=eqearth")

g_joint <- gam(ejectives ~ elev_km +
                 s(glottocode, bs = "mrf", xt = list(penalty = Q)) +
                 s(lat, lon, bs = "sos", k = 60),
               family = binomial, data = d, method = "REML")

summary(g_joint)$p.table
summary(g_joint)$s.table

fits <- list("family intercept" = g_fam, "phylogeny" = g_phy,
             "space: sphere spline" = g_sos, "phylogeny + space" = g_joint)

round(t(sapply(fits, function(g) summary(g)$p.table["elev_km", 1:3])), 2)

x  <- seq(0, 4.5, by = 0.1)
m0 <- glm(ejectives ~ elev_km, family = binomial, data = d)

line_of <- function(g, label) {
  b      <- coef(g)[["elev_km"]]
  at_sea <- predict(g) - b * d$elev_km    # every language moved to 0 km
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

round(sqrt(diag(vcov(g_joint, unconditional = TRUE))["elev_km"]), 2)
