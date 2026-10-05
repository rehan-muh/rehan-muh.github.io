library(ape)
library(phytools)
library(spdep)
library(sf)
library(rnaturalearth)
library(ggplot2)

base <- "https://rehan-muh.github.io/tutorials/data/"
d    <- read.csv(paste0(base, "languages.csv"))
tree <- read.tree(paste0(base, "glottolog_tree.nwk"))

d$elev_km <- d$elevation / 1000
d[c(1, 120, 250, 380, 500), c("name", "family", "elevation", "ejectives")]

m0 <- glm(ejectives ~ elev_km, family = binomial, data = d)
round(summary(m0)$coefficients, 3)

grid <- data.frame(elev_km = seq(0, 4.5, by = 0.05))
pr   <- predict(m0, newdata = grid, se.fit = TRUE)
grid$p  <- plogis(pr$fit)
grid$lo <- plogis(pr$fit - 1.96 * pr$se.fit)
grid$hi <- plogis(pr$fit + 1.96 * pr$se.fit)

breaks <- c(0, 0.25, 0.5, 1, 1.5, 2, 3, 5)
d$band <- cut(d$elev_km, breaks, include.lowest = TRUE)
obs    <- aggregate(cbind(ejectives, elev_km) ~ band, data = d, FUN = mean)
obs$n  <- as.vector(table(d$band))

ggplot(grid, aes(elev_km, p)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.2) +
  geom_line(linewidth = 0.9) +
  geom_point(data = obs, aes(y = ejectives, size = n),
             shape = 21, fill = "white") +
  scale_size_area(max_size = 7, name = "languages in band") +
  labs(x = "elevation (km)", y = "probability of ejectives")

world <- ne_countries(scale = "small", returnclass = "sf")
pts   <- st_as_sf(d, coords = c("lon", "lat"), crs = 4326)

ggplot() +
  geom_sf(data = world, fill = "grey94", colour = NA) +
  geom_sf(data = pts, aes(fill = factor(ejectives)),
          shape = 21, size = 1.9, stroke = 0.3) +
  scale_fill_manual(values = c("0" = "white", "1" = "#EB6834"),
                    labels = c("no ejectives", "ejectives"), name = NULL) +
  coord_sf(crs = "+proj=eqearth")

ej <- subset(d, ejectives == 1)
head(sort(table(ej$family), decreasing = TRUE), 8)
length(unique(ej$family))

library(ggtree)

ggtree(tree, layout = "fan", open.angle = 8,
       linewidth = 0.2, colour = "grey45") %<+% d +
  geom_tippoint(aes(fill = factor(ejectives), size = factor(ejectives)),
                shape = 21, stroke = 0.25) +
  scale_fill_manual(values = c("0" = "white", "1" = "#EB6834"),
                    labels = c("no ejectives", "ejectives"), name = NULL) +
  scale_size_manual(values = c("0" = 0.9, "1" = 2.2), guide = "none")

set.seed(1)
fake_phy <- fastBM(tree, nsim = 1000)[d$glottocode, ]

p_value <- function(x) {
  summary(glm(d$ejectives ~ x, family = binomial))$coefficients[2, 4]
}
p_phy   <- apply(fake_phy, 2, p_value)
mean(p_phy < 0.05)

rad <- pi / 180
xyz <- cbind(cos(d$lat * rad) * cos(d$lon * rad),
             cos(d$lat * rad) * sin(d$lon * rad),
             sin(d$lat * rad))

fake_sp <- xyz %*% t(matrix(rnorm(3000), ncol = 3))
p_sp    <- apply(fake_sp, 2, p_value)
mean(p_sp < 0.05)

sims <- data.frame(
  p    = c(p_phy, p_sp),
  kind = rep(c("Traits evolved on the tree", "Random geographic gradients"),
             each = 1000))

ggplot(sims, aes(p)) +
  geom_histogram(breaks = seq(0, 1, 0.05), fill = "grey35", colour = "white") +
  geom_hline(yintercept = 50, linetype = "dashed") +
  facet_wrap(~kind) +
  labs(x = "p-value of the slope", y = "simulations out of 1,000")

coords <- as.matrix(d[, c("lon", "lat")])
nb <- knn2nb(knearneigh(coords, k = 5, longlat = TRUE))
nb <- make.sym.nb(nb)
lw <- nb2listw(nb, style = "W")

moran.mc(residuals(m0, type = "pearson"), lw, nsim = 999)



res <- setNames(residuals(m0, type = "pearson"), d$glottocode)
phylosig(tree, res, method = "lambda", test = TRUE)

log_cons <- setNames(log(d$n_consonants), d$glottocode)
phylosig(tree, log_cons, method = "lambda", test = TRUE)
