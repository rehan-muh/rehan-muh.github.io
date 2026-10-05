library(ape)
library(INLA)
library(fmesher)
library(mgcv)
library(spdep)
library(sf)
library(ggplot2)

base <- "https://rehan-muh.github.io/tutorials/data/"
d    <- read.csv(paste0(base, "languages.csv"))
tree <- read.tree(paste0(base, "glottolog_tree.nwk"))
d$elev_km <- d$elevation / 1000
m0 <- glm(ejectives ~ elev_km, family = binomial, data = d)

# the tree as a matrix and its inverse (phylogeny chapters)
A <- vcv(tree, corr = TRUE)[d$glottocode, d$glottocode]
Q <- solve(A)
dimnames(Q) <- dimnames(A)

# indices and factors for INLA and mgcv
d$fam_id <- as.integer(factor(d$family))
d$phy_id <- seq_len(nrow(d))
d$sp_id  <- seq_len(nrow(d))
d$family_f     <- factor(d$family)
d$glottocode_f <- factor(d$glottocode, levels = rownames(Q))

# the neighbour graph (space chapters)
coords <- as.matrix(d[, c("lon", "lat")])
W <- nb2mat(make.sym.nb(knn2nb(knearneigh(coords, k = 5, longlat = TRUE))),
            style = "B")

# the mesh on the globe and the surface defined on it (space chapters)
to_sphere <- function(lon, lat) {
  rad <- pi / 180
  cbind(cos(lat * rad) * cos(lon * rad),
        cos(lat * rad) * sin(lon * rad),
        sin(lat * rad))
}
mesh <- fm_rcdt_2d(globe = 12)
spde <- inla.spde2.pcmatern(mesh, prior.range = c(0.05, 0.05),
                            prior.sigma = c(3, 0.05))
stack <- inla.stack(
  data    = list(y = d$ejectives),
  A       = list(fm_basis(mesh, loc = to_sphere(d$lon, d$lat)), 1),
  effects = list(field = seq_len(mesh$n),
                 data.frame(intercept = 1, elev_km = d$elev_km,
                            phy_id = d$phy_id)))

# priors and helpers for INLA
pc_sd <- list(prec = list(prior = "pc.prec", param = c(3, 0.05)))
fixed <- list(mean = 0, prec = 1)
slope_of <- function(fit) {
  cols <- c("mean", "sd", "0.025quant", "0.975quant")
  round(unlist(fit$summary.fixed["elev_km", cols]), 2)
}
sd_of <- function(fit, name) {
  m <- inla.tmarginal(function(p) 1 / sqrt(p), fit$marginals.hyperpar[[name]])
  z <- inla.zmarginal(m, silent = TRUE)
  round(c(mean = z$mean, lower = z$quant0.025, upper = z$quant0.975), 2)
}
classic_of <- function(fit) {
  args <- fit$.args
  args$inla.mode <- "classic"
  do.call(inla, args)
}

i_joint <- inla(y ~ 0 + intercept + elev_km + f(field, model = spde) +
                  f(phy_id, model = "generic0", Cmatrix = Q, hyper = pc_sd),
                family = "binomial", data = inla.stack.data(stack),
                control.predictor = list(A = inla.stack.A(stack)),
                control.fixed = fixed)

rbind(default = slope_of(i_joint), classic = slope_of(classic_of(i_joint)))

ci <- c("mean", "0.025quant", "0.975quant")
sd_of(i_joint, "Precision for phy_id")
round(i_joint$summary.hyperpar[c("Range for field", "Stdev for field"), ci], 2)

g_joint <- gam(ejectives ~ elev_km +
                 s(glottocode_f, bs = "mrf", xt = list(penalty = Q)) +
                 s(lat, lon, bs = "sos", k = 60),
               family = binomial, data = d, method = "REML")

summary(g_joint)$p.table
summary(g_joint)$s.table

round(sqrt(diag(vcov(g_joint, unconditional = TRUE))["elev_km"]), 2)

library(brms)
options(mc.cores = 4, brms.backend = "cmdstanr")
dir.create("fits", showWarnings = FALSE)

pts <- st_as_sf(d, coords = c("lon", "lat"), crs = 4326)
xy  <- st_coordinates(st_transform(pts, "+proj=eqearth")) / 1e6
d$x <- xy[, 1]
d$y <- xy[, 2]

p_b <- prior(normal(0, 1), class = b)
b_joint <- brm(ejectives ~ elev_km + (1 | gr(glottocode, cov = A)) +
                 gp(x, y, k = 20, c = 5/4),
               data = d, data2 = list(A = A), family = bernoulli(),
               prior = p_b + prior(exponential(1), class = sd) +
                       prior(exponential(1), class = sdgp),
               control = list(adapt_delta = 0.95),
               seed = 1, file = "fits/m_joint")

x <- seq(0, 4.5, by = 0.1)
averaged <- function(eta, b) {
  at_sea <- eta - b * d$elev_km           # every language moved to 0 km
  sapply(x, function(xi) mean(plogis(at_sea + b * xi)))
}

draws <- as_draws_df(b_joint)
eta   <- posterior_linpred(b_joint)
p     <- sapply(x, function(xi) {
  b <- draws$b_elev_km
  rowMeans(plogis(eta - outer(b, d$elev_km) + b * xi))
})
band <- data.frame(elev_km = x, lo = apply(p, 2, quantile, 0.025),
                   hi = apply(p, 2, quantile, 0.975))

lines <- rbind(
  data.frame(elev_km = x, engine = "brms", p = colMeans(p)),
  data.frame(elev_km = x, engine = "INLA",
             p = averaged(i_joint$summary.linear.predictor$mean[1:nrow(d)],
                          i_joint$summary.fixed["elev_km", "mean"])),
  data.frame(elev_km = x, engine = "mgcv",
             p = averaged(predict(g_joint), coef(g_joint)[["elev_km"]])))
naive <- data.frame(elev_km = x)
naive$p <- predict(m0, newdata = naive, type = "response")

ggplot(lines, aes(elev_km, p)) +
  geom_ribbon(data = band, aes(ymin = lo, ymax = hi, y = NULL),
              fill = "grey10", alpha = 0.12) +
  geom_line(aes(linetype = engine), colour = "grey10", linewidth = 0.9) +
  geom_line(data = naive, linetype = "22", colour = "grey35") +
  scale_linetype_manual(values = c("solid", "42", "13"), name = NULL) +
  labs(x = "elevation (km)", y = "probability of ejectives")

# INLA: family, phylogeny, neighbours (classic mode), surface
i_fam <- inla(ejectives ~ elev_km + f(fam_id, model = "iid", hyper = pc_sd),
              family = "binomial", data = d, control.fixed = fixed)
i_phy <- inla(ejectives ~ elev_km +
                f(phy_id, model = "generic0", Cmatrix = Q, hyper = pc_sd),
              family = "binomial", data = d, control.fixed = fixed)
i_bym <- inla(ejectives ~ elev_km +
                f(sp_id, model = "bym2", graph = W, scale.model = TRUE,
                  hyper = c(pc_sd, list(phi = list(prior = "pc",
                                                   param = c(0.5, 0.5))))),
              family = "binomial", data = d, control.fixed = fixed,
              inla.mode = "classic")
i_spde <- inla(y ~ 0 + intercept + elev_km + f(field, model = spde),
               family = "binomial", data = inla.stack.data(stack),
               control.predictor = list(A = inla.stack.A(stack)),
               control.fixed = fixed)

# mgcv: family, phylogeny, surface
g_fam <- gam(ejectives ~ elev_km + s(family_f, bs = "re"),
             family = binomial, data = d, method = "REML")
g_phy <- gam(ejectives ~ elev_km +
               s(glottocode_f, bs = "mrf", xt = list(penalty = Q)),
             family = binomial, data = d, method = "REML")
g_sos <- gam(ejectives ~ elev_km + s(lat, lon, bs = "sos", k = 60),
             family = binomial, data = d, method = "REML")

# brms: the saved fits of the earlier chapters
dimnames(W) <- list(d$glottocode, d$glottocode)
saved <- function(formula, prior, name, ...) {
  brm(formula, data = d, family = bernoulli(), prior = prior, seed = 1,
      file = paste0("fits/", name), ...)
}
b_fam <- saved(ejectives ~ elev_km + (1 | family),
               p_b + prior(exponential(1), class = sd), "m_fam")
b_phy <- saved(ejectives ~ elev_km + (1 | gr(glottocode, cov = A)),
               p_b + prior(exponential(1), class = sd), "m_phy",
               data2 = list(A = A))
b_gp  <- saved(ejectives ~ elev_km + gp(x, y, k = 20, c = 5/4),
               p_b + prior(exponential(1), class = sdgp), "m_gp",
               control = list(adapt_delta = 0.95))
b_bym <- saved(ejectives ~ elev_km + car(W, gr = glottocode, type = "bym2"),
               p_b + prior(exponential(1), class = sdcar), "m_bym",
               data2 = list(W = W))

from_brms <- function(fit) unname(fixef(fit)["elev_km", ])
from_inla <- function(fit) unname(slope_of(fit))
from_mgcv <- function(fit) {
  b <- summary(fit)$p.table["elev_km", 1:2]
  unname(c(b, b[1] - 1.96 * b[2], b[1] + 1.96 * b[2]))
}
entry <- function(structure, engine, v) data.frame(structure, engine, t(v))
grid <- rbind(
  entry("family intercept", "brms", from_brms(b_fam)),
  entry("family intercept", "INLA", from_inla(i_fam)),
  entry("family intercept", "mgcv", from_mgcv(g_fam)),
  entry("phylogeny", "brms", from_brms(b_phy)),
  entry("phylogeny", "INLA", from_inla(i_phy)),
  entry("phylogeny", "mgcv", from_mgcv(g_phy)),
  entry("space: surface", "brms", from_brms(b_gp)),
  entry("space: surface", "INLA", from_inla(i_spde)),
  entry("space: surface", "mgcv", from_mgcv(g_sos)),
  entry("space: neighbours", "brms", from_brms(b_bym)),
  entry("space: neighbours", "INLA", from_inla(i_bym)),
  entry("phylogeny + space", "brms", from_brms(b_joint)),
  entry("phylogeny + space", "INLA", from_inla(i_joint)),
  entry("phylogeny + space", "mgcv", from_mgcv(g_joint)))
names(grid)[3:6] <- c("slope", "error", "lower", "upper")
grid$ratio <- grid$slope / grid$error
cbind(grid[1:2], round(grid[3:7], 2))

kind_of <- function(structure) {
  ifelse(grepl("+", structure, fixed = TRUE), "both",
         ifelse(grepl("space", structure), "space", "ancestry"))
}
hues <- c(ancestry = "#1BAF7A", space = "#2A78D6", both = "grey10")
grid$kind <- kind_of(grid$structure)
grid$structure <- factor(grid$structure, levels = rev(unique(grid$structure)))

ggplot(grid, aes(slope, structure, colour = kind, shape = engine)) +
  geom_vline(xintercept = 0, linetype = "13", colour = "grey35") +
  geom_vline(xintercept = coef(m0)[2], linetype = "22", colour = "grey35") +
  geom_linerange(aes(xmin = lower, xmax = upper), linewidth = 0.6,
                 position = position_dodge(width = 0.6)) +
  geom_point(size = 2.6, position = position_dodge(width = 0.6)) +
  scale_colour_manual(values = hues, guide = "none") +
  scale_shape_manual(values = c(brms = 16, INLA = 17, mgcv = 15), name = NULL) +
  labs(x = "log-odds of ejectives per km of elevation", y = NULL)
