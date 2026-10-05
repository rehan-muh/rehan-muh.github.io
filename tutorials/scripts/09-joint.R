library(ape)
library(brms)
library(sf)
library(spdep)
library(ggplot2)

base <- "https://rehan-muh.github.io/tutorials/data/"
d    <- read.csv(paste0(base, "languages.csv"))
tree <- read.tree(paste0(base, "glottolog_tree.nwk"))
d$elev_km <- d$elevation / 1000

A <- vcv(tree, corr = TRUE)                                   # phylogeny

pts <- st_as_sf(d, coords = c("lon", "lat"), crs = 4326)      # surface
xy  <- st_coordinates(st_transform(pts, "+proj=eqearth")) / 1e6
d$x <- xy[, 1]
d$y <- xy[, 2]

coords <- as.matrix(d[, c("lon", "lat")])                     # neighbours
W <- nb2mat(make.sym.nb(knn2nb(knearneigh(coords, k = 5, longlat = TRUE))),
            style = "B")
dimnames(W) <- list(d$glottocode, d$glottocode)

options(mc.cores = 4, brms.backend = "cmdstanr")
dir.create("fits", showWarnings = FALSE)

km   <- sp::spDists(coords, longlat = TRUE)
pair <- upper.tri(A)
same_family <- A > 0

round(c(same      = median(km[pair & same_family]),
        different = median(km[pair & !same_family])))

priors <- prior(normal(0, 1), class = b) +
          prior(exponential(1), class = sd) +
          prior(exponential(1), class = sdgp)

m_joint <- brm(ejectives ~ elev_km +
                 (1 | gr(glottocode, cov = A)) +
                 gp(x, y, k = 20, c = 5/4),
               data = d, data2 = list(A = A),
               family = bernoulli(), prior = priors,
               control = list(adapt_delta = 0.95),
               seed = 1, file = "fits/m_joint")

summary(m_joint)

m0 <- glm(ejectives ~ elev_km, family = binomial, data = d)

p_sd  <- prior(normal(0, 1), class = b) + prior(exponential(1), class = sd)
p_gp  <- prior(normal(0, 1), class = b) + prior(exponential(1), class = sdgp)
p_car <- prior(normal(0, 1), class = b) + prior(exponential(1), class = sdcar)

m_fam <- brm(ejectives ~ elev_km + (1 | family),
             data = d, family = bernoulli(), prior = p_sd,
             seed = 1, file = "fits/m_fam")
m_phy <- brm(ejectives ~ elev_km + (1 | gr(glottocode, cov = A)),
             data = d, data2 = list(A = A), family = bernoulli(), prior = p_sd,
             seed = 1, file = "fits/m_phy")
m_gp  <- brm(ejectives ~ elev_km + gp(x, y, k = 20, c = 5/4),
             data = d, family = bernoulli(), prior = p_gp,
             control = list(adapt_delta = 0.95),
             seed = 1, file = "fits/m_gp")
m_bym <- brm(ejectives ~ elev_km + car(W, gr = glottocode, type = "bym2"),
             data = d, data2 = list(W = W), family = bernoulli(), prior = p_car,
             seed = 1, file = "fits/m_bym")

sd_row <- function(m, par) {
  posterior_summary(m, variable = par)[, c("Estimate", "Q2.5", "Q97.5")]
}
credit <- as.data.frame(rbind(
  "lineages, alone"        = sd_row(m_phy,   "sd_glottocode__Intercept"),
  "lineages, with surface" = sd_row(m_joint, "sd_glottocode__Intercept"),
  "surface, alone"         = sd_row(m_gp,    "sdgp_gpxy"),
  "surface, with lineages" = sd_row(m_joint, "sdgp_gpxy")))
round(credit, 2)

credit$term <- factor(rownames(credit), levels = rev(rownames(credit)))

ggplot(credit, aes(Estimate, term)) +
  geom_linerange(aes(xmin = Q2.5, xmax = Q97.5), linewidth = 0.8) +
  geom_point(size = 2.6) +
  labs(x = "standard deviation, logit scale", y = NULL)

fits <- list("family intercept" = m_fam, "phylogeny" = m_phy,
             "space: GP" = m_gp, "space: BYM2" = m_bym,
             "phylogeny + space" = m_joint)

naive  <- unname(c(coef(m0)[2], sqrt(vcov(m0)[2, 2]), confint.default(m0)[2, ]))
slopes <- t(sapply(fits, function(m) fixef(m)["elev_km", ]))
slopes <- as.data.frame(rbind("naive glm" = naive, slopes))
slopes$ratio <- slopes$Estimate / slopes$Est.Error
round(slopes, 2)

kind_of <- function(model) {
  ifelse(grepl("+", model, fixed = TRUE), "both",
         ifelse(grepl("space", model), "space", "ancestry"))
}
hues <- c(naive = "grey45", ancestry = "#1BAF7A", space = "#2A78D6",
          both = "grey10")

slopes$model <- factor(rownames(slopes), levels = rev(rownames(slopes)))
slopes$kind  <- c("naive", kind_of(rownames(slopes)[-1]))

ggplot(slopes, aes(Estimate, model, colour = kind)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey45") +
  geom_linerange(aes(xmin = Q2.5, xmax = Q97.5), linewidth = 0.8) +
  geom_point(size = 2.6) +
  scale_colour_manual(values = hues, guide = "none") +
  labs(x = "log-odds of ejectives per km of elevation", y = NULL)

x <- seq(0, 4.5, by = 0.1)

averaged <- function(m, label) {
  b      <- as_draws_df(m)$b_elev_km
  eta    <- posterior_linpred(m)            # draws by languages, logit scale
  at_sea <- eta - outer(b, d$elev_km)       # every language moved to 0 km
  p <- sapply(x, function(xi) rowMeans(plogis(at_sea + b * xi)))
  data.frame(elev_km = x, model = label, p = colMeans(p),
             lo = apply(p, 2, quantile, 0.025),
             hi = apply(p, 2, quantile, 0.975))
}
lines <- do.call(rbind, Map(averaged, fits, names(fits)))
lines$kind <- kind_of(lines$model)
lines$line <- lines$model

# a sixth panel with all five lines and no bands, for direct comparison
overlay <- transform(lines, model = "all five together", lo = NA, hi = NA)
lines   <- rbind(lines, overlay)
lines$model <- factor(lines$model, levels = c(names(fits), "all five together"))

naive <- data.frame(elev_km = x)
naive$p <- predict(m0, newdata = naive, type = "response")

ggplot(lines, aes(elev_km, p, group = line)) +
  geom_ribbon(aes(ymin = lo, ymax = hi, fill = kind), alpha = 0.18) +
  geom_line(aes(colour = kind), linewidth = 0.9) +
  geom_line(data = naive, aes(elev_km, p), inherit.aes = FALSE,
            linetype = "22", colour = "grey35") +
  scale_colour_manual(values = hues, aesthetics = c("colour", "fill"),
                      name = NULL) +
  facet_wrap(~model, nrow = 2) +
  labs(x = "elevation (km)", y = "probability of ejectives")

hypothesis(m_joint, "elev_km > 0")
