library(ape)
library(brms)
library(sf)
library(ggplot2)

base <- "https://rehan-muh.github.io/tutorials/data/"
d    <- read.csv(paste0(base, "languages.csv"))
tree <- read.tree(paste0(base, "glottolog_tree.nwk"))
d$elev_km <- d$elevation / 1000

A   <- vcv(tree, corr = TRUE)
pts <- st_as_sf(d, coords = c("lon", "lat"), crs = 4326)
xy  <- st_coordinates(st_transform(pts, "+proj=eqearth")) / 1e6
d$x <- xy[, 1]
d$y <- xy[, 2]

options(mc.cores = 4, brms.backend = "cmdstanr")
dir.create("fits", showWarnings = FALSE)

priors <- prior(normal(0, 1), class = b) +
          prior(exponential(1), class = sd) +
          prior(exponential(1), class = sdgp)

joint <- function(outcome, family, name,
                  control = list(adapt_delta = 0.95)) {
  f <- paste(outcome, "~ elev_km + (1 | gr(glottocode, cov = A)) +",
             "gp(x, y, k = 20, c = 5/4)")
  brm(as.formula(f), data = d, data2 = list(A = A),
      family = family, prior = priors, control = control,
      seed = 1, file = paste0("fits/", name))
}

grid  <- seq(0, 4.5, by = 0.5)
draws <- seq(1, 4000, by = 20)             # 200 of the 4,000 draws

averaged <- function(fit) {
  sapply(grid, function(km) {
    at <- transform(d, elev_km = km)
    rowMeans(posterior_epred(fit, newdata = at, draw_ids = draws))
  })                                       # draws by elevations
}
band <- function(p, label) {
  data.frame(elev_km = grid, model = label, fit = colMeans(p),
             lo = apply(p, 2, quantile, 0.025),
             hi = apply(p, 2, quantile, 0.975))
}
two_lines <- function(lines, y) {
  ggplot(lines, aes(elev_km, fit, colour = model, fill = model)) +
    geom_point(data = d, aes(elev_km, .data[[y]]), inherit.aes = FALSE,
               colour = "grey80", size = 0.8) +
    geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.15, colour = NA) +
    geom_line(linewidth = 0.9) +
    scale_colour_manual(values = c("grey45", "grey10"),
                        aesthetics = c("colour", "fill"), name = NULL)
}

naive_c <- MASS::glm.nb(n_consonants ~ elev_km, data = d)
m_count <- joint("n_consonants", poisson(), "o_count",
                 control = list(adapt_delta = 0.99))

est_c <- rbind(naive = coef(summary(naive_c))["elev_km", 1:2],
               joint = fixef(m_count)["elev_km", 1:2])
round(est_c, 3)

round(exp(fixef(m_count)["elev_km", c("Estimate", "Q2.5", "Q97.5")]), 3)

pr <- predict(naive_c, newdata = data.frame(elev_km = grid), se.fit = TRUE)
lines_c <- rbind(
  data.frame(elev_km = grid, model = "naive", fit = exp(pr$fit),
             lo = exp(pr$fit - 1.96 * pr$se.fit),
             hi = exp(pr$fit + 1.96 * pr$se.fit)),
  band(averaged(m_count), "phylogeny + space"))

two_lines(lines_c, "n_consonants") +
  coord_cartesian(ylim = c(10, 60)) +
  labs(x = "elevation (km)", y = "number of consonants")

d$cons_class <- cut(d$n_consonants, breaks = c(0, 14, 18, 25, 33, Inf),
                    labels = c("small", "moderately small", "average",
                               "moderately large", "large"),
                    ordered_result = TRUE)
table(d$cons_class)

naive_o <- MASS::polr(cons_class ~ elev_km, data = d, Hess = TRUE)
m_ord   <- joint("cons_class", cumulative("logit"), "o_ord")

est_o <- rbind(naive = coef(summary(naive_o))["elev_km", 1:2],
               joint = fixef(m_ord)["elev_km", 1:2])
round(est_o, 3)

classes <- levels(d$cons_class)

joint_o <- do.call(rbind, lapply(grid, function(km) {
  p <- posterior_epred(m_ord, newdata = transform(d, elev_km = km),
                       draw_ids = draws)    # draws by languages by classes
  p <- apply(p, c(1, 3), mean)              # draws by classes
  data.frame(elev_km = km, class = classes, p = colMeans(p),
             lo = apply(p, 2, quantile, 0.025),
             hi = apply(p, 2, quantile, 0.975))
}))
naive_p <- predict(naive_o, newdata = data.frame(elev_km = grid),
                   type = "probs")          # elevations by classes
naive_lines <- data.frame(elev_km = rep(grid, times = 5),
                          class = rep(classes, each = length(grid)),
                          p = as.vector(naive_p))
joint_o$class     <- factor(joint_o$class, levels = classes)
naive_lines$class <- factor(naive_lines$class, levels = classes)

ggplot(joint_o, aes(elev_km, p)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), fill = "grey10", alpha = 0.15) +
  geom_line(colour = "grey10", linewidth = 0.9) +
  geom_line(data = naive_lines, linetype = "22", colour = "grey35") +
  facet_wrap(~class, nrow = 1) +
  labs(x = "elevation (km)", y = "probability of the class")

d$log_cons <- log(d$n_consonants)
naive_g <- lm(log_cons ~ elev_km, data = d)

A <- A[d$glottocode, d$glottocode]         # fcor() matches rows by position

p_gauss <- prior(normal(0, 1), class = b) +
           prior(exponential(1), class = sigma) +
           prior(exponential(1), class = sdgp)

m_gauss <- brm(log_cons ~ elev_km + fcor(A) + gp(x, y, k = 20, c = 5/4),
               data = d, data2 = list(A = A), family = gaussian(),
               prior = p_gauss, control = list(adapt_delta = 0.95),
               seed = 1, file = "fits/o_gauss")

est_g <- rbind(naive = coef(summary(naive_g))["elev_km", 1:2],
               joint = fixef(m_gauss)["elev_km", 1:2])
round(est_g, 3)

sds <- c("sigma", "sdgp_gpxy")
round(posterior_summary(m_gauss, variable = sds)[, c(1, 3, 4)], 2)

pr <- predict(naive_g, newdata = data.frame(elev_km = grid), se.fit = TRUE)
lines_g <- rbind(
  data.frame(elev_km = grid, model = "naive", fit = pr$fit,
             lo = pr$fit - 1.96 * pr$se.fit, hi = pr$fit + 1.96 * pr$se.fit),
  band(averaged(m_gauss), "phylogeny + space"))

two_lines(lines_g, "log_cons") +
  labs(x = "elevation (km)", y = "log number of consonants")

ratios <- data.frame(
  outcome = c("count", "ordered classes", "continuous (log)"),
  naive   = c(est_c[1, 1] / est_c[1, 2], est_o[1, 1] / est_o[1, 2],
              est_g[1, 1] / est_g[1, 2]),
  joint   = c(est_c[2, 1] / est_c[2, 2], est_o[2, 1] / est_o[2, 2],
              est_g[2, 1] / est_g[2, 2]))
ratios$naive <- round(ratios$naive, 1)
ratios$joint <- round(ratios$joint, 1)
ratios
