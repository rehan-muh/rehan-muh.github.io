library(ape)
library(nlme)
library(phylolm)
library(mgcv)
library(ggplot2)

base <- "https://rehan-muh.github.io/tutorials/data/"
d    <- read.csv(paste0(base, "languages.csv"))
tree <- read.tree(paste0(base, "glottolog_tree.nwk"))
d$family    <- factor(d$family)
rownames(d) <- d$glottocode
n <- nrow(d)

dir.create("fits", showWarnings = FALSE)

A  <- vcv(tree, corr = TRUE)[d$glottocode, d$glottocode]
km <- sp::spDists(as.matrix(d[, c("lon", "lat")]), longlat = TRUE)
S  <- exp(-km / 1000)

# multiplying independent noise by these factors gives noise with the
# correlation structure of the tree (LA) or of the map (LS)
LA <- t(chol(A))
LS <- t(chol(S + diag(1e-8, n)))

simulate_world <- function(beta, shared = FALSE) {
  field <- function() 0.6 * LA %*% rnorm(n) + 0.6 * LS %*% rnorm(n)
  fx <- field()                          # what shapes the predictor
  fy <- if (shared) fx else field()      # what shapes the outcome
  w  <- d
  w$x <- as.vector(fx + 0.5 * rnorm(n))
  w$y <- as.vector(beta * w$x + fy + 0.5 * rnorm(n))
  w
}

one_per_family <- function(w) {
  pick <- tapply(seq_len(nrow(w)), w$family,
                 function(i) i[sample.int(length(i), 1)])
  w[pick, ]
}
thinned <- function(w, min_km) {
  keep <- integer()
  for (i in sample.int(nrow(w))) {
    if (all(km[i, keep] >= min_km)) keep <- c(keep, i)
  }
  w[keep, ]
}

fit_all <- function(w) {
  ols <- function(data) summary(lm(y ~ x, data))$coefficients["x", 1:2]
  fam <- lme(y ~ x, random = ~ 1 | family, data = w)
  phy <- phylolm(y ~ x, data = w, phy = tree, model = "lambda")
  spa <- gam(y ~ x + s(lat, lon, bs = "sos", k = 40),
             data = w, method = "REML")
  jnt <- gam(y ~ x + s(family, bs = "re") + s(lat, lon, bs = "sos", k = 40),
             data = w, method = "REML")
  est <- rbind(
    "naive OLS"         = ols(w),
    "one per family"    = ols(one_per_family(w)),
    "thinned to 500 km" = ols(thinned(w, 500)),
    "family intercept"  = summary(fam)$tTable["x", 1:2],
    "phylogeny (PGLS)"  = summary(phy)$coefficients["x", 1:2],
    "space (spline)"    = summary(spa)$p.table["x", 1:2],
    "family + space"    = summary(jnt)$p.table["x", 1:2])
  data.frame(method = rownames(est), est = est[, 1], se = est[, 2])
}

set.seed(10)
w   <- simulate_world(beta = 0.3)
one <- fit_all(w)
cbind(one["method"], round(one[c("est", "se")], 3))

kind_of <- function(method) {
  ifelse(method == "naive OLS", "naive",
  ifelse(method %in% c("one per family", "thinned to 500 km"), "sampling",
  ifelse(grepl("+", method, fixed = TRUE), "both",
  ifelse(grepl("space", method), "space", "ancestry"))))
}
hues <- c(naive = "grey45", sampling = "#EDA100", ancestry = "#1BAF7A",
          space = "#2A78D6", both = "grey10")
one$kind <- factor(kind_of(one$method), levels = names(hues))

ggplot(w, aes(x, y)) +
  geom_point(colour = "grey80", size = 0.8) +
  geom_abline(data = one, aes(intercept = 0, slope = est, colour = kind),
              linewidth = 0.6) +
  geom_abline(intercept = 0, slope = 0.3, linewidth = 1.1) +
  scale_colour_manual(values = hues, name = NULL) +
  labs(x = "simulated predictor", y = "simulated outcome")

nsim <- 100
scenarios <- data.frame(
  scenario = c("no effect", "true effect", "shared history"),
  beta     = c(0, 0.3, 0),
  shared   = c(FALSE, FALSE, TRUE))

if (file.exists("fits/simulation.rds")) {
  sims <- readRDS("fits/simulation.rds")
} else {
  set.seed(2026)
  sims <- do.call(rbind, lapply(seq_len(nrow(scenarios)), function(i) {
    do.call(rbind, lapply(seq_len(nsim), function(r) {
      w <- simulate_world(scenarios$beta[i], scenarios$shared[i])
      cbind(scenario = scenarios$scenario[i], truth = scenarios$beta[i],
            world = r, fit_all(w))
    }))
  }))
  saveRDS(sims, "fits/simulation.rds")
}

sims$method   <- factor(sims$method, levels = one$method)
sims$scenario <- factor(sims$scenario, levels = scenarios$scenario)
sims$lower    <- sims$est - 1.96 * sims$se
sims$upper    <- sims$est + 1.96 * sims$se
sims$reject   <- sims$lower > 0 | sims$upper < 0
sims$covered  <- sims$lower <= sims$truth & sims$truth <= sims$upper
sims$width    <- sims$upper - sims$lower

score <- aggregate(cbind(est, reject, covered, width) ~ method + scenario,
                   data = sims, FUN = mean)
names(score)[3:6] <- c("mean_estimate", "share_rejecting", "coverage",
                       "mean_width")

null <- subset(score, scenario == "no effect")
cbind(null["method"], round(null[, 3:6], 3))

null$kind <- factor(kind_of(as.character(null$method)), levels = names(hues))
null$method <- factor(null$method, levels = rev(levels(sims$method)))

ggplot(null, aes(share_rejecting, method, colour = kind)) +
  geom_vline(xintercept = 0.05, linetype = "22", colour = "grey35") +
  geom_segment(aes(x = 0, xend = share_rejecting, yend = method),
               linewidth = 0.8) +
  geom_point(size = 2.8) +
  scale_colour_manual(values = hues, guide = "none") +
  scale_x_continuous(labels = function(v) paste0(100 * v, "%")) +
  labs(x = "worlds with a false positive", y = NULL)



effect <- subset(score, scenario == "true effect")
cbind(effect["method"], round(effect[, 3:6], 3))

lines <- subset(sims, scenario == "true effect")
lines$kind <- factor(kind_of(as.character(lines$method)), levels = names(hues))

ggplot(lines) +
  geom_abline(aes(intercept = 0, slope = est, colour = kind),
              alpha = 0.3, linewidth = 0.3) +
  geom_abline(intercept = 0, slope = 0.3, linewidth = 0.9) +
  scale_colour_manual(values = hues, guide = "none") +
  scale_x_continuous(limits = c(-2, 2)) +
  scale_y_continuous(limits = c(-1.2, 1.2)) +
  facet_wrap(~method, nrow = 2) +
  labs(x = "simulated predictor", y = "simulated outcome")



shared <- subset(score, scenario == "shared history")
cbind(shared["method"], round(shared[, 3:6], 3))

conf <- subset(sims, scenario == "shared history")
conf$kind <- factor(kind_of(as.character(conf$method)), levels = names(hues))
conf$method <- factor(conf$method, levels = rev(levels(sims$method)))

ggplot(conf, aes(est, method, colour = kind)) +
  geom_vline(xintercept = 0, linetype = "22", colour = "grey35") +
  geom_jitter(height = 0.25, width = 0, alpha = 0.35, size = 0.8) +
  scale_colour_manual(values = hues, guide = "none") +
  labs(x = "estimated slope (truth is 0)", y = NULL)
