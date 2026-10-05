library(ape)
library(INLA)
library(mgcv)
library(phylolm)
library(ggplot2)

base <- "https://rehan-muh.github.io/tutorials/data/"
d    <- read.csv(paste0(base, "languages.csv"))
tree <- read.tree(paste0(base, "glottolog_tree.nwk"))
d$elev_km <- d$elevation / 1000

m0 <- glm(ejectives ~ elev_km, family = binomial, data = d)

A <- vcv(tree, corr = TRUE)[d$glottocode, d$glottocode]
Q <- solve(A)
dimnames(Q) <- dimnames(A)

d$fam_id <- as.integer(factor(d$family))
d$phy_id <- seq_len(nrow(d))          # row i of Q is language i

pc_sd <- list(prec = list(prior = "pc.prec", param = c(3, 0.05)))
fixed <- list(mean = 0, prec = 1)

# INLA reports precisions; these helpers turn them into what brms reports
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

i_phy <- inla(ejectives ~ elev_km +
                f(phy_id, model = "generic0", Cmatrix = Q, hyper = pc_sd),
              family = "binomial", data = d, control.fixed = fixed)

rbind(family = slope_of(i_fam), phylogeny = slope_of(i_phy))
rbind(family    = sd_of(i_fam, "Precision for fam_id"),
      phylogeny = sd_of(i_phy, "Precision for phy_id"))

d$family_f     <- factor(d$family)
d$glottocode_f <- factor(d$glottocode, levels = rownames(Q))

g_fam <- gam(ejectives ~ elev_km + s(family_f, bs = "re"),
             family = binomial, data = d, method = "REML")

g_phy <- gam(ejectives ~ elev_km +
               s(glottocode_f, bs = "mrf", xt = list(penalty = Q)),
             family = binomial, data = d, method = "REML")

rbind(family    = summary(g_fam)$p.table["elev_km", 1:2],
      phylogeny = summary(g_phy)$p.table["elev_km", 1:2])
gam.vcomp(g_phy)

d$log_cons  <- log(d$n_consonants)
rownames(d) <- d$glottocode

ols     <- lm(log_cons ~ elev_km, data = d)
pgls_bm <- phylolm(log_cons ~ elev_km, data = d, phy = tree, model = "BM")
pgls_l  <- phylolm(log_cons ~ elev_km, data = d, phy = tree, model = "lambda")

rbind("OLS"          = summary(ols)$coefficients["elev_km", 1:3],
      "PGLS, BM"     = summary(pgls_bm)$coefficients["elev_km", 1:3],
      "PGLS, lambda" = summary(pgls_l)$coefficients["elev_km", 1:3]) |> round(3)
pgls_l$optpar

lines <- data.frame(model = c("OLS", "PGLS, lambda"),
                    intercept = c(coef(ols)[1], coef(pgls_l)[1]),
                    slope     = c(coef(ols)[2], coef(pgls_l)[2]))

ggplot(d, aes(elev_km, log_cons)) +
  geom_point(colour = "grey75", size = 0.9) +
  geom_abline(data = lines, linewidth = 0.9,
              aes(intercept = intercept, slope = slope, colour = model)) +
  scale_colour_manual(values = c("grey35", "#1BAF7A"), name = NULL) +
  labs(x = "elevation (km)", y = "log number of consonants")

plog <- phyloglm(ejectives ~ elev_km, data = d, phy = tree,
                 method = "logistic_MPLE", btol = 30)
round(summary(plog)$coefficients, 3)
unname(plog$alpha)

library(brms)
options(mc.cores = 4, brms.backend = "cmdstanr")
dir.create("fits", showWarnings = FALSE)

b_phy <- brm(ejectives ~ elev_km + (1 | gr(glottocode, cov = A)),
             data = d, data2 = list(A = A), family = bernoulli(),
             prior = prior(normal(0, 1), class = b) +
                     prior(exponential(1), class = sd),
             seed = 1, file = "fits/m_phy")

rbind("brms" = fixef(b_phy)["elev_km", 1:2],
      "INLA" = slope_of(i_phy)[1:2],
      "mgcv" = summary(g_phy)$p.table["elev_km", 1:2]) |> round(2)

x <- seq(0, 4.5, by = 0.1)
averaged <- function(eta, b) {
  at_sea <- eta - b * d$elev_km           # every language moved to 0 km
  sapply(x, function(xi) mean(plogis(at_sea + b * xi)))
}

draws <- as_draws_df(b_phy)
eta   <- posterior_linpred(b_phy)
p     <- sapply(x, function(xi) {
  b <- draws$b_elev_km
  rowMeans(plogis(eta - outer(b, d$elev_km) + b * xi))
})
band <- data.frame(elev_km = x, lo = apply(p, 2, quantile, 0.025),
                   hi = apply(p, 2, quantile, 0.975))

lines <- rbind(
  data.frame(elev_km = x, engine = "brms", p = colMeans(p)),
  data.frame(elev_km = x, engine = "INLA",
             p = averaged(i_phy$summary.linear.predictor$mean,
                          i_phy$summary.fixed["elev_km", "mean"])),
  data.frame(elev_km = x, engine = "mgcv",
             p = averaged(predict(g_phy), coef(g_phy)[["elev_km"]])))
naive <- data.frame(elev_km = x)
naive$p <- predict(m0, newdata = naive, type = "response")

ggplot(lines, aes(elev_km, p)) +
  geom_ribbon(data = band, aes(ymin = lo, ymax = hi, y = NULL),
              fill = "#1BAF7A", alpha = 0.15) +
  geom_line(aes(linetype = engine), colour = "#1BAF7A", linewidth = 0.9) +
  geom_line(data = naive, linetype = "22", colour = "grey35") +
  scale_linetype_manual(values = c("solid", "42", "13"), name = NULL) +
  labs(x = "elevation (km)", y = "probability of ejectives")
