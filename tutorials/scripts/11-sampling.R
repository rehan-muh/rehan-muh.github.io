library(spdep)
library(mgcv)
library(ggplot2)

base <- "https://rehan-muh.github.io/tutorials/data/"
d    <- read.csv(paste0(base, "languages.csv"))
d$elev_km <- d$elevation / 1000
d$family  <- factor(d$family)

# great-circle distance between every pair of languages, in km
km <- sp::spDists(as.matrix(d[, c("lon", "lat")]), longlat = TRUE)

m0 <- glm(ejectives ~ elev_km, family = binomial, data = d)

# one language drawn at random from every family
one_per_family <- function(d) {
  pick <- tapply(seq_len(nrow(d)), d$family,
                 function(i) i[sample.int(length(i), 1)])
  d[pick, ]
}

# one language per family within each macroarea, so that families
# spread over several areas contribute once per area
family_by_area <- function(d) {
  cell <- interaction(d$family, d$macroarea, drop = TRUE)
  pick <- tapply(seq_len(nrow(d)), cell,
                 function(i) i[sample.int(length(i), 1)])
  d[pick, ]
}

# spatial thinning: visit the languages in random order and keep one
# only if it is at least min_km from every language already kept
thinned <- function(d, min_km) {
  keep <- integer()
  for (i in sample.int(nrow(d))) {
    if (all(km[i, keep] >= min_km)) keep <- c(keep, i)
  }
  d[keep, ]
}

set.seed(1989)
one <- list("one per family"     = one_per_family(d),
            "family within area" = family_by_area(d),
            "thinned to 500 km"  = thinned(d, 500))

sapply(one, function(s) c(languages = nrow(s),
                          with_ejectives = sum(s$ejectives)))

m_one <- glm(ejectives ~ elev_km, family = binomial, data = one[[1]])
round(summary(m_one)$coefficients, 3)

fit_sample <- function(s) {
  m <- glm(ejectives ~ elev_km, family = binomial, data = s)
  c(coef(m), coef(summary(m))["elev_km", c(2, 4)], nrow(s))
}

designs <- list("one per family"     = function() one_per_family(d),
                "family within area" = function() family_by_area(d),
                "thinned to 500 km"  = function() thinned(d, 500))

set.seed(1989)
draws <- do.call(rbind, lapply(names(designs), function(design) {
  r <- as.data.frame(t(replicate(1000, fit_sample(designs[[design]]()))))
  names(r) <- c("intercept", "slope", "se", "p", "n")
  r$design <- design
  r
}))
draws$design <- factor(draws$design, levels = names(designs))

summarise <- function(r) {
  c(languages   = median(r$n),
    slope       = median(r$slope),
    lowest      = unname(quantile(r$slope, 0.025)),
    highest     = unname(quantile(r$slope, 0.975)),
    std_error   = median(r$se),
    significant = mean(r$p < 0.05))
}
tab <- t(sapply(split(draws, draws$design), summarise))
round(rbind(tab, "all 500 languages" = c(500, coef(m0)[2], NA, NA,
                                         sqrt(vcov(m0)[2, 2]), 1)), 2)

ggplot(draws, aes(slope)) +
  geom_histogram(bins = 40, fill = "#EDA100", colour = "white",
                 linewidth = 0.1) +
  geom_vline(xintercept = coef(m0)[2], linetype = "22", colour = "grey35") +
  facet_wrap(~design) +
  labs(x = "log-odds of ejectives per km of elevation",
       y = "samples out of 1,000")

x <- seq(0, 4.5, by = 0.1)

g <- gam(ejectives ~ elev_km + s(family, bs = "re") +
           s(lat, lon, bs = "sos", k = 60),
         family = binomial, data = d, method = "REML")
b <- coef(g)[["elev_km"]]
at_sea <- predict(g) - b * d$elev_km          # every language moved to 0 km
model  <- data.frame(elev_km = x,
                     p = sapply(x, function(xi) mean(plogis(at_sea + b * xi))))
naive  <- data.frame(elev_km = x)
naive$p <- predict(m0, newdata = naive, type = "response")

some <- do.call(rbind, lapply(split(draws, draws$design), head, 150))
some$id <- seq_len(nrow(some))
curves  <- merge(some[, c("id", "design", "intercept", "slope")],
                 data.frame(elev_km = x))
curves$p <- plogis(curves$intercept + curves$slope * curves$elev_km)

ggplot(curves, aes(elev_km, p)) +
  geom_line(aes(group = id), colour = "#EDA100", alpha = 0.25,
            linewidth = 0.3) +
  geom_line(data = naive, linetype = "22", colour = "grey35") +
  geom_line(data = model, colour = "grey10", linewidth = 0.9) +
  facet_wrap(~design) +
  labs(x = "elevation (km)", y = "probability of ejectives")

residual_moran <- function(s) {
  m  <- glm(ejectives ~ elev_km, family = binomial, data = s)
  xy <- as.matrix(s[, c("lon", "lat")])
  nb <- knn2nb(knearneigh(xy, k = 5, longlat = TRUE))
  lw <- nb2listw(make.sym.nb(nb), style = "W")
  test <- moran.test(residuals(m, type = "pearson"), lw)
  c(I = unname(test$estimate[1]), p = test$p.value)
}

set.seed(1989)
checks <- lapply(designs, function(draw) {
  t(replicate(200, residual_moran(draw())))
})
round(sapply(checks, function(r) {
  c(median_I = median(r[, "I"]), share_autocorrelated = mean(r[, "p"] < 0.05))
}), 2)
