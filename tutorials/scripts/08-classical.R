library(ape)
library(phylolm)
library(spdep)
library(spatialreg)
library(ggplot2)

base <- "https://rehan-muh.github.io/tutorials/data/"
d    <- read.csv(paste0(base, "languages.csv"))
tree <- read.tree(paste0(base, "glottolog_tree.nwk"))

d$elev_km  <- d$elevation / 1000
d$log_cons <- log(d$n_consonants)
rownames(d) <- d$glottocode       # phylolm matches rows to tips by row name

ols <- lm(log_cons ~ elev_km, data = d)
round(summary(ols)$coefficients, 3)

pgls_bm <- phylolm(log_cons ~ elev_km, data = d, phy = tree, model = "BM")
round(summary(pgls_bm)$coefficients, 3)

pgls_l <- phylolm(log_cons ~ elev_km, data = d, phy = tree, model = "lambda")
round(summary(pgls_l)$coefficients, 3)
pgls_l$optpar

plog <- phyloglm(ejectives ~ elev_km, data = d, phy = tree,
                 method = "logistic_MPLE", btol = 30)
round(summary(plog)$coefficients, 3)
plog$alpha

coords <- as.matrix(d[, c("lon", "lat")])
nb <- make.sym.nb(knn2nb(knearneigh(coords, k = 5, longlat = TRUE)))
lw <- nb2listw(nb, style = "W")

lm.morantest(ols, lw)

sar_err <- errorsarlm(log_cons ~ elev_km, data = d, listw = lw)
round(summary(sar_err)$Coef, 3)
sar_err$lambda

sar_lag <- lagsarlm(log_cons ~ elev_km, data = d, listw = lw)
round(summary(sar_lag)$Coef, 3)
sar_lag$rho

impacts(sar_lag, listw = lw)

tab <- rbind(
  "OLS"         = summary(ols)$coefficients["elev_km", 1:2],
  "PGLS, BM"    = summary(pgls_bm)$coefficients["elev_km", 1:2],
  "PGLS, lambda" = summary(pgls_l)$coefficients["elev_km", 1:2],
  "SAR error"   = summary(sar_err)$Coef["elev_km", 1:2])
tab <- cbind(tab, ratio = tab[, 1] / tab[, 2])
round(tab, 3)

lines <- data.frame(
  model     = c("OLS", "PGLS, lambda", "SAR error"),
  intercept = c(coef(ols)[1], coef(pgls_l)[1], sar_err$coefficients[1]),
  slope     = c(coef(ols)[2], coef(pgls_l)[2], sar_err$coefficients[2]))

ggplot(d, aes(elev_km, log_cons)) +
  geom_point(colour = "grey75", size = 0.9) +
  geom_abline(data = lines, linewidth = 0.9,
              aes(intercept = intercept, slope = slope, colour = model)) +
  scale_colour_manual(values = c("grey35", "#1BAF7A", "#2A78D6"), name = NULL) +
  labs(x = "elevation (km)", y = "log number of consonants")
