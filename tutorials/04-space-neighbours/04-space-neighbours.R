library(brms)
library(spdep)
library(sf)
library(rnaturalearth)
library(ggplot2)

base <- "https://rehan-muh.github.io/tutorials/data/"
d    <- read.csv(paste0(base, "languages.csv"))
d$elev_km <- d$elevation / 1000

options(mc.cores = 4, brms.backend = "cmdstanr")
dir.create("fits", showWarnings = FALSE)

coords <- as.matrix(d[, c("lon", "lat")])
nb <- knn2nb(knearneigh(coords, k = 5, longlat = TRUE))
nb <- make.sym.nb(nb)

summary(card(nb))        # number of neighbours per language
n.comp.nb(nb)$nc         # number of separate pieces

world <- ne_countries(scale = "small", returnclass = "sf")
pts   <- st_as_sf(d, coords = c("lon", "lat"), crs = 4326)
links <- nb2lines(nb, coords = st_geometry(pts))
# split the links that cross the 180th meridian, so they are drawn the short way
links <- st_wrap_dateline(links, options = c("WRAPDATELINE=YES",
                                             "DATELINEOFFSET=60"))

ggplot() +
  geom_sf(data = world, fill = "grey94", colour = NA) +
  geom_sf(data = links, colour = "#2A78D6", linewidth = 0.25) +
  geom_sf(data = pts, size = 0.5) +
  coord_sf(crs = "+proj=eqearth")

W <- nb2mat(nb, style = "B")
dimnames(W) <- list(d$glottocode, d$glottocode)

priors <- prior(normal(0, 1), class = b) +
          prior(exponential(1), class = sdcar)

m_icar <- brm(ejectives ~ elev_km + car(W, gr = glottocode, type = "icar"),
              data = d, data2 = list(W = W),
              family = bernoulli(), prior = priors,
              seed = 1, file = "fits/m_icar")

summary(m_icar)

m_bym <- brm(ejectives ~ elev_km + car(W, gr = glottocode, type = "bym2"),
             data = d, data2 = list(W = W),
             family = bernoulli(), prior = priors,
             seed = 1, file = "fits/m_bym")

summary(m_bym)

sea_level <- transform(d, elev_km = 0)
pts$baseline <- fitted(m_bym, newdata = sea_level)[, "Estimate"]

ggplot() +
  geom_sf(data = world, fill = "grey94", colour = NA) +
  geom_sf(data = pts[order(pts$baseline), ], aes(colour = baseline),
          size = 1.6) +
  scale_colour_viridis_c(name = "baseline probability") +
  coord_sf(crs = "+proj=eqearth")

m0 <- glm(ejectives ~ elev_km, family = binomial, data = d)
x  <- seq(0, 4.5, by = 0.1)
pr <- predict(m0, newdata = data.frame(elev_km = x), se.fit = TRUE)
naive <- data.frame(elev_km = x, model = "naive glm", p = plogis(pr$fit),
                    lo = plogis(pr$fit - 1.96 * pr$se.fit),
                    hi = plogis(pr$fit + 1.96 * pr$se.fit))

b      <- as_draws_df(m_bym)$b_elev_km
eta    <- posterior_linpred(m_bym)        # draws by languages, logit scale
at_sea <- eta - outer(b, d$elev_km)      # every language moved to 0 km
p      <- sapply(x, function(xi) rowMeans(plogis(at_sea + b * xi)))
model  <- data.frame(elev_km = x, model = "neighbour graph (BYM2)",
                     p  = colMeans(p),
                     lo = apply(p, 2, quantile, 0.025),
                     hi = apply(p, 2, quantile, 0.975))

ggplot(rbind(naive, model), aes(elev_km, p, colour = model, fill = model)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.15, colour = NA) +
  geom_line(linewidth = 0.9) +
  scale_colour_manual(values = c("grey40", "#2A78D6"),
                      aesthetics = c("colour", "fill"), name = NULL) +
  labs(x = "elevation (km)", y = "probability of ejectives")

nb10 <- make.sym.nb(knn2nb(knearneigh(coords, k = 10, longlat = TRUE)))
W10  <- nb2mat(nb10, style = "B")
dimnames(W10) <- list(d$glottocode, d$glottocode)

m_bym10 <- brm(ejectives ~ elev_km + car(W, gr = glottocode, type = "bym2"),
               data = d, data2 = list(W = W10),
               family = bernoulli(), prior = priors,
               seed = 1, file = "fits/m_bym10")

slopes <- rbind(fixef(m_icar)["elev_km", ], fixef(m_bym)["elev_km", ],
                fixef(m_bym10)["elev_km", ])
rownames(slopes) <- c("ICAR, k = 5", "BYM2, k = 5", "BYM2, k = 10")
round(slopes, 2)
