library(ape)
library(brms)
library(ggplot2)

base <- "https://rehan-muh.github.io/tutorials/data/"
d    <- read.csv(paste0(base, "languages.csv"))
tree <- read.tree(paste0(base, "glottolog_tree.nwk"))
d$elev_km <- d$elevation / 1000

m0 <- glm(ejectives ~ elev_km, family = binomial, data = d)

options(mc.cores = 4, brms.backend = "cmdstanr")
dir.create("fits", showWarnings = FALSE)

A <- vcv(tree, corr = TRUE)
dim(A)

show <- c("Tigrinya", "Tigre", "Harari", "Eastern Oromo", "Zulu")
i <- d$glottocode[match(show, d$name)]
round(A[i, i], 2) |> `dimnames<-`(list(show, show))

long <- as.data.frame(as.table(A))
names(long) <- c("row", "col", "r")

ggplot(long, aes(col, factor(row, levels = rev(rownames(A))), fill = r)) +
  geom_raster() +
  scale_fill_gradient(low = "white", high = "#1BAF7A", name = "correlation") +
  coord_fixed() +
  theme(axis.text = element_blank(), axis.title = element_blank(),
        panel.grid = element_blank())

priors <- prior(normal(0, 1), class = b) +
          prior(exponential(1), class = sd)

m_fam <- brm(ejectives ~ elev_km + (1 | family),
             data = d, family = bernoulli(), prior = priors,
             seed = 1, file = "fits/m_fam")

summary(m_fam)

m_phy <- brm(ejectives ~ elev_km + (1 | gr(glottocode, cov = A)),
             data = d, data2 = list(A = A),
             family = bernoulli(), prior = priors,
             seed = 1, file = "fits/m_phy")

summary(m_phy)

naive  <- unname(c(coef(m0)[2], sqrt(vcov(m0)[2, 2]), confint.default(m0)[2, ]))
slopes <- rbind("naive glm"        = naive,
                "family intercept" = fixef(m_fam)["elev_km", ],
                "phylogeny"        = fixef(m_phy)["elev_km", ])
round(slopes, 2)

draws <- as_draws_df(m_phy)
averaged <- draws$b_elev_km / sqrt(1 + 0.346 * draws$sd_glottocode__Intercept^2)
round(quantile(averaged, c(0.025, 0.5, 0.975)), 2)

x <- seq(0, 4.5, by = 0.1)
pr <- predict(m0, newdata = data.frame(elev_km = x), se.fit = TRUE)
naive <- data.frame(elev_km = x, model = "naive glm", p = plogis(pr$fit),
                    lo = plogis(pr$fit - 1.96 * pr$se.fit),
                    hi = plogis(pr$fit + 1.96 * pr$se.fit))

band <- function(p, label) {
  data.frame(elev_km = x, model = label, p = colMeans(p),
             lo = apply(p, 2, quantile, 0.025),
             hi = apply(p, 2, quantile, 0.975))
}
b      <- draws$b_elev_km
eta    <- posterior_linpred(m_phy)       # draws by languages, logit scale
at_sea <- eta - outer(b, d$elev_km)      # every language moved to 0 km

typical  <- plogis(draws$b_Intercept + outer(b, x))
averaged <- sapply(x, function(xi) rowMeans(plogis(at_sea + b * xi)))

lines <- rbind(naive,
               band(averaged, "phylogeny, averaged over the sample"),
               band(typical, "phylogeny, typical lineage"))

ggplot(lines, aes(elev_km, p, colour = model, fill = model, linetype = model)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.13, colour = NA) +
  geom_line(linewidth = 0.9) +
  scale_colour_manual(values = c("grey40", "#1BAF7A", "#1BAF7A"),
                      aesthetics = c("colour", "fill"), name = NULL) +
  scale_linetype_manual(values = c("solid", "solid", "22"), name = NULL) +
  labs(x = "elevation (km)", y = "probability of ejectives")

signal <- draws$sd_glottocode__Intercept^2 /
          (draws$sd_glottocode__Intercept^2 + pi^2 / 3)
round(quantile(signal, c(0.025, 0.5, 0.975)), 2)

library(ggtree)

u <- ranef(m_phy)$glottocode[, "Estimate", "Intercept"]
effects <- data.frame(glottocode = names(u), effect = u)

ggtree(tree, layout = "fan", open.angle = 8,
       linewidth = 0.2, colour = "grey45") %<+% effects +
  geom_tippoint(aes(colour = effect), size = 1.3) +
  scale_colour_gradient2(low = "#2A78D6", mid = "#F0EFEC", high = "#E34948",
                         name = "lineage effect")
