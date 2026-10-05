library(sf)
library(spdep)
library(rnaturalearth)
library(ggplot2)

base <- "https://rehan-muh.github.io/tutorials/data/"
d    <- read.csv(paste0(base, "languages.csv"))
d$elev_km <- d$elevation / 1000

pts <- st_as_sf(d, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
pts[1:3, c("name", "family", "geometry")]

coords <- as.matrix(d[, c("lon", "lat")])
km <- sp::spDists(coords, longlat = TRUE)
dimnames(km) <- list(d$name, d$name)

show <- c("Tigrinya", "Harari", "Zulu", "Central Aymara")
round(km[show, show])

nearest <- apply(km + diag(Inf, nrow(km)), 1, min)
round(quantile(nearest, c(0, 0.25, 0.5, 0.75, 1)))

xy <- st_coordinates(st_transform(pts, "+proj=eqearth")) / 1000   # km
flat_km <- as.matrix(dist(xy))

close <- km < 3000 & upper.tri(km)
round(quantile(flat_km[close] / km[close], c(0.05, 0.5, 0.95)), 2)

to_sphere <- function(lon, lat) {
  rad <- pi / 180
  cbind(x = cos(lat * rad) * cos(lon * rad),
        y = cos(lat * rad) * sin(lon * rad),
        z = sin(lat * rad))
}
head(round(to_sphere(d$lon, d$lat), 3), 3)

# 1. the k nearest languages, made mutual
knn <- make.sym.nb(knn2nb(knearneigh(coords, k = 5, longlat = TRUE)))

# 2. every language within a fixed distance
band <- dnearneigh(coords, 0, 500, longlat = TRUE)

# 3. natural neighbours: a triangulation of the points
tri <- tri2nb(coords)

compare <- function(nb) {
  c(links_per_language = round(mean(card(nb)), 1),
    without_neighbours = sum(card(nb) == 0),
    separate_pieces    = n.comp.nb(nb)$nc)
}
rbind("5 nearest" = compare(knn), "within 500 km" = compare(band),
      "triangulation" = compare(tri))

world <- ne_countries(scale = "small", returnclass = "sf")
wrap  <- function(nb) st_wrap_dateline(nb2lines(nb, coords = st_geometry(pts)),
                                       options = c("WRAPDATELINE=YES",
                                                   "DATELINEOFFSET=60"))
ggplot() +
  geom_sf(data = world, fill = "grey94", colour = NA) +
  geom_sf(data = wrap(knn), colour = "#2A78D6", linewidth = 0.25) +
  geom_sf(data = wrap(band), colour = "#EB6834", linewidth = 0.2, alpha = 0.6) +
  geom_sf(data = pts, size = 0.4) +
  coord_sf(crs = "+proj=eqearth")

W_binary <- nb2mat(knn, style = "B")   # 1 for a neighbour, 0 otherwise
W_rows   <- nb2mat(knn, style = "W")   # each row divided by its row sum

W_binary[1:4, 1:4]
round(rowSums(W_rows)[1:4], 2)

m0  <- glm(ejectives ~ elev_km, family = binomial, data = d)
res <- residuals(m0, type = "pearson")

cg <- sp.correlogram(knn, res, order = 6, method = "I", style = "W")
steps <- data.frame(step = 1:6, I = cg$res[, 1], se = sqrt(cg$res[, 3]))

ggplot(steps, aes(step, I)) +
  geom_hline(yintercept = 0, linetype = "22", colour = "grey35") +
  geom_linerange(aes(ymin = I - 2 * se, ymax = I + 2 * se),
                 colour = "#2A78D6", linewidth = 0.8) +
  geom_point(colour = "#2A78D6", size = 2.6) +
  scale_x_continuous(breaks = 1:6) +
  labs(x = "steps apart on the neighbour graph",
       y = "Moran's I of the residuals")

src   <- paste0("https://raw.githubusercontent.com/Glottography/",
                "asher2007world/v2.0/cldf/")
areas <- st_read(paste0(src, "contemporary/languages.geojson"), quiet = TRUE)
names(areas)[names(areas) == "cldf.languageReference"] <- "glottocode"

sf_use_s2(FALSE)                  # repair the shapes on the flat map
areas <- st_make_valid(areas)
nrow(areas)

has  <- intersect(d$glottocode, areas$glottocode)
mine <- areas[match(has, areas$glottocode), ]
here <- pts[match(has, pts$glottocode), ]

c(sample = nrow(d), with_an_area = length(has))

sf_use_s2(TRUE)
km2 <- as.numeric(st_area(mine)) / 1e6
round(quantile(km2, c(0, 0.25, 0.5, 0.75, 1)))

gap <- as.numeric(st_distance(here, mine, by_element = TRUE)) / 1000
c(inside = sum(gap == 0), outside = sum(gap > 0))
round(quantile(gap[gap > 0], c(0.5, 0.9, 1)))

sf_use_s2(FALSE)
box  <- st_bbox(c(xmin = 32, ymin = 2, xmax = 49, ymax = 19), crs = 4326)

ggplot() +
  geom_sf(data = st_crop(world, box), fill = "grey94", colour = NA) +
  geom_sf(data = st_crop(areas, box), fill = "#2A78D6", alpha = 0.1,
          colour = "#2A78D6", linewidth = 0.2) +
  geom_sf(data = st_crop(pts, box), aes(fill = factor(ejectives)),
          shape = 21, size = 2.2, stroke = 0.3) +
  scale_fill_manual(values = c("0" = "white", "1" = "#EB6834"),
                    labels = c("no ejectives", "ejectives"), name = NULL)

touching <- poly2nb(mine, snap = 0.01)
c(links_per_language = round(mean(card(touching)), 1),
  without_neighbours = sum(card(touching) == 0),
  separate_pieces    = n.comp.nb(touching)$nc)

past <- st_read(paste0(src, "traditional/languages.geojson"), quiet = TRUE)
names(past)[names(past) == "cldf.languageReference"] <- "glottocode"
past <- st_make_valid(past)

both <- intersect(mine$glottocode, past$glottocode)
centre_now  <- st_centroid(st_geometry(mine[match(both, mine$glottocode), ]))
centre_past <- st_centroid(st_geometry(past[match(both, past$glottocode), ]))

sf_use_s2(TRUE)
moved <- st_distance(centre_now, centre_past, by_element = TRUE)
moved <- as.numeric(moved) / 1000
c(languages = length(both), moved_over_100_km = sum(moved > 100),
  furthest_km = round(max(moved)))
