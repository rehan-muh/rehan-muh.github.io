# Build the tutorial dataset: one row per language, plus a Glottolog family tree.
#
# Sources (all open):
#   PHOIBLE 2.0 (Moran & McCloy 2019), CC BY-SA 3.0   phoneme inventories
#   Glottolog (Hammarstrom et al.), CC BY 4.0          coordinates, classification
#   ETOPO1 (NOAA NCEI), public domain                  elevation, via opentopodata.org
#
# Run from the project root:  Rscript data-raw/make_data.R
# Writes data/languages.csv and data/glottolog_tree.nwk

suppressPackageStartupMessages({
  library(ape)
})

set.seed(2013)
N_SAMPLE <- 500
raw <- "data-raw/downloads"
dir.create("data", showWarnings = FALSE)

# ---- Glottolog: languages with coordinates and their classification ---------

glot <- read.csv(file.path(raw, "glottolog_languages.csv"), encoding = "UTF-8")
glot <- subset(glot, Level == "language" & !is.na(Latitude) & !is.na(Longitude))

vals <- read.csv(file.path(raw, "glottolog_values.csv"), encoding = "UTF-8")
cls  <- subset(vals, Parameter_ID == "classification", c(Language_ID, Value))
names(cls) <- c("Glottocode", "path")
glot <- merge(glot, cls, by = "Glottocode", all.x = TRUE)
glot$path[is.na(glot$path)] <- ""          # isolates have no ancestors

fam_names <- read.csv(file.path(raw, "glottolog_languages.csv"), encoding = "UTF-8")
fam_names <- setNames(fam_names$Name, fam_names$Glottocode)

# ---- PHOIBLE: one inventory per language ------------------------------------

ph <- read.csv(file.path(raw, "phoible.csv"), encoding = "UTF-8",
               colClasses = "character")
ph$InventoryID <- as.integer(ph$InventoryID)

inv <- do.call(rbind, lapply(split(ph, ph$InventoryID), function(d) {
  data.frame(
    InventoryID  = d$InventoryID[1],
    Glottocode   = d$Glottocode[1],
    n_consonants = sum(d$SegmentClass == "consonant"),
    n_vowels     = sum(d$SegmentClass == "vowel"),
    n_tones      = sum(d$SegmentClass == "tone"),
    ejectives    = as.integer(any(d$raisedLarynxEjective == "+" &
                                  d$SegmentClass == "consonant", na.rm = TRUE))
  )
}))

# Several inventories can describe one language; keep the first one PHOIBLE lists.
inv <- inv[order(inv$InventoryID), ]
inv <- inv[!duplicated(inv$Glottocode), ]

d <- merge(inv, glot[, c("Glottocode", "Name", "Macroarea", "Latitude",
                         "Longitude", "Family_ID", "path")], by = "Glottocode")
d$family <- ifelse(d$Family_ID == "" | is.na(d$Family_ID),
                   paste0("Isolate: ", d$Name), fam_names[d$Family_ID])

cat("PHOIBLE languages with Glottolog coordinates:", nrow(d), "\n")
cat("  with ejectives:", sum(d$ejectives), "\n")

# ---- Sample -----------------------------------------------------------------

d <- d[sort(sample(nrow(d), N_SAMPLE)), ]
cat("Sampled", nrow(d), "languages;", sum(d$ejectives), "with ejectives;",
    length(unique(d$family)), "families\n")

# ---- Elevation from ETOPO1 --------------------------------------------------

get_elevation <- function(lat, lon) {
  out <- numeric(length(lat))
  for (chunk in split(seq_along(lat), ceiling(seq_along(lat) / 100))) {
    loc <- paste(sprintf("%.4f,%.4f", lat[chunk], lon[chunk]), collapse = "|")
    url <- paste0("https://api.opentopodata.org/v1/etopo1?locations=", loc)
    res <- jsonlite::fromJSON(url)
    stopifnot(res$status == "OK")
    out[chunk] <- res$results$elevation
    Sys.sleep(1.2)
  }
  out
}
cache <- file.path(raw, "elevation_cache.rds")
if (file.exists(cache)) {
  elev <- readRDS(cache)
} else {
  elev <- data.frame(Glottocode = character(), elevation = numeric())
}
need <- setdiff(d$Glottocode, elev$Glottocode)
if (length(need)) {
  i <- match(need, d$Glottocode)
  elev <- rbind(elev, data.frame(Glottocode = need,
                                 elevation = get_elevation(d$Latitude[i], d$Longitude[i])))
  saveRDS(elev, cache)
}
d$elevation <- pmax(0, round(elev$elevation[match(d$Glottocode, elev$Glottocode)]))

# ---- Tree from the Glottolog classification ---------------------------------
# Each family becomes a clade whose shape follows Glottolog's subgroups. Every
# level of the classification counts as one step, the family itself included,
# and tips are extended so that all languages end at the same height. Each
# family is then scaled to height 1. Two languages therefore share the
# fraction of classification levels they have in common. Families join only at
# the root: Glottolog makes no claim about deeper relationships, so languages
# in different families share no history in this tree.

newick_from_paths <- function(tips, paths) {
  kids <- function(prefix_len, idx) {
    heads <- vapply(paths[idx], function(p) if (length(p) > prefix_len) p[prefix_len + 1] else NA_character_, "")
    parts <- character()
    for (i in idx[is.na(heads)]) parts <- c(parts, tips[i])
    for (h in unique(heads[!is.na(heads)])) {
      sub <- idx[!is.na(heads) & heads == h]
      inner <- kids(prefix_len + 1, sub)
      parts <- c(parts, if (length(sub) == 1) inner else paste0("(", paste(inner, collapse = ","), ")"))
    }
    parts
  }
  paste0("(", paste(kids(0, seq_along(tips)), collapse = ","), ");")
}

paths <- strsplit(d$path, "/", fixed = TRUE)
fam_key <- ifelse(lengths(paths) == 0, d$Glottocode, vapply(paths, function(p) if (length(p)) p[1] else "", ""))

clades <- vapply(split(seq_len(nrow(d)), fam_key), function(idx) {
  if (length(idx) == 1) return(paste0(d$Glottocode[idx], ":1"))
  tr <- read.tree(text = newick_from_paths(d$Glottocode[idx], paths[idx]))
  tr <- collapse.singles(tr)
  tr <- compute.brlen(tr, 1)                       # one step per level
  n  <- Ntip(tr)
  depth <- node.depth.edgelength(tr)[seq_len(n)]   # steps from the family node to each tip
  tip_edge <- match(seq_len(n), tr$edge[, 2])
  tr$edge.length[tip_edge] <- tr$edge.length[tip_edge] + max(depth) - depth
  height <- max(depth) + 1                         # plus one step for the family itself
  tr$edge.length <- tr$edge.length / height
  paste0(sub(";$", "", write.tree(tr)), ":", 1 / height)
}, "")

tree <- read.tree(text = paste0("(", paste(clades, collapse = ","), ");"))
stopifnot(setequal(tree$tip.label, d$Glottocode), is.ultrametric(tree))

# ---- Write ------------------------------------------------------------------

out <- data.frame(
  glottocode   = d$Glottocode,
  name         = d$Name,
  family       = unname(d$family),
  macroarea    = d$Macroarea,
  lat          = round(d$Latitude, 4),
  lon          = round(d$Longitude, 4),
  elevation    = d$elevation,
  ejectives    = d$ejectives,
  n_consonants = d$n_consonants,
  n_vowels     = d$n_vowels,
  tone         = as.integer(d$n_tones > 0)
)
out <- out[match(tree$tip.label, out$glottocode), ]
write.csv(out, "data/languages.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.tree(tree, "data/glottolog_tree.nwk")

cat("\nWrote data/languages.csv (", nrow(out), " rows) and data/glottolog_tree.nwk\n", sep = "")
print(summary(out[, c("elevation", "ejectives", "n_consonants", "n_vowels", "tone")]))
print(head(sort(table(out$family), decreasing = TRUE), 12))
print(table(out$macroarea, out$ejectives))
cat("glm check:\n")
print(coef(summary(glm(ejectives ~ I(elevation / 1000), binomial, out))))
