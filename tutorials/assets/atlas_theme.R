# The figure style used on the tutorial pages.
# Optional: source this file after loading ggplot2 and your plots will match
# the ones on the site. Nothing in the tutorials depends on it.
#
# Colours are a validated categorical palette: the first three hues stay
# distinguishable under the common forms of colour blindness in any pairing.

atlas <- list(
  ink     = "#0B0B0B",
  ink2    = "#52514E",
  muted   = "#898781",
  grid    = "#E1E0D9",
  land    = "#F0EFEC",
  space   = "#2A78D6",   # blue: anything spatial
  present = "#EB6834",   # orange: the feature is there
  phylo   = "#1BAF7A",   # aqua: anything genealogical
  note    = "#EDA100",   # yellow: highlights
  red     = "#E34948"
)

# Sequential (one hue, light to dark) and diverging (two hues, grey middle).
atlas_seq <- c("#CDE2FB", "#9EC5F4", "#6DA7EC", "#3987E5", "#256ABF", "#104281")
atlas_div <- c("#2A78D6", "#9EC5F4", "#F0EFEC", "#F0A5A4", "#E34948")

theme_atlas <- function(base_size = 12, base_family = "") {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      text             = ggplot2::element_text(colour = atlas$ink),
      axis.text        = ggplot2::element_text(colour = atlas$ink2),
      axis.title       = ggplot2::element_text(colour = atlas$ink2),
      panel.grid.major = ggplot2::element_line(colour = atlas$grid, linewidth = 0.3),
      panel.grid.minor = ggplot2::element_blank(),
      plot.title       = ggplot2::element_text(face = "bold", size = ggplot2::rel(1.1)),
      plot.title.position = "plot",
      plot.subtitle    = ggplot2::element_text(colour = atlas$ink2),
      plot.caption     = ggplot2::element_text(colour = atlas$ink2, hjust = 0),
      plot.caption.position = "plot",
      legend.position  = "top",
      legend.justification = "left",
      legend.title     = ggplot2::element_text(size = ggplot2::rel(0.9)),
      legend.key.size  = ggplot2::unit(0.9, "lines"),
      legend.margin    = ggplot2::margin(0, 0, 0, 0),
      strip.text       = ggplot2::element_text(face = "bold", hjust = 0, colour = atlas$ink),
      plot.background  = ggplot2::element_rect(fill = "white", colour = NA)
    )
}

# Maps: white sea, faint graticule, no axes. Add to any ggplot that uses geom_sf().
theme_plate <- function(base_size = 12, base_family = "") {
  theme_atlas(base_size, base_family) +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "white", colour = NA),
      panel.grid.major = ggplot2::element_line(colour = "#ECEBE6", linewidth = 0.25),
      axis.text        = ggplot2::element_blank(),
      axis.title       = ggplot2::element_blank(),
      axis.ticks       = ggplot2::element_blank()
    )
}
