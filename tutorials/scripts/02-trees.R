library(ape)
library(phytools)

base <- "https://rehan-muh.github.io/tutorials/data/"
d    <- read.csv(paste0(base, "languages.csv"))
tree <- read.tree(paste0(base, "glottolog_tree.nwk"))

tree
head(tree$tip.label)
c(tips = Ntip(tree), internal_nodes = Nnode(tree))

is.ultrametric(tree)    # do all languages end at the same height?
is.binary(tree)         # does every node split in exactly two?

setdiff(d$glottocode, tree$tip.label)    # in the data, missing from the tree
setdiff(tree$tip.label, d$glottocode)    # in the tree, missing from the data

some  <- d$glottocode[d$macroarea == "Australia"]
small <- keep.tip(tree, some)       # the tree of just these languages
small

d <- d[match(tree$tip.label, d$glottocode), ]
all(d$glottocode == tree$tip.label)

aa <- keep.tip(tree, d$glottocode[d$family == "Afro-Asiatic"])
aa$tip.label <- d$name[match(aa$tip.label, d$glottocode)]

plot(aa, cex = 0.75)
axisPhylo()

grafen <- compute.brlen(aa, method = "Grafen")
ours   <- as.vector(vcv(aa, corr = TRUE))
theirs <- as.vector(vcv(grafen, corr = TRUE))
round(cor(ours, theirs), 2)

tab <- data.frame(family = factor(d$family), language = factor(d$glottocode))
flat <- as.phylo(~family / language, data = tab)
flat <- compute.brlen(flat, method = "Grafen")
flat

log_cons <- setNames(log(d$n_consonants), d$glottocode)
phylosig(tree, log_cons, method = "K", test = TRUE, nsim = 999)
