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

src   <- paste0("https://raw.githubusercontent.com/phlorest/",
                "bouckaert_et_al2018/main/cldf/")
taxa  <- read.csv(paste0(src, "languages.csv"))
dated <- read.nexus(paste0(src, "summary.trees"))

dated
head(taxa[, c("ID", "Glottocode")], 4)

code <- taxa$Glottocode[match(dated$tip.label, taxa$ID)]
c(tips = length(code), distinct_glottocodes = length(unique(code)))

pn    <- d$glottocode[d$family == "Pama-Nyungan"]
keep  <- !duplicated(code) & code %in% pn
dated <- keep.tip(dated, dated$tip.label[keep])
dated$tip.label <- taxa$Glottocode[match(dated$tip.label, taxa$ID)]

c(in_sample = length(pn), in_dated_tree = Ntip(dated),
  missing = length(setdiff(pn, dated$tip.label)))

named <- dated
named$tip.label <- d$name[match(named$tip.label, d$glottocode)]

plot(named, cex = 0.7)
axisPhylo()

both   <- dated$tip.label
# from the full tree, so that the family's own stem is counted
ours   <- vcv(tree, corr = TRUE)[both, both]
theirs <- vcv(dated, corr = TRUE)[both, both]
pairs  <- upper.tri(ours)

round(cor(ours[pairs], theirs[pairs]), 2)
round(c(classification = mean(ours[pairs]), dated = mean(theirs[pairs])), 2)

log_cons <- setNames(log(d$n_consonants), d$glottocode)
phylosig(tree, log_cons, method = "K", test = TRUE, nsim = 999)
