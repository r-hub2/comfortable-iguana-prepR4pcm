## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment  = "#>",
  eval     = FALSE   # most chunks need rotl + metafor + network
)

## ----setup, eval = TRUE-------------------------------------------------------
library(prepR4pcm)
# install.packages("rotl")     # required: Open Tree of Life access
# install.packages("metafor")  # required: the meta-analysis engine

## ----species-list-------------------------------------------------------------
# species <- c(
#   # Actinopterygii (ray-finned fish)
#   "Salmo trutta",              "Acipenser fulvescens",
#   # Reptilia
#   "Anolis sagrei",
#   # Amphibia
#   "Xenopus laevis",            "Rhinella marina",
#   # Insecta
#   "Ischnura elegans",          "Anopheles albimanus",
#   # Malacostraca (crustaceans)
#   "Litopenaeus vannamei",
#   # Branchiopoda
#   "Daphnia magna",
#   # Bivalvia (mussels)
#   "Lampsilis abrupta",
#   # Echinoidea (sea urchins)
#   "Mesocentrotus franciscanus",
#   # Cephalopoda
#   "Octopus maya",
#   # Chondrichthyes (sharks/rays)
#   "Heterodontus portusjacksoni",
#   # Hyperoartia (lampreys)
#   "Petromyzon marinus"
# )
# length(species)   # 14

## ----manual-pipeline, eval = FALSE--------------------------------------------
# # Step 1: get the rotl topology (no extras)
# res_manual    <- pr_get_tree(species, source = "rotl")
# tree_manual   <- res_manual$tree
# 
# # Step 2: resolve polytomies at random (bifurcating tree)
# tree_manual   <- ape::multi2di(tree_manual, random = TRUE)
# 
# # Step 3: Grafen branch lengths
# tree_manual   <- ape::compute.brlen(tree_manual, method = "Grafen")
# 
# res_manual$tree <- tree_manual
# plot(tree_manual)

## ----get-tree-----------------------------------------------------------------
# res <- pr_get_tree(
#   species,
#   source             = "rotl",
#   resolve_polytomies = TRUE,        # multi2di(random = TRUE)
#   branch_lengths     = "grafen"     # compute.brlen(method = "Grafen")
# )
# res$tree            # ultrametric, bifurcating, Grafen branches
# res$matched         # species Open Tree of Life resolved
# res$unmatched       # species Open Tree of Life couldn't place
# plot(res$tree)      # visualise the topology
# res$tree$tip.label  # has _ott<digits> suffixes (see note below)

## ----compare-pipelines, eval = FALSE------------------------------------------
# # Same tip set?
# setequal(res$tree$tip.label, res_manual$tree$tip.label)   # TRUE
# # Same branch-length range (modulo RNG order for the bifurcation)?
# range(res$tree$edge.length)
# range(res_manual$tree$edge.length)
# # Side-by-side plot
# op <- par(mfrow = c(1, 2))
# plot(res_manual$tree, main = "manual ape pipeline")
# plot(res$tree,        main = "one-call pr_get_tree()")
# par(op)

## ----clean-labels-------------------------------------------------------------
# res$tree$tip.label <- gsub("_ott\\d+", "", res$tree$tip.label)
# res$tree$tip.label <- gsub("_",        " ",  res$tree$tip.label)
# res$tree$tip.label                          # now "Salmo trutta", etc.

## ----phylo-cor----------------------------------------------------------------
# phy_cor <- pr_phylo_cor(res)
# dim(phy_cor)            # 14 x 14 (one row/col per species)
# isSymmetric(phy_cor)    # TRUE
# all(diag(phy_cor) == 1) # TRUE (correlation matrix; ultrametric tree)
# rownames(phy_cor)       # matches the binomials in `species`

## ----meta---------------------------------------------------------------------
# library(metafor)
# 
# # Toy effect-size data. Start by including every species at least
# # once (otherwise random sampling can leave some species out and
# # rma.mv complains about "levels in species without matching rows
# # in R"). Then add 16 extra studies sampled with replacement to get
# # a realistic multi-study-per-species shape.
# set.seed(42)
# toy_df <- data.frame(
#   species = c(species,                          # 14 guaranteed rows
#               sample(species, 16, replace = TRUE)),  # 16 extra
#   study   = sprintf("study_%02d", seq_len(30)),
#   yi      = rnorm(30, mean = 0.20, sd = 0.15),   # lnRR
#   vi      = runif(30, min = 0.01, max = 0.08)    # sampling variance
# )
# 
# # Sanity-check: every species in the data is also a row in phy_cor.
# stopifnot(all(toy_df$species %in% rownames(phy_cor)))
# length(intersect(toy_df$species, rownames(phy_cor)))   # 14
# 
# # Fit: random study effect (within-study) + random species effect
# # (between-species, with phylogenetic correlation structure).
# fit <- rma.mv(
#   yi ~ 1,
#   vi,
#   random  = list(~1 | study, ~1 | species),
#   R       = list(species = phy_cor),
#   data    = toy_df,
#   method  = "REML"
# )
# summary(fit)

## ----reconcile-then-fit, eval = FALSE-----------------------------------------
# result <- reconcile_tree(
#   x         = toy_df,
#   tree      = res$tree,    # already gsub-cleaned, see Step 4
#   x_species = "species",
#   authority = NULL          # skip synonym lookup; tree labels already binomials
# )
# aligned <- reconcile_apply(
#   result,
#   data            = toy_df,
#   tree            = res$tree,
#   species_col     = "species",
#   drop_unresolved = TRUE
# )
# phy_cor <- pr_phylo_cor(aligned$tree)
# fit <- rma.mv(yi ~ 1, vi,
#               random = list(~1 | study, ~1 | species),
#               R      = list(species = phy_cor),
#               data   = aligned$data,
#               method = "REML")

## ----graft-demo, eval = FALSE-------------------------------------------------
# # Trait data carrying one species the retrieved tree lacks: Anolis
# # carolinensis, a congener of Anolis sagrei (which is in the tree).
# graft_df <- data.frame(
#   species = c(res$tree$tip.label, "Anolis carolinensis")
# )
# nrow(graft_df)               # 15 species in the trait data
# 
# # Reconcile data names against the tree: 14 match, 1 is unresolved.
# rec <- reconcile_tree(graft_df, res$tree,
#                       x_species = "species", authority = NULL)
# 
# # Graft each unresolved species next to a congener already in the tree.
# aug <- reconcile_augment(rec, res$tree)
# 
# aug$meta$original_n_tips     # 14  (tree tips before grafting)
# aug$meta$augmented_n_tips    # 15  (tree tips after grafting)
# aug$augmented[, c("species", "placed_near", "n_congeners")]
# #>               species   placed_near n_congeners
# #> 1 Anolis carolinensis Anolis sagrei           1
# 
# # The augmented tree is ready for a comparative model: still fully
# # resolved, and still ultrametric.
# ape::is.binary(aug$tree)       # TRUE
# ape::is.ultrametric(aug$tree)  # TRUE

## ----graft-plot, eval = FALSE-------------------------------------------------
# # reconcile_augment() writes the grafted tip label with an
# # underscore; convert to a space for a consistent set of binomials.
# aug$tree$tip.label <- gsub("_", " ", aug$tree$tip.label)
# 
# op <- par(mfrow = c(1, 2))
# plot(aug$original, main = "Before grafting: 14 tips")
# plot(aug$tree,     main = "After reconcile_augment(): 15 tips")
# par(op)

## ----datelife, eval = FALSE---------------------------------------------------
# # Alternative: get rotl topology, then date it with DateLife
# res    <- pr_get_tree(species, source = "rotl",
#                       resolve_polytomies = TRUE)   # NO branch_lengths
# dated  <- pr_date_tree(res$tree)                   # DateLife dates it
# phy_cor <- pr_phylo_cor(dated)

