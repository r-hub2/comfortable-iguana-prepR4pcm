## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----install, eval = FALSE----------------------------------------------------
# # Install pak if you don't have it
# # install.packages("pak")
# 
# # Install prepR4pcm from GitHub
# pak::pak("itchyshin/prepR4pcm")

## ----setup--------------------------------------------------------------------
library(prepR4pcm)

## ----example-data-------------------------------------------------------------
# Simulated trait data for 6 primate species
trait_data <- data.frame(
  species = c(
    "Homo sapiens",
    "Pan_troglodytes",       # underscore instead of space
    "Gorilla gorilla",
    "Pongo pygmaeus",
    "Macaca mulatta",
    "Cebus capucinus"
  ),
  body_mass = c(70, 50, 160, 80, 8, 3),
  brain_mass = c(1.35, 0.39, 0.50, 0.37, 0.11, 0.07)
)

# Simulated phylogenetic tree (built manually for this example)
tree <- ape::read.tree(text = paste0(
  "((((Homo_sapiens:5,Pan_troglodytes:5):3,",
  "Gorilla_gorilla:8):4,Pongo_pygmaeus:12):6,",
  "(Macaca_mulatta:10,Papio_anubis:10):8);"
))

tree$tip.label   # the tip labels (species names) on the tree
plot(tree)       # quick visual; underscores in tip labels render as spaces

## ----reconcile-tree-----------------------------------------------------------
result <- reconcile_tree(
  x = trait_data,
  tree = tree,
  x_species = "species",
  authority = NULL,        # skip synonym lookup for this example
  quiet = FALSE
)

## ----print-result-------------------------------------------------------------
print(result)

## ----mapping------------------------------------------------------------------
reconcile_mapping(result)

## ----summary, eval = FALSE----------------------------------------------------
# reconcile_summary(result)

## ----override-----------------------------------------------------------------
result <- reconcile_override(
  result,
  name_x = "Cebus capucinus",
  name_y = NA,
  action = "reject",
  note = "Not in target phylogeny; exclude from analysis"
)

## ----apply--------------------------------------------------------------------
aligned <- reconcile_apply(
  result,
  data = trait_data,
  tree = tree,
  species_col = "species",
  drop_unresolved = TRUE
)

# Aligned data frame — only species present in both data and tree
aligned$data

# Aligned tree — pruned to matched species
ape::Ntip(aligned$tree)
plot(aligned$tree)   # the pruned tree

## ----data-data----------------------------------------------------------------
# df1: body mass for three primates (df1 uses an underscore for chimp)
df1 <- data.frame(
  species = c("Homo sapiens", "Pan_troglodytes", "Gorilla gorilla"),
  mass = c(70, 50, 160)
)

# df2: lifespan for three primates (df2 uses a space for chimp; orang
# is here but not gorilla)
df2 <- data.frame(
  species = c("Homo sapiens", "Pan troglodytes", "Pongo pygmaeus"),
  lifespan = c(79, 40, 45)
)

# Reconcile the species columns of df1 and df2 against each other.
# `authority = NULL` skips the synonym-lookup stage (no taxonomic
# database needed for this small example). `quiet = TRUE` suppresses
# progress messages.
result2 <- reconcile_data(
  x = df1,
  y = df2,
  authority = NULL,
  quiet = TRUE
)

# The output shows how many names matched, and via which stage.
print(result2)

## ----authority, eval = FALSE--------------------------------------------------
# # Requires taxadb and a local database download (automatic on first use)
# result3 <- reconcile_tree(
#   x = trait_data,
#   tree = tree,
#   x_species = "species",
#   authority = "col"        # Catalogue of Life
# )

## ----overrides-table, eval = FALSE--------------------------------------------
# # A data frame of known corrections
# corrections <- data.frame(
#   name_x = c("Corvus sp.", "Turdus merulaa"),
#   name_y = c("Corvus corax", "Turdus merula"),
#   user_note = c("Only one Corvus in our tree", "Typo in source data")
# )
# 
# result4 <- reconcile_tree(
#   x = my_data,
#   tree = my_tree,
#   overrides = corrections
# )
# 
# # Or from a CSV file:
# result5 <- reconcile_tree(
#   x = my_data,
#   tree = my_tree,
#   overrides = "lab_corrections.csv"
# )

## ----multi, eval = FALSE------------------------------------------------------
# # Suppose you have several data frames to reconcile against one tree.
# # `my_ecology_data`, `my_morpho_data`, and `my_tree` are **hypothetical**
# # user-supplied objects; substitute your own.
# datasets <- list(
#   traits  = trait_data,        # defined above
#   ecology = my_ecology_data,   # your own data frame
#   morpho  = my_morpho_data     # your own data frame
# )
# 
# result6 <- reconcile_multi(datasets, my_tree)
# print(result6)

## ----workflow, eval = FALSE---------------------------------------------------
# library(prepR4pcm)
# 
# # 1. Load your data and tree (hypothetical paths -- substitute your own)
# my_data <- read.csv("species_traits.csv")
# my_tree <- ape::read.tree("species_tree.nwk")
# 
# # 2. Reconcile
# result <- reconcile_tree(my_data, my_tree, authority = "col")
# 
# # 3. Review
# print(result)
# reconcile_summary(result, detail = "mismatches_only")
# 
# # 4. Fix manually if needed
# result <- reconcile_override(result, "Corvus sp.", "Corvus corax",
#                              note = "Only one Corvus in tree")
# 
# # 5. Apply
# aligned <- reconcile_apply(result, data = my_data, tree = my_tree,
#                             drop_unresolved = TRUE)
# 
# # 6. Analyse
# # aligned$data and aligned$tree are ready for caper, phytools, MCMCglmm, etc.

