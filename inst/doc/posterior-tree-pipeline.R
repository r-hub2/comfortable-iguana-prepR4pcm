## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment  = "#>",
  eval     = FALSE      # most of this requires datelife / pigauto / network
)

## ----setup--------------------------------------------------------------------
# library(prepR4pcm)
# # Suggests packages we'll lean on:
# # install.packages("fishtree")
# # pak::pak("phylotastic/datelife")
# # Optional, for the pooling step at the end:
# # pak::pak("itchyshin/pigauto")

## ----data---------------------------------------------------------------------
# # Your trait data — typically loaded from disk. Names need not be
# # tidy: `Esox_lucius` carries an underscore, which pr_get_tree()
# # normalises away.
# my_data <- data.frame(
#   species   = c("Salmo salar", "Esox_lucius", "Oncorhynchus mykiss"),
#   body_size = c(98, NA, 50)        # NA is fine; pigauto will impute it
# )

## ----retrieve, eval = FALSE---------------------------------------------------
# trees <- pr_get_tree(my_data,
#                      species_col = "species",
#                      source      = "fishtree",
#                      n_tree      = 50)
# 
# class(trees$tree)      # "multiPhylo"
# length(trees$tree)     # 50
# trees$matched          # the 3 species, in their original input form
# trees$unmatched        # character(0) — every species was placed
# 
# # Per-tree provenance: one list entry per returned tree, each
# # recording source_index, citation, calibration_method, and n_tips.
# length(trees$backend_meta$tree_provenance)   # 50
# trees$backend_meta$tree_provenance[[1]]      # the provenance of tree 1

## ----datelife, eval = FALSE---------------------------------------------------
# trees <- pr_get_tree(my_data,
#                      species_col = "species",
#                      source      = "datelife",
#                      n_tree      = 50)
# # trees$tree is multiPhylo; per-source citations are in
# # trees$backend_meta$tree_provenance[[i]]$citation

## ----date, eval = FALSE-------------------------------------------------------
# my_topology <- ape::read.tree("my_topology.nex")
# dated_trees <- pr_date_tree(my_topology, n_dated = 50)
# class(dated_trees$tree)        # "multiPhylo"
# length(dated_trees$tree)       # up to 50

## ----reconcile, eval = FALSE--------------------------------------------------
# rec <- reconcile_tree(my_data, trees$tree[[1]],
#                       x_species = "species",
#                       authority = NULL)   # tree tips are plain binomials
# rec        # match summary: 1 exact, 2 normalized, 0 unresolved
# 
# aligned <- reconcile_apply(rec,
#                            data            = my_data,
#                            tree            = trees$tree[[1]],
#                            species_col     = "species",
#                            drop_unresolved = TRUE)
# 
# aligned$data    # one row per tree tip — the model-ready frame
# #>               species body_size
# #> 1         Salmo salar        98
# #> 2         Esox_lucius        NA
# #> 3 Oncorhynchus mykiss        50

## ----cite, eval = FALSE-------------------------------------------------------
# # Plain text for an email or a methods paragraph
# pr_cite_tree(trees)
# 
# # Markdown for a GitHub issue or PR description
# cat(pr_cite_tree(trees, format = "markdown"))
# 
# # BibTeX for a manuscript bibliography (sanity-check before submitting)
# cat(pr_cite_tree(trees, format = "bibtex"))

## ----pigauto, eval = FALSE----------------------------------------------------
# library(pigauto)
# 
# # 5 imputations per tree × 50 trees = 250 complete datasets
# mi <- multi_impute_trees(aligned$data,
#                          trees      = trees$tree,
#                          m_per_tree = 5)
# 
# # Fit your model to each completed dataset. Replace the formula
# # with whatever your hypothesis is.
# fits <- with_imputations(mi, function(df) {
#   glmmTMB::glmmTMB(body_size ~ 1 + (1 | species),
#                    data = df)
# })
# 
# # Pool via Rubin's rules — the standard errors reflect BOTH the
# # imputation uncertainty AND the tree-posterior uncertainty.
# pooled <- pool_mi(fits,
#                   coef_fun = function(fit) fixef(fit)$cond,
#                   vcov_fun = function(fit) vcov(fit)$cond)
# 
# pooled

