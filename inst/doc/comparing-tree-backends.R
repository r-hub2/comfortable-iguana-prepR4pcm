## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment  = "#>",
  eval     = FALSE
)

## ----setup, eval = TRUE-------------------------------------------------------
library(prepR4pcm)
# install.packages(c("rotl", "rtrees", "clootl", "fishtree")) # CRAN
# pak::pak("phylotastic/datelife")                        # GitHub (heavy)

## ----status, eval = TRUE------------------------------------------------------
pr_get_tree_status()

## ----retrieve-----------------------------------------------------------------
# species <- c("Salmo salar", "Esox lucius", "Oncorhynchus mykiss",
#              "Gadus morhua", "Thunnus thynnus")
# 
# # Three backends that all cover fish
# res_rotl     <- pr_get_tree(species, source = "rotl")
# res_fishtree <- pr_get_tree(species, source = "fishtree")
# res_rtrees   <- pr_get_tree(species, source = "rtrees", taxon = "fish")

## ----compare------------------------------------------------------------------
# cmp <- pr_tree_compare(
#   rotl     = res_rotl,
#   fishtree = res_fishtree,
#   rtrees   = res_rtrees
# )
# cmp

## ----cite---------------------------------------------------------------------
# pr_cite_tree(res_fishtree, format = "markdown")

## ----unique-to----------------------------------------------------------------
# cmp$unique_to$rotl     # species rotl placed but the others didn't
# cmp$unique_to$fishtree # species fishtree placed but the others didn't
# cmp$shared_tips        # species placed by all backends

## ----pigauto-multi-backend----------------------------------------------------
# # Fitting across backends (not just within a backend's posterior)
# all_trees <- structure(list(res_rotl$tree, res_fishtree$tree,
#                             res_rtrees$tree[[1]]),
#                        class = "multiPhylo")
# # pigauto::multi_impute_trees(trait_data, all_trees, m_per_tree = 5)

## ----cache--------------------------------------------------------------------
# old_cache <- getOption("prepR4pcm.cache_dir", NULL)
# tmp_cache <- tempfile("prepR4pcm-cache-")
# pr_tree_cache_dir(tmp_cache)
# 
# res_rotl     <- pr_get_tree(species, source = "rotl",
#                              cache = TRUE)
# res_fishtree <- pr_get_tree(species, source = "fishtree",
#                              cache = TRUE)
# res_rtrees   <- pr_get_tree(species, source = "rtrees",
#                              taxon  = "fish", cache = TRUE)
# 
# # See what's cached
# pr_tree_cache_status()
# 
# # Wipe just the rotl entries (e.g. after an OTT version refresh)
# pr_tree_cache_clear(source = "rotl", confirm = FALSE)
# 
# options(prepR4pcm.cache_dir = old_cache)
# unlink(tmp_cache, recursive = TRUE)

## ----project-cache------------------------------------------------------------
# old_cache <- getOption("prepR4pcm.cache_dir", NULL)
# tmp_cache <- tempfile("prepR4pcm-cache-")
# pr_tree_cache_dir(tmp_cache)
# options(prepR4pcm.cache_dir = old_cache)
# unlink(tmp_cache, recursive = TRUE)

