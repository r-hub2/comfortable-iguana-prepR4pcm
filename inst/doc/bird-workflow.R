## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(prepR4pcm)

## ----load-data----------------------------------------------------------------
data(avonet_subset)    # AVONET morphological traits (Tobias et al. 2022)
data(tree_jetz)    # Jetz et al. (2012) phylogeny, Corvoidea + allies

cat(sprintf("Data: %d species\n", nrow(avonet_subset)))
cat(sprintf("Tree: %d tips\n", ape::Ntip(tree_jetz)))

# The data uses spaces; the tree uses underscores
head(avonet_subset$Species1, 3)
head(tree_jetz$tip.label, 3)

## ----reconcile-tree-----------------------------------------------------------
result <- reconcile_tree(
  x = avonet_subset,
  tree = tree_jetz,
  x_species = "Species1",
  authority = NULL       # skip synonym lookup for speed
)
print(result)

## ----mapping------------------------------------------------------------------
mapping <- reconcile_mapping(result)

# Match type breakdown
table(mapping$match_type)

# Show normalised matches (formatting differences resolved automatically)
norm <- mapping[mapping$match_type == "normalized",
                c("name_x", "name_y", "notes")]
if (nrow(norm) > 0) head(norm, 5)

# Unresolved: in data but not in tree
unresolved <- mapping[mapping$match_type == "unresolved" & mapping$in_x, ]
cat(sprintf("\nSpecies in data but not in tree: %d\n", nrow(unresolved)))

## ----summary, eval = FALSE----------------------------------------------------
# reconcile_summary(result, detail = "mismatches_only")

## ----apply--------------------------------------------------------------------
aligned <- reconcile_apply(
  result,
  data = avonet_subset,
  tree = tree_jetz,
  species_col = "Species1",
  drop_unresolved = TRUE
)

cat(sprintf("Aligned data: %d rows\nAligned tree: %d tips\n",
            nrow(aligned$data), ape::Ntip(aligned$tree)))

## ----pgls, message = FALSE, warning = FALSE, eval = requireNamespace("caper", quietly = TRUE)----
library(caper)

# reconcile_apply() aligns names so data$Species1 matches tree tip labels
cd <- comparative.data(aligned$tree, aligned$data,
                       names.col = "Species1", vcv = TRUE)

# PGLS: body mass ~ wing length
model_pgls <- pgls(log(Mass) ~ log(Wing.Length), data = cd)
summary(model_pgls)

## ----pglmm, message = FALSE, warning = FALSE, results = "hide", eval = requireNamespace("MCMCglmm", quietly = TRUE)----
library(MCMCglmm)

# Species column as the phylogenetic grouping factor
aligned$data$phylo <- aligned$data$Species1

# Inverse phylogenetic covariance matrix
# Replace any zero-length branches (can arise after pruning)
tree_mcmc <- aligned$tree
tree_mcmc$edge.length[tree_mcmc$edge.length < .Machine$double.eps] <- 1e-6
inv_phylo <- inverseA(tree_mcmc, nodes = "ALL", scale = FALSE)

# PGLMM: continuous response
prior <- list(R = list(V = 1, nu = 0.002),
              G = list(G1 = list(V = 1, nu = 0.002)))

model_mcmc <- MCMCglmm(
  log(Mass) ~ log(Wing.Length) + Trophic.Level,
  random = ~phylo,
  family = "gaussian",
  ginverse = list(phylo = inv_phylo$Ainv),
  data = aligned$data,
  prior = prior,
  nitt = 50000, burnin = 10000, thin = 20,
  verbose = FALSE
)

## ----pglmm-summary, eval = requireNamespace("MCMCglmm", quietly = TRUE)-------
summary(model_mcmc)

## ----reconcile-data-----------------------------------------------------------
data(nesttrait_subset)   # Nest traits (Chia et al. 2023)

rec_data <- reconcile_data(
  x = nesttrait_subset,
  y = avonet_subset,
  x_species = "Scientific_name",
  y_species = "Species1",
  authority = NULL,
  quiet = TRUE
)
print(rec_data)

## ----merge-data---------------------------------------------------------------
merged <- reconcile_merge(
  rec_data,
  data_x = nesttrait_subset,
  data_y = avonet_subset,
  species_col_x = "Scientific_name",
  species_col_y = "Species1"
)
cat(sprintf("Merged: %d rows, %d columns\n", nrow(merged), ncol(merged)))

## ----multirow-aggregate, eval = FALSE-----------------------------------------
# # Example: averaging individual measurements to species means
# species_means <- aggregate(
#   cbind(Mass, Wing.Length) ~ Species1,
#   data = individual_measurements,
#   FUN  = mean
# )
# merged <- reconcile_merge(rec_data, species_means, avonet_subset,
#                           species_col_x = "Species1",
#                           species_col_y = "Species1")

## ----multirow-lookup, eval = FALSE--------------------------------------------
# # Reconcile on unique species
# species_level <- data.frame(
#   Species1 = unique(individual_measurements$Species1)
# )
# rec <- reconcile_data(species_level, avonet_subset,
#                       x_species = "Species1", y_species = "Species1",
#                       authority = NULL, quiet = TRUE)
# 
# # Join the mapping back to the full, multi-row dataset
# mapping <- reconcile_mapping(rec)
# individual_measurements$species_resolved <- mapping$name_resolved[
#   match(individual_measurements$Species1, mapping$name_x)
# ]

## ----asymmetric, eval = FALSE-------------------------------------------------
# # Keep only species present in both: inner join
# inner <- reconcile_merge(rec_data, small_data, large_data,
#                          species_col_x = "species",
#                          species_col_y = "Species1",
#                          how = "inner")
# 
# # Keep all small_data rows; fill large_data columns with NA
# # for species missing from the reference: left join
# left <- reconcile_merge(rec_data, small_data, large_data,
#                         species_col_x = "species",
#                         species_col_y = "Species1",
#                         how = "left")

## ----crosswalk----------------------------------------------------------------
data(crosswalk_birdlife_birdtree)
table(crosswalk_birdlife_birdtree$Match.type)

## ----make-overrides-----------------------------------------------------------
result_xw <- reconcile_crosswalk_supplement(
  result,
  crosswalk_birdlife_birdtree,
  from_col = "Species1",
  to_col = "Species3",
  match_type_col = "Match.type",
  one_to_one_only = TRUE,
  quiet = TRUE
)

if (result_xw$meta$crosswalk_supplement$n_applied == 0) {
  cat("No reviewed one-to-one crosswalk rows add matches in this subset.\n")
}

# Compare: how many more matches with the crosswalk?
cat(sprintf("Without crosswalk: %d matched\n",
            sum(result$mapping$in_x & result$mapping$in_y, na.rm = TRUE)))
cat(sprintf("With crosswalk:    %d matched\n",
            sum(result_xw$mapping$in_x & result_xw$mapping$in_y, na.rm = TRUE)))

## ----manual-overrides, eval = FALSE-------------------------------------------
# my_overrides <- data.frame(
#   name_x    = c("Old name A", "Old name B"),
#   name_y    = c("Tree name A", "Tree name B"),
#   user_note = c("Reclassified in 2023", "Spelling correction")
# )
# result <- reconcile_tree(my_data, my_tree, overrides = my_overrides)

## ----multi-tree---------------------------------------------------------------
data(tree_clements25)  # Clements 2025 tree

results <- reconcile_to_trees(
  x = avonet_subset,
  trees = list(
    jetz      = tree_jetz,
    clements  = tree_clements25
  ),
  x_species = "Species1",
  authority = NULL
)

# Compare overlap across trees
sapply(results, function(r) {
  c(matched = sum(r$mapping$in_x & r$mapping$in_y, na.rm = TRUE),
    unresolved_x = r$counts$n_unresolved_x)
})

## ----fuzzy, eval = FALSE------------------------------------------------------
# result <- reconcile_tree(
#   x = my_data,
#   tree = my_tree,
#   fuzzy = TRUE,              # enable fuzzy matching
#   fuzzy_threshold = 0.9,     # minimum similarity (0-1)
#   resolve = "flag"           # flag low-confidence matches for review
# )
# 
# # Check flagged matches
# flagged <- reconcile_mapping(result)
# flagged[flagged$match_type == "flagged", c("name_x", "name_y", "match_score")]

## ----augment------------------------------------------------------------------
aug <- reconcile_augment(
  result,
  tree_jetz,
  where = "genus",                # sister to a random congener
  branch_length = "congener_median",  # median terminal branch of congeners
  seed = 42,                      # for reproducibility
  quiet = TRUE
)

cat(sprintf("Original tips: %d\nAugmented tips: %d\n",
            ape::Ntip(aug$original), ape::Ntip(aug$tree)))
cat(sprintf("Added: %d | Skipped (no congener): %d\n",
            nrow(aug$augmented), nrow(aug$skipped)))

# Which species were added, and where?
if (nrow(aug$augmented) > 0) head(aug$augmented[, c("species", "placed_near", "branch_length")])

## ----augment-apply, eval = FALSE----------------------------------------------
# aligned_aug <- reconcile_apply(
#   result,
#   data = avonet_subset,
#   tree = aug$tree,        # augmented tree, not the original
#   species_col = "Species1",
#   drop_unresolved = FALSE  # keep augmented tips (they are now in the tree)
# )

## ----export, eval = FALSE-----------------------------------------------------
# out_dir <- file.path(tempdir(), "prepr4pcm-export")
# reconcile_export(
#   result,
#   data = avonet_subset,
#   tree = tree_jetz,
#   species_col = "Species1",
#   dir = out_dir,
#   prefix = "avonet_jetz"
# )
# # Writes: avonet_jetz_data.csv, avonet_jetz_tree.nex, avonet_jetz_mapping.csv
# unlink(out_dir, recursive = TRUE)

## ----report, eval = FALSE-----------------------------------------------------
# report_file <- tempfile(fileext = ".html")
# reconcile_report(result, file = report_file)
# unlink(report_file)

