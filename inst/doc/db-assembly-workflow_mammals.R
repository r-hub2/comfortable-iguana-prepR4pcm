## ----include = FALSE----------------------------------------------------------
# This vignette uses dplyr / readr / stringr (and ape, which is in
# Imports). The first three are in Suggests because they're only used
# here, not by the R/ code itself. Set a single eval gate so every
# chunk skips cleanly if any are absent -- the vignette still knits
# and the rest of the package is unaffected.
have_vignette_deps <- requireNamespace("dplyr",   quietly = TRUE) &&
                      requireNamespace("readr",   quietly = TRUE) &&
                      requireNamespace("stringr", quietly = TRUE) &&
                      requireNamespace("ape",     quietly = TRUE)

knitr::opts_chunk$set(
  collapse = TRUE,
  comment  = "#>",
  eval     = have_vignette_deps
)

if (!have_vignette_deps) {
  message(
    "This vignette requires dplyr, readr, stringr, and ape. ",
    "Skipping all code chunks; install the missing package(s) to ",
    "see the worked example."
  )
}

## ----setup, message = FALSE---------------------------------------------------
library(dplyr)
library(readr)
library(stringr)
library(ape)
library(prepR4pcm)

## ----helper-functions, include = FALSE----------------------------------------
pull_number_or_na <- function(df, col) {
  if (is.null(col) || !col %in% names(df)) {
    return(rep(NA_real_, nrow(df)))
  }

  readr::parse_number(
    as.character(df[[col]]),
    na = c("", "NA", "NaN", "-999", "-9999")
  )
}

prep_source <- function(df,
                        source_name,
                        species_col,
                        female_mass_col = NULL,
                        adult_mass_col = NULL,
                        litter_size_col = NULL,
                        litter_y_col = NULL) {
  tibble::tibble(
    source = source_name,
    row_in_source = seq_len(nrow(df)),
    species = as.character(df[[species_col]]),
    female_mass_g = pull_number_or_na(df, female_mass_col),
    adult_mass_g = pull_number_or_na(df, adult_mass_col),
    litter_size_n = pull_number_or_na(df, litter_size_col),
    litters_per_year_n = pull_number_or_na(df, litter_y_col)
  ) |>
    mutate(
      species = stringr::str_squish(species),
      across(
        c(female_mass_g, adult_mass_g, litter_size_n, litters_per_year_n),
        ~ ifelse(is.finite(.x) & .x > 0, .x, NA_real_)
      )
    ) |>
    filter(!is.na(species), species != "")
}

safe_sources <- function(x) {
  paste(sort(unique(stats::na.omit(x))), collapse = "; ")
}

safe_median <- function(x) {
  if (all(is.na(x))) {
    NA_real_
  } else {
    stats::median(x, na.rm = TRUE)
  }
}

## ----load-example-objects-----------------------------------------------------
data(mammal_amniote_example)
data(mammal_pantheria_example)
data(mammal_tetrapodtraits_example)
data(mammal_tree_example)

cat(sprintf("Amniote-like source:        %d rows\n", nrow(mammal_amniote_example)))
cat(sprintf("PanTHERIA-like source:      %d rows\n", nrow(mammal_pantheria_example)))
cat(sprintf("TetrapodTraits-like source: %d rows\n", nrow(mammal_tetrapodtraits_example)))
cat(sprintf("Tree:                       %d tips\n", ape::Ntip(mammal_tree_example)))

## ----inspect-inputs-----------------------------------------------------------
source_columns <- tibble::tibble(
  source = c("Amniote", "PanTHERIA", "TetrapodTraits"),
  n_rows = c(
    nrow(mammal_amniote_example),
    nrow(mammal_pantheria_example),
    nrow(mammal_tetrapodtraits_example)
  ),
  n_columns = c(
    ncol(mammal_amniote_example),
    ncol(mammal_pantheria_example),
    ncol(mammal_tetrapodtraits_example)
  ),
  species_column = c("name", "MSW05_Binomial", "Scientific.Name")
)

knitr::kable(source_columns)

## ----standardise-sources------------------------------------------------------
amniote_std <- prep_source(
  mammal_amniote_example,
  source_name     = "AMNIOTE",
  species_col     = "name",
  female_mass_col = "female_body_mass_g",
  adult_mass_col  = "adult_body_mass_g",
  litter_size_col = "litter_or_clutch_size_n",
  litter_y_col    = "litters_or_clutches_per_y"
)

pantheria_std <- prep_source(
  mammal_pantheria_example,
  source_name     = "PANTHERIA",
  species_col     = "MSW05_Binomial",
  adult_mass_col  = "5-1_AdultBodyMass_g",
  litter_size_col = "15-1_LitterSize",
  litter_y_col    = "16-1_LittersPerYear"
)

tetrapodtraits_std <- prep_source(
  mammal_tetrapodtraits_example,
  source_name     = "TETRAPODTRAITS",
  species_col     = "Scientific.Name",
  adult_mass_col  = "BodyMass_g",
  litter_size_col = "LitterSize"
)

db_long_raw <- bind_rows(
  amniote_std,
  pantheria_std,
  tetrapodtraits_std
)

knitr::kable(slice_head(db_long_raw, n = 10))

## ----source-coverage----------------------------------------------------------
source_coverage <- db_long_raw |>
  group_by(source) |>
  summarise(
    n_records           = n(),
    n_species           = n_distinct(species),
    adult_mass_records  = sum(!is.na(adult_mass_g)),
    female_mass_records = sum(!is.na(female_mass_g)),
    litter_size_records = sum(!is.na(litter_size_n)),
    litter_y_records    = sum(!is.na(litters_per_year_n)),
    .groups = "drop"
  )

knitr::kable(source_coverage)

## ----species-lookup-----------------------------------------------------------
species_lookup <- db_long_raw |>
  distinct(species) |>
  rename(species_raw = species)

knitr::kable(slice_head(species_lookup, n = 10))

## ----reconcile-pass-0---------------------------------------------------------
rec0 <- reconcile_tree(
  x         = species_lookup,
  tree      = mammal_tree_example,
  x_species = "species_raw",
  authority = NULL,
  fuzzy     = FALSE,
  quiet     = TRUE
)

reconcile_summary(rec0, detail = "brief")

## ----mapping-pass-0-----------------------------------------------------------
mapping0 <- reconcile_mapping(rec0)

mapping_preview <- mapping0 |>
  select(any_of(c("name_x", "name_y", "match_type", "in_x", "in_y"))) |>
  arrange(match_type, name_x) |>
  slice_head(n = 15)

knitr::kable(mapping_preview)

## ----review-and-suggestions---------------------------------------------------
review_names <- mapping0 |>
  filter(in_x, match_type %in% c("unresolved", "flagged")) |>
  arrange(match_type, name_x)

if (nrow(review_names) == 0) {
  cat("No unresolved or flagged names in this example.\n")
} else {
  cat(sprintf(
    "Showing 10 of %d unresolved or flagged names.\n\n",
    nrow(review_names)
  ))
  knitr::kable(slice_head(review_names, n = 10) |>
                 select(any_of(c("name_x", "name_y", "match_type",
                                 "in_x", "in_y"))))
}

suggestions0 <- reconcile_suggest(rec0, n = 3, threshold = 0.9)

suggestions_to_review <- suggestions0 |>
  transmute(
    name_x = unresolved,
    name_y = suggestion,
    score  = score
  ) |>
  filter(score >= 0.9, score < 1) |>
  arrange(desc(score))

if (nrow(suggestions_to_review) == 0) {
  cat("No high-confidence, non-perfect suggestions were found.\n")
} else {
  cat(
    "Showing up to 10 high-confidence suggested matches with score below 1.\n\n",
    sep = ""
  )
  knitr::kable(slice_head(suggestions_to_review, n = 10), digits = 3)
}

## ----manual-overrides---------------------------------------------------------
manual_overrides <- suggestions_to_review |>
  slice_head(n = 2) |>
  mutate(user_note = "Accepted from high-confidence reconciliation suggestion") |>
  select(name_x, name_y, user_note)

if (nrow(manual_overrides) == 0) {
  cat("No manual corrections were added in this example.\n")
} else {
  knitr::kable(manual_overrides, digits = 3)
}

## ----apply-manual-overrides---------------------------------------------------
mapping_final <- mapping0 |>
  left_join(
    manual_overrides |>
      rename(manual_name_y = name_y, manual_note = user_note),
    by = "name_x"
  ) |>
  mutate(
    species_tree    = coalesce(manual_name_y, name_y),
    matched_to_tree = species_tree %in% mammal_tree_example$tip.label,
    match_type      = if_else(!is.na(manual_name_y), "manual", match_type),
    notes           = manual_note
  ) |>
  select(-manual_name_y, -manual_note)

## ----final-reconciliation-summary---------------------------------------------
final_reconciliation_summary <- mapping_final |>
  filter(in_x) |>
  count(match_type, name = "n_names") |>
  arrange(desc(n_names), match_type)

knitr::kable(final_reconciliation_summary)

## ----name-map-and-full-database-----------------------------------------------
name_map <- mapping_final |>
  filter(in_x) |>
  transmute(
    species_raw     = name_x,
    species_tree    = species_tree,
    matched_to_tree = matched_to_tree,
    match_type      = match_type,
    notes           = notes
  )

db_full <- db_long_raw |>
  rename(species_raw = species) |>
  left_join(name_map, by = "species_raw") |>
  relocate(source, row_in_source, species_raw, species_tree,
           matched_to_tree, match_type)

db_tree_matched <- db_full |>
  filter(matched_to_tree, !is.na(species_tree))

knitr::kable(
  db_tree_matched |>
    select(source, species_raw, species_tree, match_type,
           adult_mass_g, female_mass_g, litter_size_n,
           litters_per_year_n) |>
    slice_head(n = 10),
  digits = 3
)

## ----species-summary----------------------------------------------------------
db_species_summary <- db_tree_matched |>
  group_by(species_tree) |>
  summarise(
    n_sources_total       = n_distinct(source),
    sources               = safe_sources(source),
    adult_mass_g          = safe_median(adult_mass_g),
    female_mass_g         = safe_median(female_mass_g),
    litter_size_n         = safe_median(litter_size_n),
    litters_per_year_n    = safe_median(litters_per_year_n),
    adult_mass_n_records  = sum(!is.na(adult_mass_g)),
    female_mass_n_records = sum(!is.na(female_mass_g)),
    litter_size_n_records = sum(!is.na(litter_size_n)),
    litter_y_n_records    = sum(!is.na(litters_per_year_n)),
    .groups = "drop"
  ) |>
  mutate(annual_offspring_n = litter_size_n * litters_per_year_n)

## ----trait-coverage-----------------------------------------------------------
trait_coverage <- db_species_summary |>
  summarise(
    n_species               = n(),
    adult_mass_species      = sum(!is.na(adult_mass_g)),
    female_mass_species     = sum(!is.na(female_mass_g)),
    litter_size_species     = sum(!is.na(litter_size_n)),
    litters_per_year_species= sum(!is.na(litters_per_year_n)),
    annual_offspring_species= sum(!is.na(annual_offspring_n))
  )

knitr::kable(trait_coverage)

## ----pcm-objects--------------------------------------------------------------
matched_tips <- intersect(
  mammal_tree_example$tip.label,
  db_species_summary$species_tree
)

tree_pcm <- keep.tip(mammal_tree_example, matched_tips)

pcm_data <- db_species_summary |>
  filter(species_tree %in% tree_pcm$tip.label) |>
  mutate(species = species_tree) |>
  arrange(match(species, tree_pcm$tip.label)) |>
  relocate(species)

stopifnot(identical(pcm_data$species, tree_pcm$tip.label))

## ----alignment-check----------------------------------------------------------
alignment_check <- tibble::tibble(
  object          = c("pcm_data", "tree_pcm"),
  species_or_tips = c(nrow(pcm_data), ape::Ntip(tree_pcm)),
  aligned         = c(
    identical(pcm_data$species, tree_pcm$tip.label),
    identical(pcm_data$species, tree_pcm$tip.label)
  )
)

knitr::kable(alignment_check)

## ----final-database-preview---------------------------------------------------
knitr::kable(
  pcm_data |>
    select(species, adult_mass_g, litter_size_n,
           litters_per_year_n, annual_offspring_n,
           n_sources_total, sources) |>
    slice_head(n = 10),
  digits = 3
)

