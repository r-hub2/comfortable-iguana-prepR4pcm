# Dataset documentation ----------------------------------------------------

#' AVONET morphological trait data (subset)
#'
#' A subset of ~920 bird species from the AVONET database (BirdLife
#' taxonomy), covering 12 passerine families within the Corvoidea and
#' allied clades. Contains morphological measurements and ecological
#' traits.
#'
#' @format A data frame with ~920 rows and 16 columns:
#' \describe{
#'   \item{Species1}{Scientific name (BirdLife taxonomy)}
#'   \item{Family1}{Family}
#'   \item{Order1}{Order}
#'   \item{Beak.Length_Culmen}{Beak length from culmen (mm)}
#'   \item{Beak.Length_Nares}{Beak length from nares (mm)}
#'   \item{Beak.Width}{Beak width (mm)}
#'   \item{Beak.Depth}{Beak depth (mm)}
#'   \item{Tarsus.Length}{Tarsus length (mm)}
#'   \item{Wing.Length}{Wing length (mm)}
#'   \item{Mass}{Body mass (g)}
#'   \item{Habitat}{Primary habitat code}
#'   \item{Habitat.Density}{Habitat density code}
#'   \item{Migration}{Migration status}
#'   \item{Trophic.Level}{Trophic level}
#'   \item{Trophic.Niche}{Trophic niche}
#'   \item{Primary.Lifestyle}{Primary lifestyle}
#' }
#'
#' @source Tobias et al. (2022) AVONET: morphological, ecological and
#'   geographical data for all birds. *Ecology Letters* 25:581--597.
#'   \doi{10.1111/ele.13898}
"avonet_subset"


#' Nest trait data (subset)
#'
#' A subset of ~920 bird species from the global nest trait database (v2),
#' covering the same Corvoidea + allied families as [avonet_subset].
#' Contains nest site and structure information.
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{Scientific_name}{Scientific name (HBW/BirdLife v5 taxonomy)}
#'   \item{Order}{Order}
#'   \item{Family}{Family}
#'   \item{Common_name}{English common name}
#'   \item{NestSite_ground}{Ground nesting (0/1)}
#'   \item{NestSite_tree}{Tree nesting (0/1)}
#'   \item{NestSite_nontree}{Non-tree elevated nesting (0/1)}
#'   \item{NestSite_cliff_bank}{Cliff/bank nesting (0/1)}
#'   \item{NestStr_scrape}{Scrape nest (0/1)}
#'   \item{NestStr_platform}{Platform nest (0/1)}
#'   \item{NestStr_cup}{Cup nest (0/1)}
#'   \item{NestStr_dome}{Dome nest (0/1)}
#'   \item{NestStr_primary_cavity}{Primary cavity nester (0/1)}
#'   \item{NestStr_second_cavity}{Secondary cavity nester (0/1)}
#' }
#'
#' @source Chia et al. (2023) A global database of bird nest traits.
#'   *Scientific Data* 10:923. \doi{10.1038/s41597-023-02837-1}
"nesttrait_subset"


#' Plumage lightness data (subset)
#'
#' A subset of ~650 passerine species from Delhey et al. (2019), with
#' plumage lightness measurements and climate variables. Covers species
#' from the same families as [avonet_subset] that have plumage data.
#' Note that species names use underscores (e.g.,
#' `"Corvus_corax"`), making this useful for demonstrating name
#' normalisation.
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{TipLabel}{Species name with underscores (tree tip label format)}
#'   \item{family}{Family name}
#'   \item{annual_mean_temperature}{Annual mean temperature at range centroid}
#'   \item{annual_precipitation}{Annual precipitation at range centroid}
#'   \item{lightness_male}{Mean plumage lightness, males}
#'   \item{lightness_female}{Mean plumage lightness, females}
#' }
#'
#' @source Delhey et al. (2019) Reconciling ecogeographical rules: rainfall
#'   and temperature predict global colour variation in the largest bird
#'   radiation. *Ecology Letters* 22:726--736. \doi{10.1111/ele.13233}
"delhey_subset"


#' BirdLife-BirdTree taxonomy crosswalk
#'
#' A crosswalk mapping species names between BirdLife International taxonomy
#' and the BirdTree (Jetz et al. 2012) taxonomy. This is useful as a
#' pre-built override table for reconciling datasets that use BirdLife names
#' against phylogenies that use BirdTree names. See
#' [reconcile_crosswalk()] to convert this into an overrides table.
#'
#' @format A data frame with ~11,000 rows and 4 columns:
#' \describe{
#'   \item{Species1}{Species name in BirdLife taxonomy}
#'   \item{Species3}{Species name in BirdTree taxonomy}
#'   \item{Match.type}{Type of match: `"1BL to 1BT"` (one-to-one),
#'     `"Many BL to 1BT"` (lump), `"1BL to many BT"` (split),
#'     `"Extinct"`, `"Newly described species"`, `"Invalid taxon"`}
#'   \item{Match.notes}{Additional notes on the match}
#' }
#'
#' @source The crosswalk is distributed as supporting information with
#'   the AVONET database release (Tobias et al. 2022). It maps two
#'   underlying taxonomies, both of which should be cited if you use
#'   the crosswalk in published work --- see the references below.
#'
#' @references
#' Tobias, J.A. et al. (2022) AVONET: morphological, ecological and
#' geographical data for all birds. \emph{Ecology Letters}
#' 25:581--597. \doi{10.1111/ele.13898}
#'
#' Jetz, W., Thomas, G.H., Joy, J.B., Hartmann, K. & Mooers, A.O.
#' (2012) The global diversity of birds in space and time.
#' \emph{Nature} 491:444--448. \doi{10.1038/nature11631}
"crosswalk_birdlife_birdtree"


#' Jetz (2012) phylogenetic tree (subset)
#'
#' A pruned version of the BirdTree Stage 2 maximum clade credibility tree
#' (Hackett backbone), containing ~660 species from the Corvoidea and
#' allied passerine families. Deliberately smaller than [avonet_subset]
#' (~920 species) so that reconciliation produces unresolved species
#' suitable for [reconcile_augment()]. Tip labels use underscores.
#'
#' @format An object of class `phylo` (from the ape package).
#'
#' @source Jetz et al. (2012) The global diversity of birds in space and
#'   time. *Nature* 491:444--448. \doi{10.1038/nature11631}
"tree_jetz"


#' Clements 2025 phylogenetic tree (subset)
#'
#' A pruned version of the Clements 2025 taxonomy phylogenetic tree,
#' containing ~850 species from the same families. Larger than
#' [tree_jetz] because the Clements taxonomy recognises more species
#' in these clades. Tip labels use underscores.
#'
#' @format An object of class `phylo` (from the ape package).
#'
#' @source Clements et al. (2025) eBird/Clements Checklist of Birds of
#'   the World, v2025.
"tree_clements25"


# ---------------------------------------------------------------------------
# Mammal example data, used by the database-assembly vignette
# (vignettes/db-assembly-workflow_mammals.Rmd).
# ---------------------------------------------------------------------------

#' Amniote-style mammal life-history sample
#'
#' A ~5,000-species sample of mammal life-history records, prepared to
#' mirror the structure of the Amniote Life-History Database. Used by
#' the `db-assembly-workflow_mammals` vignette to demonstrate
#' assembling trait data from multiple sources before reconciling
#' against a phylogenetic tree.
#'
#' @format A tibble with ~4,953 rows and 5 columns:
#' \describe{
#'   \item{`name`}{Length-1 character vector. Scientific name (genus
#'     species), space-separated. Some rows carry trinomials.}
#'   \item{`female_body_mass_g`}{Numeric. Female adult body mass (g);
#'     `NA` when unknown.}
#'   \item{`adult_body_mass_g`}{Numeric. Sex-pooled adult body mass
#'     (g); `NA` when unknown.}
#'   \item{`litter_or_clutch_size_n`}{Numeric. Mean offspring per
#'     reproductive event; `NA` when unknown.}
#'   \item{`litters_or_clutches_per_y`}{Numeric. Number of reproductive
#'     events per year; `NA` when unknown.}
#' }
#'
#' @source Myhrvold et al. (2015) An amniote life-history database to
#'   perform comparative analyses with birds, mammals, and reptiles.
#'   *Ecology* 96:3109. \doi{10.1890/15-0846R.1}
"mammal_amniote_example"


#' PanTHERIA-style mammal life-history sample
#'
#' A ~5,400-species sample of mammal life-history records, prepared to
#' mirror the structure of the PanTHERIA database. Used by the
#' `db-assembly-workflow_mammals` vignette.
#'
#' @format A tibble with ~5,416 rows and 4 columns:
#' \describe{
#'   \item{`MSW05_Binomial`}{Length-1 character vector. Scientific name
#'     under MSW3 (Mammal Species of the World 3) taxonomy.}
#'   \item{`5-1_AdultBodyMass_g`}{Numeric. Adult body mass (g); `NA`
#'     when unknown.}
#'   \item{`15-1_LitterSize`}{Numeric. Mean litter size; `NA` when
#'     unknown.}
#'   \item{`16-1_LittersPerYear`}{Numeric. Litters per year; `NA` when
#'     unknown.}
#' }
#'
#' @source Jones et al. (2009) PanTHERIA: a species-level database of
#'   life history, ecology, and geography of extant and recently
#'   extinct mammals. *Ecology* 90:2648.
#'   \doi{10.1890/08-1494.1}
"mammal_pantheria_example"


#' TetrapodTraits-style mammal sample
#'
#' A ~5,900-species sample of mammal trait records, prepared to
#' mirror the structure of the TetrapodTraits 1.0.0 database. Used by
#' the `db-assembly-workflow_mammals` vignette.
#'
#' @format A tibble with ~5,911 rows and 3 columns:
#' \describe{
#'   \item{`Scientific.Name`}{Length-1 character vector. Scientific
#'     name (genus species), period-separated genus.species column
#'     name as in the source release.}
#'   \item{`BodyMass_g`}{Numeric. Body mass (g); `NA` when unknown.}
#'   \item{`LitterSize`}{Numeric. Mean litter size; `NA` when
#'     unknown.}
#' }
#'
#' @source Moura et al. (2024) A phylogeny-informed characterisation
#'   of global tetrapod traits addresses data gaps and biases.
#'   *PLOS Biology* 22:e3002658.
#'   \doi{10.1371/journal.pbio.3002658}
"mammal_tetrapodtraits_example"


#' Mammal phylogenetic tree (example)
#'
#' A 5,987-tip subset of the Upham, Esselstyn & Jetz (2019) VertLife
#' mammal phylogeny, used by the `db-assembly-workflow_mammals`
#' vignette to demonstrate reconciling species names from multiple
#' trait sources against a tree. Tip labels use underscores
#' (`Genus_species`); 76 tips carry an `X_` prefix, denoting Mesozoic
#' stem-mammal fossils grafted onto the molecular backbone via the
#' Upham et al. "backbone-and-patch" framework.
#'
#' Source confirmed by Santiago Ortega, who contributed the data, on
#' issue #11.
#'
#' If you use this tree in published work, please cite Upham et al.
#' (2019) directly. The bundled object is a *subset* used for examples
#' only --- for analysis-grade trees, download the full credible set
#' from <https://vertlife.org/phylosubsets/>.
#'
#' @format An object of class `phylo` (from the ape package), with
#'   5,987 tips and 5,986 internal nodes.
#'
#' @source Upham, N.S., Esselstyn, J.A. & Jetz, W. (2019) Inferring
#'   the mammal tree: Species-level sets of phylogenies for questions
#'   in ecology, evolution, and conservation. \emph{PLOS Biology}
#'   17(12):e3000494. \doi{10.1371/journal.pbio.3000494}.
#'   Full credible sets at \url{https://vertlife.org/phylosubsets/}.
#'
#' @references
#' Other published mammal phylogenies suitable for comparative
#' analysis (alternatives to Upham et al. 2019):
#'
#' Faurby, S. & Svenning, J.-C. (2015) A species-level phylogeny of
#' all extant and late Quaternary extinct mammals using a novel
#' heuristic-hierarchical Bayesian approach. \emph{Molecular
#' Phylogenetics and Evolution} 84:14--26.
#' \doi{10.1016/j.ympev.2014.11.001}
#'
#' Bininda-Emonds, O.R.P. et al. (2007) The delayed rise of
#' present-day mammals. \emph{Nature} 446:507--512.
#' \doi{10.1038/nature05634}
"mammal_tree_example"
