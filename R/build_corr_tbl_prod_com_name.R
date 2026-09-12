#' Build the production common name manual corrections table
#'
#' Constructs a manual correction table of standardized common names for
#' broad taxonomic groups (e.g., class, order, family) that appear in FAO
#' or SAU production data with inconsistent or absent common name values.
#' Called by [correct_prod_common_names()] to resolve production `CommonName`
#' values that are missing or vary across records sharing the same `SciName`.
#'
#' @details
#' # Workflow to make manual corrections:
#'
#' ## 1) Identify which taxa need corrections
#'
#' * The `common_name_multiples` dataframe is output from
#'   [correct_prod_common_names()]. View it to see which `SciName` values
#'   have more than one associated `CommonName` and require a correction.
#'
#' ## 2) Determine the correct common name
#'
#' * Prefer the most widely used and unambiguous name for the taxonomic rank
#'   represented (e.g., use `"ray-finned fishes"` for `actinopterygii`, not a
#'   species-level common name). 
#' * This process removes some information about taxa that could be extracted
#'   and used by ARTIS. This is known, but not within the scope of ARTIS at the moment.
#'
#' ## 3) Add a new row to the tribble
#'
#' 1) Add a new row to `prod_common_name_corrections` 
#' 2) Fill in `SciName` as it appears in corrected production taxa.
#' 3) Fill in `CommonName_corrected` with the standardized common name.
#' 4) Fill in `notes` with rationale, source, or any caveats for your future
#'   self or a colleague.
#' 5) Test additions by calling `devtools::load_all()` and
#'   `build_corr_tbl_prod_com_name()`.
#'
#' @return
#' A tibble with one row per taxon requiring a common name correction. Columns:
#'
#' * `SciName` — scientific name (at any taxonomic rank) as it appears in
#'   FAO or SAU production taxa table.
#' * `CommonName_corrected` — standardized English common name to use in place
#'   of any inconsistent or missing `CommonName` values for this taxon.
#' * `notes` — additional context, source, or rationale for the correction;
#'   `NA` when no note is needed.
#'
#' @seealso
#' * [correct_prod_common_names()] — applies this correction table to
#'   production data to standardize `CommonName` values.
#'
#' @importFrom tibble tribble
#' @export
build_corr_tbl_prod_com_name <- function(
) {

  prod_common_name_corrections <- tribble(
    ~SciName,   ~CommonName_corrected,              ~notes,
    "actinopterygii", "ray-finned fishes",               NA,
    "bivalvia",       "bivalves",                        NA,
    "clupeiformes",   "clupeoids",                       NA,
    "crustacea",      "crustaceans",                     NA,
    "elasmobranchii", "elasmobranchs",                   NA,
    "gobiidae",       "gobies",                          NA,
    "mollusca",       "molluscs",                        NA,
    "palaemonidae",   "palaemonid shrimps and prawns",   NA,
    "perciformes",    "perch-like fishes",               NA,
  )
  
  return(prod_common_name_corrections)
}