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
#' * Cross-check these recommended sources for common / vernacular names: 
#'   *  gbif.org
#'   *  wikipedia.org
#'   *  fisheries.noaa.gov/find-species
#'   *  britannica.com
#' * Prefer the most widely used and unambiguous name for the taxonomic rank
#'   represented (e.g., use `"ray-finned fishes"` for `actinopterygii`, not a
#'   species-level common name). 
#' * This process removes some information about taxa that could be extracted
#'   and used by ARTIS. This is known, but not within the scope of ARTIS at the moment.
#' * Enter text with all lower case. 
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
#' @section NA SciName entries:
#' Taxa with `NA` for `SciName` in `common_name_multiples` cannot be corrected
#' here — there is no key to join on. Those cases must be resolved upstream in
#' the production taxa cleaning process.
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
    ~SciName,                    ~CommonName_corrected,              ~notes,
    # original corrections
    "actinopterygii",            "ray-finned fishes",                NA,
    "bivalvia",                  "bivalves",                         NA,
    "clupeiformes",              "clupeoids",                        NA,
    "crustacea",                 "crustaceans",                      NA,
    "elasmobranchii",            "elasmobranchs",                    NA,
    "gobiidae",                  "gobies",                           NA,
    "mollusca",                  "molluscs",                         NA,
    "palaemonidae",              "palaemonid shrimps and prawns",    NA,
    "perciformes",               "perch-like fishes",                NA,
    # new corrections
    "alosa",                     "shads",                            NA,
    "astacidae",                 "astacid crayfishes",               NA,
    "asteroidea",                "sea stars",                        NA,
    "branchiopoda",              "branchiopods",                     NA,
    "clarias",                   "clarias catfishes",                NA,
    "clupeidae",                 "herrings, sardines and anchovies", NA,
    "decapoda",                  "decapods",                         NA,
    "epinephelus",               "epinephelus groupers",             NA,
    "gadus macrocephalus",       "pacific cod",                      "G. macrocephalus is Pacific cod; greenland cod entries are likely misclassified G. ogac or G. morhua records",
    "labridae",                  "wrasses",                          NA,
    "lutjanidae",                "snappers and jobfishes",           NA,
    "merluccius",                "hakes",                            NA,
    "morone",                    "atlantic temperate basses",        NA,
    "mytella bicolor",           "guyana swamp mussel",              "Two common names reflect regional usage; guyana swamp mussel is the more widely cited name for this species",
    "ommastrephes bartramii",    "neon flying squid",                "Neon flying squid is the standard common name; webbed flying squid is a less common synonym",
    "oreochromis",               "blue tilapias",                         NA,
    "osteichthyes",              "bony fishes",                      NA,
    "pandalidae",                "pandalid shrimps",                 NA,
    "parastacidae",              "southern hemisphere crayfishes",   NA,
    "penaeidae",                 "penaeid shrimps",                  NA,
    "perciformes/scorpaenoidei", "scorpaenoid perciform fishes",     NA,
    "serrasalmidae",             "piranhas, pacus and relatives",    NA,
    "sparidae",                  "porgies and seabreams",            NA,
    "thunnus",                   "true tunas",                       NA,
    "xiphopenaeus kroyeri",      "atlantic seabob",                  "X. kroyeri is the Atlantic seabob; pacific seabob records likely reflect misclassification or a data entry error",
  )

  return(prod_common_name_corrections)
}
