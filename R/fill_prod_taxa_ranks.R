#' Fill and expand taxonomic rank columns in production taxa classification
#'
#' Applies rule-based gap-filling and forward slash normalization to the
#' production taxa classification table, adding `Infraclass` and `Suborder`
#' columns and filling `Kingdom` and `Phylum` where missing.
#'
#' @details
#' Called in `01-clean-input-data.R` as the final taxonomy preparation step,
#' immediately after [calc_taxa_habitat()]. The output is assigned back to
#' `prod_taxa_classification` for use in downstream matching and estimation
#' steps.
#'
#' ## Gap-filling rules
#'
#' Two new rank columns are initialized as `NA` and populated via
#' `rows_update()`:
#'
#' * **`Infraclass`** — relocated after `Order`; derived from `Order` for shark
#'   and ray clades (WoRMS classification), and assigned directly by `SciName`
#'   for the synthetic `"batoidea"` and `"selachii"` rows added upstream in
#'   `01-clean-input-data.R`.
#' * **`Suborder`** — relocated after `Family`; populated from the suborder
#'   segment encoded in `perciformes/*` `Order` values (see *Forward slash
#'   handling* below).
#'
#' Three existing columns are gap-filled by rule:
#'
#' * **`Kingdom`** — set to `"animalia"` for all rows.
#' * **`Phylum`** — derived from `Superclass` (chordate superclasses) and
#'   `Class` (`"thecostraca"`). A named exception assigns `"annelida"` to
#'   `"sipunculus nudus"` directly by `SciName`.
#' * **`Order`** — `perciformes/*` compound values are standardized to
#'   `"perciformes"` after the suborder segment is extracted.
#'
#' ## Forward slash handling and validation
#'
#' FishBase and SeaLifeBase encode two kinds of compound taxonomic values using
#' a forward slash:
#'
#' * **`perciformes/*`** — the `Order` value encodes both order and suborder
#'   (e.g. `"perciformes/serranoidei"`). The suborder segment is extracted into
#'   `Suborder` via an internal correction table and `Order` is standardized to
#'   `"perciformes"`.
#' * **`*/misc`** — a non-specific grouping suffix, used as a stand in for 
#'   `"incertae sedis"` as used in WoRMS. The `/misc` segment is
#'   removed in-place across all rank columns.
#'
#' Two `cli` validation checks are emitted at run time:
#'
#' * **Unhandled slash patterns** — any forward slash value that does not match
#'   either known pattern triggers a warning listing the unhandled values with
#'   developer instructions to extend the function.
#' * **Unmatched perciformes suborders** — any `perciformes/*` suborder segment
#'   not present in the internal `add_suborder_by_order` correction table
#'   triggers a warning with instructions to add the missing entry.
#'
#' @param prod_taxa Data frame. The production taxa classification table after
#'   [calc_taxa_habitat()] and the inline manual taxonomy corrections applied
#'   in `01-clean-input-data.R`. Typically `prod_taxa_classification`.
#'
#' @return
#' A data frame with the same rows as `prod_taxa` and two additional columns:
#'
#' * `Infraclass` — positioned after `Order`; populated for shark and ray clade
#'   taxa; `NA` elsewhere.
#' * `Suborder` — positioned after `Family`; populated for `perciformes/*`
#'   taxa; `NA` elsewhere.
#'
#' Existing columns modified in place:
#'
#' * `Kingdom` — set to `"animalia"` for all rows.
#' * `Phylum` — gap-filled from adjacent rank values and named exceptions.
#' * `Order` — `perciformes/*` compound values standardized to `"perciformes"`.
#' * All rank columns — `/misc` suffixes removed where present.
#'
#' @seealso
#' * [calc_taxa_habitat()] — called immediately before this function in
#'   `01-clean-input-data.R`
#' * [match_prod_taxa_to_fb_slb()] — upstream function that produces the
#'   `prod_taxa_classification` input passed to this function
#'
#' ---
#'
#' *Documentation generated with `claude-sonnet-4-5` using the
#' [`roxygen2-function-documentation`](https://github.com/Seafood-Globalization-Lab/lab-genAI-toolbox/commit/7d207f2be4e1b1f670e8bf423e563b8a920949d9)
#' skill (commit `7d207f2`).*
#'
#' @import dplyr
#' @import cli
#' @importFrom magrittr %>%
#' @importFrom stringr str_detect str_remove str_subset
#' @importFrom tibble tribble
#' @export

fill_prod_taxa_ranks <- function(
  prod_taxa) {

  # Fill Kingdom (universally animalia) ------------------------------------

  prod_taxa_expanded <- prod_taxa %>%
    mutate(Kingdom = "animalia") %>% 

  # Add Infraclass taxa rank --------------------------------------------------
  # Infraclass does not exist in prod_taxa; initialize as NA then assign values
  # via rows_update(). Two passes are needed: SciName-keyed for the synthetic
  # non-FB/SLB rows added upstream, and Order-keyed for shark/ray clades.

    mutate(Infraclass = NA_character_) %>%
    relocate(Infraclass, .after = Order) %>% 

  # Add Suborder taxa rank -------------------------------------------------
  # Suborder does not exist in prod_taxa; initalize as NA then assign values below.
  # Suborder column specifically used to accomidate `perciformes/serranoidei` Order values
  # that contain order and suborder information - inhertited from FB/SLB. 

    mutate(Suborder = NA_character_) %>%
    relocate(Suborder, .after = Family) 

  # Fill Phylum ----------------------------------------------------

    add_phylum_by_superclass <- tribble(
      ~Superclass,        ~Phylum,
      "osteichthyes",     "chordata",
      "chondrichthyes",   "chordata",
      "agnatha",          "chordata",
      "sarcopterygii",    "chordata"
    )
  
    add_phylum_by_class <- tribble(
      ~Class,             ~Phylum,
      "thecostraca",      "anthropoda"
    )

  ## Phylum exceptions — taxa missing from SeaLifeBase / FishBase --------------
  # Known permanent exceptions not covered by the rule-based Phylum gap-fill above.

  # FIXIT: This exception - Class == "no assigned" - write test for other values like this
  phylum_exceptions <- tribble(
    ~SciName,           ~Phylum,
    "sipunculus nudus", "annelida"
  )

  ## Update prod_taxa  ------------------------------------------------------


  prod_taxa_expanded <- prod_taxa_expanded %>%
    rows_update(add_phylum_by_superclass, by = "Superclass", unmatched = "ignore") %>% 
    rows_update(add_phylum_by_class, by = "Class", unmatched = "ignore") %>% 
    rows_update(phylum_exceptions, by = "SciName", unmatched = "ignore")


  # Fill Infraclass --------------------------------------------------------

  # SciName-based Infraclass assignments
  add_infraclass_by_sciname <- tribble(
    ~SciName,   ~Infraclass,
    "batoidea", "batoidea",
    "selachii", "selachii"
  )

  # Order-based Infraclass assignments (WoRMS classification)
  add_infraclass_by_order <- tribble(
    ~Order,                ~Infraclass,
    # Infraclass Selachii — Galeomorphi and Squalomorphi Superorder children
    "carcharhiniformes",   "selachii",
    "heterodontiformes",   "selachii",
    "lamniformes",         "selachii",
    "orectolobiformes",    "selachii",
    "echinorhiniformes",   "selachii",
    "hexanchiformes",      "selachii",
    "pristiophoriformes",  "selachii",
    "squaliformes",        "selachii",
    "squatiniformes",      "selachii",
    # Infraclass Batoidea
    "myliobatiformes",     "batoidea",
    "rajiformes",          "batoidea",
    "rhinopristiformes",   "batoidea",
    "torpediniformes",     "batoidea"
  )


  ## Add to prod_Taxa -------------------------------------------------------

  prod_taxa_expanded <- prod_taxa_expanded %>% 
    rows_update(add_infraclass_by_sciname, by = "SciName", unmatched = "ignore") %>% 
    rows_update(add_infraclass_by_order, by = "Order", unmatched = "ignore")


  # Deal with forward slash taxa values from FB/SLB ------------------------------


  ## Detect forward slashes -------------------------------------------------

  # Known forward slash patterns in taxonomic rank columns:
  #   - "perciformes/*"  (Order column — suborder info encoded in the Order value by FB/SLB)
  #   - "*/misc"         (various columns — non-specific grouping suffix from FB/SLB)
  # Warn if an unhandled pattern is detected so it can be addressed explicitly.

  slash_detected <- prod_taxa_expanded %>%
    select(SciName_prod:Kingdom) %>%
    unlist() %>%
    unique() %>%
    str_subset(pattern = "/")

  # list of known kinds of forward slash syntax inherited from FB/SLB
  unknown_slash <- slash_detected[
    !str_detect(slash_detected, "^perciformes/") &
    !str_detect(slash_detected, "/misc$")
  ]

  cli::cli_h2("Production taxa classification ranks check - unhandled forward slash values")

  if (length(unknown_slash) == 0) {
    cli::cli_alert_success("No unhandled forward slash patterns detected in taxonomic rank columns.")
  } else if (length(unknown_slash) > 0) {
    cli::cli_alert_warning("{length(unknown_slash)} unhandled forward slash pattern{?s} detected in taxonomic rank columns.")
    cli::cli_alert_info("Unhandled forward slash values are: {.val {unknown_slash}}")
    cli::cli_alert_info("{.strong Developer Notes}: ")
    cli::cli_ul(c(
      "Known patterns {.code perciformes/*} and {.code */misc} are handled automatically by {.fn fill_prod_taxa_ranks}.",
      "Inspect the unhandled values above and update {.fn fill_prod_taxa_ranks} to address the new patterns."
    ))
  }

  ## Remove "/misc" suffix from all rank column values -----------------------

  prod_taxa_expanded <- prod_taxa_expanded %>%
    mutate(across(SciName_prod:Kingdom, ~str_remove(., "/misc")))

  ## Add Suborder and standardize Order for perciformes/* values -------------
  # rows_update() populates Suborder from the encoded "perciformes/<suborder>"
  # Order value, then Order is cleaned to "perciformes".

  add_suborder_by_order <- tribble(
    ~Order,                          ~Suborder,
    "perciformes/cottoidei",         "cottoidei",
    "perciformes/gasterosteoidei",   "gasterosteoidei",
    "perciformes/notothenioidei",    "notothenioidei",
    "perciformes/percoidei",         "percoidei",
    "perciformes/percophoidei",      "percophoidei",
    "perciformes/scorpaenoidei",     "scorpaenoidei",
    "perciformes/serranoidei",       "serranoidei",
    "perciformes/uranoscopoidei",    "uranoscopoidei",
    "perciformes/zoarcoidei",        "zoarcoidei"
  )

  ## Check perciformes/* suborders against add_suborder_by_order -------------
  # Extract the suborder segment from all "perciformes/*" values in the rank
  # columns and verify each one is covered by add_suborder_by_order$Suborder.

  perciformes_suborders <- slash_detected %>%
    str_subset("^perciformes/") %>%
    str_remove("^perciformes/")

  unmatched_suborders <- perciformes_suborders[
    !perciformes_suborders %in% add_suborder_by_order$Suborder
  ]

  if (length(unmatched_suborders) > 0) {
    cli::cli_h2("Production taxa rank classification check - missing {.val perciformes/*} suborders")
    cli::cli_alert_warning(
      "{length(unmatched_suborders)} {.code perciformes/*} value{?s} not covered by correction dataframe {.var add_suborder_by_order}."
    )
    cli::cli_alert_info("{.strong Developer Notes}:")
    cli::cli_ul(c(
      "Add the missing suborder{?s} to {.var add_suborder_by_order} in {.fn fill_prod_taxa_ranks}.",
      "Each new entry requires an {.field Order} value in the format {.code perciformes/<suborder>} and the corresponding {.field Suborder} value."
    ))
  }

  prod_taxa_expanded <- prod_taxa_expanded %>%
    rows_update(add_suborder_by_order, by = "Order", unmatched = "ignore") %>%
    # Change "perciformes/*"" Order values to "perciformes"
    mutate(Order = if_else(str_detect(Order, "^perciformes/"), "perciformes", Order))

  return(prod_taxa_expanded)
}
