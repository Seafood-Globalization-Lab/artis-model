#' Build the production sciname manual corrections table
#'
#' Constructs a manually curated lookup table of scientific name corrections
#' for FAO and SAU production data. Resolves names that are unmatched by
#' FishBase and SeaLifeBase, and one-hot encodes the taxonomic rank of each
#' corrected name.
#'
#' @details
#' Called by [match_prod_taxa_to_fbslb()] to resolve production scinames that fail
#' to match the FishBase/SeaLifeBase taxa table.
#'
#' ## One-hot encoding
#'
#' After building the correction lookup, the function detects the taxonomic
#' rank of each `sciname_corrected` value and encodes it as a binary flag
#' across four columns. Exactly one column per row will equal `1`. Detection
#' rules applied in order:
#'
#' * **`Genus01`** — name contains `"spp"`
#' * **`Family01`** — name contains no space and matches the family suffix
#'   pattern `([^\s])*dae`
#' * **`Species01`** — name contains a space and is not already assigned to
#'   `Family01` or `Genus01`
#' * **`Other01`** — all remaining names (typically higher-rank taxa without
#'   consistent string-detectable suffixes)
#'
#' The `" spp"` suffix is stripped from `sciname_corrected` after genus
#' detection and before the table is returned.
#'
#' ## Data integrity checks
#'
#' Runs checks and emits `cli` warnings if violations are found:
#'
#' * **Duplicate `sciname_raw` check** — detects multiple rows for the same
#'   `sciname_raw`, which would cause one-to-many join errors in
#'   [classify_prod_dat()].
#' * **Encoding uniqueness check** — detects rows where more than one of
#'   `Species01`, `Genus01`, `Family01`, `Other01` equals `1`.
#' * **FB/SLB valid `sciname_corrected` values** - detects that corrected
#'   values match to FB/SLB taxa tables
#'
#' @return
#' A tibble with one row per raw scientific name. Columns:
#'
#' * `sciname_raw` — unresolved scientific name as it appears in FAO or SAU
#'   production data.
#' * `sciname_corrected` — corrected name for downstream use.
#' * `correction_category` — reason for the correction; one of `"hybrid"`,
#'   `"multi_taxa"`, `"rank_mismatch"`, `"taxonomy_update"`,
#'   `"unresolved_taxon"`, `"informal_name"`, `"name_formatting"`,
#'   `"spelling_error"`, `"fixit_temporary"`.
#' * `notes` — additional context or rationale for the correction.
#' * `Species01`, `Genus01`, `Family01`, `Other01` — binary (0/1) one-hot
#'   encoding of the taxonomic rank of `sciname_corrected`; exactly one
#'   column equals `1` per row.
#'
#' @note
#' `sciname_corrected` values are not final — they are subject to further
#' synonym resolution in [match_prod_taxa_to_fbslb()], which may change the
#' name again. This table is not suitable for external documentation, it is 
#' an operational internal software table only.  
#'
#' @seealso
#' * [classify_prod_dat()] — calls this function to resolve unmatched
#'   production scinames
#' * [match_prod_taxa_to_fbslb()] — further resolves corrected scinames via
#'   synonym matching against FishBase and SeaLifeBase
#'
#' @import dplyr
#' @import cli
#' @importFrom tibble tribble
#' @importFrom magrittr %>%
#' @export

build_corr_tbl_prod_sciname <- function(
  the_fb_slb_dir
){

  # Manual Corrections Table
  # RULE: Ensure any new `sciname_corrected` value exists in the fishbase / sealifebase taxa info file that the scinames will join to. 
  # RULE: Keep " spp" in name of any genera assigned to `sciname_corrected` for taxa rank encoding downstream


  # Create corrections dataframe -------------------------------------------
  prod_sciname_corrections <- tribble(
    ~sciname_raw, ~sciname_corrected, ~correction_category, ~notes,

    # FAO - hybrid
    # RULE: Replace hybrid name with lowest shared taxa classification rank name
    "clarias gariepinus x c. macrocephalus",             "clarias spp",         "hybrid", "",
    "morone chrysops x m. saxatilis",                    "morone spp",          "hybrid", "",
    "oreochromis aureus x o. niloticus",                 "oreochromis spp",     "hybrid", "",
    "colossoma macropomum x piaractus brachypomus",      "serrasalmidae",       "hybrid", "Replace with common family for hybrid",
    "epinephelus fuscoguttatus x e. lanceolatus",        "epinephelus spp",     "hybrid", "Replace with common genus for hybrid",
    "heterobranchus longifilis x clarias gariepinus",    "clariidae",           "hybrid", "Replace with common family for hybrid",
    "piaractus mesopotamicus x colossoma macropomum",    "serrasalmidae",       "hybrid", "Replace with common family for hybrid",

    # FAO - multi_taxa
    # RULE: Replace hybrid name with lowest shared taxa classification rank name
    "astacidae, cambaridae",                              "cambaridae",         "multi_taxa", "Choose cambaridae as the larger family",
    "auxis thazard, a. rochei",                          "auxis spp",           "multi_taxa", "",
    "loliginidae, ommastrephidae",                        "teuthida",            "multi_taxa", "",
    "merluccius capensis, m.paradoxus",                  "merluccius spp",       "multi_taxa", "",
    "selachimorpha (pleurotremata)",                      "carcharhiniformes",   "multi_taxa", "Essentially an unidentified shark; code defines sharks as list of orders so assign to carcharhiniformes for now",
    "sepiidae, sepiolidae",                               "sepiidae",            "multi_taxa", "Sepiidae = cuttlefish; Sepiolidae = bobtail squid; assigning to cuttlefish",
    "squalidae, scyliorhinidae",                          "carcharhiniformes",   "multi_taxa", "Two different orders of sharks; code defines sharks as list of orders so assign to carcharhiniformes for now",
    "stolothrissa, limnothrissa",                         "clupeidae",           "multi_taxa", "",
    "xiphopenaeus, trachypenaeus",                        "penaeidae",           "multi_taxa", "",
    "alosa alosa, a. fallax",                            "alosa spp",           "multi_taxa", "Common genus between both",

    # FAO - rank_mismatch
    # RULE: 

    "brachyura",                  "decapoda",                 "rank_mismatch", "Infraorder not part of fishbase database",
    "anomura",                    "decapoda",                 "rank_mismatch", "Infraorder name to order name",
    "caridea",                    "decapoda",                 "rank_mismatch", "Infraorder within decapoda so move up",
    "thunnini",                   "thunnus spp",              "rank_mismatch", "",
    "actinopterygii",             "osteichthyes",             "rank_mismatch", "FishBase updated class from actinopterygii to teleostei; decided to lump all actinopterygii as osteichthyes",
    "crustacea",                  "branchiopoda",             "rank_mismatch", "Downstream code IDs crustacea to class level; assign to branchiopoda for now; downstream code defines crustaceans as list of classes c('branchiopoda', 'malacostraca', 'maxillopoda', 'merostomata'); assuming non-crab/lobster/shrimp crustacean",
    "invertebrata",               "asteroidea",               "rank_mismatch", "Assign to asteroidea for now; downstream code defines aquatic invertebrates as list of classes (if we went by phylum ascidians would be omitted as chordata)",
    "natantia",                   "crangonidae",              "rank_mismatch", "Natantia is obsolete term for shrimp; assign to order = crangonidae for now",
    "reptantia",                  "cancridae",                "rank_mismatch", "Reptantia is obsolete term for crab; multiple families of crab so assign to family = cancridae for now",

    # FAO - taxonomy_update
    "liza spp",                             "planiliza spp",              "taxonomy_update", "Referring to mullets",
    "tritia reticulata",                    "nassarius reticulatus",      "taxonomy_update", "Netted Dog whelk",
    "afruca tangeri",                       "uca tangeri",                "taxonomy_update", "Worms has afruca tangeri as accepted name with uca tangeri as synonym",
    "ageneiosus dentatus",                  "ageneiosus ucayalensis",     "taxonomy_update", "ageneiosus dentatus listed in Fishbase as an ambiguous synonym",
    "amphithrax armatus",                   "mithrax armatus",            "taxonomy_update", "Worms has amphithrax armatus as accepted name with mithrax armatus as original name",
    "callaus deliciosa",                    "sciaena deliciosa",          "taxonomy_update", "Update to name identified as accepted in Fishbase and Worms",
    "dallocardia muricata",                 "trachycardium muricatum",    "taxonomy_update", "dallocardia muricata is accepted in Worms but lists trachycardium muricatum as a superseded combination",
    "grimothea gregaria",                   "munida gregaria",            "taxonomy_update", "grimothea gregaria is accepted in Worms but lists munida gregaria as a superseded combination",
    "hansarsia megalops",                   "nematoscelis megalops",      "taxonomy_update", "hansarsia megalops is accepted in Worms but nematoscelis megalops is superseded combination",
    "hyporthodus drummondhayi",             "epinephelus drummondhayi",   "taxonomy_update", "",
    "iliochione subrugosa",                 "chione subrugosa",           "taxonomy_update", "",
    "larkinia grandis",                     "anadara grandis",            "taxonomy_update", "",
    "lutraria oblonga",                     "lutraria magna",             "taxonomy_update", "",
    "michalisquilla parva",                 "squilla parva",              "taxonomy_update", "",
    "pinirampus argentina",                 "megalonema argentinum",      "taxonomy_update", "",
    "polybius depurator",                   "liocarcinus depurator",      "taxonomy_update", "",
    "polybius navigator",                   "liocarcinus navigator",      "taxonomy_update", "",
    "polybius vernalis",                    "liocarcinus vernalis",       "taxonomy_update", "",
    "proteopitar patagonicus",              "pitar patagonicus",          "taxonomy_update", "",
    "spisula sibyllae",                     "spisula sachalinensis",      "taxonomy_update", "",
    "ylistrum japonicum",                   "amusium japonicum",          "taxonomy_update", "",

    # FAO - unresolved_taxon
    "cantherhines",               "cantherhines spp",         "unresolved_taxon", "Genus with missing spp",
    "cherax cainii",              "cherax spp",               "unresolved_taxon", "Maron - classified into two species both cherax cainii and cherax tenuimanus however only cherax tenuimanus accepted in sealifebase synonyms but does not occur in sealifebase taxa table",
    "austrofusus glans",          "buccinum spp",             "unresolved_taxon", "Whelk",
    "astacopsis franklinii",      "parastacidae spp",         "unresolved_taxon", "FIXIT: Temporary change to genus - remove once species is added to rfishbase data version (FAO 2025 uses rfishbase latest version 24.07)",

    # FAO - name_formatting
    "holothuria (holothuria) tubulosa",          "holothuria tubulosa",        "name_formatting", "",
    "uroteuthis (photololigo) duvaucelii",       "uroteuthis duvaucelii",      "name_formatting", "",
    "perciformes (others)",                      "perciformes",                "name_formatting", "",
    "perciformes (percoidei)",                   "perciformes/percoidei",      "name_formatting", "",
    "perciformes (scorpaenoidei)",               "perciformes/scorpaenoidei",  "name_formatting", "",
    "scombriformes (scombroidei)",               "scombriformes",              "name_formatting", "",
    "lutjanidae (ex caesionidae)",               "lutjanidae",                 "name_formatting", "",
    "labridae (ex scaridae)",                    "labridae",                   "name_formatting", "",
    "alitta virens (formerly nereis virens)",    "alitta virens",              "name_formatting", "Remove note of former name",
    "batoidea or batoidimorpha (hypotremata)",   "batoidea",                   "name_formatting", "",

    # FAO - spelling_error

    # FAO - fixit_temporary
    "lophiosilurus apurensis",   "osteichthyes",  "fixit_temporary", "FIXIT: Repull rfishbase data and remove once species is verified in the record - currently not listed at all (2025-09)",
    "pimelodus yuma",            "pimelodus spp", "fixit_temporary", "FIXIT: Temporary change to genus - remove once species is added to rfishbase data version (FAO 2025 uses rfishbase latest version 24.07)",

    # SAU - informal_name
    "marine finfishes not identified",       "osteichthyes",  "informal_name", "Non-scientific name",
    "marine fishes not identified",          "osteichthyes",  "informal_name", "Non-scientific name",
    "marine groundfishes not identified",    "osteichthyes",  "informal_name", "Non-scientific name",
    "marine pelagic fishes not identified",  "osteichthyes",  "informal_name", "Non-scientific name",
    "miscellaneous aquatic invertebrates",   "asteroidea",    "informal_name", "Non-scientific name; assign to asteroidea for now; downstream code defines aquatic invertebrates as list of classes (if we went by phylum ascidians would be omitted as chordata)",
    "miscellaneous diadromous fishes",       "osteichthyes",  "informal_name", "Non-scientific name",
    "miscellaneous marine crustaceans",      "malacostraca",  "informal_name", "Non-scientific name; assuming some sort of crab/lobster/shrimp/prawn/crayfish crustacean",

    # SAU - rank_mismatch
    "dendrobranchiata", "decapoda",          "rank_mismatch", "",
    "pleuronectoidei",  "pleuronectiformes", "rank_mismatch", "Moving from suborder to order",

    # SAU - taxonomy_update
    "inermiidae",     "haemulidae",         "taxonomy_update", "",
    "liza",           "planiliza spp",      "taxonomy_update", "Not matching to taxa table",
    "valamugil",      "mugilidae",          "taxonomy_update", "unaccepted name, Worms accepted genus crenimugil not in FB taxa table, bump to family",
    "tridacnidae",    "cardiidae",          "taxonomy_update", "Moving from subfamily to family name",

    # SAU - unresolved_taxon
    "asterozoa",                  "asteroidea",    "unresolved_taxon", "",
    "batoidea",                   "rajiformes",    "unresolved_taxon", "Several potential orders; assume rajiformes for now",
    "echinozoa",                  "echinodermata", "unresolved_taxon", "",
    "pteriomorphia",              "bivalvia",      "unresolved_taxon", "",
    "azurina cyanea",             "azurina",       "unresolved_taxon", "Moving up a taxonomic level",
    "sinistrofulgur sinistrum",   "neogastropoda", "unresolved_taxon", "Move from species to order",
    "austropotamobius pallipes",  "astacidae",     "unresolved_taxon", "SeaLifeBase doesn't recognize the genus or species; just use family",
    "euastacus armatus",          "parastacidae",  "unressolved_taxon", "SeaLifeBase doesn't recognize the genus or species; just use family"
  )

  # Add encoded columns and detect values ----------------------------------

  # One-hot encoding Taxa sciname_corrected classificataion hierarchitcal rank
  # same code as clean_prod_dat.R - required for subsetting data and joining onto Fishbase and Sealifebase taxa data

  # Create empty columns 
  prod_sciname_corrections$Species01 <- 0
  prod_sciname_corrections$Genus01   <- 0
  prod_sciname_corrections$Family01  <- 0
  prod_sciname_corrections$Other01   <- 0

  # Assign Genera by "spp" string
  prod_sciname_corrections$Genus01[grepl(prod_sciname_corrections$sciname_corrected, pattern = "spp")] <- 1
  # Assign Family - No space in name and family affix
  prod_sciname_corrections$Family01[
    grepl(prod_sciname_corrections$sciname_corrected, pattern = " ") == FALSE &
      grepl(pattern = "([^\\s])*dae", prod_sciname_corrections$sciname_corrected)
  ] <- 1
  # Assign Species - space in sciname_corrected string
  prod_sciname_corrections$Species01[
    grepl(prod_sciname_corrections$sciname_corrected, pattern = " ") &
      prod_sciname_corrections$Family01 == 0 &
      prod_sciname_corrections$Genus01  == 0
  ] <- 1
  # Assign Other - leftovers
  # Consistent string detection not available to classification ranks higher than family
  prod_sciname_corrections$Other01[
    prod_sciname_corrections$Species01 == 0 &
      prod_sciname_corrections$Genus01  == 0 &
      prod_sciname_corrections$Family01 == 0
  ] <- 1

  # Remove " spp" suffix now that genera are identified
  prod_sciname_corrections <- prod_sciname_corrections %>%
    mutate(sciname_corrected = gsub(sciname_corrected, pattern = " spp", replacement = ""))


  # Checks ----------------------------------
  # Check for duplicate records
  n_duplicates <- prod_sciname_corrections %>% 
    group_by(sciname_raw) %>% 
    mutate(
      n_raw = n_distinct(sciname_raw)
    ) 

  # Check for duplicate raw scinames
  n_raw <- n_duplicates %>% 
    filter(n_raw > 1)

  if(nrow(n_raw)) {
    cli::cli_h2("Malformed manual corrections table - Check 1")
    cli::cli_alert_warning("{.fn build_corr_tbl_prod_sciname} table has duplicate {.field sciname_raw} values.")
    cli::cli_alert_info("Multiple rows detected for: {n_raw$sciname_raw}")
  }

  # Check encoded columns only have one one value
  add_to_one <- prod_sciname_corrections %>% 
    group_by(sciname_raw) %>% 
    mutate(n_ones = sum(Species01, Genus01, Family01, Other01)) %>% 
    filter(n_ones > 1)

  if(nrow(add_to_one)) {
    cli::cli_h2("Malformed manual corrections table - Check 2")
    cli::cli_alert_warning("Some {.field sciname_raw} values have more than one {.field Species01, Genus01, Family01, Other01} assignments.")
    cli::cli_alert_info("Check {.fn build_corr_tbl_prod_sciname} for duplicate {.field sciname_raw} values or other entry errors.")
    cli::cli_alert_info("Multiple encoded values detected for: {.val add_to_one$sciname_raw}")
  }

  # Check corrected names show up in FB/SLB taxa tables
  fb_taxa <- fread(file.path(the_fb_slb_dir, "fb_taxa_info.csv"), data.table = FALSE)
  slb_taxa <- fread(file.path(the_fb_slb_dir, "slb_taxa_info.csv"), data.table = FALSE)

  # get all unique taxa names (regardless of classifcation rank)
  all_taxa_vec <- unique(c(unlist(fb_taxa, use.names = FALSE), unlist(slb_taxa, use.names = FALSE)))

  # Check that all `sciname_corrected` values are valid Fb / Slb taxa values
  not_valid_taxa <- prod_sciname_corrections %>% 
    select(sciname_corrected) %>% 
    filter(!sciname_corrected %in% all_taxa_vec)

  if(nrow(not_valid_taxa)) {
    cli::cli_h2("Malformed manual corrections table - Check 3")
    cli::cli_alert_warning("{.val {nrow(not_valid_taxa)}} {.field sciname_corrected} values that are not found within Fishbase and Sealifebase taxa tables.")
    cli::cli_alert_info("{.fn build_corr_tbl_prod_sciname} {.field sciname_corrected} values not in FB/SLB: {.val {not_valid_taxa}} ")
    cli::cli_alert_info("Make corrections where possible, but there may be instances where we choose to insert taxa not represented 
    in FB/SLB into the ARTIS. These instance are contained within the downstream {.fn fill_taxa_classification_gaps} function.")
  }

  return(prod_sciname_corrections)
}