#' Build the production sciname manual corrections table
#'
#' Constructs a manual correction table of scientific name
#' for FAO and SAU production data that were unmatched by the 
#' FishBase and SeaLifeBase taxa and synonym tables. The function then one-hot encodes 
#' the taxonomic rank of each corrected name to re-run through the FB/SLB matching process
#' which is subset by taxanomic rank. Called by [match_prod_taxa_to_fbslb()] to resolve production 
#' scinames that fail to match the FishBase/SeaLifeBase taxa table.
#'
#' @details
#' # Workflow to make manual corrections:
#' 
#' ## 1) Identify which taxa need corrections
#'
#' * The `taxa_need_corrections` dataframe is output from `match_prod_taxa_to_fbslb()`, view 
#'   this dataframe to see which taxa require manual corrections. 
#' 
#' ## 2) Review common correction categories
#' 
#' Often unmatched taxa fall in one of several common correction categories. Identify which correction
#' category a taxa name falls under to identify the appropriate correction strategy to apply.
#' 
#' * `"hybrid"`, `"mutli_taxa"`, `"name_formatting"`, and `"informal_name"` are often easy to visually identify
#' * `"rank_mismatch"`, `"adjust_to_fb_slb"`, and `"adjust_to_fb_slb"` require a deeper dive 
#'   to understand what is happening. 
#' 
#' ## 3) Determine best `sciname_corrected` value
#' 
#' 1) Follow instructions for each correction category (listed below) to identify `sciname_corrected` value
#' 2) Add a new row to the `prod_sciname_corrections` tribble below under the corresponding correction category section
#' 3) Fill in appropriate fields, including the `notes` column for your future self or colleague. 
#' 4) Be sure to keep the " spp" portion of genus corrections so the downstream can properly 
#'   identify and classify it. 
#' 5) Test your additions by calling `devtools::load_all()` and `build_corr_tbl_prod_sciname()` to see if any checks or errors are thrown
#' 6) Make notes of any unmatched taxa that are acceptable to leave unmatched. 
#' 
#' ## `"hybrid"` correction instructions
#' 
#' **RULE: Replace hybrid name with lowest shared taxa classification rank name.** Usually indicated with 
#' "species x species" name pattern (e.g "morone chrysops x m. saxatilis"). 
#' 
#' 1) Investigate each species on [WoRMS](https://www.marinespecies.org/) to find the lowest
#' shared taxa rank name. Likely genus or family. 
#' 2) The lowest taxa rank name must be present in either the Fishbase or Sealifebase taxa table 
#'   ("fb_taxa_info.csv", "slb_taxa_info.csv") to be a valid correction.
#' 
#' ## `"multi_taxa"` correction instructions
#' 
#' **RULE: Replace multiple taxa names with lowest shared taxa classification rank name.** Usually indicated
#' with a "taxa, taxa" name pattern (e.g. "loliginidae, ommastrephidae"). 
#' 
#' 1) Investigate each taxa on [WoRMS](https://www.marinespecies.org/) to find the lowest
#' shared taxa rank name. Likely genus or family. 
#' 2) The lowest taxa rank name must be present in either the Fishbase or Sealifebase taxa table 
#'   ("fb_taxa_info.csv", "slb_taxa_info.csv") to be a valid correction.
#' 
#' ## `"name_formatting"` correction instructions
#' 
#' **RULE: Replace non-standard formatted taxa name with an expected format that aligns with Fishbase/Sealifebase formatting**
#' 
#' 1) Investigate each taxa on [WoRMS](https://www.marinespecies.org/)
#' 2) These taxa names are usually creative work-arounds to capture, note, or combine more taxonomic information 
#'    into a single value than what the data schema supports. Some of these values could be corrected with simple 
#'    text string cleaning code (e.g "alitta virens (formerly nereis virens)"), but we want to explicitly 
#'   and transparently document each manual correction in this table. 
#' 3) Determine the lowest taxa classification rank name that appropriately represents the taxa name. 
#' 4) The lowest taxa rank name must be present in either the Fishbase or Sealifebase taxa table 
#'   ("fb_taxa_info.csv", "slb_taxa_info.csv") to be a valid correction.
#' 5) NOTE EXCEPTION: "perciformes" is an Order that is a large and important grouping in production with several important suborders not represented in the
#'   inherited Fishbase/Sealifebase taxonomic schema (i.e. suborder is not a column in the FB/SLB taxa tables). The perciformes corrections are 
#'   an exception due to their large production volumes. The corrected names use the syntax that FB/SLB taxa tables use to force this taxaonomic information
#'   into the simplified hierarchical taxonomic rank schema FB/SLB uses. 
#' 
#' ## `"informal_name"` correction instructions
#' 
#' **RULE: There isn't a great rule for informal names, work with Jessica.**
#' 
#' 1) Investigate and SAU or FAO documentation 
#' 2) Investigate downstream code that may group by large general taxa groups
#' 
#' ## "rank_mismatch"` correction instructions
#' 
#' **RULE: Replace the rank name that is not part of Fishbase/Sealifebase (ARTIS) with the taxa rank name one step up that is included in Fishbase/Sealifebase (ARTIS).**
#' 
#' 1) Investigate each taxa on [WoRMS](https://www.marinespecies.org/) to find the next step taxa rank name. 
#' 2) The lowest taxa rank name must be present in either the Fishbase or Sealifebase taxa table 
#'   ("fb_taxa_info.csv", "slb_taxa_info.csv") to be a valid correction.
#' 3) Investigate downstream code (e.g. the taxa and hs match functions) to understand how the corrected name may be included or excluded from some groupings. 
#' 
#' ## `"adjust_to_fb_slb"` correction instructions
#' 
#' **RULE: Replace the taxa name with its Fishbase/Sealifebase accepted name.**
#' 
#' 1) Investigate each taxa on [WoRMS](https://www.marinespecies.org/) to understand if the unmatched taxa name is outdated or a synonym.
#' 2) Replace with the name (even though it might be outdated) that is represented in the Fishbase or Sealifebase taxa tables
#'   ("fb_taxa_info.csv", "slb_taxa_info.csv").
#' 
#' @param the_fb_slb_dir File path to the directory containing the current version of Fishbase and Sealifebase data.
#' 
#' @return
#' A tibble with one row per raw scientific name. Columns:
#'
#' * `sciname_raw` — unresolved scientific name as it appears in FAO or SAU
#'   production data.
#' * `sciname_corrected` — corrected name for downstream use.
#' * `correction_category` — reason for the correction; one of: 
#'   * `"hybrid"`
#'   * `"multi_taxa"`
#'   * `"name_formatting"`
#'   * `"rank_mismatch"`
#'   * `"adjust_to_fb_slb"``
#'   * `"spelling_error"``
#'   * `"informal_name"`
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
#' The function checks and emits `cli` warnings if violations are found:
#'
#' * **Duplicate `sciname_raw` check** — detects multiple rows for the same
#'   `sciname_raw`, which would cause one-to-many join errors.
#' * **Encoding uniqueness check** — detects rows where more than one of
#'   `Species01`, `Genus01`, `Family01`, `Other01` equals `1`.
#' * **FB/SLB valid `sciname_corrected` values** - detects that corrected
#'   values match to FB/SLB taxa tables  
#'
#' @seealso
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

    # hybrid
    # RULE: Replace hybrid name with lowest shared taxa classification rank name
    "clarias gariepinus x c. macrocephalus",             "clarias spp",         "hybrid", "",
    "morone chrysops x m. saxatilis",                    "morone spp",          "hybrid", "",
    "oreochromis aureus x o. niloticus",                 "oreochromis spp",     "hybrid", "",
    "colossoma macropomum x piaractus brachypomus",      "serrasalmidae",       "hybrid", "Replace with common family for hybrid",
    "epinephelus fuscoguttatus x e. lanceolatus",        "epinephelus spp",     "hybrid", "Replace with common genus for hybrid",
    "heterobranchus longifilis x clarias gariepinus",    "clariidae",           "hybrid", "Replace with common family for hybrid",
    "piaractus mesopotamicus x colossoma macropomum",    "serrasalmidae",       "hybrid", "Replace with common family for hybrid",

    # multi_taxa
    # RULE: Replace multiple taxa names with lowest shared taxa classification rank name
    "astacidae, cambaridae",                              "cambaridae",         "multi_taxa", "Choose cambaridae as the larger family",
    "auxis thazard, a. rochei",                          "auxis spp",           "multi_taxa", "",
    "loliginidae, ommastrephidae",                        "teuthida",            "multi_taxa", "",
    "merluccius capensis, m.paradoxus",                  "merluccius spp",       "multi_taxa", "",
    "selachimorpha (pleurotremata)",                      "carcharhiniformes",   "multi_taxa", "Essentially an unidentified shark; code defines sharks as list of orders so assign to carcharhiniformes for now",
    "sepiidae, sepiolidae",                               "sepiidae",            "multi_taxa", "Sepiidae = cuttlefish; Sepiolidae = bobtail squid; assigning to cuttlefish",
    "squalidae, scyliorhinidae",                          "carcharhiniformes",   "multi_taxa", "Two different orders of sharks; code defines sharks as list of orders so assign to carcharhiniformes for now",
    "stolothrissa, limnothrissa",                         "clupeidae",           "multi_taxa", "",
    "xiphopenaeus, trachypenaeus",                        "penaeidae",           "multi_taxa", "",
    "alosa alosa, a. fallax",                             "alosa spp",           "multi_taxa", "Common genus between both",

    # name_formatting
    # RULE: Replace non-standard formatted taxa name with an expected format that aligns with Fishbase/Sealifebase formatting
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

    # rank_mismatch
    # RULE: Replace the rank name that is not part of Fishbase/Sealifebase (ARTIS) with the taxa rank name one step up that is included in Fishbase/Sealifebase (ARTIS).
    "brachyura",                  "decapoda",                 "rank_mismatch", "Infraorder not part of fishbase database",
    "anomura",                    "decapoda",                 "rank_mismatch", "Infraorder name to order name",
    "caridea",                    "decapoda",                 "rank_mismatch", "Infraorder within decapoda so move up",
    "thunnini",                   "thunnus spp",              "rank_mismatch", "",
    "actinopterygii",             "osteichthyes",             "rank_mismatch", "FishBase updated class from actinopterygii to teleostei; decided to lump all actinopterygii as osteichthyes",
    "crustacea",                  "branchiopoda",             "rank_mismatch", "Downstream code IDs crustacea to class level; assign to branchiopoda for now; downstream code defines crustaceans as list of classes c('branchiopoda', 'malacostraca', 'maxillopoda', 'merostomata'); assuming non-crab/lobster/shrimp crustacean",
    "invertebrata",               "asteroidea",               "rank_mismatch", "Assign to asteroidea for now; downstream code defines aquatic invertebrates as list of classes (if we went by phylum ascidians would be omitted as chordata)",
    "natantia",                   "crangonidae",              "rank_mismatch", "Natantia is obsolete term for shrimp; assign to order = crangonidae for now",
    "reptantia",                  "cancridae",                "rank_mismatch", "Reptantia is obsolete term for crab; multiple families of crab so assign to family = cancridae for now",

    # adjust_to_fb_slb
    # RULE: Replace the taxa name with its Fishbase/Sealifebase accepted name. 
    "liza spp",                             "planiliza spp",              "adjust_to_fb_slb", "Referring to mullets",
    "tritia reticulata",                    "nassarius reticulatus",      "adjust_to_fb_slb", "Netted Dog whelk",
    "afruca tangeri",                       "uca tangeri",                "adjust_to_fb_slb", "Worms has afruca tangeri as accepted name with uca tangeri as synonym",
    "ageneiosus dentatus",                  "ageneiosus ucayalensis",     "adjust_to_fb_slb", "ageneiosus dentatus listed in Fishbase as an ambiguous synonym",
    "amphithrax armatus",                   "mithrax armatus",            "adjust_to_fb_slb", "Worms has amphithrax armatus as accepted name with mithrax armatus as original name",
    "callaus deliciosa",                    "sciaena deliciosa",          "adjust_to_fb_slb", "Update to name identified as accepted in Fishbase and Worms",
    "dallocardia muricata",                 "trachycardium muricatum",    "adjust_to_fb_slb", "dallocardia muricata is accepted in Worms but lists trachycardium muricatum as a superseded combination",
    "grimothea gregaria",                   "munida gregaria",            "adjust_to_fb_slb", "grimothea gregaria is accepted in Worms but lists munida gregaria as a superseded combination",
    "hansarsia megalops",                   "nematoscelis megalops",      "adjust_to_fb_slb", "hansarsia megalops is accepted in Worms but nematoscelis megalops is superseded combination",
    "hyporthodus drummondhayi",             "epinephelus drummondhayi",   "adjust_to_fb_slb", "",
    "iliochione subrugosa",                 "chione subrugosa",           "adjust_to_fb_slb", "",
    "larkinia grandis",                     "anadara grandis",            "adjust_to_fb_slb", "",
    "lutraria oblonga",                     "lutraria magna",             "adjust_to_fb_slb", "",
    "michalisquilla parva",                 "squilla parva",              "adjust_to_fb_slb", "",
    "pinirampus argentina",                 "megalonema argentinum",      "adjust_to_fb_slb", "",
    "polybius depurator",                   "liocarcinus depurator",      "adjust_to_fb_slb", "",
    "polybius navigator",                   "liocarcinus navigator",      "adjust_to_fb_slb", "",
    "polybius vernalis",                    "liocarcinus vernalis",       "adjust_to_fb_slb", "",
    "proteopitar patagonicus",              "pitar patagonicus",          "adjust_to_fb_slb", "",
    "spisula sibyllae",                     "spisula sachalinensis",      "adjust_to_fb_slb", "",
    "ylistrum japonicum",                   "amusium japonicum",          "adjust_to_fb_slb", "",
    "cantherhines",                         "cantherhines spp",           "adjust_to_fb_slb", "Genus with missing spp",
    "cherax cainii",                        "cherax spp",                 "adjust_to_fb_slb", "Maron - classified into two species both cherax cainii and cherax tenuimanus however only cherax tenuimanus accepted in sealifebase synonyms but does not occur in sealifebase taxa table",
    "austrofusus glans",                    "buccinum spp",               "adjust_to_fb_slb", "Whelk",
    "lophiosilurus apurensis",              "osteichthyes",               "adjust_to_fb_slb", "FIXIT: Repull rfishbase data and remove once species is verified in the record - currently not listed at all (2025-09)",
    "pimelodus yuma",                       "pimelodus spp",              "adjust_to_fb_slb", "FIXIT: Temporary change to genus - remove once species is added to rfishbase data version (FAO 2025 uses rfishbase latest version 24.07)",
    "astacopsis franklinii",                "parastacidae spp",           "adjust_to_fb_slb", "FIXIT: Temporary change to genus - remove once species is added to rfishbase data version (FAO 2025 uses rfishbase latest version 24.07)",
    "adinaefiola aurantiaca",               "sepiola aurantiaca",         "adjust_to_fb_slb", "sepiola aurantiaca is unaccepted (original combination) in Worms, with adinaefiola aurantiaca as the accepted name. Sealifebase uses old name",
    #"amphiarius rugispinis",                "notarius rugispinis",        "adjust_to_fb_slb", "amphiarius rugispinis is accepted on worms, unaccepted synonym is used by fishbase",
    #"auxis",                                "auxis spp",                  "adjust_to_fb_slb", "missing spp in genus name",
    #"brycinus imberi",                      "brachyalestes imberi",       "adjust_to_fb_slb", "accepted on worms, unaccepted name used on Fishbase. Listed as synonym on Fishbase website, likely to showup in synonym table in an updated snapshot",
    #"brycinus nurse",                       "brachyalestes nurse",        "adjust_to_fb_slb", "accepted on worms, unaccepted name used on Fishbase. Listed as synonym on Fishbase website, likely to showup in synonym table in an updated snapshot",
    #"buccinum",                             "buccinum spp",               "adjust_to_fb_slb", "Genus with missing spp",                        

   
    # FAO - spelling_error

  # SAU corrections --------------------------------------------------------
  # As of 2026-07-15 These corrections are being retained until the next SAU version is released to have a record of our corrections. This section will also be culled of unused corrections. 


    # SAU - informal_name
    # RULE: There isn't a great rule for informal names, work with Jessica. 
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

    # SAU - adjust_to_fb_slb
    "inermiidae",     "haemulidae",         "adjust_to_fb_slb", "",
    "liza",           "planiliza spp",      "adjust_to_fb_slb", "Not matching to taxa table",
    "valamugil",      "mugilidae",          "adjust_to_fb_slb", "unaccepted name, Worms accepted genus crenimugil not in FB taxa table, bump to family",
    "tridacnidae",    "cardiidae",          "adjust_to_fb_slb", "Moving from subfamily to family name",

    # SAU - adjust_to_fb_slb
    "asterozoa",                  "asteroidea",    "adjust_to_fb_slb", "",
    "batoidea",                   "rajiformes",    "adjust_to_fb_slb", "Several potential orders; assume rajiformes for now",
    "echinozoa",                  "echinodermata", "adjust_to_fb_slb", "",
    "pteriomorphia",              "bivalvia",      "adjust_to_fb_slb", "",
    "azurina cyanea",             "azurina",       "adjust_to_fb_slb", "Moving up a taxonomic level",
    "sinistrofulgur sinistrum",   "neogastropoda", "adjust_to_fb_slb", "Move from species to order",
    "austropotamobius pallipes",  "astacidae",     "adjust_to_fb_slb", "SeaLifeBase doesn't recognize the genus or species; just use family",
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