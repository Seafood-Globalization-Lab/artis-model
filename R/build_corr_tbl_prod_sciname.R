#' Build the Production Scientific Name Corrections Table
#'
#' Constructs a manually curated lookup table used in \code{classify_prod_dat()}
#' to correct FAO and SAU production scientific names that are unresolved by
#' FishBase and SeaLifeBase.
#' 
#' NOTE: This is not a final record of corrected scinames. Corrected scinames from this 
#' table are subject to the synonym resolution step in \code{match_prod_taxa_to_fbslb()},
#' which may further change the sciname. 
#'
#' @return A tibble with the following columns:
#'   \describe{
#'     \item{prod_data_type}{Data source; one of \code{"FAO"} or \code{"SAU"}.}
#'     \item{sciname_raw}{The unresolved scientific name as it appears in the source data.}
#'     \item{sciname_corrected}{The corrected name to use downstream.}
#'     \item{correction_category}{Reason category for the correction. One of:
#'       \code{"hybrid"} (cross-species hybrid entry identified by \code{x} in the name
#'       string; resolved to common genus or family),
#'       \code{"multi_taxa"} (comma-separated list of two or more distinct taxa in a
#'       single name string; resolved to a representative taxon),
#'       \code{"rank_mismatch"} (valid taxon name at a rank not supported by
#'       FishBase/SeaLifeBase, e.g. infraorder, suborder, tribe, or class; bumped up
#'       to nearest supported rank),
#'       \code{"taxonomy_update"} (name is synonymised, superseded, or non-accepted in
#'       WoRMS or FishBase/SeaLifeBase; replaced with the current accepted name),
#'       \code{"unresolved_taxon"} (scientific name not found in FishBase/SeaLifeBase;
#'       resolved upward to nearest identifiable taxon),
#'       \code{"informal_name"} (non-scientific vernacular name in source data, primarily
#'       SAU; mapped to nearest appropriate taxon),
#'       \code{"name_formatting"} (extraneous notation in the name string such as subgenus
#'       parentheticals, alternative name text, or taxonomic history qualifiers; the
#'       underlying taxon is correct),
#'       \code{"spelling_error"} (genuine typographical errors or truncations in the source
#'       name string),
#'       \code{"fixit_temporary"} (temporary correction pending a FishBase/rfishbase data
#'       update; should be reviewed and removed once resolved).}
#'     \item{notes}{Additional context or rationale for the correction.}
#'   }
#'
#' @seealso \code{\link{classify_prod_dat}}
#'
#' @importFrom tibble tribble
#' @export

build_corr_tbl_prod_sciname <- function(){

  prod_sciname_corrections <- tribble(
    ~prod_data_type, ~sciname_raw, ~sciname_corrected, ~correction_category, ~notes, ~Species01, 

    # FAO - hybrid
    # RULE: Replace hybrid name with lowest shared taxa classification name
    "FAO", "clarias gariepinus x c. macrocephalus",             "clarias spp",         "hybrid", "",
    "FAO", "c. macropomum x p. brachypomus",                    "serrasalmidae",       "hybrid", "Colossoma macropomum x Piaractus brachypomus",
    "FAO", "morone chrysops x m. saxatilis",                    "morone spp",          "hybrid", "",
    "FAO", "oreochromis aureus x o. niloticus",                 "oreochromis",         "hybrid", "",
    "FAO", "p. mesopotamicus x c. macropomum",                  "serrasalmidae",       "hybrid", "Piaractus mesopotamicus x Colossoma macropomum",
    "FAO", "h. longifilis x c. gariepinus",                     "clariidae",           "hybrid", "Matched to the larger family because genus was different",
    "FAO", "e. fuscoguttatus x e. lanceolatus",                 "epinephelus",         "hybrid", "Matched by same genus",
    "FAO", "colossoma macropomum x piaractus brachypomus",      "serrasalmidae",       "hybrid", "Replace with common family for hybrid",
    "FAO", "epinephelus fuscoguttatus x e. lanceolatus",        "epinephelus",         "hybrid", "Replace with common genus for hybrid",
    "FAO", "heterobranchus longifilis x clarias gariepinus",    "clariidae",           "hybrid", "Replace with common family for hybrid",
    "FAO", "piaractus mesopotamicus x colossoma macropomum",    "serrasalmidae",       "hybrid", "Replace with common family for hybrid",

    # FAO - multi_taxa
    # RULE: Replace hybrid name with lowest shared taxa classification name
    "FAO", "astacidae, cambaridae",                              "cambaridae",         "multi_taxa", "Choose cambaridae as the larger family",
    "FAO", "auxis thazard, a. rochei",                          "auxis spp",           "multi_taxa", "",
    "FAO", "loliginidae, ommastrephidae",                        "teuthida",            "multi_taxa", "",
    "FAO", "merluccius capensis, m.paradoxus",                  "merluccius",          "multi_taxa", "",
    "FAO", "osmerus spp, hypomesus spp",                        "osmeridae",           "multi_taxa", "",
    "FAO", "selachimorpha (pleurotremata)",                      "carcharhiniformes",   "multi_taxa", "Essentially an unidentified shark; code defines sharks as list of orders so assign to carcharhiniformes for now",
    "FAO", "sepiidae, sepiolidae",                               "sepiidae",            "multi_taxa", "Sepiidae = cuttlefish; Sepiolidae = bobtail squid; assigning to cuttlefish",
    "FAO", "squalidae, scyliorhinidae",                          "carcharhiniformes",   "multi_taxa", "Two different orders of sharks; code defines sharks as list of orders so assign to carcharhiniformes for now",
    "FAO", "stolothrissa, limnothrissa",                         "clupeidae",           "multi_taxa", "",
    "FAO", "stolothrissa, limnothrissa spp",                    "clupeidae",           "multi_taxa", "",
    "FAO", "xiphopenaeus, trachypenaeus",                        "penaeidae",           "multi_taxa", "",
    "FAO", "xiphopenaeus, trachypenaeus spp",                   "penaeidae",           "multi_taxa", "",
    "FAO", "alosa alosa, a. fallax",                            "alosa spp",           "multi_taxa", "Common genus between both",
    "FAO", "pandalus spp, pandalopsis spp",                     "pandalus spp",        "multi_taxa", "Prawn",

    # FAO - rank_mismatch
    "FAO", "clupeoidei",                 "clupeiformes",             "rank_mismatch", "",
    "FAO", "percoidei",                  "perciformes",              "rank_mismatch", "",
    "FAO", "scombroidei",                "perciformes",              "rank_mismatch", "FishBase doesn't list scombiformes as an order (See fishbase %>% filter(Family == 'scombridae'))",
    "FAO", "siluroidei",                 "siluriformes",             "rank_mismatch", "",
    "FAO", "brachyura",                  "decapoda",                 "rank_mismatch", "Infraorder not part of fishbase database",
    "FAO", "anomura",                    "decapoda",                 "rank_mismatch", "Infraorder name to order name",
    "FAO", "caridea",                    "decapoda",                 "rank_mismatch", "Infraorder within decapoda so move up",
    "FAO", "thunnini",                   "thunnus spp",              "rank_mismatch", "",
    "FAO", "actinopterygii",             "osteichthyes",             "rank_mismatch", "FishBase updated class from actinopterygii to teleostei; decided to lump all actinopterygii as osteichthyes",
    "FAO", "crustacea",                  "branchiopoda",             "rank_mismatch", "Downstream code IDs crustacea to class level; assign to branchiopoda for now; downstream code defines crustaceans as list of classes c('branchiopoda', 'malacostraca', 'maxillopoda', 'merostomata'); assuming non-crab/lobster/shrimp crustacean",
    "FAO", "invertebrata",               "asteroidea",               "rank_mismatch", "Assign to asteroidea for now; downstream code defines aquatic invertebrates as list of classes (if we went by phylum ascidians would be omitted as chordata)",
    "FAO", "natantia",                   "crangonidae",              "rank_mismatch", "Natantia is obsolete term for shrimp; assign to order = crangonidae for now",
    "FAO", "reptantia",                  "cancridae",                "rank_mismatch", "Reptantia is obsolete term for crab; multiple families of crab so assign to family = cancridae for now",

    # FAO - taxonomy_update
    "FAO", "branchiostegidae",                    "malacanthidae",              "taxonomy_update", "",
    "FAO", "caspialosa spp",                      "alosa spp",                  "taxonomy_update", "",
    "FAO", "mobulidae",                            "myliobatidae",               "taxonomy_update", "",
    "FAO", "aliger gigas",                         "lobatus gigas",              "taxonomy_update", "Queen Conch",
    "FAO", "liza spp",                             "planiliza spp",              "taxonomy_update", "Referring to mullets",
    "FAO", "mytilus unguiculatus",                 "mytilus coruscus",           "taxonomy_update", "Korean Mussel",
    "FAO", "tritia mutabilis",                     "nassarius mutabilis",        "taxonomy_update", "Mutable/Changeable Nassa",
    "FAO", "tritia reticulata",                    "nassarius reticulatus",      "taxonomy_update", "Netted Dog whelk",
    "FAO", "moroteuthopsis ingens",                "onykia ingens",              "taxonomy_update", "",
    "FAO", "caridina denticulata",                 "neocaridina denticulata",    "taxonomy_update", "Synonym to accepted name not caught by fb or slb",
    "FAO", "afruca tangeri",                       "uca tangeri",                "taxonomy_update", "Worms has afruca tangeri as accepted name with uca tangeri as synonym",
    "FAO", "ageneiosus dentatus",                  "ageneiosus ucayalensis",     "taxonomy_update", "ageneiosus dentatus listed in Fishbase as an ambiguous synonym",
    "FAO", "amphithrax armatus",                   "mithrax armatus",            "taxonomy_update", "Worms has amphithrax armatus as accepted name with mithrax armatus as original name",
    "FAO", "callaus deliciosa",                    "sciaena deliciosa",          "taxonomy_update", "Update to name identified as accepted in Fishbase and Worms",
    "FAO", "dallocardia muricata",                 "trachycardium muricatum",    "taxonomy_update", "dallocardia muricata is accepted in Worms but lists trachycardium muricatum as a superseded combination",
    "FAO", "grimothea gregaria",                   "munida gregaria",            "taxonomy_update", "grimothea gregaria is accepted in Worms but lists munida gregaria as a superseded combination",
    "FAO", "hansarsia megalops",                   "nematoscelis megalops",      "taxonomy_update", "hansarsia megalops is accepted in Worms but nematoscelis megalops is superseded combination",
    "FAO", "hyporthodus drummondhayi",             "epinephelus drummondhayi",   "taxonomy_update", "",
    "FAO", "iliochione subrugosa",                 "chione subrugosa",           "taxonomy_update", "",
    "FAO", "larkinia grandis",                     "anadara grandis",            "taxonomy_update", "",
    "FAO", "lutraria oblonga",                     "lutraria magna",             "taxonomy_update", "",
    "FAO", "michalisquilla parva",                 "squilla parva",              "taxonomy_update", "",
    "FAO", "mytella strigata",                     "mytella charruana",          "taxonomy_update", "",
    "FAO", "pinirampus argentina",                 "megalonema argentinum",      "taxonomy_update", "",
    "FAO", "polybius depurator",                   "liocarcinus depurator",      "taxonomy_update", "",
    "FAO", "polybius navigator",                   "liocarcinus navigator",      "taxonomy_update", "",
    "FAO", "polybius vernalis",                    "liocarcinus vernalis",       "taxonomy_update", "",
    "FAO", "proteopitar patagonicus",              "pitar patagonicus",          "taxonomy_update", "",
    "FAO", "spisula sibyllae",                     "spisula sachalinensis",      "taxonomy_update", "",
    "FAO", "ylistrum japonicum",                   "amusium japonicum",          "taxonomy_update", "",
    "FAO", "macroramphosidae",                     "centriscidae",               "taxonomy_update", "Bellowfish; macroramphosidae used to be classified as a subfamily of centriscidae",

    # FAO - unresolved_taxon
    "FAO", "cantherhines",               "cantherhines spp",         "unresolved_taxon", "Genus with missing spp",
    "FAO", "anodonta cygnea",            "anodonta spp",             "unresolved_taxon", "Because of its morphological variability and wide range of distribution there are over 500 synonyms for this species; just use genus",
    "FAO", "astacus astacus",            "astacus spp",              "unresolved_taxon", "",
    "FAO", "austropotamobius pallipes",  "astacidae",                "unresolved_taxon", "SeaLifeBase doesn't recognize the genus or species; just use family",
    "FAO", "cherax tenuimanus",          "cherax spp",               "unresolved_taxon", "",
    "FAO", "cipangopaludina chinensis",  "cipangopaludina spp",      "unresolved_taxon", "",
    "FAO", "clupea pallasii",            "clupea pallasii pallasii", "unresolved_taxon", "Match to clupea pallasii pallasii to allow match with rfishbase then rename to clupea pallasii in the final step",
    "FAO", "emmelichthys nitidus",       "emmelichthys spp",         "unresolved_taxon", "",
    "FAO", "euastacus armatus",          "parastacidae",             "unresolved_taxon", "SeaLifeBase doesn't recognize the genus or species; just use family",
    "FAO", "macrobrachium lar",          "macrobrachium spp",        "unresolved_taxon", "",
    "FAO", "macrobrachium malcolmsonii", "macrobrachium spp",        "unresolved_taxon", "",
    "FAO", "merluccius gayi",            "merluccius spp",           "unresolved_taxon", "",
    "FAO", "mullus barbatus",            "mullus spp",               "unresolved_taxon", "",
    "FAO", "oreochromis",                "oreochromis spp",          "unresolved_taxon", "",
    "FAO", "procambarus clarkii",        "procambarus spp",          "unresolved_taxon", "",
    "FAO", "sebastes marinus",           "sebastes spp",             "unresolved_taxon", "",
    "FAO", "cherax cainii",              "cherax spp",               "unresolved_taxon", "Maron - classified into two species both cherax cainii and cherax tenuimanus however only cherax tenuimanus accepted in sealifebase synonyms but does not occur in sealifebase taxa table",
    "FAO", "sinanodonta woodiana",       "anodonta spp",             "unresolved_taxon", "Check to see if could be anodonta dejecta",
    "FAO", "corbicula manilensis",       "corbicula spp",            "unresolved_taxon", "",
    "FAO", "maguimithrax spinosissimus", "mithrax spp",              "unresolved_taxon", "This is a type species of mithrax (sea spiders)",
    "FAO", "austrofusus glans",          "buccinum spp",             "unresolved_taxon", "Whelk",
    "FAO", "astacopsis franklinii",      "parastacidae",             "unresolved_taxon", "FIXIT: Temporary change to genus - remove once species is added to rfishbase data version (FAO 2025 uses rfishbase latest version 24.07)",

    # FAO - name_formatting
    "FAO", "holothuria (holothuria) tubulosa",          "holothuria tubulosa",        "name_formatting", "",
    "FAO", "uroteuthis (photololigo) duvaucelii",       "uroteuthis duvaucelii",      "name_formatting", "",
    "FAO", "perciformes (others)",                      "perciformes",                "name_formatting", "",
    "FAO", "perciformes (percoidei)",                   "perciformes/percoidei",      "name_formatting", "",
    "FAO", "perciformes (scorpaenoidei)",               "perciformes/scorpaenoidei",  "name_formatting", "",
    "FAO", "scombriformes (scombroidei)",               "scombriformes",              "name_formatting", "",
    "FAO", "lutjanidae (ex caesionidae)",               "lutjanidae",                 "name_formatting", "",
    "FAO", "labridae (ex scaridae)",                    "labridae",                   "name_formatting", "",
    "FAO", "alitta virens (formerly nereis virens)",    "alitta virens",              "name_formatting", "Remove note of former name",
    "FAO", "batoidea or batoidimorpha (hypotremata)",   "batoidea",                   "name_formatting", "",
    "FAO", "selachii or selachimorpha (pleurotremata)", "selachii",                   "name_formatting", "",

    # FAO - spelling_error
    "FAO", "herklotsichthys quadrimaculat.",   "herklotsichthys quadrimaculatus",    "spelling_error", "",
    "FAO", "pleuronectes quadrituberculat.",   "pleuronectes quadrituberculatus",    "spelling_error", "",
    "FAO", "pseudopleuronectes herzenst.",     "pseudopleuronectes herzensteini",    "spelling_error", "",
    "FAO", "salmonoidei",                      "salmonidae",                         "spelling_error", "",
    "FAO", "mobulinae",                        "mobulidae",                          "spelling_error", "",

    # FAO - fixit_temporary
    "FAO", "lophiosilurus apurensis",   "osteichthyes",  "fixit_temporary", "FIXIT: Repull rfishbase data and remove once species is verified in the record - currently not listed at all (2025-09)",
    "FAO", "orthopristis chalcea",      "osteichthyes",  "fixit_temporary", "FIXIT: Repull rfishbase data and remove once species is verified in the record - currently not listed at all (2025-09)",
    "FAO", "meuschenia scabra",         "osteichthyes",  "fixit_temporary", "FIXIT: Repull rfishbase data and remove once species is verified in the record - currently not listed at all (2025-09)",
    "FAO", "ratabulus prionotus",       "osteichthyes",  "fixit_temporary", "FIXIT: Repull rfishbase data and remove once species is verified in the record - currently not listed at all (2025-09)",
    "FAO", "bodianus parrae",           "bodianus",      "fixit_temporary", "FIXIT: Temporary change to genus - remove once species is added to rfishbase data version (FAO 2025 uses rfishbase latest version 24.07)",
    "FAO", "bodianus pulcher",          "bodianus",      "fixit_temporary", "FIXIT: Temporary change to genus - remove once species is added to rfishbase data version (FAO 2025 uses rfishbase latest version 24.07)",
    "FAO", "haemulopsis nitida",        "haemulopsis",   "fixit_temporary", "FIXIT: Temporary change to genus - remove once species is added to rfishbase data version (FAO 2025 uses rfishbase latest version 24.07)",
    "FAO", "parupeneus heptacantha",    "parupeneus",    "fixit_temporary", "FIXIT: Temporary change to genus - remove once species is added to rfishbase data version (FAO 2025 uses rfishbase latest version 24.07)",
    "FAO", "pimelodus yuma",            "pimelodus",     "fixit_temporary", "FIXIT: Temporary change to genus - remove once species is added to rfishbase data version (FAO 2025 uses rfishbase latest version 24.07)",

    # SAU - informal_name
    "SAU", "marine finfishes not identified",       "osteichthyes",  "informal_name", "Non-scientific name",
    "SAU", "marine fishes not identified",          "osteichthyes",  "informal_name", "Non-scientific name",
    "SAU", "marine groundfishes not identified",    "osteichthyes",  "informal_name", "Non-scientific name",
    "SAU", "marine pelagic fishes not identified",  "osteichthyes",  "informal_name", "Non-scientific name",
    "SAU", "miscellaneous aquatic invertebrates",   "asteroidea",    "informal_name", "Non-scientific name; assign to asteroidea for now; downstream code defines aquatic invertebrates as list of classes (if we went by phylum ascidians would be omitted as chordata)",
    "SAU", "miscellaneous diadromous fishes",       "osteichthyes",  "informal_name", "Non-scientific name",
    "SAU", "miscellaneous marine crustaceans",      "malacostraca",  "informal_name", "Non-scientific name; assuming some sort of crab/lobster/shrimp/prawn/crayfish crustacean",

    # SAU - rank_mismatch
    "SAU", "anomura",          "decapoda",          "rank_mismatch", "fish/sealifebase doesn't go to infraorder-level of classification",
    "SAU", "brachyura",        "decapoda",          "rank_mismatch", "",
    "SAU", "dendrobranchiata", "decapoda",          "rank_mismatch", "",
    "SAU", "scombroidea",      "perciformes",       "rank_mismatch", "",
    "SAU", "scombroidei",      "perciformes",       "rank_mismatch", "Not matching to taxa table",
    "SAU", "pleuronectoidei",  "pleuronectiformes", "rank_mismatch", "Moving from suborder to order",
    "SAU", "actinopterygii",   "osteichthyes",      "rank_mismatch", "FishBase updated class from actinopterygii to teleostei; decided to lump all actinopterygii as osteichthyes",

    # SAU - taxonomy_update
    "SAU", "inermiidae",   "haemulidae",   "taxonomy_update", "",
    "SAU", "liza",         "planiliza",    "taxonomy_update", "Not matching to taxa table",
    "SAU", "valamugil",    "crenimugil",   "taxonomy_update", "Not matching to taxa table",
    "SAU", "tridacnidae",  "cardiidae",    "taxonomy_update", "Moving from subfamily to family name",
    "SAU", "phrontis vibex", "nassarius vibex", "taxonomy_update", "",

    # SAU - unresolved_taxon
    "SAU", "asterozoa",              "asteroidea",    "unresolved_taxon", "",
    "SAU", "batoidea",               "rajiformes",    "unresolved_taxon", "Several potential orders; assume rajiformes for now",
    "SAU", "echinozoa",              "echinodermata", "unresolved_taxon", "",
    "SAU", "pteriomorphia",          "bivalvia",      "unresolved_taxon", "",
    "SAU", "azurina cyanea",         "azurina",       "unresolved_taxon", "Moving up a taxonomic level",
    "SAU", "macrostrombus costatus", "strombidae",    "unresolved_taxon", "Move from species to family name for identification",
    "SAU", "sinistrofulgur sinistrum", "neogastropoda", "unresolved_taxon", "Move from species to order"
  )

# FIXIT Add check to ensure there are no duplicate records or other possible join conflicts produced

  return(prod_sciname_corrections)
}