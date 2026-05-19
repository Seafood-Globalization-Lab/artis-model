#' Build the Production Scientific Name Corrections Table
#'
#' Constructs a manually curated lookup table used in \code{classify_prod_dat()}
#' to correct FAO and SAU production scientific names that are unresolved by
#' FishBase and SeaLifeBase.
#'
#' @return A tibble with the following columns:
#'   \describe{
#'     \item{prod_data_type}{Data source; one of \code{"FAO"} or \code{"SAU"}.}
#'     \item{sciname_raw}{The unresolved scientific name as it appears in the source data.}
#'     \item{sciname_corrected}{The corrected name to use downstream.}
#'     \item{correction_category}{Reason category for the correction. One of:
#'       \code{"multi_taxa"} (entry refers to multiple taxa; resolved to best representative),
#'       \code{"outdated_name"} (taxonomically obsolete name replaced with accepted name),
#'       \code{"incorrect_name"} (wrong name replaced with correct accepted name),
#'       \code{"spelling_error"} (typographical or truncation error in source data),
#'       \code{"unrecognized_name"} (name not found in FishBase/SeaLifeBase; resolved to higher taxon),
#'       \code{"tribe_to_genus"} (tribe-level name resolved to genus),
#'       \code{"fao_2025_addition"} (new entries added for the FAO 2025 data release),
#'       \code{"actinopterygii_class"} (FishBase class reclassification from actinopterygii to osteichthyes),
#'       \code{"fixit_temporary"} (temporary correction pending FishBase/rfishbase data update).}
#'     \item{notes}{Additional context or rationale for the correction.}
#'   }
#'
#' @seealso \code{\link{classify_prod_dat}}
#'
#' @importFrom tibble tribble
#' @export

build_corr_tbl_prod_sciname <- function(){

  prod_sciname_corrections <- tribble(
    ~prod_data_type, ~sciname_raw, ~sciname_corrected, ~correction_category, ~notes,

    # FAO - multi_taxa
    "FAO", "astacidae, cambaridae",                              "cambaridae",         "multi_taxa", "Choose cambaridae as the larger family",
    "FAO", "auxis thazard, a. rochei",                          "auxis spp",           "multi_taxa", "",
    "FAO", "clarias gariepinus x c. macrocephalus",             "clarias spp",         "multi_taxa", "",
    "FAO", "c. macropomum x p. brachypomus",                    "serrasalmidae",       "multi_taxa", "Colossoma macropomum x Piaractus brachypomus",
    "FAO", "loliginidae, ommastrephidae",                        "teuthida",            "multi_taxa", "",
    "FAO", "merluccius capensis, m.paradoxus",                  "merluccius",          "multi_taxa", "",
    "FAO", "morone chrysops x m. saxatilis",                    "morone spp",          "multi_taxa", "",
    "FAO", "oreochromis aureus x o. niloticus",                 "oreochromis",         "multi_taxa", "",
    "FAO", "osmerus spp, hypomesus spp",                        "osmeridae",           "multi_taxa", "",
    "FAO", "p. mesopotamicus x c. macropomum",                  "serrasalmidae",       "multi_taxa", "Piaractus mesopotamicus x Colossoma macropomum",
    "FAO", "selachimorpha (pleurotremata)",                      "carcharhiniformes",   "multi_taxa", "Essentially an unidentified shark; code defines sharks as list of orders so assign to carcharhiniformes for now",
    "FAO", "sepiidae, sepiolidae",                               "sepiidae",            "multi_taxa", "Sepiidae = cuttlefish; Sepiolidae = bobtail squid; assigning to cuttlefish",
    "FAO", "squalidae, scyliorhinidae",                          "carcharhiniformes",   "multi_taxa", "Two different orders of sharks; code defines sharks as list of orders so assign to carcharhiniformes for now",
    "FAO", "stolothrissa, limnothrissa",                         "clupeidae",           "multi_taxa", "",
    "FAO", "stolothrissa, limnothrissa spp",                    "clupeidae",           "multi_taxa", "",
    "FAO", "xiphopenaeus, trachypenaeus",                        "penaeidae",           "multi_taxa", "",
    "FAO", "xiphopenaeus, trachypenaeus spp",                   "penaeidae",           "multi_taxa", "",
    "FAO", "h. longifilis x c. gariepinus",                     "clariidae",           "multi_taxa", "Matched to the larger family because genus was different",
    "FAO", "e. fuscoguttatus x e. lanceolatus",                 "epinephelus",         "multi_taxa", "Matched by same genus",
    "FAO", "alosa alosa, a. fallax",                            "alosa spp",           "multi_taxa", "Common genus between both",

    # FAO - outdated_name
    "FAO", "branchiostegidae",   "malacanthidae",  "outdated_name", "",
    "FAO", "caspialosa spp",     "alosa spp",      "outdated_name", "",
    "FAO", "invertebrata",       "asteroidea",     "outdated_name", "Assign to asteroidea for now; downstream code defines aquatic invertebrates as list of classes (if we went by phylum ascidians would be omitted as chordata)",
    "FAO", "mobulidae",          "myliobatidae",   "outdated_name", "",
    "FAO", "natantia",           "crangonidae",    "outdated_name", "Natantia is obsolete term for shrimp; assign to order = crangonidae for now",
    "FAO", "reptantia",          "cancridae",      "outdated_name", "Reptantia is obsolete term for crab; multiple families of crab so assign to family = cancridae for now",
    "FAO", "siluroidei",         "siluriformes",   "outdated_name", "",
    "FAO", "aliger gigas",       "lobatus gigas",  "outdated_name", "Queen Conch",
    "FAO", "liza spp",           "planiliza spp",  "outdated_name", "Referring to mullets",

    # FAO - incorrect_name
    "FAO", "mytilus unguiculatus",  "mytilus coruscus",       "incorrect_name", "Korean Mussel",
    "FAO", "tritia mutabilis",      "nassarius mutabilis",    "incorrect_name", "Mutable/Changeable Nassa",
    "FAO", "tritia reticulata",     "nassarius reticulatus",  "incorrect_name", "Netted Dog whelk",

    # FAO - spelling_error
    "FAO", "herklotsichthys quadrimaculat.",   "herklotsichthys quadrimaculatus",    "spelling_error", "",
    "FAO", "pleuronectes quadrituberculat.",   "pleuronectes quadrituberculatus",    "spelling_error", "",
    "FAO", "pseudopleuronectes herzenst.",     "pseudopleuronectes herzensteini",    "spelling_error", "",
    "FAO", "salmonoidei",                      "salmonidae",                         "spelling_error", "",
    "FAO", "mobulinae",                        "mobulidae",                          "spelling_error", "",
    "FAO", "moroteuthopsis ingens",            "onykia ingens",                      "spelling_error", "",
    "FAO", "pandalus spp, pandalopsis spp",    "pandalus spp",                       "spelling_error", "Prawn",

    # FAO - unrecognized_name
    "FAO", "crustacea",                  "branchiopoda",             "unrecognized_name", "Downstream code IDs crustacea to class level; assign to branchiopoda for now; downstream code defines crustaceans as list of classes c('branchiopoda', 'malacostraca', 'maxillopoda', 'merostomata'); assuming non-crab/lobster/shrimp crustacean",
    "FAO", "cantherhines",               "cantherhines spp",         "unrecognized_name", "Genus with missing spp",
    "FAO", "anodonta cygnea",            "anodonta spp",             "unrecognized_name", "Because of its morphological variability and wide range of distribution there are over 500 synonyms for this species; just use genus",
    "FAO", "astacus astacus",            "astacus spp",              "unrecognized_name", "",
    "FAO", "austropotamobius pallipes",  "astacidae",                "unrecognized_name", "SeaLifeBase doesn't recognize the genus or species; just use family",
    "FAO", "cherax tenuimanus",          "cherax spp",               "unrecognized_name", "",
    "FAO", "cipangopaludina chinensis",  "cipangopaludina spp",      "unrecognized_name", "",
    "FAO", "clupea pallasii",            "clupea pallasii pallasii", "unrecognized_name", "Match to clupea pallasii pallasii to allow match with rfishbase then rename to clupea pallasii in the final step",
    "FAO", "clupeoidei",                 "clupeiformes",             "unrecognized_name", "",
    "FAO", "emmelichthys nitidus",       "emmelichthys spp",         "unrecognized_name", "",
    "FAO", "euastacus armatus",          "parastacidae",             "unrecognized_name", "SeaLifeBase doesn't recognize the genus or species; just use family",
    "FAO", "macrobrachium lar",          "macrobrachium spp",        "unrecognized_name", "",
    "FAO", "macrobrachium malcolmsonii", "macrobrachium spp",        "unrecognized_name", "",
    "FAO", "merluccius gayi",            "merluccius spp",           "unrecognized_name", "",
    "FAO", "mullus barbatus",            "mullus spp",               "unrecognized_name", "",
    "FAO", "oreochromis",                "oreochromis spp",          "unrecognized_name", "",
    "FAO", "percoidei",                  "perciformes",              "unrecognized_name", "",
    "FAO", "procambarus clarkii",        "procambarus spp",          "unrecognized_name", "",
    "FAO", "scombroidei",                "perciformes",              "unrecognized_name", "FishBase doesn't list scombiformes as an order (See fishbase %>% filter(Family == 'scombridae'))",
    "FAO", "sebastes marinus",           "sebastes spp",             "unrecognized_name", "",
    "FAO", "brachyura",                  "decapoda",                 "unrecognized_name", "Infraorder not part of fishbase database",
    "FAO", "cherax cainii",              "cherax spp",               "unrecognized_name", "Maron - classified into two species both cherax cainii and cherax tenuimanus however only cherax tenuimanus accepted in sealifebase synonyms but does not occur in sealifebase taxa table",
    "FAO", "sinanodonta woodiana",       "anodonta spp",             "unrecognized_name", "Check to see if could be anodonta dejecta",
    "FAO", "caridina denticulata",       "neocaridina denticulata",  "unrecognized_name", "Synonym to accepted name not caught by fb or slb",
    "FAO", "anomura",                    "decapoda",                 "unrecognized_name", "Infraorder name to order name",
    "FAO", "corbicula manilensis",       "corbicula spp",            "unrecognized_name", "",
    "FAO", "maguimithrax spinosissimus", "mithrax spp",              "unrecognized_name", "This is a type species of mithrax (sea spiders)",
    "FAO", "macroramphosidae",           "centriscidae",             "unrecognized_name", "Bellowfish; macroramphosidae used to be classified as a subfamily of centriscidae",
    "FAO", "austrofusus glans",          "buccinum spp",             "unrecognized_name", "Whelk",

    # FAO - tribe_to_genus
    "FAO", "thunnini", "thunnus spp", "tribe_to_genus", "",

    # FAO - fao_2025_addition
    "FAO", "afruca tangeri",                                                 "uca tangeri",                "fao_2025_addition",   "Worms has afruca tangeri as accepted name with uca tangeri as synonym",
    "FAO", "ageneiosus dentatus",                                            "ageneiosus ucayalensis",     "fao_2025_addition",   "ageneiosus dentatus listed in Fishbase as an ambiguous synonym",
    "FAO", "alitta virens (formerly nereis virens)",                         "alitta virens",              "fao_2025_addition",   "Remove note of former name",
    "FAO", "amphithrax armatus",                                             "mithrax armatus",            "fao_2025_addition",   "Worms has amphithrax armatus as accepted name with mithrax armatus as original name",
    "FAO", "callaus deliciosa",                                              "sciaena deliciosa",          "fao_2025_addition",   "Update to name identified as accepted in Fishbase and Worms",
    "FAO", "caridea",                                                        "decapoda",                   "fao_2025_addition",   "Infraorder within decapoda so move up",
    "FAO", "colossoma macropomum x piaractus brachypomus",                  "serrasalmidae",              "fao_2025_addition",   "Replace with common family for hybrid",
    "FAO", "dallocardia muricata",                                           "trachycardium muricatum",    "fao_2025_addition",   "dallocardia muricata is accepted in Worms but lists trachycardium muricatum as a superseded combination",
    "FAO", "epinephelus fuscoguttatus x e. lanceolatus",                    "epinephelus",                "fao_2025_addition",   "Replace with common genus for hybrid",
    "FAO", "grimothea gregaria",                                             "munida gregaria",            "fao_2025_addition",   "grimothea gregaria is accepted in Worms but lists munida gregaria as a superseded combination",
    "FAO", "hansarsia megalops",                                             "nematoscelis megalops",      "fao_2025_addition",   "hansarsia megalops is accepted in Worms but nematoscelis megalops is superseded combination",
    "FAO", "heterobranchus longifilis x clarias gariepinus",                "clariidae",                  "fao_2025_addition",   "Replace with common family for hybrid",
    "FAO", "holothuria (holothuria) tubulosa",                               "holothuria tubulosa",        "fao_2025_addition",   "",
    "FAO", "hyporthodus drummondhayi",                                       "epinephelus drummondhayi",   "fao_2025_addition",   "",
    "FAO", "iliochione subrugosa",                                           "chione subrugosa",           "fao_2025_addition",   "",
    "FAO", "larkinia grandis",                                               "anadara grandis",            "fao_2025_addition",   "",
    "FAO", "lutjanidae (ex caesionidae)",                                    "lutjanidae",                 "fao_2025_addition",   "",
    "FAO", "lutraria oblonga",                                               "lutraria magna",             "fao_2025_addition",   "",
    "FAO", "michalisquilla parva",                                           "squilla parva",              "fao_2025_addition",   "",
    "FAO", "mytella strigata",                                               "mytella charruana",          "fao_2025_addition",   "",
    "FAO", "perciformes (others)",                                           "perciformes",                "fao_2025_addition",   "",
    "FAO", "perciformes (percoidei)",                                        "perciformes/percoidei",      "fao_2025_addition",   "",
    "FAO", "perciformes (scorpaenoidei)",                                    "perciformes/scorpaenoidei",  "fao_2025_addition",   "",
    "FAO", "piaractus mesopotamicus x colossoma macropomum",                "serrasalmidae",              "fao_2025_addition",   "Replace with common family for hybrid",
    "FAO", "pinirampus argentina",                                           "megalonema argentinum",      "fao_2025_addition",   "",
    "FAO", "polybius depurator",                                             "liocarcinus depurator",      "fao_2025_addition",   "",
    "FAO", "polybius navigator",                                             "liocarcinus navigator",      "fao_2025_addition",   "",
    "FAO", "polybius vernalis",                                              "liocarcinus vernalis",       "fao_2025_addition",   "",
    "FAO", "proteopitar patagonicus",                                        "pitar patagonicus",          "fao_2025_addition",   "",
    "FAO", "scombriformes (scombroidei)",                                    "scombriformes",              "fao_2025_addition",   "",
    "FAO", "spisula sibyllae",                                               "spisula sachalinensis",      "fao_2025_addition",   "",
    "FAO", "uroteuthis (photololigo) duvaucelii",                           "uroteuthis duvaucelii",      "fao_2025_addition",   "",
    "FAO", "ylistrum japonicum",                                             "amusium japonicum",          "fao_2025_addition",   "",
    "FAO", "labridae (ex scaridae)",                                         "labridae",                   "fao_2025_addition",   "",
    "FAO", "batoidea or batoidimorpha (hypotremata)",                        "batoidea",                   "fao_2025_addition",   "",
    "FAO", "selachii or selachimorpha (pleurotremata)",                      "selachii",                   "fao_2025_addition",   "",
    # actinopterygii_class - FAO
    "FAO", "actinopterygii",                                                 "osteichthyes",               "actinopterygii_class", "FishBase updated class from actinopterygii to teleostei; decided to lump all actinopterygii as osteichthyes",
    # fixit_temporary - FAO
    "FAO", "lophiosilurus apurensis",                                        "osteichthyes",               "fixit_temporary",     "FIXIT: Repull rfishbase data and remove once species is verified in the record - currently not listed at all (2025-09)",
    "FAO", "orthopristis chalcea",                                           "osteichthyes",               "fixit_temporary",     "FIXIT: Repull rfishbase data and remove once species is verified in the record - currently not listed at all (2025-09)",
    "FAO", "meuschenia scabra",                                              "osteichthyes",               "fixit_temporary",     "FIXIT: Repull rfishbase data and remove once species is verified in the record - currently not listed at all (2025-09)",
    "FAO", "ratabulus prionotus",                                            "osteichthyes",               "fixit_temporary",     "FIXIT: Repull rfishbase data and remove once species is verified in the record - currently not listed at all (2025-09)",
    "FAO", "bodianus parrae",                                                "bodianus",                   "fixit_temporary",     "FIXIT: Temporary change to genus - remove once species is added to rfishbase data version (FAO 2025 uses rfishbase latest version 24.07)",
    "FAO", "bodianus pulcher",                                               "bodianus",                   "fixit_temporary",     "FIXIT: Temporary change to genus - remove once species is added to rfishbase data version (FAO 2025 uses rfishbase latest version 24.07)",
    "FAO", "haemulopsis nitida",                                             "haemulopsis",                "fixit_temporary",     "FIXIT: Temporary change to genus - remove once species is added to rfishbase data version (FAO 2025 uses rfishbase latest version 24.07)",
    "FAO", "parupeneus heptacantha",                                         "parupeneus",                 "fixit_temporary",     "FIXIT: Temporary change to genus - remove once species is added to rfishbase data version (FAO 2025 uses rfishbase latest version 24.07)",
    "FAO", "astacopsis franklinii",                                          "parastacidae",               "fixit_temporary",     "FIXIT: Temporary change to genus - remove once species is added to rfishbase data version (FAO 2025 uses rfishbase latest version 24.07)",
    "FAO", "pimelodus yuma",                                                 "pimelodus",                  "fixit_temporary",     "FIXIT: Temporary change to genus - remove once species is added to rfishbase data version (FAO 2025 uses rfishbase latest version 24.07)",
    # unrecognized_name - SAU
    "SAU", "marine finfishes not identified",                                "osteichthyes",               "unrecognized_name",   "Non-scientific name",
    "SAU", "marine fishes not identified",                                   "osteichthyes",               "unrecognized_name",   "Non-scientific name",
    "SAU", "marine groundfishes not identified",                             "osteichthyes",               "unrecognized_name",   "Non-scientific name",
    "SAU", "marine pelagic fishes not identified",                           "osteichthyes",               "unrecognized_name",   "Non-scientific name",
    "SAU", "miscellaneous aquatic invertebrates",                            "asteroidea",                 "unrecognized_name",   "Non-scientific name; assign to asteroidea for now; downstream code defines aquatic invertebrates as list of classes (if we went by phylum ascidians would be omitted as chordata)",
    "SAU", "miscellaneous diadromous fishes",                                "osteichthyes",               "unrecognized_name",   "Non-scientific name",
    "SAU", "miscellaneous marine crustaceans",                               "malacostraca",               "unrecognized_name",   "Non-scientific name; assuming some sort of crab/lobster/shrimp/prawn/crayfish crustacean",
    "SAU", "anomura",                                                        "decapoda",                   "unrecognized_name",   "fish/sealifebase doesn't go to infraorder-level of classification",
    "SAU", "asterozoa",                                                      "asteroidea",                 "unrecognized_name",   "",
    "SAU", "batoidea",                                                       "rajiformes",                 "unrecognized_name",   "Several potential orders; assume rajiformes for now",
    "SAU", "brachyura",                                                      "decapoda",                   "unrecognized_name",   "",
    "SAU", "dendrobranchiata",                                               "decapoda",                   "unrecognized_name",   "",
    "SAU", "echinozoa",                                                      "echinodermata",              "unrecognized_name",   "",
    "SAU", "inermiidae",                                                     "haemulidae",                 "unrecognized_name",   "",
    "SAU", "pteriomorphia",                                                  "bivalvia",                   "unrecognized_name",   "",
    "SAU", "scombroidea",                                                    "perciformes",                "unrecognized_name",   "",
    "SAU", "liza",                                                           "planiliza",                  "unrecognized_name",   "Not matching to taxa table",
    "SAU", "scombroidei",                                                    "perciformes",                "unrecognized_name",   "Not matching to taxa table",
    "SAU", "pleuronectoidei",                                                "pleuronectiformes",          "unrecognized_name",   "Moving from suborder to order",
    "SAU", "valamugil",                                                      "crenimugil",                 "unrecognized_name",   "Not matching to taxa table",
    "SAU", "tridacnidae",                                                    "cardiidae",                  "unrecognized_name",   "Moving from subfamily to family name",
    "SAU", "azurina cyanea",                                                 "azurina",                    "unrecognized_name",   "Moving up a taxonomic level",
    "SAU", "macrostrombus costatus",                                         "strombidae",                 "unrecognized_name",   "Move from species to family name for identification",
    "SAU", "phrontis vibex",                                                 "nassarius vibex",            "unrecognized_name",   "",
    "SAU", "sinistrofulgur sinistrum",                                       "neogastropoda",              "unrecognized_name",   "Move from species to order",
    # actinopterygii_class - SAU
    "SAU", "actinopterygii",                                                 "osteichthyes",               "actinopterygii_class", "FishBase updated class from actinopterygii to teleostei; decided to lump all actinopterygii as osteichthyes"
  )
  
  return(prod_sciname_corrections)
}