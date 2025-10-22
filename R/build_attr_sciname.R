#' Build Scientific Name Attribute Table
#'
#' Creates a comprehensive scientific name attribute table combining taxonomic
#' classification, common names, and ISSCAAP group information for use in 
#' ARTIS model workflows.
#'
#' @param taxa_data Data frame containing cleaned FAO taxa with SciName and 
#'   CommonName columns, typically from "clean_fao_taxa.csv".
#' @param isscaap_attribute Data frame containing ISSCAAP group classifications 
#'   by scientific name.
#' @param running_sau Logical. Whether to include SAU taxa data in the attribute table.
#' @param sau_taxa_data Data frame containing cleaned SAU taxa (only used if 
#'   running_sau = TRUE).
#' @param output_dir Character string. Directory path where the scientific name 
#'   attribute CSV file will be saved.
#'
#' @return NULL (invisible). Function is called for its side effects of creating
#'   the scientific name attribute CSV file.
#'
#' @details 
#' This function performs the following operations:
#' \itemize{
#'   \item Applies custom common name corrections for specific scientific names
#'   \item Adds phylogenetic classification data for higher-order taxa
#'   \item Joins ISSCAAP group classifications
#'   \item Optionally incorporates SAU taxa data if running_sau = TRUE
#'   \item Resolves duplicate entries by selecting records with fewer NA values
#'   \item Saves result as "sciname_attribute.csv" in the specified directory
#' }
#' 
#' The function includes extensive manual corrections for common names and adds
#' taxonomic hierarchy information (Genus through Kingdom levels) for taxa
#' commonly used in ARTIS model workflows.
#'
#' @import dplyr
#' @importFrom utils write.csv
#' @export
build_attr_sciname <- function(taxa_data, 
                              isscaap_attribute, 
                              running_sau = FALSE, 
                              sau_taxa_data = NULL, 
                              output_dir) {
  
  # Create 1-to-1 matching for common names and taxa info
  taxa_attribute <- taxa_data %>%
    mutate(common_name = case_when(
      sciname == "alosa" ~ "shads nei", 
      sciname == "asteroidea" ~ "starfishes nei", 
      sciname == "branchiopoda" ~ "crustaceans nei", 
      sciname == "carcharhiniformes" ~ "ground sharks nei", 
      sciname == "clarias" ~ "catfishes nei", 
      sciname == "clupeidae" ~ "sardines nei", 
      sciname == "clupeiformes" ~ "clupeoids nei", 
      sciname == "dentex tumifrons" ~ "yellowback seabream",
      sciname == "epinephelus" ~ "groupers nei", 
      sciname == "gadus macrocephalus" ~ "pacific cod", 
      sciname == "gobiidae" ~ "gobies nei", 
      sciname == "jasus edwardsii" ~ "red rock lobster", 
      sciname == "lepidonotothen squamifrons" ~ "grey rockcod", 
      sciname == "macrobrachium" ~ "river prawns nei", 
      sciname == "merluccius" ~ "hakes nei", 
      sciname == "mollusca" ~ "molluscs nei", 
      sciname == "mullus" ~ "surmullets(=red mullets) nei", 
      sciname == "myliobatidae" ~ "eagle and manta rays nei", 
      sciname == "oreochromis" ~ "tilapias nei", 
      sciname == "osteichthyes" ~ "fish nei", 
      sciname == "palaemonidae" ~ "palaemonid shrimps nei", 
      sciname == "parastacidae" ~ "crayfishes nei", 
      sciname == "penaeidae" ~ "penaeid shrimps nei", 
      sciname == "perciformes" ~ "tuna-like fishes nei", 
      sciname == "planiliza haematocheilus" ~ "so-iny (redlip) mullet", 
      sciname == "salmonidae" ~ "almonids nei", 
      sciname == "sardinops sagax" ~ "south american pilchard", 
      sciname == "sebastes" ~ "redfishes nei", 
      sciname == "serrasalmidae" ~ "serrasalmids nei", 
      sciname == "thunnus" ~ "tunas nei", 
      sciname == "xiphopenaeus kroyeri" ~ "atlantic seabob", 
      sciname == "bryzoa" ~ "bryzoa",
      TRUE ~ common_name
    )) %>%
    distinct() %>%
    bind_rows(
      tibble::tribble(
  ~sciname,           ~common_name,                                   ~Genus,               ~Subfamily,          ~Family,               ~Order,               ~Class,               ~Superclass,         ~Phylum,            ~Kingdom,
  "arthropoda",       "arthropods",                                   NA_character_,        NA_character_,       NA_character_,         NA_character_,        NA_character_,        NA_character_,        "arthropoda",       "animalia",
  "engraulis",        "anchovies",                                    "engraulis",          "engraulinae",       "engraulidae",         "clupeiformes",       "teleostei",          "osteichthyes",       "chordata",         "animalia",
  "hippoglossinae",   "flounders",                                    NA_character_,        "hippoglossinae",    "pleuronectidae",      "pleuronectiformes",  "teleostei",          "osteichthyes",       "chordata",         "animalia",
  "scombrinae",       "mackerels, tunas, and bonitos",                NA_character_,        "scombrinae",        "scombridae",          "scombriformes",      "teleostei",          "osteichthyes",       "chordata",         "animalia",
  "clupea",           "herrings",                                     "clupea",             "clupeinae",         "clupeidae",           "clupeiformes",       "teleostei",          "osteichthyes",       "chordata",         "animalia",
  "chondrichthyes",   "sharks, skates, rays, and chimaeras",          NA_character_,        NA_character_,       NA_character_,         NA_character_,        "chondrichthyes",     NA_character_,        "chordata",         "animalia",
  "salmoninae",       "salmons and trouts",                           NA_character_,        "salmoninae",        "salmonidae",          "salmoniformes",      "teleostei",          "osteichthyes",       "chordata",         "animalia",
  "mytilinae",        "saltwater mussels",                            NA_character_,        "mytilinae",         "mytilidae",           "mytilida",           "bivalvia",           NA_character_,        "mollusca",         "animalia",
  "actinopteri",      "ray-finned fish",                              NA_character_,        NA_character_,       NA_character_,         NA_character_,        "teleostei",          "osteichthyes",       "chordata",         "animalia",
  "animalia",         "aquatic animals",                              NA_character_,        NA_character_,       NA_character_,         NA_character_,        NA_character_,        NA_character_,        NA_character_,      "animalia",
  "homarus",          "lobsters",                                     "homarus",            NA_character_,       "nephropidae",         "decapoda",           "malacostraca",       NA_character_,        "arthropoda",       "animalia",
  "cypriniformes",    "carps, minnows, loaches, etc",                 NA_character_,        NA_character_,       NA_character_,         "cypriniformes",      "teleostei",          "osteichthyes",       "chordata",         "animalia",
  "dissostichus",     "toothfish",                                    "dissostichus",       NA_character_,       "nototheniidae",       "perciformes",        "teleostei",          "osteichthyes",       "chordata",         "animalia",
  "micromesistius",   "blue whitings",                                "micromesistius",     NA_character_,       "gadidae",             "gadiformes",         "teleostei",          "osteichthyes",       "chordata",         "animalia",
  "echinoida",        "sea urchins",                                  NA_character_,        NA_character_,       NA_character_,         "echinoida",          "echinoidea",         NA_character_,        "echinodermata",    "animalia"
)
  ) 

  # Join with ISSCAAP attribute data
  taxa_attribute <- taxa_attribute %>%
    left_join(isscaap_attribute, by = "sciname")

  # Add missing scinames from SAU if running SAU
  if (running_sau && !is.null(sau_taxa_data)) {
    sau_taxa_filtered <- sau_taxa_data %>%
      filter(!(sciname %in% taxa_attribute$sciname))
    
    taxa_attribute <- taxa_attribute %>%
      bind_rows(sau_taxa_filtered)
  }

  # Resolve duplicates by selecting records with fewer NAs
  taxa_attribute <- taxa_attribute %>%
    distinct() %>%
    ungroup() %>%
    mutate(sum_na = rowSums(is.na(.))) %>%
    group_by(sciname) %>%
    slice_min(order_by = sum_na, n = 1, with_ties = FALSE) %>%
    select(-sum_na)

  # Save to file
  write.csv(taxa_attribute, file.path(output_dir, "sciname_attribute.csv"), row.names = FALSE)
  
  return(invisible(NULL))
}