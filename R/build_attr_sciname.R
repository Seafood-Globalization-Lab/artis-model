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
    bind_rows(data.frame(
      sciname = c("arthropoda", "engraulis", "hippoglossinae", "scombrinae", "clupea",     
                  "chondrichthyes", "salmoninae", "mytilinae", "actinopteri", "animalia",    
                  "homarus", "cypriniformes", "dissostichus", "micromesistius", "echinoida"),
      
      common_name = c("arthropods", "anchovies", "flounders", "mackerels, tunas, and bonitos",
                     "herrings", "sharks, skates, rays, and chimaeras", "salmons and trouts",
                     "saltwater mussels", "ray-finned fish", "aquatic animals", "lobsters", 
                     "carps, minnows, loaches, etc", "toothfish", "blue whitings", "sea urchins"),
      
      Genus = c(NA, "engraulis", NA, NA, "clupea",     
                NA, NA, NA, NA, NA,    
                "homarus", NA, "dissostichus", "micromesistius", NA),
      
      Subfamily = c(NA, "engraulinae", "hippoglossinae", "scombrinae", "clupeinae",     
                    NA, "salmoninae", "mytilinae", NA, NA,    
                    NA, NA, NA, NA, NA),
      
      Family = c(NA, "engraulidae", "pleuronectidae", "scombridae", "clupeidae",     
                 NA, "salmonidae", "mytilidae", NA, NA,    
                 "nephropidae", NA, "nototheniidae", "gadidae", NA), 
      
      Order = c(NA, "clupeiformes", "pleuronectiformes", "scombriformes", "	clupeiformes",     
                NA, "salmoniformes", "mytilida", NA, NA,    
                "decapoda", "cypriniformes", "perciformes", "gadiformes", "echinoida"), 
      
      Class = c(NA, "teleostei", "teleostei", "teleostei", "teleostei",     
                "chondrichthyes", "teleostei", "bivalvia", "teleostei", NA,    
                "malacostraca", "teleostei", "teleostei", "teleostei", "echinoidea"),
      
      Superclass = c(NA, "osteichthyes", "osteichthyes", "osteichthyes", "osteichthyes",     
                     NA, "osteichthyes", NA, "osteichthyes", NA,    
                     NA, "osteichthyes", "osteichthyes", "osteichthyes", NA),
      
      Phylum = c("arthropoda", "chordata", "chordata", "chordata", "chordata",     
                 "chordata", "chordata", "mollusca", "chordata", NA,    
                 "arthropoda", "chordata", "chordata", "chordata", "echinodermata"),
      
      Kingdom = c("animalia", "animalia", "animalia", "animalia", "animalia",     
                  "animalia", "animalia", "animalia", "animalia", "animalia",    
                  "animalia", "animalia", "animalia", "animalia", "animalia")
    )) 

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