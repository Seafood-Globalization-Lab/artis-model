# TITLE: create_metadata.R

# Load packages
library(artis)
library(tidyverse)
library(janitor)

# Set directories
datadir <- "model_inputs_raw"
inputsdir <- "model_inputs_clean"
outdir <- "model_outputs"

# Load production data
prod <- read.csv(file.path(inputsdir, "clean_fao_prod.csv"))
prod <- prod %>%
  rename(sciname = SciName, common_name = CommonName,
         method = prod_method)

# Load cleaned taxa details
taxa <- read.csv(file.path(inputsdir, "clean_fao_taxa.csv")) %>%
  rename(sciname = SciName, common_name = CommonName) %>%
  distinct()

# Load nutrient data
# Note that values are all expressed per 100 g
# Nutrient data for FAO 2020 version
# nutrient <- read.csv(file.path(datadir, "ARTIS_spp_nutrients.csv"))

# Load NCEAS group files
#nceas_marine_capture <- read.csv(file.path(datadir, "nceas_marine_capture_groups.csv"))
nceas_marine_aquaculture <- read.csv(file.path(datadir, "nceas_marine_aquaculture_groups.csv"))

# Load SAU functional group data
sau_functional_groups <- read.csv(file.path(datadir, "sau_species.csv"))

#___________________________________________________________________________________________________________________#
# Nutrient data
#___________________________________________________________________________________________________________________#
# Load nutrient data from Daniel Viana

# Nutrient merge notes
# - Standardizing names to join them across the data sets
# - Deciding how best to aggregate up to broader taxonomic groups
# - Deciding how to fill in any missing values
#   * Imputed or hierarchical approach 
# - Discussing options for connecting data for both live weight and different product forms
#   * Likely focus on using edible portion 

# # Format nutrient data
# nutrient <- nutrient %>%
#   # Clean nutrient names to make them better for pivoting wider
#   mutate(nutrient = case_when(
#     nutrient == "Calcium" ~ "calcium",
#     nutrient == "Iron" ~ "iron",
#     nutrient == "Protein" ~ "protein",
#     nutrient == "DHA+EPA" ~ "fattyacids",
#     nutrient == "Vitamin A" ~ "vitamina",
#     nutrient == "Vitamin B12" ~ "vitaminb12",
#     nutrient == "Zinc" ~ "zinc"
#   )) %>%
#   mutate(nutrient = paste(nutrient, nutrient_units, sep = "_")) %>%
#   select(-c("nutrient_units", "ssd", "count", "se", "lower_ci", "upper_ci", "taxa_match")) %>%
#   pivot_wider(names_from = nutrient, values_from = value)
# 
# # Format nutrient data to write out metadata file
# nutrient <- nutrient %>%
#   select(sciname, calcium_mg, iron_mg, protein_g, fattyacids_g, 
#          vitamina_mcg, vitaminb12_mcg, zinc_mg)
# 
# write.csv(nutrient, file.path(outdir, "nutrient_metadata.csv"), row.names = FALSE)

#___________________________________________________________________________________________________________________#
# Clean scientific name information
#___________________________________________________________________________________________________________________#
# Create 1-to-1 matching for ISSCAAP
isscaap_metadata <- prod %>%
  select(sciname, isscaap_group) %>%
  distinct()

multiple_isscaap <- isscaap_metadata %>% 
  group_by(sciname) %>%
  tally() %>%
  filter(n>1) %>%
  pull(sciname)

isscaap_metadata <- isscaap_metadata %>%
  mutate(isscaap_group = case_when(
    sciname %in% multiple_isscaap ~ "Multiple ISSCAAP groups",
    !(sciname %in% multiple_isscaap) ~ isscaap_group
  )) %>%
  distinct()

# Add ISSCAAP groups for custom "unknown origin" scinames
unknown_isscaap <- data.frame(sciname = c("arthropoda", "chondrichthyes", 
                                  "engraulis", "actinopteri", "homarus",
                                  "mytilinae", "clupea", "hippoglossinae", 
                                  "scombrinae", "salmoninae", "animalia", 
                                  "dissostichus", "cypriniformes", 
                                  "micromesistius", "echinoida", "chordata"),
                      isscaap_group = c("Multiple ISSCAAP groups", "Sharks, rays, chimaeras",
                                        "Herrings, sardines, anchovies", "Multiple ISSCAAP groups",
                                        "Lobsters, spiny-rock lobsters", "Mussels",
                                        "Herrings, sardines, anchovies", "Flounders, halibuts, soles", 
                                        "Multiple ISSCAAP groups", "Salmons, trouts, smelts", 
                                        "Multiple ISSCAAP groups", "Miscellaneous demersal fishes", 
                                        "Carps, barbels and other cyprinids", "Cods, hakes, haddocks",
                                        "Sea-urchins and other echinoderms", "Multiple ISSCAAP groups"))

isscaap_metadata <- isscaap_metadata %>%
  bind_rows(unknown_isscaap)

# Create file for phylogenetic metadata
# Create 1-to-1 matching for common names and taxa info
taxa_metadata <- taxa %>%
  mutate(common_name = case_when(
    sciname =="alosa" ~ "shads nei", 
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
    Genus = NA,
    Subfamily = NA,
    Family = NA, 
    Order = NA, 
    Class = NA,
    Superclass = NA,
    Phylum = NA,
    Kingdom = NA
  )) 
 

taxa_metadata <- taxa_metadata %>%
  left_join(isscaap_metadata, by = "sciname")

write.csv(taxa_metadata, file.path(outdir, "sciname_metadata.csv"), row.names = FALSE)


#___________________________________________________________________________________________________________________#
# Create sciname_prod_enviro_metadata
#___________________________________________________________________________________________________________________#
# Create marine capture groups
marine_capture_sciname <- prod %>%
  select(sciname, common_name, method, habitat, isscaap_group) %>%
  filter(method == "capture", habitat == "marine") %>%
  distinct()

# Create variable based on SAU functional group
# Marine capture NCEAS group addition
sau_functional_groups <- sau_functional_groups %>%
  mutate(scientific_name = tolower(scientific_name),
         functional_group = tolower(functional_group),
         common_name = tolower(common_name)) %>%
  mutate(nceas_group = case_when(
    functional_group == "small pelagics (<30 cm)" ~ "small pelagics",
    functional_group == "medium pelagics (30 - 89 cm)" ~ "medium pelagics",
    functional_group == "large pelagics (>=90 cm)" ~ "large pelagics",
    
    functional_group == "small bathypelagics (<30 cm)" ~ "small pelagics",    
    functional_group == "medium bathypelagics (30 - 89 cm)" ~ "medium pelagics",
    functional_group == "large bathypelagics (>=90 cm)" ~ "large pelagics",
    
    functional_group == "small benthopelagics (<30 cm)" ~ "demersal",
    functional_group == "medium benthopelagics (30 - 89 cm)" ~ "demersal",
    functional_group == "large benthopelagics (>=90 cm)" ~ "demersal",     
    
    functional_group == "small demersals (<30 cm)" ~ "demersal",
    functional_group == "medium demersals (30 - 89 cm)" ~ "demersal",
    functional_group == "large demersals (>=90 cm)" ~ "demersal",
    
    functional_group == "small bathydemersals (<30 cm)" ~ "demersal",
    functional_group == "medium bathydemersals (30 - 89 cm)" ~ "demersal",   
    functional_group == "large bathydemersals (>=90 cm)" ~ "demersal",
    
    functional_group == "small to medium flatfishes (<90 cm)" ~ "demersal",
    functional_group == "large flatfishes (>=90 cm)" ~ "demersal",
    
    functional_group == "small reef assoc. fish (<30 cm)" ~ "reef associated",
    functional_group == "medium reef assoc. fish (30 - 89 cm)" ~ "reef associated",
    functional_group == "large reef assoc. fish (>=90 cm)" ~ "reef associated",
    
    functional_group == "lobsters, crabs" ~ "benthic",
    
    functional_group == "shrimps" ~ "demersal",
    functional_group == "other demersal invertebrates" ~ "demersal",
    
    functional_group == "small to medium sharks (<90 cm)" ~ "demersal",
    functional_group == "large sharks (>=90 cm)" ~ "demersal",
    
    functional_group == "small to medium rays (<90 cm)" ~ "demersal",
    functional_group == "large rays (>=90 cm)" ~ "demersal",
    
    functional_group == "krill" ~ "fofm",
    functional_group == "jellyfish" ~ "jellyfish",
    
    # Specify cephalopods by scientific name:
    scientific_name == "octopus vulgaris" ~ "reef associated",
    scientific_name == "octopodidae" ~ "reef associated",
    scientific_name %in% c("eledone", "eledone cirrhosa", "turbo cornutus", "bellator militaris") ~ "reef associated",
    scientific_name == "ommastrephidae" ~ "reef associated",
    scientific_name == "eledone cirrhosa" ~ "reef associated",
    scientific_name == "octopoda" ~ "reef associated",
    scientific_name == "illex argentinus" ~ "small pelagics",
    scientific_name == "doryteuthis gahi" ~ "small pelagics",
    scientific_name == "illex illecebrosus" ~ "small pelagics",
    scientific_name == "doryteuthis opalescens" ~ "small pelagics",
    scientific_name == "dosidicus gigas" ~ "medium pelagics",
    scientific_name == "illex coindetii" ~ "medium pelagics",
    scientific_name == "todarodes sagittatus" ~ "medium pelagics",
    scientific_name == "todarodes pacificus" ~ "medium pelagics",
    scientific_name == "nototodarus sloanii" ~ "medium pelagics",
    scientific_name == "ommastrephes bartramii" ~ "medium pelagics",
    scientific_name == "loligo forbesii" ~ "medium pelagics",
    scientific_name == "pleuragramma antarctica" ~ "small pelagics",
    str_detect(common_name, "octopus") ~ "reef associated",
    str_detect(common_name, "cuttlefish") ~ "reef associated",
    str_detect(common_name, "squid") ~ "medium pelagics"
  ))

marine_capture_sciname <- marine_capture_sciname %>% 
  left_join(sau_functional_groups, by = c("sciname" = "scientific_name")) %>%
  select(sciname, habitat, method, common_name = common_name.x, 
         isscaap = isscaap_group, nceas_group) %>% 
  mutate(isscaap = tolower(isscaap)) %>%
  mutate(nceas_group = case_when(
    !is.na(nceas_group) ~ nceas_group,
    str_detect(isscaap, "herring") ~ "small pelagics",
    str_detect(isscaap, "demersal") ~ "demersal",
    str_detect(isscaap, "shark") ~ "demersal",
    str_detect(isscaap, "cod") ~ "demersal",
    str_detect(isscaap, "shrimp") ~ "demersal",
    str_detect(isscaap, "flounders") ~ "demersal",
    str_detect(isscaap, "lobster") ~ "benthic",
    str_detect(isscaap, "crab") ~ "benthic",
    str_detect(common_name, "crab") ~ "benthic",
    str_detect(isscaap, "clams") ~ "benthic", 
    str_detect(isscaap, "mussels") ~ "benthic",
    str_detect(isscaap, "abalones") ~ "benthic",
    str_detect(isscaap, "oysters") ~ "benthic",
    str_detect(common_name, "octopus") ~ "reef associated",
    str_detect(common_name, "cuttlefish") ~ "reef associated",
    str_detect(common_name, "squid") ~ "medium pelagics",
    str_detect(common_name, "shrimp") ~ "demersal",
    str_detect(common_name, "jellyfish") ~ "jellyfish"
  )) %>% # Unmatched scinames represent less than 0.1% of trade
  select(sciname, habitat, method, nceas_group)

# Marine aquaculture
marine_aquaculture_sciname <- prod %>%
  select(sciname, common_name, method, habitat, isscaap = isscaap_group) %>%
  filter(method == "aquaculture", habitat == "marine") %>%
  distinct() %>%
  left_join(nceas_marine_aquaculture %>%
              select(species, nceas_group = aq_group) %>%
              rename(common_name = species) %>%
              unique() %>%
              mutate(common_name = tolower(common_name)),
            by = c("common_name")) %>%
  mutate(isscaap = tolower(isscaap)) %>%
  # Categorize missing scinames based on isscaap group
  mutate(nceas_group = case_when(
    !is.na(nceas_group) ~ nceas_group,
    str_detect(isscaap, "crustaceans") ~ "crustaceans",
    str_detect(isscaap, "lobster") ~ "crustaceans",
    str_detect(isscaap, "crab") ~ "crustaceans",
    str_detect(isscaap, "clams") ~ "bivalves",
    str_detect(isscaap, "mussels") ~ "bivalves",
    str_detect(isscaap, "scallops") ~ "bivalves",
    str_detect(isscaap, "oysters") ~ "bivalves",
    str_detect(isscaap, "coastal fishes") ~ "marine_fish_general",
    str_detect(isscaap, "demersal fishes") ~ "marine_fish_general",
    str_detect(isscaap, "pelagic fishes") ~ "marine_fish_general",
    str_detect(isscaap, "cods") ~ "marine_fish_general",
    str_detect(isscaap, "flounder") ~ "marine_fish_general",
    str_detect(isscaap, "sturgeons") ~ "marine_fish_general",
    str_detect(isscaap, "marine fishes") ~ "marine_fish_general",
    str_detect(isscaap, "salmons") ~ "Salmons",
    str_detect(isscaap, "shrimp") ~ "shrimps_prawns",
  )) %>% 
  select(sciname, habitat, method, nceas_group)

# Inland capture
inland_capture_sciname <- prod %>%
  select(sciname, habitat, method) %>%
  filter(method == "capture", habitat == "inland") %>%
  distinct() %>%
  mutate(nceas_group = "inland capture")

# Inland aquaculture
inland_aquaculture_sciname <- prod %>%
  select(sciname, habitat, method, isscaap = isscaap_group) %>%
  filter(method == "aquaculture", habitat == "inland") %>%
  distinct() %>%
  # For now, use isscaap group
  mutate(nceas_group = case_when(
    str_detect(sciname, "hypophthalmichthys") ~ "hypoph_carp", 
    TRUE ~ isscaap)) %>% 
  mutate(nceas_group = str_replace(nceas_group, 
                                   pattern = "Carps, barbels and other cyprinids",
                                   replacement = "oth_carp")) %>%
  select(sciname, habitat, method, nceas_group)

nceas_groups <- marine_capture_sciname %>%
  bind_rows(marine_aquaculture_sciname) %>%
  bind_rows(inland_capture_sciname) %>%
  bind_rows(inland_aquaculture_sciname)

write.csv(nceas_groups, file.path(outdir, "sciname_habitat_method_metadata.csv"), row.names = FALSE)

#___________________________________________________________________________________________________________________#
# Create commodity metadata
#___________________________________________________________________________________________________________________#
hs_taxa_match <- data.frame(Code = integer(),
                            SciName = character(),
                            Match_category = character(),
                            HS_version = character(),
                            Description = character(),
                            Modification = character())

hs_clade_match <- data.frame(Code = character(),
                             hs_clade = factor(),
                             classification_level = character(),
                             hs_version = character())

for(i in c("96", "02", "07", "12", "17")){
  HS_year_rep <- i
  
  hs_taxa_match_i <- read.csv(file.path(inputsdir, paste("hs-taxa-match_HS", HS_year_rep, ".csv", sep = "")))
    
  hs_clade_match_i <- match_hs_to_clade(hs_taxa_match = hs_taxa_match_i ,
                                      prod_taxa_classification = taxa %>%
                                        rename(CommonName = common_name, SciName = sciname),
                                      match_to_prod = FALSE) %>% 
    # pad HS codes with zeroes
    mutate(Code = as.character(Code)) %>%
    mutate(Code = if_else(str_detect(Code, "^30"), true = str_replace(Code, pattern = "^30", replacement = "030"),
                          if_else(str_detect(Code, "^511"), true = str_replace(Code, pattern = "^511", replacement = "0511"),
                                  false = Code))) %>%
    mutate(hs_version = HS_year_rep)
  
  hs_clade_match <- hs_clade_match %>% 
    bind_rows(hs_clade_match_i)
  
  hs_taxa_match <- hs_taxa_match %>%
    bind_rows(hs_taxa_match_i)
}

hs_clade_match <- hs_clade_match %>%
  rename("hs6" = "Code")

hs_clade_match <- hs_clade_match %>%
  mutate(hs6 = as.integer(hs6),
         hs_version = as.integer(hs_version)) %>%
  rename("code_taxa_level" = "classification_level")

prod_taxa_classification <- taxa %>%
  select(-common_name) %>%
  unique() %>% 
  mutate(
    prod_taxa_level = case_when(
      (str_count(sciname, pattern = " ") == 1) ~ "Species", 
      sciname == Genus ~ "Genus", 
      sciname == Subfamily ~ "Subfamily", 
      sciname == Family ~ "Family", 
      sciname == Order ~ "Order", 
      sciname == Class ~ "Class", 
      sciname == Superclass ~ "Superclass", 
      sciname == Phylum ~ "Phylum", 
      sciname == Kingdom ~ "Kingdom"
    )
  ) %>%
  select(sciname, prod_taxa_level) %>%
  bind_rows(data.frame(
    sciname = c("animalia", "osteichthyes", "actinopteri"),
    prod_taxa_level = c("Kingdom", "Superclass", "Class")
  )) %>%
  distinct()

code_max_resolved_taxa <- hs_taxa_match %>%
  rename(hs6 = Code, sciname = SciName) %>%
  left_join(hs_clade_match %>% mutate(hs_version = paste("HS", hs_version, sep="")),
            by = c("hs6", "HS_version" = "hs_version")) %>%
  left_join(prod_taxa_classification, by = c("sciname")) %>%
  mutate(code_taxa_level_numeric = case_when(
    code_taxa_level == "Species" ~ 1, 
    code_taxa_level == "Genus" ~ 2,
    code_taxa_level == "Subfamily" ~ 3,
    code_taxa_level == "Family" ~ 4,
    code_taxa_level == "Order" ~ 5, 
    code_taxa_level == "Class" ~ 6, 
    code_taxa_level == "Superclass" ~ 7,
    code_taxa_level == "Phylum" ~ 8,
    code_taxa_level == "Kingdom" ~ 9
  )) %>%
  mutate(prod_taxa_level_numeric = case_when(
    prod_taxa_level == "Species" ~ 1, 
    prod_taxa_level == "Genus" ~ 2,
    prod_taxa_level == "Subfamily" ~ 3,
    prod_taxa_level == "Family" ~ 4,
    prod_taxa_level == "Order" ~ 5, 
    prod_taxa_level == "Class" ~ 6,
    prod_taxa_level == "Superclass" ~ 7,
    code_taxa_level == "Phylum" ~ 8,
    prod_taxa_level == "Kingdom" ~ 9
  )) %>%
  mutate(hs_clade = as.character(hs_clade)) %>%
  mutate(sciname_hs_modified = case_when(
    prod_taxa_level_numeric < code_taxa_level_numeric ~ sciname, 
    code_taxa_level_numeric < prod_taxa_level_numeric ~ hs_clade,
    prod_taxa_level_numeric == code_taxa_level_numeric ~ sciname
  )) %>%
  mutate(sciname_hs_modified = ifelse(is.na(sciname_hs_modified), sciname, sciname_hs_modified)) %>%
  # Leave chordata as original names
  mutate(sciname_hs_modified = case_when(
    sciname_hs_modified == "chordata" ~ sciname_hs_modified,
    sciname_hs_modified != "chordata" ~ sciname_hs_modified
  )) %>%
  select("hs_version" = "HS_version", hs6, sciname, sciname_hs_modified) %>%
  mutate(hs_version = as.integer(parse_number(hs_version)),
         hs6 = as.character(hs6))

write.csv(code_max_resolved_taxa, file.path(outdir, "code_max_resolved_taxa.csv"), row.names = FALSE)
