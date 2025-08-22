library(tidyverse)

# Load data
data_filepath <- "model_inputs"
hs_taxa_old <- read.csv(file.path(data_filepath, "hs-taxa-match_HS12_fao1_1.csv")) %>%
  select(Code, SciName)
hs_taxa_new <- read.csv(file.path(data_filepath, "hs-taxa-match_HS12_fao2_0.csv")) %>%
  select(Code, SciName)

# Set species lists
species_old <- unique(hs_taxa_old$SciName)
species_new <- unique(hs_taxa_new$SciName)

species_added <- species_new[!(species_new %in% species_old)]
species_removed <- species_old[!(species_old %in% species_new)]
species_common <- union(species_old, species_new)

# Check matches for species in both datasets 
# Unless the matching logic has changed, all species present in the 
# old data should match to the same codes in the new data
match_check_common_scinames <- hs_taxa_old %>%
  filter(SciName %in% species_common) %>%
  mutate(old = 1) %>%
  full_join(hs_taxa_new %>%
              filter(SciName %in% species_common) %>%
              mutate(new = 1),
            by = c("SciName", "Code")) %>%
  mutate(test = old + new) %>%
  filter(test != 2) %>%
  nrow()

if(match_check_common_scinames > 0){
  warning(paste(match_check_common_scinames, "scinames common to the old and new production data matched to different codes"))
}

