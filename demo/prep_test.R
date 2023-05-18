
library(tidyverse)

datadir <- "demo/model_inputs_complete"
outdir <- "demo/model_inputs"

prod_data <- read.csv(file.path(datadir, "standardized_fao_prod.csv"))
prod_taxa_classification <- read.csv(file.path(datadir, "clean_fao_taxa.csv"))
baci_data <- read.csv(file.path(datadir, "standardized_baci_seafood_hs12_y2018.csv"))
hs_taxa_match <- read.csv(file.path(datadir, "hs-taxa-match_HS12.csv"))
hs_hs_match <- read.csv(file.path(datadir, "hs-hs-match_HS12.csv"))
hs_taxa_CF_match <- read.csv(file.path(datadir, "hs-taxa-CF_strict-match_HS12.csv"))

shrimp_codes <- c("030617", "160529", "160521", "030627", "030616", "030626")
shrimp_codes <- as.numeric(shrimp_codes)
shrimp_scinames <- read.csv("demo/sciname_shrimps_prawns.csv") %>%
  pull(sciname)

prod_data_shrimp <- prod_data %>%
  filter(year == 2018) %>%
  filter(SciName %in% shrimp_scinames)

write.csv(prod_data_shrimp, file.path(outdir, "standardized_fao_prod.csv"), row.names = FALSE)

prod_taxa_shrimp <- prod_taxa_classification %>%
  filter(SciName %in% shrimp_scinames)

write.csv(prod_taxa_shrimp, file.path(outdir, "clean_fao_taxa.csv"), row.names = FALSE)

baci_data_shrimp <- baci_data %>%
  filter(year == 2018) %>%
  filter(hs6 %in% shrimp_codes)

write.csv(baci_data_shrimp, file.path(outdir, "standardized_baci_seafood_hs12_y2018.csv"), row.names = FALSE)

hs_taxa_shrimp <- hs_taxa_match %>%
  filter(Code %in% shrimp_codes,
         SciName %in% shrimp_scinames)

write.csv(hs_taxa_shrimp, file.path(outdir, "hs-taxa-match_HS12.csv"), row.names = FALSE)

hs_hs_shrimp <- hs_hs_match %>%
  filter(Code_pre %in% shrimp_codes &
           Code_post %in% shrimp_codes)

write.csv(hs_hs_shrimp, file.path(outdir, "hs-hs-match_HS12.csv"), row.names = FALSE)

hs_taxa_CF_shrimp <- hs_taxa_CF_match %>%
  filter(Taxa %in% shrimp_scinames,
         Code %in% shrimp_codes)

write.csv(hs_taxa_CF_shrimp, file.path(outdir, "hs-taxa-CF_strict-match_HS12.csv"), row.names = FALSE)

write.csv(clean_pop, file.path(outdir, "fao_annual_pop.csv"), row.names = FALSE)


