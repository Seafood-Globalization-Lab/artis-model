# R packages needed
library(artis)
library(data.table)
library(magrittr)
library(Matrix)
library(parallel)
library(reticulate)
library(slam)
library(tidyverse)
library(doParallel)
library(aws.s3)

# Variables needed
artis = s_net
prod = prod_data_analysis_year
curr_year = analysis_year
curr_hs_version = curr_hs
W_long = W_long
X_long = X_long
pop = pop
code_max_resolved = code_max_resolved
max_percap_consumption = 100
consumption_threshold = 1e-9

tmp <- consumption_domestic %>%
  filter(hs6 == 999999) %>%
  group_by(iso3c, sciname) %>%
  summarise(consumption_t = sum(consumption_t))

W_long_check <- W_long %>%
  group_by(exporter_iso3c, hs6_original) %>%
  summarise(prop_sum = sum(estimated_W))

tmp <- processed_imports %>%
  select(importer_iso3c, hs6_original=hs6, hs6_processed, 
         prop_processed_to_original_product) %>%
  distinct()

tmp %>% 
  group_by(importer_iso3c, hs6_processed) %>%
  summarise(prop_processed_to_original_product = sum(prop_processed_to_original_product)) %>%
  arrange(desc(-prop_processed_to_original_product))



# Check how disaggregate_foreign_consumption adds up


tmp4 <- complete_consumption %>% 
  group_by(source_country_iso3c, sciname, habitat, method) %>% 
  summarise(consumption_live_weight_total = sum(consumption_live_t)) %>% 
  full_join(prod %>% 
              group_by(country_iso3_alpha, sciname, habitat, method) %>% 
              summarise(live_weight_t = sum(live_weight_t)), 
            by = c("source_country_iso3c" = "country_iso3_alpha", "sciname", "habitat", "method")) %>% 
  mutate(diff = consumption_live_weight_total - live_weight_t)

tmp5 <- complete_consumption %>% 
  filter(!is.na(exporter_iso3c)) %>% 
  group_by(exporter_iso3c, sciname, habitat, method) %>% 
  summarise(total_live_weight_t = sum(consumption_live_t)) %>% 
  left_join(artis %>% 
              group_by(exporter_iso3c, sciname, habitat, method) %>% 
              summarise(artis_total_live_weight_t = sum(live_weight_t)),
            by = c("exporter_iso3c", "sciname", "habitat", "method")) %>% 
  mutate(diff = total_live_weight_t - artis_total_live_weight_t)


# Check product weights add back up in disagregate_foreign_consumption_all
tmp <- disagregate_foreign_consumption_all %>% 
  group_by(importer_iso3c, hs6_original) %>%
  summarise(foreign_consumption_product_t_2 = sum(foreign_consumption_product_t)) %>%
  left_join(disagregate_foreign_consumption %>%
              # note import props are in the form of the original hs6 product and therefore
              # foreign consumption needs to be aggregated back to importer and hs6 original
              group_by(importer_iso3c, hs6_original) %>%
              summarize(foreign_consumption_product_t_1 = sum(foreign_consumption_original_product_t)),
            by = c("importer_iso3c", "hs6_original")) %>%
  mutate(diff = foreign_consumption_product_t_2-foreign_consumption_product_t_1)
# YES - THESE ADD BACK UP CORRECTLY

# Test case: explore flows of RUS gadus chalcogrammus
tmp <- complete_consumption %>% 
  filter(source_country_iso3c == "RUS", sciname == "gadus chalcogrammus")

# QUESTION: Does domestic consumption ever exceed production? 
# ANSWER: No. Suggests this is a problem with foreign
tmp <- complete_consumption %>% 
  filter(source_country_iso3c == consumer_iso3c, is.na(exporter_iso3c)) %>%
  group_by(source_country_iso3c, sciname, habitat, method) %>%
  summarise(consumption_live_weight_total = sum(consumption_live_t)) %>% 
  full_join(prod %>% 
              group_by(country_iso3_alpha, sciname, habitat, method) %>% 
              summarise(live_weight_t = sum(live_weight_t)), 
            by = c("source_country_iso3c" = "country_iso3_alpha", "sciname", "habitat", "method")) %>% 
  mutate(diff = consumption_live_weight_total - live_weight_t)

# QUESTION: Does foreign consumption alone ever exceed production from the source country? 
# ANSWER: Yes
tmp <- complete_consumption %>% 
  filter(consumption_type == "foreign") %>%
  group_by(source_country_iso3c, sciname, habitat, method) %>%
  summarise(consumption_live_weight_total = sum(consumption_live_t)) %>% 
  full_join(prod %>% 
              group_by(country_iso3_alpha, sciname, habitat, method) %>% 
              summarise(live_weight_t = sum(live_weight_t)), 
            by = c("source_country_iso3c" = "country_iso3_alpha", "sciname", "habitat", "method")) %>% 
  mutate(diff = consumption_live_weight_total - live_weight_t)


# The problem appears related to foreign consumption
# It does not seem likely related to live weight conversion factors since
# the same species swings in both ways e.g., too much Russian pollock consumed
# AND too little US pollock consumed. 
# 030475 = pollock fillets
# 030367 = frozen pollock

# Is this a problem with products moving through intermediate countries and how
# import_prop is calculated? 

tmp <- consumption_foreign %>%
  group_by(importer_iso3c, hs6_processed) %>%
  summarise(foreign_consumption_product_t = sum(foreign_consumption_product_t))

tmp2 <- disagregate_foreign_consumption_all %>% 
  group_by(importer_iso3c, hs6_processed) %>%
  summarise(foreign_consumption_product_t = sum(foreign_consumption_product_t))

tmp3 <- tmp %>%
  full_join(tmp2, by = c("importer_iso3c", "hs6_processed")) %>%
  mutate(diff = foreign_consumption_product_t.x - foreign_consumption_product_t.y)


# Confirm import_prop production adds back up
tmp <- artis_import_props %>%
  group_by(source_country_iso3c, sciname, habitat, method) %>%
  summarise(consumption_live_weight_total = sum(live_weight_t)) %>%
  left_join(prod %>% 
              group_by(country_iso3_alpha, sciname, habitat, method) %>% 
              summarise(live_weight_t = sum(live_weight_t)), 
            by = c("source_country_iso3c" = "country_iso3_alpha", "sciname", "habitat", "method")) %>% 
  mutate(diff = consumption_live_weight_total - live_weight_t)

# THERE IS A PROBLEM WITH IMPORT PROPS!!!!


tmp <- artis %>%
  filter(dom_source == "domestic") %>%
  group_by(source_country_iso3c, sciname, habitat, method) %>%
  summarise(trade_live_weight_t = sum(live_weight_t)) %>%
  left_join(prod %>% 
              group_by(country_iso3_alpha, sciname, habitat, method) %>% 
              summarise(live_weight_t = sum(live_weight_t)), 
            by = c("source_country_iso3c" = "country_iso3_alpha", "sciname", "habitat", "method")) %>% 
  mutate(diff = trade_live_weight_t - live_weight_t)
  



# NOTES
# CHN consumed 201470 t product weight of 30475 that came from 30367
