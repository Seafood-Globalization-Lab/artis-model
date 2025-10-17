# Set up for stepping through calculate_consumption.R

# Load library
library(qs2)

# Created from local run environment save outputs/snet/hs_version/year
qs2::qs_readm("qa/2025-09-03_workspace_consumption_midpoint_HS12_2017.qs2")

artis = s_net
prod = prod_data_analysis_year
curr_year = analysis_year
curr_hs_version = curr_hs
W_long = W_long 
reweight_W_long = reweight_W_long
X_long = X_long
V1_long = V1_long
V2_long = V2_long
pop = pop
code_max_resolved = code_max_resolved
max_percap_consumption = 100
consumption_threshold = 1e-9
dev_mode = FALSE


# Checks
# FishStat has processed production total of 3,509,096 t

compare_consumption_prod <- complete_consumption %>%
  group_by(year, source_country_iso3c, sciname, habitat, method) %>%
  summarise(consumption_live_t = sum(consumption_live_t)) %>%
  full_join(prod %>%
              group_by(year, country_iso3_alpha, sciname, habitat, method) %>%
              summarise(live_weight_t = sum(live_weight_t)),
            by = c("year", "source_country_iso3c" = "country_iso3_alpha", "sciname", 
                   "habitat", "method")) %>%
  mutate(diff = abs(consumption_live_t - live_weight_t), 
         percent_diff = 100*(consumption_live_t - live_weight_t)/live_weight_t)

large_diff <- compare_consumption_prod %>% 
  filter(abs(diff) > 10)


primary_fm_sp <- c("engraulis ringens",
                   "brevoortia patronus",
                   "brevoortia tyrannus",
                   "engraulis capensis",
                   "cetengraulis mysticetus",
                   "trisopterus esmarkii",
                   "sardinella brasiliensis",
                   "anchoa nasus",
                   "ammodytidae",
                   "capros aper",
                   "caproidae",
                   "brevoortia aurea",
                   "lepophidium brevibarbe",
                   "ogcocephalus")


tmp2 <- complete_consumption %>%
  filter(consumption_source == "domestic", 
         sciname %in% primary_fm_sp)


fm_prod <- complete_consumption %>% 
  filter(end_use == "fishmeal") %>%
  group_by(source_country_iso3c) %>%
  summarise(consumption_live_t = sum(consumption_live_t))
