# Sensitivity of fishmeal_min_threshold_sp
param_range <- seq(0, 5, 0.1)
n_species_df <- data.frame(param = numeric(length(param_range)), 
                           n_species = numeric(length(param_range)))

for(i in 1:length(param_range)){
  fmfo_species <- get_fmfo_species(
    sau_fp = file.path(outdir, 'standardized_sau_prod_more_cols.csv'),
    fishmeal_min_threshold_sp = param_range[i],
    fishmeal_min_threshold_global = 0,
    fishmeal_primary_threshold = 0)
    
    n_species_df$param[i] <- param_range[i]
    n_species_df$n_species[i] <- nrow(fmfo_species)
}

n_species_df %>%
  ggplot(aes(x = param, y = n_species)) +
  geom_point() +
  theme_minimal()


# Sensitivity of fishmeal_primary_threshold
param_range <- seq(50, 95, 1)
n_species_df <- data.frame(param = numeric(length(param_range)), 
                           n_species = numeric(length(param_range)))

for(i in 1:length(param_range)){
  fmfo_species <- get_fmfo_species(
    sau_fp = file.path(outdir, 'standardized_sau_prod_more_cols.csv'),
    fishmeal_min_threshold_sp = 0,
    fishmeal_min_threshold_global = 0,
    fishmeal_primary_threshold = param_range[i])
  
  n_species_df$param[i] <- param_range[i]
  n_species_df$n_species[i] <- fmfo_species %>%
    filter(primary_fishmeal == 1) %>%
    nrow()
}

n_species_df %>%
  ggplot(aes(x = param, y = n_species)) +
  geom_point() +
  theme_minimal()