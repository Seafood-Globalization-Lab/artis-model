#' @export
get_fmfo_species <- function(sau_fp,
                             fishmeal_min_threshold_sp = 1,
                             fishmeal_min_threshold_global = 0.5,
                             fishmeal_primary_threshold = 75) {
  
  # fishmeal_min_threshold default is 1% of production going to FM: 
  #this defines the species allowed to go into FM
  
  # fishmeal_primary_threshold default is min of 75% of production is going into FM : 
  # this defines the species that are favored to go into FM
  
  #-----------------------------------------------------------------------------
  # Production Data
  sau <- read.csv(sau_fp)
  
  #-----------------------------------------------------------------------------
  # Get fmfo list based on percent produced (SAU)
  sau_grouped <- sau %>%
    group_by(SciName, end_use) %>%
    summarize(quantity = sum(quantity, na.rm = TRUE)) %>%
    ungroup() %>% 
    group_by(SciName) %>%
    # Sciname total quantity across all end_uses - assigned to each row of a sciname
    mutate(total = sum(quantity, na.rm = TRUE)) %>%
    ungroup() %>% 
    filter(end_use == "Fishmeal and fish oil") %>% 
    # quantity is fishmeal only quantity
           # percent_sp is percent of species production going into FM
    mutate(percent_sp = 100 * quantity / total,
           # percent_global is percent FM prod by species of all global FM prod
           percent_global = 100 * quantity / sum(quantity)) %>% 
    arrange(desc(percent_global)) %>% 
    mutate(percent_cumulative = cumsum(percent_global))
  
  fmfo_species <- sau_grouped %>%
    filter(percent_sp > fishmeal_min_threshold_sp | percent_global > fishmeal_min_threshold_global) %>% 
    mutate(primary_fishmeal = case_when(
      percent_sp >= fishmeal_primary_threshold ~ 1,
      TRUE ~ 0))
  
  fmfo_species_output <- fmfo_species %>% 
    select(SciName, primary_fishmeal)

  return(fmfo_species_output)
}


