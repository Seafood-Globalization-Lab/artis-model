#' @export

combine_snet <- function(year_folders, hs_version, file.date, snet_type = "NA", outdir) {
  
  snet <- data.frame()
  summary_consumption_raw <- data.frame()
  complete_consumption_raw <- data.frame()
  summary_consumption_100kg <- data.frame()
  complete_consumption_100kg <- data.frame()
  
  # Go through all years within HS version and read in appropriate snet created
  for (i in 1:length(year_folders)) {
    curr_folder <- year_folders[i]
    curr_year <- substring(curr_folder, nchar(curr_folder) - 3, nchar(curr_folder))
    # Will read snet version dictated by snet_type, ie max, midpoint, min
    curr_fp <- file.path(curr_folder, 
                         paste(file.date, "S-net_raw", snet_type, curr_year, paste(hs_version, ".csv", sep=""), sep="_"))
    
    # Check if file exists before adding to snet
    if (file.exists(curr_fp)) {
      
      print(curr_fp)
      
      # Reading in and adding the year and hs version to snet
      curr_snet <- read.csv(curr_fp)
      curr_snet <- curr_snet %>% 
        mutate(year = as.numeric(curr_year),
               hs_version = hs_version)
      
      # Combining snets together
      snet <- snet %>%
        bind_rows(curr_snet)
    } else {
      print(paste(curr_fp, "DOESN'T EXIST"))
    }
    
    #---------------------------------------------------------------------------
    # Consumption files
    curr_summary_consumption_raw <- read.csv(
      file.path(
        curr_folder,
        paste("summary_consumption_raw_", snet_type, ".csv", sep = "")
      )
    ) %>%
      mutate(hs_version = hs_version)
    
    curr_complete_consumption_raw <- read.csv(
      file.path(
        curr_folder,
        paste("complete_consumption_raw_", snet_type, ".csv", sep = "")
      )
    ) %>%
      mutate(hs_version = hs_version)
    
    curr_summary_consumption_100kg <- read.csv(
      file.path(
        curr_folder,
        paste("summary_consumption_", snet_type, ".csv", sep = "")
      )
    ) %>%
      mutate(hs_version = hs_version)
    
    curr_complete_consumption_100kg <- read.csv(
      file.path(
        curr_folder,
        paste("complete_consumption_100kg_per_capita_", snet_type, ".csv", sep = "")
      )
    ) %>%
      mutate(hs_version = hs_version)
    
    summary_consumption_raw <- summary_consumption_raw %>%
      bind_rows(curr_summary_consumption_raw)
    
    complete_consumption_raw <- complete_consumption_raw %>%
      bind_rows(curr_complete_consumption_raw)
    
    summary_consumption_100kg <- summary_consumption_100kg %>%
      bind_rows(curr_summary_consumption_100kg)
    
    complete_consumption_100kg <- complete_consumption_100kg %>%
      bind_rows(curr_complete_consumption_100kg)
  }
  
  write.csv(
    summary_consumption_raw,
    file.path(outdir, paste("summary_consumption_raw_", snet_type, "_", hs_version, ".csv", sep = "")),
    row.names = FALSE
  )
  
  write.csv(
    complete_consumption_raw,
    file.path(outdir, paste("complete_consumption_raw_", snet_type, "_", hs_version, ".csv", sep = "")),
    row.names = FALSE
  )
  
  write.csv(
    summary_consumption_100kg,
    file.path(outdir, paste("summary_consumption_", snet_type, "_", hs_version, ".csv", sep = "")),
    row.names = FALSE
  )
  
  write.csv(
    complete_consumption_100kg,
    file.path(outdir, paste("complete_consumption_100kg_per_capita_", snet_type, "_", hs_version, ".csv", sep = "")),
    row.names = FALSE
  )
  
  # Fix habitat info
  
  # Look across dataset to find habitat data that can be used to fill in "unknowns"
  unknown_taxa <- snet %>% 
    filter(habitat == "unknown") %>% 
    select(sciname, habitat) %>% 
    distinct()
  
  unknown_lookup <- snet %>%
    filter(sciname %in% unknown_taxa$sciname) %>%
    select(sciname, habitat) %>%
    distinct() %>%
    filter(habitat != "unknown") %>%
    # Remove taxa that don't have unique habitat info (n == 1) when looking across the data (e.g., oncorhynchus nerka can be inland or marine)
    group_by(sciname) %>%
    mutate(n = n()) %>%
    filter(n == 1) %>%
    select(sciname, habitat_lookup = habitat)
  
  # Join lookup habitat with snet
  snet <- snet %>%
    left_join(unknown_lookup, by = "sciname") %>%
    # If habitat is unknown, and there is a non-NA value in the habitat lookup column, use this as the new habitat data
    mutate(habitat = case_when(habitat == "unknown" & is.na(habitat_lookup) == FALSE ~ habitat_lookup,
                                   TRUE ~ habitat)) %>%
    select(-habitat_lookup)
    
  
  # Writing out Complete Snet for a specific HS version
  write.csv(snet, file.path(outdir, paste(snet_type, "_artis_ts_", hs_version, ".csv", sep = "")), row.names = FALSE)
  
  # Writing out total by species, habitat and production method
  snet_species <- snet %>%
    group_by(year, importer_iso3c, exporter_iso3c, sciname, habitat, method) %>%
    summarise(product_weight_t = sum(product_weight_t, na.rm = TRUE))#,
              # live_weight_t = sum(live_weight_t, na.rm = TRUE))

  write.csv(snet_species, file.path(outdir, paste(snet_type, "_artis_species_ts_", hs_version, ".csv", sep = "")), row.names = FALSE)

  # Writing out total by habitat and production method
  snet_habitat_prod <- snet %>%
    group_by(year, importer_iso3c, exporter_iso3c, habitat, method) %>%
    summarise(product_weight_t = sum(product_weight_t, na.rm = TRUE))#,
              # live_weight_t = sum(live_weight_t, na.rm = TRUE))

  write.csv(snet_habitat_prod, file.path(outdir, paste(snet_type, "_artis_habitat_prod_ts_", hs_version, ".csv", sep = "")), row.names = FALSE)

}
