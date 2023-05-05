
packdir <- "/project/ARTIS/Package"
setwd(packdir) # note: If running on zorro need to set directory to packdir before #devtools::install()

# Build and install artis package
# devtools::install()

# Load Packages
library(tidyverse, lib.loc = "/home/rahulab/R/x86_64-pc-linux-gnu-library/3.6/")

# Directories
artis_run_path <-  "/project/ARTIS/ARTIS/snet_20221129_NEW"
outdir <- "/project/ARTIS/ARTIS/snet_20230331"
outdir_building_blocks <- file.path(outdir,
                                    "building_blocks")
outdir_ts <- file.path(outdir,
                       "timeseries")


# Function that list all snet files for a particular pattern
get_snet_files <- function(dir_path, file_pattern) {
  
  snet_files <- list()
  
  hs_versions <- list.dirs(dir_path, recursive=FALSE)
  
  # For each HS version
  for (i in 1:length(hs_versions)) {
    curr_hs_version <- hs_versions[i]
    curr_snet_files <- list.files(curr_hs_version, pattern=file_pattern, recursive=FALSE, full.names=TRUE)
    snet_files <- append(snet_files, curr_snet_files)
  }
  
  return(snet_files)
}

# Function that compiles all of artis snet given a file pattern
compile_artis <- function(in_path, file_pattern, out_path) {
  
  snet <- data.frame()
  
  snet_files <- get_snet_files(in_path, file_pattern)
  
  for (i in 1:length(snet_files)) {
    curr_snet <- read.csv(snet_files[[i]])
    snet <- snet %>%
      bind_rows(curr_snet)
  }
  
  write.csv(snet, out_path, row.names=FALSE)
  print(paste(out_path, "DONE"))
}

build_artis_timeseries <- function(artis_path, out_path) {
  
  artis <- read.csv(artis_path)
  artis <- artis %>%
    group_by(year, hs_version) %>%
    summarize(product_weight_t = sum(product_weight_t))
  
  write.csv(artis, out_path, row.names=FALSE)
  print(paste(out_path, "DONE"))
}

print("Compiling ARTIS")
compile_artis(file.path(artis_run_path, 'snet'),
              "^midpoint_artis_ts", 
              file.path(outdir_building_blocks, "midpoint_artis_ts.csv"))
compile_artis(file.path(artis_run_path, 'snet'),
              "^midpoint_artis_species_ts", 
              file.path(outdir_building_blocks, "midpoint_artis_species_ts.csv"))
compile_artis(file.path(artis_run_path, 'snet'),
              "^midpoint_artis_habitat_prod_ts",
              file.path(outdir_building_blocks, "midpoint_artis_habitat_prod_ts.csv"))

#-------------------------------------------------------------------------------
# Consumption Files
compile_artis(file.path(artis_run_path, 'snet'),
              "^summary_consumption_midpoint",
              file.path(outdir_building_blocks, "summary_consumption_midpoint.csv"))

compile_artis(file.path(artis_run_path, 'snet'),
              "^domestic_consumption_midpoint",
              file.path(outdir_building_blocks, "domestic_consumption_midpoint.csv"))

compile_artis(file.path(artis_run_path, 'snet'),
              "^foreign_consumption_midpoint",
              file.path(outdir_building_blocks, "foreign_consumption_midpoint.csv"))

compile_artis(file.path(artis_run_path, 'snet'),
              "^summary_consumption_max",
              file.path(outdir_building_blocks, "summary_consumption_max.csv"))

compile_artis(file.path(artis_run_path, 'snet'),
              "^domestic_consumption_max",
              file.path(outdir_building_blocks, "domestic_consumption_max.csv"))

compile_artis(file.path(artis_run_path, 'snet'),
              "^foreign_consumption_max",
              file.path(outdir_building_blocks, "foreign_consumption_max.csv"))

compile_artis(file.path(artis_run_path, 'snet'),
              "^summary_consumption_min",
              file.path(outdir_building_blocks, "summary_consumption_min.csv"))

compile_artis(file.path(artis_run_path, 'snet'),
              "^domestic_consumption_min",
              file.path(outdir_building_blocks, "domestic_consumption_min.csv"))

compile_artis(file.path(artis_run_path, 'snet'),
              "^foreign_consumption_min",
              file.path(outdir_building_blocks, "foreign_consumption_min.csv"))

# Build custom time series with one HS version per year for min, mid and max
# ARTIS RUN File Path
datadir <- "/project/ARTIS/ARTIS/snet_20230331/building_blocks"
outdir <- "/project/ARTIS/ARTIS/snet_20230331/custom_ts"

prep_custom_ts <- function(df) {
  
  df <- df %>%
    mutate(hs_version = as.character(hs_version)) %>%
    mutate(hs_version = case_when(
      str_length(hs_version) == 1 ~ paste("0", hs_version, sep=""),
      TRUE ~ hs_version)) %>%
    mutate(hs_version = paste("HS", hs_version, sep="")) %>%
    filter(
      # Use HS96 from 1996-2003 (inclusive)
      ((hs_version == "HS96") & (year <= 2003)) |
        # Use HS02 from 2004-2009 (inclusive)
        ((hs_version == "HS02") & (year >= 2004 & year <= 2009)) |
        # Use HS07 from 2010-2012 (inclusive)
        ((hs_version == "HS07") & (year >= 2010 & year <= 2012)) |
        # Use HS12 from 2013-2019 (inclusive)
        ((hs_version == "HS12") & (year >= 2013 & year <= 2020))
    )
  
  return(df)
}

# max
max <- read.csv(file.path(datadir, "max_artis_ts.csv"))
max <- prep_custom_ts(max)
write.csv(max, file.path(outdir, "max_custom_ts.csv"), row.names = FALSE)

# mid
mid <- read.csv(file.path(datadir, "midpoint_artis_ts.csv"))
mid <- prep_custom_ts(mid)
write.csv(mid, file.path(outdir, "mid_custom_ts.csv"), row.names = FALSE)

# min
min <- read.csv(file.path(datadir, "min_artis_ts.csv"))
min <- prep_custom_ts(min)
write.csv(min, file.path(outdir, "min_custom_ts.csv"), row.names = FALSE)
