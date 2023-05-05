#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom readxl read_excel
#' @import stringr
#' @export
rebuild_fao_2022_dat <- function(datadir, filename){
  unzip_folder <- file.path(datadir, str_remove(filename, ".zip"))
  # Test if file was already unzipped
  if (dir.exists(unzip_folder)==FALSE){
    unzip(zipfile = file.path(datadir, filename), exdir = unzip_folder, overwrite = TRUE)
  }
  
  fish_files <- list.files(unzip_folder)
  
  # the DSD.xlsx file explains the data structure of time series
  # each row gives info for how this time series column should be merged with a code list (CL) file
  ds_file <- fish_files[grep("DSD", fish_files)]
  
  # skip removes title row
  ds <- read_excel(file.path(unzip_folder, ds_file), skip=1)
  
  # Manually correct different parts of the data structure file:
  ds <- ds %>%
    # Manually correct data structure file: filename CL_FI_UNIT is actually FSJ_UNIT
    mutate(Codelist_id = if_else(Codelist_id == "CL_FI_UNIT", true = "FSJ_UNIT", false = Codelist_id)) %>%
    # Change "Measure" to "Unit_Code" and "Staus" to "Symbol" to match 2020 version - i.e. so that rest of ARTIS code (developed in 2020) works
    mutate(Concept_id = if_else(Concept_id == "MEASURE", true = "UNIT_CODE", false = Concept_id)) %>%
    mutate(Concept_id = if_else(Concept_id == "STAUS", true = "SYMBOL", false = Concept_id)) %>%
    # Create a simplifed version of concept_id names - remove everything after "." and "_"
    mutate(Concept_id_simple = Concept_id) %>%
    mutate(Concept_id_simple = str_replace(Concept_id_simple, pattern = "\\..*", replacement = "")) %>%
    mutate(Concept_id_simple = str_replace(Concept_id_simple, pattern = "\\_.*", replacement = "")) %>%
    # Convert all relevant columns to lowercase to avoid errors in matching upper/lowercase
    mutate(Concept_id_simple = tolower(Concept_id_simple))
  
  # Multiple CL files have the following column names in common: "Identifier" and "Code"
  # Which means after merge, below, you get "Identifier.x" and "Identifier.y", etc.
  # To disambiguate, Append Codelist with a simplified version of Concept_id
  code_ids_to_change<-ds$Codelist_Code_id[ds$Codelist_Code_id == "CODE" & is.na(ds$Codelist_Code_id)==FALSE]
  concept_ids_to_append<-ds$Concept_id_simple[ds$Codelist_Code_id == "CODE" & is.na(ds$Codelist_Code_id)==FALSE]
  new_code_ids <- paste(concept_ids_to_append, code_ids_to_change, sep = "_")
  ds$Codelist_Code_id[ds$Codelist_Code_id == "CODE" & is.na(ds$Codelist_Code_id)==FALSE]<-new_code_ids
  ds$Codelist_Code_id <- tolower(ds$Codelist_Code_id)
  
  # read in time series.csv
  time_file <- fish_files[grep("Quantity", fish_files)] # Was labeled as "TS" for time series in 2020 version
  time_series <- read.csv(file.path(unzip_folder, time_file))
  
  
  
  # IMPORTANT: row ORDER (ABCDEF) in DSD file should match columns ABCDEF in time series for looping to work below
  # Change "Measure" to "Unit" and "Staus" to "Symbol" to match 2020 version
  time_series_join <- time_series %>%
    rename("UNIT_CODE" = "MEASURE",
           "SYMBOL" = "STATUS") %>%
    # REORDER to match order of DSD rows
    select(all_of(ds$Concept_id))
  names(time_series_join) <- tolower(names(time_series_join))
  
  
  for (i in 1:nrow(ds)) {
    # TRUE/FALSE: is there a filename listed in Codelist_id?
    if (!is.na(ds$Codelist_id[i])) {
      # Use ds file to generate path_to_cl individually
      code_file_i <- fish_files[str_detect(string = fish_files, pattern = ds$Codelist_id[i])]
      cl_i <- read.csv(file.path(unzip_folder, code_file_i), check.names = FALSE) # check.names = FALSE to prevent R from adding "X" in front of column names that begin with numeric (e.g., 2020 version had column "3Alpha_Code")
      names(cl_i) <- tolower(names(cl_i))
      
      # Many CL files have "Name" as a column, also Name_En, Name_Fr, Name_Es, etc
      # Also, "Identifier", "Major Group", and "Code" are common across some CL files
      # To disambiguate, append "Concept_id_simple" from DS file to all columns in CL that contain these terms
      # Exception to this is if name appears in ds$Codelist_code_id (i.e., keep UN_Code as is... don't change this to country_code_un)
      concept_names <- paste(ds$Concept_id_simple[i], names(cl_i)[grepl("name|major_group|identifier|code|description", names(cl_i)) & names(cl_i) %in% ds$Codelist_Code_id == FALSE], sep = "_")
      names(cl_i)[grepl("name|major_group|identifier|code|description", names(cl_i)) & names(cl_i) %in% ds$Codelist_Code_id == FALSE] <- concept_names
      
      merge_col <- ds$Codelist_Code_id[i]
      # If needed, convert to from factor to character
      if (is.factor(cl_i[[merge_col]])){
        cl_i[[merge_col]]<-as.character(cl_i[[merge_col]])
        time_series_join[[names(time_series_join)[i]]]<-as.character(time_series_join[[names(time_series_join)[i]]])
      }
      
      # Merge by column name
      # Note: the following code does not work: #time_series_join<-left_join(time_series, cl_i, by = c(names(time_series)[i] = merge_col))
      # To make the argument "by" dynamic for each loop, it should be formatted as a named character as shown below
      firstname <- names(time_series_join)[i]
      join_cols <- merge_col
      names(join_cols) <- firstname
      time_series_join <- left_join(time_series_join, cl_i, by = join_cols)
      
      # Convert back to factor
      if (is.character(time_series_join[[names(time_series_join)[i]]])){
        time_series_join[[names(time_series_join)[i]]]<-as.factor(time_series_join[[names(time_series_join)[i]]])
      }
    }
  } # end for i in nrow(ds)
  
  # Clean up column names to match ARTIS code developed for 2020 data
  time_series_join <- time_series_join %>%
    rename(country = country.un_code,
           species_major_group = species_major_group_en,
           isscaap_group = isscaap_group_en,
           quantity = value,
           year = period,
           # Standardize production and habitat columns for function: add_prod_source.R
           prod_method = production_source_det.code,
           habitat = inlandmarine_group_en)
}