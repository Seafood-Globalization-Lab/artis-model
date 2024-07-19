#' @export
get_no_solve_countries <- function(snet_dir, artis_run_date_no_dash,
                                   run_env = "aws", s3_bucket_name = "", s3_region = "") {
  
  no_solve_countries <- data.frame()
  
  if (run_env == "aws") {
    
    no_solve_files <- get_bucket_df(
      bucket = s3_bucket_name,
      region = s3_region,
      prefix = snet_dir,
      max = Inf
    ) %>%
      filter(str_detect(Key, pattern="countries-with-no-solve-qp-solution")) %>%
      pull(Key) %>%
      unique()
    
    for (i in 1:length(no_solve_files)) {
      
      curr_no_solve_aws_fp <- no_solve_files[i]
      local_no_solve_fp <- file.path(snet_dir, curr_no_solve_aws_fp)
      
      # Download no solve country files from AWS S3
      save_object(
        object = curr_no_solve_aws_fp,
        bucket = s3_bucket_name,
        region = s3_region,
        file = local_no_solve_fp
      )
      
      curr_no_solve <- read.csv(local_no_solve_fp, header=FALSE, sep=" ")
      
      if (nrow(curr_no_solve) > 0) {
        curr_no_solve <- curr_no_solve %>%
          pivot_longer(colnames(curr_no_solve), names_to="col1", values_to="country_iso3") %>%
          select(country_iso3) %>%
          filter(!is.na(country_iso3)) %>%
          filter(str_detect(country_iso3, "^[A-Z][A-Z][A-Z]$")) # Filter for iso3 code format
        
        curr_year <- str_extract(curr_no_solve_aws_fp, pattern = "_[0-9][0-9][0-9][0-9]_")
        curr_year <- as.numeric(substr(curr_year, 2, str_length(curr_year) - 1))
        
        curr_hs_version <- str_extract(curr_no_solve_aws_fp, pattern = "HS[0-9][0-9]")
        
        curr_no_solve <- curr_no_solve %>%
          mutate(year=curr_year,
                 hs_version=curr_hs_version)
        
        no_solve_countries <- no_solve_countries %>%
          bind_rows(curr_no_solve)
      }
    }
    
  } else {
    hs_dirs <- list.dirs(snet_dir, recursive=FALSE)
    
    for (i in 1:length(hs_dirs)) {
      curr_hs_dir <- hs_dirs[i]
      curr_hs_version <- substring(curr_hs_dir, nchar(curr_hs_dir) - 3, nchar(curr_hs_dir))
      
      year_dirs <- list.dirs(curr_hs_dir, recursive=FALSE)
      
      for (j in 1:length(year_dirs)) {
        curr_year_dir <- year_dirs[j]
        curr_year <- as.numeric(substring(curr_year_dir, nchar(curr_year_dir) - 3, nchar(curr_year_dir)))
        
        filename <- paste(
          artis_run_date_no_dash,
          "analysis-documentation",
          "countries-with-no-solve-qp-solution",
          curr_year,
          curr_hs_version,
          sep="_"
        )
        filename <- paste(filename, ".txt", sep="")
        curr_fp <- file.path(curr_year_dir, filename)
        
        if (file.exists(curr_fp)) {
          curr_no_solve <- read.csv(curr_fp, header=FALSE, sep=" ")
          
          if (nrow(curr_no_solve) > 0) {
            curr_no_solve <- curr_no_solve %>%
              pivot_longer(colnames(curr_no_solve), names_to="col1", values_to="country_iso3") %>%
              select(country_iso3) %>%
              filter(!is.na(country_iso3)) %>%
              filter(str_detect(country_iso3, "^[A-Z][A-Z][A-Z]$")) # Filter for iso3 code format
            
            curr_no_solve <- curr_no_solve %>%
              mutate(year=curr_year,
                     hs_version=curr_hs_version)
            
            no_solve_countries <- no_solve_countries %>%
              bind_rows(curr_no_solve)
          }
          
        }
        
      }
    }
  }
  
  return(no_solve_countries)
}
