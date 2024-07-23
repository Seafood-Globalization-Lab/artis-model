#' @export
collect_data <- function(artis_dir,
                         file_pattern,
                         run_env = "aws",
                         s3_bucket_name = "", s3_region = "") {
  
  # Get list of AWS files to download
  if (run_env == "aws") {
    df_files <- get_bucket_df(
      bucket = s3_bucket_name,
      region = s3_region,
      prefix = artis_dir,
      max = Inf
    ) %>%
      filter(str_detect(Key, pattern=file_pattern)) %>%
      pull(Key) %>%
      unique()
  } else {
    # Get all trade data tables
    df_files <- list.files(path = artis_dir,
                           pattern = file_pattern,
                           recursive = TRUE,
                           full.names = TRUE,
                           include.dirs = TRUE)
  }
  
  
  
  df <- data.frame()
  
  for (i in 1:length(df_files)) {
    if (run_env == "aws") {
      save_object(
        object = df_files[i],
        bucket = s3_bucket_name,
        file = df_files[i]
      )
    }
    
    print(paste("adding", df_files[i]))
    curr_df <- read.csv(df_files[i])
    df <- df %>%
      bind_rows(curr_df)
    
    # delete file from local server to preserve space
    file.remove(df_files[i])
    gc()
  }
  
  print(paste("Done...collecting all files with pattern", file_pattern))
  
  return(df)
}