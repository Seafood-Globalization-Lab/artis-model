#' @export
collect_data <- function(artis_dir,
                         file_pattern) {
  # Get all trade data tables
  df_files <- list.files(path = artis_dir,
                         pattern = file_pattern,
                         recursive = TRUE,
                         full.names = TRUE,
                         include.dirs = TRUE)
  
  df <- data.frame()
  
  for (i in 1:length(df_files)) {
    print(paste("adding", df_files[i]))
    curr_df <- read.csv(df_files[i])
    df <- df %>%
      bind_rows(curr_df)
  }
  
  print(paste("Done...collecting all files with pattern", file_pattern))
  
  return(df)
}