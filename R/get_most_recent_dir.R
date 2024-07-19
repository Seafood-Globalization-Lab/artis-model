#' @export
get_most_recent_dir <- function(dir_path, dir_pattern) {
  
  # Get a list of all directories that follow dir_pattern within the dir_path directory
  recent_dir <- data.frame(directory = as.character(list.dirs(path = dir_path, full.names = TRUE))) %>%
    filter(str_detect(directory, pattern = dir_pattern)) %>%
    # Identify dates for all directories
    mutate(dir_date = as.numeric(substr(directory, str_length(directory) - 7, str_length(directory)))) %>%
    # Only retain most recent directory
    filter(dir_date == max(dir_date))
  
  return(recent_dir)
}