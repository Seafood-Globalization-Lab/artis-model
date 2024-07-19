#' @import dplyr
#' @export
query_synonyms <- function(df, in_query) {
  # INPUTS:
  #       - synonym data frame to be queried (DATA FRAME)
  #       - query string with a potential species synonym (STRING)
  # OUTPUTS:
  #       - results data frame which will either contain (DATA FRAME):
  #                 - 1 row with an accepted name for the species
  #                 - 0 rows because no accepted name has been found
  
  # setting up results data frame
  out_df <- data.frame(
    synonym=as.character(),
    status=as.character(),
    spec_code=as.numeric(),
    syn_code=as.character(),
    taxon_level=as.character()
  )
  
  in_query <- tolower(in_query)
  in_query <- gsub('\\.', '', in_query) # eliminate dots
  in_query <- gsub(',', '', in_query) # eliminates commas
  in_query <- gsub('-', ' ', in_query) # replaces hyphens with spaces
  
  result <- df %>%
    filter(synonym == in_query)
  
  # if there are no results return empty results data frame back
  if (nrow(result) > 0) {
    out_df <- result %>%
      select(accepted_name, spec_code) %>%
      rename(synonym = accepted_name) %>%
      mutate(status = "accepted")
  }
  
  return(out_df)
}
