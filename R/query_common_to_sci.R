#' @import dplyr
#' @export
query_common_to_sci <- function(df, in_query) {
  # INPUTS:
  #       - common to sciname data frame to be queried (DATA FRAME)
  #       - query string with a potential species synonym (STRING)
  # OUTPUTS:
  #       - results data frame which will either contain (DATA FRAME):
  #                 - 1 row with an accepted name for the species
  #                 - 0 rows because no accepted name has been found
  
  # standardize in_query
  in_query <- tolower(in_query)
  
  # query common name to sciname dataset
  query_results <- df %>%
    filter(CommonName == in_query)
  
  return(query_results)
}