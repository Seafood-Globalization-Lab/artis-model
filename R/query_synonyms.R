#' @import dplyr
#' @export
query_synonyms <- function(df, in_query) {
  # DESCRIPTION
  #       - 
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
  
  # get spec code for the query synonynm
  query_row <- df %>%
    filter(tolower(synonym) == in_query)
  
  # if there are no results return empty results data frame back
  if (nrow(query_row) > 0) {
    # if result include an "accepted" name, filter to just that row (e.g., try "salmo trutta")
    if ("accepted" %in% query_row$status){
      query_row <- query_row %>% 
        filter(status == "accepted")
    }
    
    # Should now be a single row - Gets query spec code
    query_spec_code <- unique(query_row$spec_code)
    
    # filters data frame down to only include 1 row with accepted species name
    out_df <- df %>%
      filter(status == 'accepted' & (spec_code == query_spec_code) & (tolower(taxon_level) == 'species'))
    
    # check if there are multiple accepted names for species
    if (nrow(out_df) > 1) {
      
      out_df <- out_df %>%
        filter(syn_code == max(syn_code))
    }
  }
  
  return(out_df)
}