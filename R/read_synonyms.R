#' @import dplyr
#' @export
read_synonyms <- function(fp) {
  # DESCRIPTION:
  #       - a function that will read and clean fishbase or sealifebase synonym datasets
  # INPUT:
  #       - filepath to a specific synonym dataset (STRING)
  # OUTPUT:
  #       - dataframe of synonyms and accepted names (DATA FRAME)
  
  df <- read.csv(fp)
  
  # filter rows to only include synonym or accepted name rows
  df <- df %>%
    mutate(Status = tolower(Status)) %>% # change Status to lower case
    mutate(Status = case_when(
      Status == 'ambiguous synonym' ~ 'synonym', # ambiguous synonyms are treated like synonyms
      Status == 'provisionally accepted name' ~ 'accepted', # provisionally accepted names are treated as accepted names
      Status == 'accepted name' ~ 'accepted', # change all accepted names into single word accepted
      TRUE ~ Status # leave the rest of the statuses the way they are - will get filtered out
    )) %>%
    filter((Status == 'accepted' | Status == 'synonym')) %>% # filter just for accepted and synonyms
    select(c(synonym, status = Status, spec_code = SpecCode, syn_code = SynCode, taxon_level = TaxonLevel)) %>%
    mutate(synonym = gsub('\\.', '', synonym)) %>% # eliminate dots
    mutate(synonym = gsub(',', '', synonym)) %>% # eliminates commas
    mutate(synonym = gsub('-', ' ', synonym)) %>% # replaces hyphens with spaces
    mutate(synonym = tolower(synonym), taxon_level = tolower(taxon_level))
    
  # Count number of accepted names per spec code
  spec_code_counts <- df %>%
    group_by(spec_code, status) %>%
    count() %>%
    filter(n > 1 & status == 'accepted') 
  
  df <- df %>%
    filter(spec_code > 0)
  
  return(df)
}
