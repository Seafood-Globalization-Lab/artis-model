#' @import dplyr
#' @export
read_common_to_sci <- function(fp) {
  # DESCRIPTION:
  #       - a function that will read and clean fishbase or sealifebase common name datasets
  # INPUT:
  #       - filepath to a specific common to scientific name dataset (STRING)
  # OUTPUT:
  #       - dataframe of common names and scientific names (DATA FRAME)
  
  df <- read.csv(fp)
  
  df <- df %>%
    mutate(Species = tolower(Species),
           ComName = tolower(ComName)) %>%
    rename(SciName = Species, CommonName = ComName, spec_code = SpecCode)
  
  return(df)
}