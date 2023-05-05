#' @export
categorize_hs_to_taxa <- function(hs_taxa_match, coproduct_codes){
  # Make Xq, matrix for controlling strength of species to product estimates in optimization problem (see transform_to_qp_with_python.R)
  
  # Columns are species, Rows are products
  Xq <- hs_taxa_match %>%
    filter(Code %in% coproduct_codes == FALSE) %>%
    mutate(Match_factor = case_when(Match_category %in% c("aquarium_trade_match", "explicit_taxa_match", "fmfo_match") ~ 1, # aquarium and fmfo matches are also explicit, based on taxa lists
                                    Match_category %in% c("NEC_by_taxa_match", "NEC_match") ~ 2,
                                    Match_category %in% c("broad_commodity_match", "broad_taxa_match") ~ 3)) %>%
    select(Code, SciName, Match_factor) %>%
    arrange(SciName, Code) %>%
    distinct() %>%
    pivot_wider(names_from = SciName, values_from = Match_factor, values_fill = 0) %>%
    arrange(Code) %>%
    select(-c(Code)) 
  
  Xq <- as.matrix(Xq, nrow = length(unique(hs_taxa_match$Code)), ncol = length(unique(hs_taxa_match$SciName)))
  
  return(Xq)
}