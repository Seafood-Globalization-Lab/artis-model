#' Build V1 live-weight conversion factor matrix (products x species)
#'
#' @param hs_taxa_CF_match Data frame.
#' @param coproduct_codes Character vector.
#' @return A list of three elements: (1) V1 numeric matrix (products x species)
#'   of live-weight-to-commodity conversion factors, (2) row_order character
#'   vector of HS codes, (3) col_order character vector of taxa names.
#' @importFrom dplyr filter select arrange distinct
#' @importFrom tidyr pivot_wider
#' @export
make_v1 <- function(hs_taxa_CF_match, coproduct_codes){
  # Columns are species, Rows are products
  dat <- hs_taxa_CF_match %>%
    filter(Code %in% coproduct_codes == FALSE) %>%
    select(Code, Taxa, CF_live_to_commod) %>%
    arrange(Taxa, Code) %>%
    distinct() %>%
    pivot_wider(names_from = Taxa, values_from = CF_live_to_commod, values_fill = 0) %>%
    arrange(Code)
  
  row_order <- dat$Code
  
  dat <- dat %>%
    select(-c(Code))
  
  col_order <- colnames(dat)
  
  v1 <- as.matrix(dat, nrow = length(unique(hs_taxa_CF_match$Code)), ncol = length(unique(hs_taxa_CF_match$Species)))
  
  return(list(v1, row_order, col_order))
}