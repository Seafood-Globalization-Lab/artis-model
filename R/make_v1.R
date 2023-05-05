#' @importFrom dplyr select 
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