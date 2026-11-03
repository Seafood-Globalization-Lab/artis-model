#' Build V2 HS-to-HS conversion factor matrix (products x products)
#'
#' @param hs_hs_match Data frame. Output from match_hs_to_hs().
#' @param hs_taxa_CF_match Data frame. HS code-to-taxa conversion factor matches.
#' @param coproduct_codes Character vector. HS codes to exclude as co-products.
#' @return A list of three elements: (1) V2 numeric matrix (Code_post x Code_pre)
#'   of commodity-to-commodity conversion factors, (2) row_order character vector
#'   of Code_post values, (3) col_order character vector of Code_pre values.
#' @import dplyr
#' @importFrom tidyr pivot_wider
#' @export
make_v2 <- function(hs_hs_match = hs_hs_match, hs_taxa_CF_match = hs_taxa_CF_match, coproduct_codes){
  CF_for_v2 <- hs_taxa_CF_match %>%
    filter(Code %in% coproduct_codes == FALSE) %>%
    group_by(Code) %>%
    summarise(CF_calc = mean(CF_calc, na.rm = TRUE)) # calculate mean CF across taxa within a product code
  
  dat <- hs_hs_match %>%
    filter(Code_pre %in% coproduct_codes==FALSE & Code_post %in% coproduct_codes==FALSE) %>%
    select(Code_pre, Code_post) %>%
    # Add live weight conversions for the original product state
    left_join(CF_for_v2, by = c("Code_pre" = "Code")) %>%
    rename(CF_pre = CF_calc) %>%
    select(Code_pre, CF_pre, Code_post) %>%
    # Add live weight conversions for the final product state
    left_join(CF_for_v2, by = c("Code_post" = "Code")) %>%
    rename(CF_post = CF_calc) %>%
    # Calculate conversion factor for HS_to_HS for each code as commodity to live
    mutate(HS_to_HS_CF = CF_pre/CF_post) %>%
    # To convert Commod_pre to Commod_post: Commod_pre weight * (live weight / commod_pre weight) * (commod_post weight / live weight) - i.e., CF_pre / CF_post
    # NOTE: CF_pre/CF_post would be the same as doing CF_live_to_commod_post / CF_live_to_commod_pre
    # Now group_by Code_pre and post to collapse taxa and calculate a commodity-level HS_to_HS conversion factor
    # Clean CF values:
    select(Code_pre, Code_post, HS_to_HS_CF) %>%
    # CF's should be < 1: Converting a commodity to another commodity should not increase total quantity, so replace with 1
    mutate(HS_to_HS_CF = if_else(HS_to_HS_CF > 1, true = 1, false = HS_to_HS_CF)) %>%
    # some CF values = NA (when 0 divided by 0, or when "NA" is in the numerator or denominator) - set to 0
    mutate(HS_to_HS_CF = if_else(is.na(HS_to_HS_CF), true = 0, false = HS_to_HS_CF)) %>%
    # some CF values = Inf (when 0 in denominator) - set to 0
    mutate(HS_to_HS_CF = if_else(HS_to_HS_CF == "Inf", true = 0, false = HS_to_HS_CF)) %>%
    # Pivot wider
    arrange(Code_pre) %>%
    pivot_wider(names_from = Code_pre, values_from = HS_to_HS_CF, values_fill = 0) %>%
    arrange(Code_post) # arrange rows in numerical order (Note: Code_pre columns are already in order; dataframe was arranged by Code_pre before pivoting these to column)
  
  row_order <- dat$Code_post
  
  dat <- dat %>%
    select(-Code_post)
  
  col_order <- colnames(dat)
  
  v2 <- as.matrix(x = dat, nrow = length(dat$Code_post), ncol = length(dat$Code_pre))
  
  return(list(v2, row_order, col_order))
}
