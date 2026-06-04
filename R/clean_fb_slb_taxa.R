#' @export
clean_fb_slb_taxa <- function(
  df
) {

  df <- df %>% 
    mutate_all(tolower) %>%
    select(-SpecCode)

# Apply manual data corrections to specific versions ---------------------
  
  # Corrections for the rfishbase sealifebase 25.04 snapshot
  if(rfishbase::available_releases(server = "sealifebase") %>% tail(n = 1) == "25.04"){
    
    # Family veneridae had two unique values of order (nuculida and venerida). Looking 
    # at WoRMS - venerida is the correct Order value. Exclude nuculida to meet assumption
    # that each unique taxa rank value has only one set of unique higher taxa assignments. 
    df <- df %>% 
      filter(!(Family == "veneridae" & Order == "nuculida"))
  }
  
  # Add future corrections here


}