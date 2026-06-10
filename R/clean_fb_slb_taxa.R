#' @export
clean_fb_slb_taxa <- function(
  the_df,
  the_snapshot,
  the_server
) {

  the_df <- the_df %>% 
    mutate_all(tolower) %>%
    select(-SpecCode)

# Apply manual data corrections to specific versions ---------------------
  # Apply manual corrections conditionally based on:
  # 1) the snapshot version and
  # 2) weather to apply to fishbase or sealifebase taxa table
  # This prevents our manual corretions from propegating quietly into future snapshot data
  
  # Corrections for the `rfishbase` pkg sealifebase 25.04 snapshot
  if(the_snapshot == "25.04" & the_server == "sealifebase"){
    
    # Family veneridae had two unique values of order (nuculida and venerida). Looking 
    # at WoRMS - venerida is the correct Order value. Exclude nuculida to meet assumption
    # that each unique taxa rank value has only one set of unique higher taxa assignments. 
    the_df <- the_df %>% 
      filter(!(Family == "veneridae" & Order == "nuculida"))
  }

  return(the_df)

}