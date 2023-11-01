#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr summarize
#' @importFrom dplyr ungroup
#' @importFrom dplyr group_by
#' @importFrom dplyr pull
#' @importFrom dplyr rename
#' @importFrom dplyr left_join
#' @importFrom stringr str_detect
#' @export
match_hs_to_hs <- function(hs_taxa_match, hs_version, prod_taxa_classification, threshold=0.0){ 
  
  hs_codes <- hs_taxa_match
  
  # Create a regex expression that identifies all possible scientific names
  all_scinames <- unique(c(prod_taxa_classification$SciName,
                           prod_taxa_classification$Genus,
                           prod_taxa_classification$Subfamily,
                           prod_taxa_classification$Family,
                           prod_taxa_classification$Order,
                           prod_taxa_classification$Class,
                           prod_taxa_classification$Superclass,
                           prod_taxa_classification$Phylum,
                           prod_taxa_classification$Kingdom))
  all_scinames <- all_scinames[!is.na(all_scinames)]
  all_scinames <- lapply(all_scinames, FUN = function(x) { paste("(?:", x, ")", sep = "")})
  all_scinames <- unique(unlist(all_scinames))
  all_scinames_regexr <- paste(all_scinames, collapse = "|")
  
  # List of broad taxa (part of match_hs_to_taxa addendum) that shouldn't count towards whether or not HS codes have matching taxa
  # Otherwise taxa elasmbranchii would be shared between both shark and skate/ray HS codes allowing for product conversion between the two
  broad_taxa <- unique(hs_codes$SciName)[!str_detect(unique(hs_codes$SciName), " ")]
    
  # Set code list for different preparation and separation categories:
  # NOTE: lists below are based on hs_version = HS17; for earlier versions, see section on "VERSION CONTROL" 
  
  # SEPARATIONS
  # fish separations: whole, fillet, other meat, livers and roes, other body parts, fats and oils, and flour/meal/pellets (inc. non fish varieties)
  # all other non-fish, non flour/meal/pellets go in the "non-fish product" category
  # Note: list is NOT exhaustive, there are some ambiguous cases which are dealt with later
  whole_list <- hs_codes %>%
    filter((str_detect(Code, pattern = "^0301")==TRUE & str_detect(Code, pattern = "^0301"))|
             (str_detect(Code, pattern = "^0302")==TRUE & str_detect(Code, pattern = "030270|030290|030291|030292|030299")==FALSE)|
             (str_detect(Code, pattern = "^0303")==TRUE & str_detect(Code, pattern = "030380|030390|030391|030392|030399")==FALSE)) %>%
    pull(Code) %>%
    unique()
  
  fillet_list <- hs_codes %>%
    filter(str_detect(Code, pattern = "^0304[1-4,6-8]")==TRUE|
             str_detect(Code, pattern = "^0305[3-4]")) %>%
    pull(Code) %>%
    unique()
  
  other_meat_list <- hs_codes %>%
    filter(str_detect(Code, pattern = "^0304[5,9]")==TRUE|
             str_detect(Code, pattern = "^0305[5-6]")==TRUE|
             str_detect(Code, pattern = "^160420")==TRUE) %>%
    pull(Code) %>%
    unique()
  
  other_parts_list <- hs_codes %>%
    filter(str_detect(Code, pattern = "^03029[2,9]")==TRUE|
             str_detect(Code, pattern = "^03039[2,9]")==TRUE|
             str_detect(Code, pattern = "^03057")==TRUE|
             str_detect(Code, pattern = "^16043")==TRUE) %>% # includes caviar
    pull(Code) %>%
    unique()
  
  livers_and_roes_list <- hs_codes %>%
    filter(str_detect(Code, pattern = "030270")==TRUE|
             str_detect(Code, pattern = "03029[0,1]")==TRUE|
             str_detect(Code, pattern = "030380")==TRUE|
             str_detect(Code, pattern = "03039[0,1]")==TRUE|
             str_detect(Code, pattern = "030520")==TRUE) %>%
    pull(Code) %>%
    unique()
  
  # We consider fats and oils vs flours/meals/pellets to be different levels of separation (but the same level of preparation - i.e., reduction fisheries)
  # fats and oils:
  fats_oils_list <- hs_codes %>%
    filter(str_detect(Code, pattern = "^1504")==TRUE) %>%
    pull(Code) %>%
    unique()
  
  # flours meals and pellets:
  fmp_list <- hs_codes %>% # fmp list includes codes that are both fit and unfit for human consumption (but consider these different levels or preparation)
    filter(str_detect(Code, pattern = "030510")==TRUE|
             str_detect(Code, pattern = "051191")==TRUE|
             str_detect(Code, pattern = "230120")==TRUE) %>%
    pull(Code) %>%
    unique()

  # all other non-fish, non flour/meal/pellets go in the "non-fish product" category
  # NOTE: Other than "flour/meal/pellets", level of separation not distinguished for non-fish commodities
  non_fish_product_list <- hs_codes %>% 
    filter(str_detect(Code, pattern = "^0306")==TRUE| # anything 0306XX that doesn't end in 9
             str_detect(Code, pattern = "^0307")==TRUE| # everything in 0307[0-9], except 03079
             str_detect(Code, pattern = "^0308")==TRUE| # everything in 0308[0-9], except 03089
             str_detect(Code, pattern = "^1605")==TRUE) %>% 
    pull(Code) %>%
    unique()
  
  
  # PREPARATIONS: live, fresh (incld all non fish non fmp), frozen, preserved (incl dried salted brined smoked), reduction-fisheries, not fit for humans
  # all categories cut across all commodity types
  # flours meals and pellets and fats and oils = reduction-fisheries
  # Note: list is NOT exhaustive, there are some ambiguous cases which are dealt with later
  live_list <- hs_codes %>% 
    filter(str_detect(Code, pattern = "^0301")==TRUE) %>%
    pull(Code) %>%
    unique()
  
  # NOTE: for non-fish categories, there are no separate codes for "live" preparations, so all codes described as live/fresh/chilled are considered fresh/chilled
  fresh_list <- hs_codes %>%
    filter(str_detect(Code, pattern = "^0302")==TRUE|
             str_detect(Code, pattern = "^0304[1,3-5]")==TRUE|
             str_detect(Code, pattern = "^03063")==TRUE|
             str_detect(Code, pattern = "0307[1-9]1")==TRUE|
             str_detect(Code, pattern = "030782")==TRUE|
             str_detect(Code, pattern = "0308[1-2]1")==TRUE) %>%
    pull(Code) %>%
    unique()
  
  frozen_list <- hs_codes %>% 
    filter(str_detect(Code, pattern = "^0303")==TRUE|
             (str_detect(Code, pattern = "^0304[2,6-9]")==TRUE & Code!="030490")| # 30420s, 60s, 80s, 90s, except 030490
             str_detect(Code, pattern = "^03061")==TRUE|
             str_detect(Code, pattern = "0307[^8]2")==TRUE| # all 0307[1-9]2, except 030782 
             str_detect(Code, pattern = "03078[3,4]")==TRUE|
             str_detect(Code, pattern = "0308[1-2]2")==TRUE|
             str_detect(Code, pattern = "030890")==TRUE) %>% 
    pull(Code) %>%
    unique()
  
  preserved_list <- hs_codes %>% # all prepared/preserved, includes dried, salted, brined, smoked, and caviar
    filter(str_detect(Code, pattern = "^0305[2-9]")==TRUE|
             str_detect(Code, pattern = "^03069")==TRUE|
             str_detect(Code, pattern = "03078[7-8]")==TRUE|
             str_detect(Code, pattern = "0307[1-9]9")==TRUE|
             str_detect(Code, pattern = "0308[1-2]9")==TRUE|
             str_detect(Code, pattern = "^160[4-5]")==TRUE) %>%
    pull(Code) %>%
    unique()
  
  reduction_list <- hs_codes %>% 
    filter(str_detect(Code, pattern = "^030510")==TRUE|
             str_detect(Code, pattern = "^0306[1-3,9]9")==TRUE|
             str_detect(Code, pattern = "^03079")==TRUE|
             str_detect(Code, pattern = "030890")==TRUE|
             str_detect(Code, pattern = "^1504")==TRUE) %>%
    pull(Code) %>%
    unique()
  
  
  not_for_humans_list <- hs_codes %>%
    filter(str_detect(Code, pattern = "051191")==TRUE|
             str_detect(Code, pattern = "230120")==TRUE) %>%
    pull(Code) %>%
    unique()
  
  # Collapse hs_taxa_matches to create lists of taxa for each code
  # Will be using this to compare species list for each HS code
  taxa_lists <- hs_codes %>%
    select(Code, SciName) %>%
    arrange(Code, SciName) %>%
    group_by(Code) %>%
    summarise(Taxa_list = paste(SciName, collapse=", ")) %>%
    ungroup() 
  
  # CREATE A SEPARATE TAXA LIST THAT REMOVES BROADLY MATCHING TAXA
  taxa_lists_for_comparison <- hs_codes %>%
    select(Code, SciName) %>%
    filter(SciName %in% broad_taxa == FALSE) %>% # REMOVE BROADLY MATCHING TAXA
    arrange(Code, SciName) %>%
    group_by(Code) %>%
    summarise(Taxa_list_for_comparison = paste(SciName, collapse=", ")) %>%
    ungroup() 
  
  # Join the two, now have one complete taxa list (needed for downstream ARTIS) and another taxa list to be used for matching hs_to_hs
  taxa_lists <- taxa_lists %>%
    left_join(taxa_lists_for_comparison, by = "Code")
  
  # Create output dataframe of all possible commodity to commodity conversions 
  output_codes <- hs_codes %>%
    select(Code, Description) %>%
    unique()

  output_grid <- expand.grid(output_codes$Code, output_codes$Code)

    
  output_df <- output_grid %>% 
    dplyr::rename(Code_pre = Var1, Code_post = Var2) %>%
    mutate(Code_pre = as.character(Code_pre),
           Code_post = as.character(Code_post)) %>% # Do this to suppress left_join warning message: joining factor and character vector, coercing into character vector 
    
    # Join with output_codes to get Code Descriptions
    left_join(output_codes, by = c("Code_pre" = "Code"))  %>%
    dplyr::rename(Description_pre = Description) %>%
    left_join(output_codes, by = c("Code_post" = "Code")) %>%
    dplyr::rename(Description_post = Description) %>%
    
    # Join with taxa_lists
    left_join(taxa_lists, by = c("Code_pre" = "Code"))  %>%
    dplyr::rename(Taxa_full = Taxa_list) %>%
    # Use Taxa_list_for_comparison (filtered out broad taxa) to create a Taxa_pre and Taxa_post to compare taxa lists between commodity codes
    dplyr::rename(Taxa_pre = Taxa_list_for_comparison) %>%
    left_join(taxa_lists, by = c("Code_post" = "Code")) %>%
    dplyr::rename(Taxa_post = Taxa_list_for_comparison) %>%
    mutate(Taxa_identical = if_else(Taxa_pre == Taxa_post, true = 1, false = 0)) %>% # Are the taxa lists identical? 1 = YES; 0 = NO 
    # Try a less stringent criteria: see if any taxa are shared at all between two codes, rather than complete list
    rowwise() %>%
    mutate(Taxa_shared = paste(intersect(strsplit(Taxa_pre, ", ")[[1]], strsplit(Taxa_post, ", ")[[1]]), collapse = ", ")) %>%
    mutate(n_pre = length(strsplit(Taxa_pre, ", ")[[1]]),
           n_post = length(strsplit(Taxa_post, ", ")[[1]])) %>%
    mutate(Pct_shared_pre = length(strsplit(Taxa_shared, ", ")[[1]]) / n_pre) %>% # What percent of the pre-commodity taxa is shared with the post-commodity taxa?
    mutate(Pct_shared_post = length(strsplit(Taxa_shared, ", ")[[1]]) / n_post) %>% # What percent of the post-commodity taxa is shared with the pre-commodity taxa?
    ungroup() %>%
    
    # Create columns, pre and post, with a description of the commodities level of preparation and separation
    mutate(Sep_pre = case_when(Code_pre %in% whole_list==TRUE ~ "whole",
                               Code_pre %in% fillet_list==TRUE ~ "fillet",
                               Code_pre %in% other_meat_list==TRUE ~ "other meat",
                               Code_pre %in% other_parts_list==TRUE ~ "other body parts",
                               Code_pre %in% livers_and_roes_list==TRUE ~ "livers and roes",
                               Code_pre %in% fats_oils_list==TRUE ~ "fats and oils",
                               Code_pre %in% fmp_list==TRUE ~ "flours, meals, pellets",
                               Code_pre %in% non_fish_product_list==TRUE ~ "non-fish, non-fmp form")) %>%
    mutate(Sep_post = case_when(Code_post %in% whole_list==TRUE ~ "whole",
                                Code_post %in% fillet_list==TRUE ~ "fillet",
                                Code_post %in% other_meat_list==TRUE ~ "other meat",
                                Code_post %in% other_parts_list==TRUE ~ "other body parts",
                                Code_post %in% livers_and_roes_list==TRUE ~ "livers and roes",
                                Code_post %in% fats_oils_list==TRUE ~ "fats and oils",
                                Code_post %in% fmp_list==TRUE ~ "flours, meals, pellets",
                                Code_post %in% non_fish_product_list==TRUE ~ "non-fish, non-fmp form")) %>%
    mutate(Prep_pre = case_when(Code_pre %in% live_list==TRUE ~ "live",
                                Code_pre %in% fresh_list==TRUE ~ "fresh",
                                Code_pre %in% frozen_list==TRUE ~ "frozen",
                                Code_pre %in% preserved_list==TRUE ~ "preserved",
                                Code_pre %in% reduction_list==TRUE ~ "reduced",
                                Code_pre %in% not_for_humans_list==TRUE ~ "not for humans")) %>%
    mutate(Prep_post = case_when(Code_post %in% live_list==TRUE ~ "live",
                                 Code_post %in% fresh_list==TRUE ~ "fresh",
                                 Code_post %in% frozen_list==TRUE ~ "frozen",
                                 Code_post %in% preserved_list==TRUE ~ "preserved",
                                 Code_post %in% reduction_list==TRUE ~ "reduced",
                                 Code_post %in% not_for_humans_list==TRUE ~ "not for humans")) %>%
    
    # Ambiguous cases: There should still be NAs within Sep_pre/post and Prep_pre/post columns
    # Deal with ambiguous separation cases by making pre vs post state least restrictive
    # Example: Code 160411-19 can be whole or in pieces; assign it to whole in the pre state (expands possibilities of things it can be converted to) and other meat in the post state (expands possibilities of things that it was converted from)
    mutate(Sep_pre = case_when(str_detect(Code_pre, pattern = "^16041[1-9]") ~ "whole",
                               TRUE ~ Sep_pre),
           Sep_post = case_when(str_detect(Code_post, pattern = "^16041[1-9]") ~ "other meat",
                                TRUE ~ Sep_post)) %>%
    
    ## Deal with ambiguous preparation cases by making pre vs. post state least restrictive
    ## Example: Code 030490 can be frozen or fresh/chilled; assign it to fresh in the pre state (expands possibilities of things it can be converted to) and frozen in the post state (expands possibilities of things that it was converted from)
    mutate(Prep_pre = case_when(str_detect(Code_pre, pattern = "030490") ~ "fresh",
                                str_detect(Code_pre, pattern = "03062[1-9]") ~ "fresh",
                                str_detect(Code_pre, pattern = "0307[1,6]0") ~ "fresh",
                                str_detect(Code_pre, pattern = "030830") ~ "fresh",
                                TRUE ~ Prep_pre),
           Prep_post = case_when(str_detect(Code_post, pattern = "030490") ~ "frozen",
                                 str_detect(Code_post, pattern = "03062[1-9]") ~ "preserved",
                                 str_detect(Code_post, pattern = "0307[1,6]0") ~ "preserved",
                                 str_detect(Code_post, pattern = "030830") ~ "preserved",
                                 TRUE ~ Prep_post)) # END FIRST CREATION OF output_df
  
  
  ########################################################################################################## 
  # VERSION CONTROL
  # THESE ARE RARE, BUT IN SOME CASES, HS CODES HAVE NOT BEEN CONSISTENT THROUGH TIME IN THEIR STATE OF PREPARATION OR SEPARATION
  # For all versions prior to HS17 (2017), make the following changes:
  # Codes in: str_detect(Code, pattern = "0307[1-9]9")==TRUE), str_detect(Code, pattern = "0308[1-2]9")==TRUE|
  # originally part of the "preserved_list", should be treated as ambiguous preparations: frozen in the pre-state, preserved in the post-state
  if (hs_version %in% c("HS92", "HS96", "HS02", "HS07", "HS12")){
    output_df <- output_df %>%
      mutate(Prep_pre = case_when(str_detect(Code_pre, pattern = "0307[1-9]9") ~ "frozen",
                                  str_detect(Code_pre, pattern = "0308[1-2]9") ~ "frozen",
                                  TRUE ~ Prep_pre),
             Prep_post = case_when(str_detect(Code_post, pattern = "0307[1-9]9") ~ "preserved",
                                   str_detect(Code_post, pattern = "0308[1-2]9") ~ "preserved",
                                   TRUE ~ Prep_post))
    
  }
  
  
  
  
  # Now create rules for conversions # 1 = conversion is possible; 0 = not possible
  # SEPARATION RULES:
  # Whole fish can become anything
  # Anything can become flours, meals, pellets, except fats and oils
  # Fillets can be processed into other meat (in addition to becoming flours, meals, pellets)
  # Only whole fish, other body parts, and livers and roes can become fats and oils
  # For non-fish, non-fmp form (e.g., molluscs, crustaceans) - code descriptions are too broad; assume conversion always possible
  output_df <- output_df %>%
    mutate(Sep_test = case_when(Sep_pre == "whole" ~ 1,
                                Sep_pre != "fats and oils" & Sep_post == "flours, meals, pellets"  ~ 1,
                                Sep_pre == "fillet" & Sep_post == "other meat" ~ 1,
                                Sep_pre %in% c("whole", "other body parts", "livers and roes") & Sep_post == "fats and oils" ~ 1,
                                Sep_pre != "flours, meals, pellets" & Sep_post == "non-fish, non-fmp form" ~ 1, # Anything crustacean or mollusc code can become another unless the starting point is flours, meals, pellets
                                TRUE ~ 0)) %>%
    # PREPARATION RULES:
    # Live fish stays as live fish, too expensive to import live fish and then process it into something else for export
    # Fresh fish can become anything except live
    # Frozen fish can become anything except live or fresh
    # Any preparation (except live fish) can be turned into a reduced product, unless it's unfit for humans
    # Any preparation (except live fish) can be used for products unfit for humans
    mutate(Prep_test = case_when(Prep_pre == "live" ~ 0,
                                 Prep_pre == "fresh" & Prep_post != "live" ~ 1,
                                 Prep_pre == "frozen" & (Prep_post %in% c("fresh", "live")==FALSE) ~ 1,
                                 Prep_pre != "not for humans" & Prep_post == "reduced" ~ 1,
                                 Prep_post == "not for humans" ~ 1,
                                 TRUE ~ 0)) %>%
    
    # HS commodity group test
    mutate(
      HS_group_pre = case_when(
        # Mentions any specific scientific name in description
        str_detect(Description_pre, all_scinames_regexr) ~ "specific",
        # Mentions "n.e.c"
        str_detect(Description_pre, "n\\.e\\.c") ~ "NEC",
        # Otherwise is classified as a broad code
        TRUE ~ "broad"
      ),
      HS_group_post = case_when(
        # Mentions any specific scientific name in description
        str_detect(Description_post, all_scinames_regexr) ~ "specific",
        # Mentions "n.e.c"
        str_detect(Description_post, "n\\.e\\.c") ~ "NEC",
        # Otherwise is classified as a broad code
        TRUE ~ "broad"
      )
    ) %>%
    mutate(HS_group_test = 0) %>%
    mutate(HS_group_test = case_when(
      HS_group_pre == HS_group_post ~ 1,
      HS_group_pre == "specific" & HS_group_post == "broad" ~ 1,
      HS_group_pre == "NEC" & HS_group_post == "broad" ~ 1,
      TRUE ~ 0
    )) %>%
    # CONVERSION TEST:
    # Also include the fact that any imported commodity can be exported as itself (Code_pre == Code_post)
    mutate(Conversion_test = case_when((Sep_test == 1 & Prep_test == 1 & Pct_shared_pre > threshold)|  # No difference between Pct_shared_pre > 0 and Pct_shared_post > 0 
                                         (Code_pre == Code_post) ~ 1,
                                       TRUE ~ 0)) %>%
    mutate(Conversion_test = case_when(
      Conversion_test == 1 & HS_group_test == 1 ~ 1,
      TRUE ~ 0
    )) %>%
    # Out of those that already passed the conversion test to be processed into an FMFO, if it isn't a whole fish or if it isn't already a flour, meal, pellet, make it fail the conversion test - i.e., only FMFOs OR whole fish should be allowed to become FMFO
    mutate(Conversion_test = case_when(Code_post == 230120 & Conversion_test == 1 & (Sep_pre %in% c("whole", "flours, meals, pellets") == FALSE) ~ 0,
                                       # This was the 20220828 version of model_inputs:
                                       #Code_pre != 230120 & Sep_pre != "whole" & Code_post == 230120 ~ 0,
                                       # ...And also allow whole, non-live fish to become fish meal fish oil
                                       #Sep_pre == "whole" & Prep_pre != "live" & Code_post == 230120 ~ 1,
                                       TRUE ~ as.numeric(Conversion_test))) %>%
    # Filter to possible product to product conversions
    filter(Conversion_test==1)
  
  return(output_df)
}




