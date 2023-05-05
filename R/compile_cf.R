#' @import dplyr
#' @import tidyr
#' @importFrom data.table rbindlist
#' @importFrom magrittr %>%
#' @import stringr
#' @export

compile_cf <- function(conversion_factors, eumofa_data, hs_hs_match, hs_version, match_criteria){
  
  
  # NOTE on warnings when creating hs_cf_full_match
  # There were 50 or more warnings (use warnings() to see the first 50): In max(conversion_factor_full, na.rm = TRUE): no non-missing arguments to max; returning -Inf
  # This warning appears whenever there are no values to consider when calculating max_cf_full = max(conversion_factor_full, na.rm = TRUE) and min(conversion_factor_full, na.rm = TRUE)
  
  # Output depends on match_criteria:
  
  # 1 - if match_criteria == EUMOFA, just use EUMOFA data
  # 2 - if match_criteria == strict, use CF data that match Taxa, Separation, AND Preparation (i.e., "full_match"); otherwise use EUMFOA
  # 3 - if match_criteria == loose, use CF data that match Taxa and Separation and/or Preparation (i.e., "loose_match"); for remaining non-matches, aggregate all taxa within a Code; if still any non-matches, use EUMFOA
  # 4 - if match_criteria == fillet, match CF values based on either strict or loose criteria, but only retain CF values for fillets; use EUMOFA for the rest
  # for options 2, 3, and 4, assign CF values to live fish, caviar, and FMFOs

  # EUMOFA cf values are reported at the 8-digit level: Calculate mean EUMOFA cf value for each Code (6-digit) + Year grouping
  eumofa_data_grouped <- eumofa_data %>% 
    select(CN.8, Year, CF) %>%
    mutate(CN.8 = str_remove(CN.8, pattern = " ")) %>%
    # Some of the "03" codes in the EUMOFA spreadsheet are missing a 0; standardize to all be "03"
    mutate(CN.8 = if_else(str_detect(CN.8, "^3"), true = str_replace(CN.8, pattern = "30", replacement = "030"), false = CN.8)) %>%
    mutate(CN.8 = str_sub(CN.8, start = 1, end = 6)) %>%
    group_by(CN.8, Year) %>%
    mutate(n_EUMOFA_raw = n(), n_0 = sum(CF == 0)) %>%
    filter(CF != 0 | (CF == 0 & n_EUMOFA_raw == n_0)) %>%  # Only filter out zeroes if there are other non-zero CF values available 
    summarise(mean_EUMOFA = mean(CF), stdev_EUMOFA = sd(CF), max_EUMOFA = max(CF), min_EUMOFA = min(CF), n_EUMOFA = n()) %>%
    arrange(CN.8, Year) %>%
    ungroup()

  hs_tibble <- tibble(HS_year = c("HS92", "HS96", "HS02", "HS07", "HS12", "HS17"),
                      year = c(1992, 1996, 2002, 2007, 2012, 2017))
  prod_year <- hs_tibble %>% filter(HS_year==hs_version) %>% pull(year) %>% as.integer()
  
  # For each Code (CN.8), decide which year should match to hs_for_cf_matching
  # If EUMOFA data exists for the same year as the production year, use that
  # If EUMOFA data only exists for years before the production year, use the most recent - i.e., max(Year)
  # If EUMOFA data only exists for years after the production year, use the one closest to production year - i.e., min(Year)
  eumofa_for_matching <- eumofa_data_grouped %>% 
    group_by(CN.8) %>%
    mutate(match_year = case_when(prod_year %in% Year ~ prod_year,
                                  prod_year > max(Year) ~ max(Year),
                                  prod_year < min(Year) ~ min(Year),
                                  TRUE ~ as.integer(NA))) %>% 
    # Still need to deal with cases where: prod_year < max(Year) & prod_year > min(Year), but do this after filtering out Year > prod_year
    filter(is.na(match_year) == FALSE | (Year < prod_year & is.na(match_year)) ) %>%
    mutate(match_year = case_when(is.na(match_year) ~ max(Year),
                                  TRUE ~ match_year)) %>%
    filter(Year == match_year) %>%
    ungroup()
  
  # Then, prepare hs_hs_match to be joined with cf_data
  hs_for_cf_matching <- hs_hs_match %>%
    select(Code_pre, Description_pre, Taxa_full, Sep_pre, Prep_pre) %>%
    rename(Code = Code_pre, Description = Description_pre, Taxa = Taxa_full, Separation = Sep_pre, Preparation = Prep_pre) %>%
    # Need to flatten hs_for_cf_matching$Taxa, one row for each taxon
    separate_rows(Taxa, sep = ", ", ) %>%
    unique() %>% # not sure why so many duplicate rows are being created 
    # create Separation category for caviar (was not part of the original hs_hs_match)
    mutate(Separation = case_when(str_detect(Code, "^16043") ~ "caviar",
                                  TRUE ~ Separation))
  
  
  
  # OUTPUT OPTION: if we only want EUMOFA data, join hs_for_cf_matching with eumofa_data
  if(match_criteria == "EUMOFA") { 

    hs_cf_match_final <- hs_for_cf_matching %>%
      left_join(eumofa_for_matching, by = c("Code" = "CN.8")) %>%
      mutate(CF_calc = NaN,
             calc_notes = NA)
    
    # Assumptions about Fishmeal and Fish oil production rates (Reference: IFFO, 2009)
    # ASSIGN: assign values to live fish, FMFOs, and caviar
    fm_rate <- .225
    fo_rate <- .05
    hs_cf_match_final <- hs_cf_match_final %>%
      mutate(CF_calc = case_when(str_detect(Code, "^0301") ~ 1, # LIVE FISH
                                 # FISH OILS GET "0"
                                 str_detect(Code, "^1504") ~ 0,
                                 # FISH FLOUR, MEALS, PELLETS
                                 str_detect(Code, pattern = "030510") ~ 1/(fm_rate+fo_rate) * (fm_rate/(fm_rate+fo_rate)),
                                 str_detect(Code, pattern = "0306[1-9]9") ~ 1/(fm_rate+fo_rate) * (fm_rate/(fm_rate+fo_rate)),
                                 str_detect(Code, pattern = "^03079") ~ 1/(fm_rate+fo_rate) * (fm_rate/(fm_rate+fo_rate)),
                                 str_detect(Code, pattern = "030890") ~ 1/(fm_rate+fo_rate) * (fm_rate/(fm_rate+fo_rate)),
                                 str_detect(Code, pattern = "051191") ~ 1/(fm_rate+fo_rate) * (fm_rate/(fm_rate+fo_rate)),
                                 str_detect(Code, pattern = "230120") ~ 1/(fm_rate+fo_rate) * (fm_rate/(fm_rate+fo_rate)),
                                 # CAVIAR:
                                 str_detect(Description, pattern = "caviar") ~ 0,
                                 TRUE ~ NaN)) %>%
      mutate(calc_notes = if_else(is.na(CF_calc) == FALSE, true = "assign", false = "no_CF")) %>%
      # For all other NON-ASSIGNED values in CF_calc, assign to mean_EUMOFA
      mutate(CF_calc = if_else(is.na(CF_calc), true = mean_EUMOFA, false = CF_calc),
             calc_notes = if_else(calc_notes == "no_CF", true = "mean_EUMOFA", false = calc_notes),
             CF_live_to_commod = 1/CF_calc)
    
  }
  
  # OUTPUT OPTION: for all other match_criteria, process conversion_factors data first:
  if (match_criteria %in% c("strict", "loose", "fillet")) { 
    
    
    # First, classify compiled data on conversion factors in terms of their methods of perparation and separation
    
    # Remove all unrealistic conversion factors that bias results. 
    # Those with CFs < 1 are considered as by-products and are thus 'free', or are processed products where the other ingredients are included in the processed form weight (e.g., canned in oil)
    cf_data <- conversion_factors %>%
      filter(Conversion.factor>=1) %>%
      filter(Type != "Aquatic plants")
    
    # Rename type column to match transformation code:
    colnames(cf_data)[1] <- "type"
    colnames(cf_data)[5] <- "type_of_processing"
    cf_data$type <- as.character(cf_data$type)
    cf_data$type_of_processing <- tolower(as.character(cf_data$type_of_processing))

    ############################################################################################################
    # Establish PREPARATION 'states' of product data based on these categories
    # Note: all preparation and separation categories match those of hs_hs_match
    cf_data$Preparation <- NA
    
    # Live
    cf_data$Preparation[str_detect(cf_data$type_of_processing, pattern="live")] <- "live"
    cf_data$Preparation[str_detect(cf_data$type_of_processing, pattern="liver")] <- NA
    
    # Fresh
    cf_data$Preparation[str_detect(cf_data$type_of_processing, pattern="fresh")] <- "fresh"
    # Note: using pattern = "chilled" doesn't get anything more, since all descriptions say "fresh/chilled"
    
    # Frozen
    cf_data$Preparation[str_detect(cf_data$type_of_processing, pattern="frozen")] <- "frozen"
    
    # Preserved (Combine dried, smoked, salted, fermented into "preserved")
    cf_data$Preparation[str_detect(cf_data$type_of_processing, pattern="dried")] <- "preserved"
    cf_data$Preparation[str_detect(cf_data$type_of_processing, pattern="canned")] <- "preserved"
    cf_data$Preparation[str_detect(cf_data$type_of_processing, pattern="fermented")] <- "preserved"
    cf_data$Preparation[str_detect(cf_data$type_of_processing, pattern="pickled")] <- "preserved"
    cf_data$Preparation[str_detect(cf_data$type_of_processing, pattern="preserved")] <- "preserved"
    cf_data$Preparation[str_detect(cf_data$type_of_processing, pattern="salted")] <- "preserved"
    cf_data$Preparation[str_detect(cf_data$type_of_processing, pattern="brine")] <- "preserved"
    cf_data$Preparation[str_detect(cf_data$type_of_processing, pattern="smoked")] <- "preserved"
    
    ############################################################################################################
    # Establish SEPARATION states of product data:
    
    cf_data$Separation <- NA
    
    # whole: 
    # note: non-fishes described as "whole" are later assigned to "non-fish, non fmp form"
    cf_data$Separation[str_detect(cf_data$type_of_processing, pattern="whole")] <- "whole"
    cf_data$Separation[cf_data$type_of_processing=="live"] <- "whole"
    
    # Commenting this out, after examining EU Commission's list of CF values, we can't assume these to mean "whole" fishes:
    # assuming that when only preparation is specified (i.e., unspecified separations) then separation-level is "whole" 
    #cf_data$Separation[cf_data$type_of_processing=="fresh/chilled"] <- "whole"
    #cf_data$Separation[cf_data$type_of_processing=="frozen"] <- "whole"
    #cf_data$Separation[cf_data$type_of_processing=="salted, wet or in brine"] <- "whole"
    #cf_data$Separation[cf_data$type_of_processing=="salted wet light"] <- "whole"
    #cf_data$Separation[cf_data$type_of_processing=="salted dry light"] <- "whole"
    #cf_data$Separation[cf_data$type_of_processing=="salted wet heavy"] <- "whole"
    #cf_data$Separation[cf_data$type_of_processing=="salted dry heavy, hand processed"] <- "whole"
    #cf_data$Separation[cf_data$type_of_processing=="smoked"] <- "whole"
    #cf_data$Separation[cf_data$type_of_processing=="salted"] <- "whole"
    #f_data$Separation[cf_data$type_of_processing=="salted dry heavy"] <- "whole"
    #cf_data$Separation[cf_data$type_of_processing=="dried, whether or not salted"] <- "whole"
    #cf_data$Separation[cf_data$type_of_processing=="dried"] <- "whole"
    
    # other whole separations (non-exact matching): loose definition, head/tail off can still be considered "whole"
    # NOTE: this is loose matching here, but many of these (e.g., frozen, gutted, split", "frozen, gutted, head off, tail off, blocks") get assigned back to "other meat" in the next section 
    cf_data$Separation[str_detect(cf_data$type_of_processing, pattern="gutted")] <- "whole" 
    cf_data$Separation[str_detect(cf_data$type_of_processing, pattern="head off")] <- "whole" 
    cf_data$Separation[str_detect(cf_data$type_of_processing, pattern="tail off")] <- "whole" 
    
    # other meat: 
    # Many of these will reassign "whole" to "other meat"
    cf_data$Separation[cf_data$type_of_processing=="canned"] <- "other meat" # originally assumed this to be "whole", but examined cf_data %>% filter(type_of_processing == "canned") to pick and choose which are whole
    cf_data$Separation[str_detect(cf_data$type_of_processing, pattern="salted dry")] <- "other meat" # refers to frozen bacalao (salted cod)
    cf_data$Separation[str_detect(cf_data$type_of_processing, pattern="gibbed")] <- "other meat" 
    cf_data$Separation[str_detect(cf_data$type_of_processing, pattern="gibbed")] <- "other meat" 
    cf_data$Separation[str_detect(cf_data$type_of_processing, pattern="surimi")] <- "other meat"
    cf_data$Separation[str_detect(cf_data$type_of_processing, pattern="blocks")] <- "other meat" # blocks are amalgamations of fillets and other meat
    cf_data$Separation[str_detect(cf_data$type_of_processing, pattern="edible flesh")] <- "other meat"
    cf_data$Separation[str_detect(cf_data$type_of_processing, pattern="meat")] <- "other meat" # includes meats, canned meat, and meat only: cf_data %>% filter(str_detect(type_of_processing, "meat")) %>% select(type_of_processing) %>% unique()
    cf_data$Separation[str_detect(cf_data$type_of_processing, pattern="minced")] <- "other meat"
    cf_data$Separation[str_detect(cf_data$type_of_processing, pattern="paste")] <- "other meat" # HS Code 160420 is for fish paste
    cf_data$Separation[str_detect(cf_data$type_of_processing, pattern="steak")] <- "other meat"
    cf_data$Separation[str_detect(cf_data$type_of_processing, pattern="split")] <- "other meat"
    cf_data$Separation[str_detect(cf_data$type_of_processing, pattern="skin off")] <- "other meat"
    cf_data$Separation[str_detect(cf_data$type_of_processing, pattern="dressed")] <- "other meat"
    cf_data$Separation[str_detect(cf_data$type_of_processing, pattern="boneless")] <- "other meat" # Note: many of the "boneless" descriptions later get changed to fillets below
    
    # livers and roes
    cf_data$Separation[str_detect(cf_data$type_of_processing, pattern="roe")] <- "livers and roes"
    cf_data$Separation[str_detect(cf_data$type_of_processing, pattern="liver") & str_detect(cf_data$type_of_processing, pattern = "liver in")==FALSE] <- "livers and roes" # avoids calling "gutted, liver in" as livers and roes
    
    
    # caviar
    cf_data$Separation[str_detect(cf_data$type_of_processing, pattern="caviar")] <- "caviar"
    
    # other body parts: includes fins, tails, heads, maws
    cf_data$Separation[str_detect(cf_data$type_of_processing, pattern="tails only")] <- "other body parts"
    cf_data$Separation[str_detect(cf_data$type_of_processing, pattern="wings only")] <- "other body parts"
    cf_data$Separation[str_detect(cf_data$type_of_processing, pattern="tail only")] <- "other body parts"
    
    # fillets:
    cf_data$Separation[str_detect(cf_data$type_of_processing, pattern="fillet") & str_detect(cf_data$type_of_processing, pattern="blocks")==FALSE] <- "fillet" # blocks are considered "other meat" since they're often a mix of fillets and other meats
    cf_data$Separation[cf_data$type_of_processing=="boneless"] <- "fillet"
    
    # Identify non-fish prouducts
    # non-fish HS codes only differentiate two types of separations: flours/meals/pellets and everything else
    # all non-fish should get NA, unless it's flours/meals/pellets
    cf_data$Separation[str_detect(cf_data$type, pattern="Fishes")==FALSE] <- "non-fish, non-fmp form"
    
    
    # RUN the following to see which products still have't been assigned a preparation and/or separation label
    # cf_data %>%
    #  filter(is.na(Separation) | is.na(Preparation)) %>%
    #  select(type_of_processing, Separation, Preparation) %>%
    #  unique()
    
    # NOTES on NAs:
    # cf_data with missing data don't specify a preparation method, only separation (e.g., "fillets, skinless")
    
    # Check how types of processing was translated into different preparations and separations
    #as.data.frame(table(cf_data$Separation, cf_data$type_of_processing)) %>% filter(Freq!=0) %>% arrange(Var1, Var2)
    #as.data.frame(table(cf_data$Preparation, cf_data$type_of_processing)) %>% filter(Freq!=0) %>% arrange(Var1, Var2)
    
    ############################################################################################################
    # Final cleaning of cf_data
    cf_data_clean <- cf_data %>%
      mutate(Species = str_replace(Species, pattern = "\xa0", " ")) %>%
      mutate(Species = tolower(Species)) %>%
      
      # CLEAN UP:
      mutate(Species = case_when(str_detect(Species, " spp.") ~ str_remove(Species, " spp."),
                                 str_detect(Species, " spp") ~ str_remove(Species, " spp"),
                                 str_detect(Species, "\\(.*\\)") ~ str_remove(Species, "\\(.*\\)"),
                                 TRUE ~ Species)) %>%
      mutate(Species = str_trim(Species)) %>%
      
      # Taxa names must match Class, Order, Family, Subfamily, Genus, Species; common columns from fishbase and sealifebase (e.g., Thunnini, a "tribe", must be changed to "Scombridae", a family)
      # HYBRIDS or whenever more than one taxa name is given:
      mutate(Species = case_when(Species == "alosa alosa, a. fallax" ~ "alosa",
                                 Species == "xiphopenaeus, trachypenaeus spp" ~ "penaeidae",
                                 Species == "loliginidae, ommastrephidae" ~ "teuthida",
                                 Species == "squalidae, scyliorhinidae" ~ "carcharhiniformes", # two different families of sharks found in different orders; code currently defines sharks as a list of orders, assign to carcharhiniformes for now
                                 Species == "merluccius capensis, m. paradox" ~ "merluccius",
                                 Species == "thunnini" ~ "scombridae",
                                 Species == "sepildae, sepiolidae" ~ "sepiidae", # Sepiidae = cuttlefish; Sepiolidae = bobtail squid; assigning to cuttlefish
                                 
                                 # SPELLING ERRORS:
                                 Species == "acipenser gueidenstaedtii" ~ "acipenser gueldenstaedtii",
                                 Species == "dicentrachurus labrax" ~ "dicentrarchus labrax",
                                 Species == "sebastes marinus" ~ "sebastes norvegicus", # recognized as a "misapplied" name by synonyms() function; just choose one
                                 Species == "microcosmus suicatus" ~ "microcosmus sulcatus",
                                 Species == "pectin maximus" ~ "pecten maximus",
                                 Species == "illex ilecebrosus" ~ "illex illecebrosus",
                                 Species == "chionectes opilio" ~ "chionoecetes opilio",
                                 Species == "urophycis chruss" ~ "urophycis chuss",
                                 Species == "argyrosomus holoiepidotus" ~ "argyrosomus hololepidotus", 
                                 Species == "dicentrachurus punctatus" ~ "dicentrarchus punctatus",
                                 Species == "nototodarus sloania" ~ "nototodarus sloanii", 
                                 Species == "psette maxima" ~ "psetta maxima", 
                                 Species == "aridae" ~ "ariidae",
                                 Species == "carinus maenas" ~ "carcinus maenas", 
                                 Species == "catharidae" ~ "citharidae", 
                                 Species == "dicentrachurus" ~ "dicentrarchus",
                                 Species == "galdropsarus" ~ "gaidropsarus",
                                 Species == "holothurioidea" ~ "holothuroidea", 
                                 Species == "lophidae" ~ "lophiidae", 
                                 Species == "mytillidae" ~ "mytilidae",
                                 Species == "salmonoidei" ~ "salmonidae",
                                 
                                 
                                 # OUTDATED names or names that need to be adjusted to match with HS codes:
                                 Species == "brachyura" ~ "portunidae", # assign to portunidae for now (force it to match to crab codes - remember matching for crabs done by common names)
                                 Species == "branchiostegidae" ~ "malacanthidae",
                                 TRUE ~ Species))
    
    
    # Deal with missing scientific names
    ############################################################################################################
    # Look up scientific names for taxa in CF data that only provide common name
    # Do this either manually or using function rfishbase::common_to_sci
    no_sci_name_pre <- cf_data_clean %>% 
      filter(Species=="") %>% 
      select(Common.name, Species) %>% 
      unique() %>%
      mutate(Common.name = case_when(Common.name == "Mackerel, Jack mackerel" ~ "Jack mackerel",
                                     TRUE ~ Common.name)) %>%
      arrange(Common.name)
    
    
    # Manually: these are either too general to be matched to a species name (e.g., catfish) or unable to be matched exactly with common_to_sci (e.g., "Common carp" also match "Common carpet shark")
    cf_data_clean <- cf_data_clean %>%
      mutate(Species = case_when(Common.name == "Akiami paste shrimp" ~ "acetes japonicus",
                                 Common.name == "Carp" ~ "cyprinidae",
                                 Common.name == "Catfish" ~ "siluriformes",
                                 Common.name == "Characins" ~ "characidae",
                                 Common.name == "Cichlids" ~ "cichlidae",
                                 Common.name == "Chilean hake" ~ "merluccius",
                                 Common.name == "Cod" ~ "gadus",
                                 Common.name == "Common carp" ~ "cyprinus carpio",
                                 Common.name == "Crab" ~ "portunidae", # assign to portunidae for now (force it to match to crab codes - remember matching for crabs done by common names)
                                 Common.name == "Crustaceans" ~ "decapoda", # assuming some sort of crab/lobster/shrimp/prawn/crayfish crustacean
                                 Common.name == "Cyprinids nei" ~ "cyprinidae",
                                 Common.name == "Eel" ~ "anguilla",
                                 Common.name == "Filefishes" ~ "monacanthidae",
                                 Common.name == "Hairtails, cutlassfishes" ~ "trichiuridae",
                                 Common.name == "Indian mackerels nei" ~ "rastrelliger", # common_to_sci matches multiple names in the same genera
                                 Common.name == "Indian oil sardine" ~ "sardinella",
                                 Common.name == "Indian scad" ~ "decapterus",
                                 Common.name == "king) mackerel" ~ "scomberomorus",
                                 Common.name == "Klipfish" ~ "clinidae",
                                 Common.name == "Lobster" ~ "nephropidae",
                                 Common.name == "Mackerel" ~ "scombridae",
                                 Common.name == "Molluscs" ~ "mollusca",
                                 Common.name == "Other crabs" ~ "cancridae",
                                 Common.name == "Other crustaceans" ~ "decapoda",
                                 Common.name == "Pacific saury" ~ "scomberesox scombroides",
                                 Common.name == "Pike" ~ "esox",
                                 Common.name == "Pilchard" ~ "clupeidae",
                                 Common.name == "Sardine" ~ "clupeidae", 
                                 Common.name == "Sardinellas nei" ~ "clupeidae", 
                                 Common.name == "Sea bream" ~ "sparidae",
                                 Common.name == "Shark" ~ "carcharhiniformes", # multiple orders of sharks; assign to carcharhiniformes for now
                                 Common.name == "Sharks" ~ "carcharhiniformes",
                                 Common.name == "Shrimp and prawns" ~ "penaeus", # assign to penaeus for now (force it to match to Shrimp codes - remember matching for shrimp done by common names)
                                 Common.name == "Shrimps" ~ "penaeus",
                                 Common.name == "Sprat" ~ "sprattus",
                                 Common.name == "Tilapia" ~ "oreochromis", # HS code descriptions define tilapia as this
                                 Common.name == "Tilapias nei" ~ "oreochromis",
                                 TRUE ~ Species)) %>% 
      filter(Common.name %in% c("Freshwater fish", "Other fish", "Sea bass", "Other shellfish", "Marine fishes", "Ornamental fish", "Fishes")==FALSE) # REMOVE THESE: Fish should be at least order-level because HS codes are often AT LEAST this specific
    
    
    # Clean some of the common names before passing to function common_to_sci
    no_sci_name <- cf_data_clean %>% 
      filter(Species=="") %>% 
      select(Common.name) %>% 
      unique() %>%
      mutate(Common.name = case_when(Common.name == "Mackerel, Jack mackerel" ~ "Jack mackerel",
                                     TRUE ~ Common.name)) %>%
      pull(Common.name)
    
    # reads in common name matching
    fb_slb_dir <- "/Volumes/jgephart/ARTIS/Data/fishbase_sealifebase"
    common_names_fp <- file.path(fb_slb_dir, "common_to_sci_fishbase_20220527.csv")
    common_names_df <- read_common_to_sci(common_names_fp)
    
    common_sci_match <- data.frame()
    for (i in 1:length(no_sci_name)){
      common_sci_match_i <- query_common_to_sci(common_names_df, no_sci_name[i])
      common_sci_match <- common_sci_match %>%
        bind_rows(common_sci_match_i)
    }
    
    # filter to retain only exact matches
    # inspect results and remove other suspicious matches
    common_sci_match_clean <- common_sci_match %>%
      rename(Species = SciName) %>%
      filter(CommonName %in% no_sci_name) %>%
      distinct(Species, .keep_all = TRUE) %>% # multiple lines of species names come from having multiple common names, just keep one each
      select(Species, CommonName) %>%
      group_by(CommonName) %>%
      filter(n() == 1) %>% # Repeated common names have different species matches; remove these ambiguous matches
      rename(Species_lookup = Species) %>%
      ungroup()
    
    # Join back with cf_data_clean
    cf_data_clean <- cf_data_clean %>%
      left_join(common_sci_match_clean, by = c("Common.name" = "CommonName")) %>%
      mutate(Species = case_when((Species == "" & is.na(Species_lookup)==FALSE) ~ tolower(Species_lookup), # If Species column is blank and Species_lookup is not NA, replace with Species_lookup
                                 TRUE ~ Species))
    
    ############################################################################################################
    # Before joining CF to HS, identify species names in cf_data_clean that are not part of hs_hs_match, then use this short list to run through synonyms
    nomatch_species_post <- unique(cf_data_clean$Species)[unique(cf_data_clean$Species) %in% unique(hs_for_cf_matching$Taxa)==FALSE]
    nomatch_species_post <- sort(nomatch_species_post[nomatch_species_post != ""])
    # Synonyms function only works on species names (Limit nomatch_fb_and_slb to just species names - i.e., two words, look for space)
    nomatch_species_post <- nomatch_species_post[grepl(nomatch_species_post, pattern = " ")]
    
    # Reading in fishbase and sealifebase synonym databases for matches
    fb_slb_dir <- '/Volumes/jgephart/ARTIS/Data/fishbase_sealifebase'
    fb_df <- read_synonyms(file.path(fb_slb_dir, "synonyms_fishbase_20220518.csv"))
    slb_df <- read_synonyms(file.path(fb_slb_dir, "synonyms_sealifebase_20220525.csv"))
    
    fb_switches <- 0
    slb_switches <- 0
    for (i in 1:length(nomatch_species_post)) {
      next_sciname <- nomatch_species_post[i]
      
      # Searching for accepted names for unmatched species
      fb_name_status <- query_synonyms(fb_df, next_sciname)
      slb_name_status <- query_synonyms(slb_df, next_sciname)
      
      # If a match was found in fishbase
      if (nrow(fb_name_status) > 0) {
        accepted_name <- tolower(fb_name_status$synonym)
        
        cf_data_clean <- cf_data_clean %>%
          mutate(Species = if_else(Species==next_sciname, true = accepted_name, false = Species))
        
        fb_switches = fb_switches + 1
      } # end of fb matching
      
      # If a match was found in sealifebase
      if (nrow(slb_name_status) > 0) {
        accepted_name <- tolower(slb_name_status$synonym)
        
        cf_data_clean <- cf_data_clean %>%
          mutate(Species = if_else(Species==next_sciname, true = accepted_name, false = Species))
        
        slb_switches = slb_switches + 1
      } # end of slb matching
      
    } # end for sciname synonym matching
    # fb_switches 29
    # slb_switches 12
    
    
    # List of remaining CF species that do not match production taxa in hs_for_cf_matching
    # Notes: does not have to be an empty list; just means that it's not in the list of taxa in production data
    # All taxa names checked for correct spelling and corrected in cf_data_clean
    nomatch_species_final <- unique(cf_data_clean$Species)[unique(cf_data_clean$Species) %in% unique(hs_for_cf_matching$Taxa)==FALSE]
    nomatch_species_final <- sort(nomatch_species_final[nomatch_species_final != ""])
    # For just the binomial nomenclature species:
    # nomatch_species_final <- nomatch_species_final[grepl(nomatch_species_final, pattern = " ")]
    
    
    ################################################################################################################
    # Now join hs_for_cf_matching with cf_data by Species, Preparation, and/or Separation
    
    # hs_cf_full_match: mean of all CF's matched by taxa, preparation, and separation
    hs_cf_full_match_raw <- hs_for_cf_matching %>%
      left_join(cf_data_clean, by = c("Taxa" = "Species",
                                      "Preparation",
                                      "Separation")) %>%
      rename(conversion_factor_full = Conversion.factor) 
    
    hs_cf_full_match <- hs_cf_full_match_raw %>%
      select(Code, Description, Taxa, Separation, Preparation, conversion_factor_full)  %>% 
      group_by(Code, Description, Taxa, Separation, Preparation) %>%
      summarise(mean_cf_full = mean(conversion_factor_full, na.rm = TRUE), 
                #max_cf_full = max(conversion_factor_full, na.rm = TRUE), 
                #min_cf_full = min(conversion_factor_full, na.rm = TRUE), 
                max_cf_full = max(conversion_factor_full), 
                min_cf_full = min(conversion_factor_full), 
                n_cf_full = n()) %>%
      ungroup()

    ## Create separate matches for sep only and prep only, then combine into one dataframe for "loose matches" (based on averaging across all sep only and prep only)
    #hs_cf_loose_match: mean of all CF's matched by taxa and (separation OR preparation)
    hs_cf_loose_match_1 <- hs_for_cf_matching %>%
      # Join by separation only:
      left_join(cf_data_clean, by = c("Taxa" = "Species",
                                      "Separation")) %>%
      rename(conversion_factor_loose = Conversion.factor,
             Preparation = Preparation.x) %>% # this is the original preparation column for hs_for_cf_matching (not to be confused with Preparation.y, which comes from cf_data_clean) 
      select(Code, Description, Taxa, Separation, Preparation, conversion_factor_loose) # matches only taxa and separation
    
    hs_cf_loose_match_2 <- hs_for_cf_matching %>%
      # Join by preparation only:
      left_join(cf_data_clean, by = c("Taxa" = "Species",
                                      "Preparation")) %>%
      rename(conversion_factor_loose = Conversion.factor,
             Separation = Separation.x) %>% # this is the original separation column for hs_for_cf_matching (not to be confused with Separation.y, which comes from cf_data_clean) 
      select(Code, Description, Taxa, Separation, Preparation, conversion_factor_loose) # matches only taxa and preparation
    
    hs_cf_loose_match <- hs_cf_loose_match_1 %>%
      bind_rows(hs_cf_loose_match_2) %>%
      filter(is.na(conversion_factor_loose)==FALSE) %>% # Need this for n() to be accurate
      group_by(Code, Description, Taxa) %>%
      summarise(mean_cf_loose = mean(conversion_factor_loose, na.rm = TRUE), 
                max_cf_loose = max(conversion_factor_loose, na.rm = TRUE), 
                min_cf_loose = min(conversion_factor_loose, na.rm = TRUE), 
                n_cf_loose = n()) %>%
      ungroup()
    
    # Now join with hs_cf_full_match, while creating new column CF_match to consolidate matching CF values and match_notes to indicate the type of match:
    hs_cf_match <- hs_cf_full_match %>%
      full_join(hs_cf_loose_match, by = intersect(names(hs_cf_full_match), names(hs_cf_loose_match))) %>%
      arrange(Code, Taxa) %>%
      # min and max functions return -Inf and Inf when there are missing values; replace these with NAs
      mutate_if(is.numeric, list(~na_if(., Inf))) %>% 
      mutate_if(is.numeric, list(~na_if(., -Inf))) %>%
      # now compile similar information (e.g., max_cf_full, max_cf_sep, and max_cf_prep) into one column (CF_max) while recording type of match in match_notes
      mutate(CF_match = if_else(is.na(mean_cf_full)==FALSE, true = mean_cf_full, false = NaN),
             CF_max = if_else(is.na(max_cf_full)==FALSE, true = max_cf_full, false = NaN),
             CF_min = if_else(is.na(min_cf_full)==FALSE, true = min_cf_full, false = NaN),
             CF_n = if_else(is.na(mean_cf_full)==FALSE, true = n_cf_full, false = as.integer(NA)), # NOTE: for CF_n need to test is.na(mean_cf_full) instead of is.na(n_cf_full), since n_cf_full is never NA
             match_notes = if_else(is.na(CF_match)==FALSE, true = "full_match", false = "no_cf_match")) %>%
      mutate(CF_match = if_else(is.na(mean_cf_loose)==FALSE & is.na(CF_match), true = mean_cf_loose, false = CF_match),
             CF_max = if_else(is.na(max_cf_loose)==FALSE & is.na(CF_max), true = max_cf_loose, false = CF_max),
             CF_min = if_else(is.na(min_cf_loose)==FALSE & is.na(CF_min), true = min_cf_loose, false = CF_min),
             CF_n = if_else(is.na(mean_cf_loose)==FALSE & is.na(CF_n), true = n_cf_loose, false = CF_n),  # NOTE: for CF_n need to test is.na(mean_cf_loose) instead of is.na(n_cf_loose), since n_cf_full is never NA
             match_notes = if_else(is.na(CF_match)==FALSE & match_notes=="no_cf_match", true = "loose_match", false = match_notes)) %>%
      select(Code, Description, Taxa, Separation, Preparation, CF_match, CF_max, CF_min, CF_n, match_notes)
    
    # Code below is for keeping separation only vs preparation only matches separate:
    # hs_cf_sep_match: mean of all CF's matched by taxa and separation
    #hs_cf_sep_match <- hs_for_cf_matching %>%
    #  left_join(cf_data_clean, by = c("Taxa" = "Species",
    #                                   "Separation")) %>%
    #  rename(conversion_factor_sep = Conversion.factor,
    #         Preparation_HS_code = Preparation.x) %>% 
    #  select(Code, Description, Taxa, Separation, Preparation_HS_code, conversion_factor_sep) %>% # matches only taxa and separation
    #  group_by(Code, Description, Taxa, Separation) %>%
    #  summarise(mean_cf_sep = mean(conversion_factor_sep, na.rm = TRUE), 
    #            stdev_cf_sep = sd(conversion_factor_sep, na.rm = TRUE), 
    #            max_cf_sep = max(conversion_factor_sep, na.rm = TRUE), 
    #            min_cf_sep = min(conversion_factor_sep, na.rm = TRUE), 
    #            n_cf_sep = n()) %>%
    #  ungroup()
    
    # hs_cf_prep_match: mean of all CF's matched by taxa and preparation  
    #hs_cf_prep_match <- hs_for_cf_matching %>%
    #  left_join(cf_data_clean, by = c("Taxa" = "Species",
    #                                  "Preparation")) %>%
    #  rename(conversion_factor_prep = Conversion.factor, # matches only taxa and preparation
    #         Separation_HS_code = Separation.x) %>%
    #  select(Code, Description, Taxa, Separation_HS_code, Preparation, conversion_factor_prep) %>%
    #  group_by(Code, Description, Taxa, Preparation) %>%
    #  summarise(mean_cf_prep = mean(conversion_factor_prep, na.rm = TRUE), 
    #            stdev_cf_prep = sd(conversion_factor_prep, na.rm = TRUE), 
    #            max_cf_prep = max(conversion_factor_prep, na.rm = TRUE), 
    #            min_cf_prep = min(conversion_factor_prep, na.rm = TRUE), 
    #            n_cf_prep = n()) %>%
    #  ungroup()
    
    # Now join all three versions together, while creating new column CF_match for matching CF values and match_notes to indicate the type of match
    # FIX IT - need to reconsider: code below prioritizes separation matches over preparation matches
    #hs_cf_match <- hs_cf_full_match %>%
    #  full_join(hs_cf_sep_match, by = intersect(names(hs_cf_full_match), names(hs_cf_sep_match))) %>%
    #  full_join(hs_cf_prep_match, by = intersect(names(.), names(hs_cf_prep_match))) %>%
    #  arrange(Code, Taxa) %>%
    # min and max functions return -Inf and Inf when there are missing values; replace these with NAs
    #  mutate_if(is.numeric, list(~na_if(., Inf))) %>% 
    #  mutate_if(is.numeric, list(~na_if(., -Inf))) %>%
    #  # now compile similar information (e.g., max_cf_full, max_cf_sep, and max_cf_prep) into one column (CF_max) while recording type of match in match_notes
    #  mutate(CF_match = if_else(is.na(mean_cf_full)==FALSE, true = mean_cf_full, false = NaN),
    #         CF_max = if_else(is.na(max_cf_full)==FALSE, true = max_cf_full, false = NaN),
    #         CF_min = if_else(is.na(min_cf_full)==FALSE, true = min_cf_full, false = NaN),
    #         CF_n = if_else(is.na(n_cf_full)==FALSE, true = n_cf_full, false = NaN),
    #         match_notes = if_else(is.na(CF_match)==FALSE, true = "full_match", false = "no_cf_match")) %>%
    #  mutate(CF_match = if_else(is.na(mean_cf_sep)==FALSE & is.na(CF_match), true = mean_cf_sep, false = CF_match),
    #         CF_max = if_else(is.na(max_cf_sep)==FALSE & is.na(CF_max), true = max_cf_sep, false = CF_max),
    #         CF_min = if_else(is.na(min_cf_sep)==FALSE & is.na(CF_min), true = min_cf_sep, false = CF_min),
    #         CF_n = if_else(is.na(n_cf_sep)==FALSE& is.na(CF_n), true = n_cf_sep, false = CF_n),
    #         match_notes = if_else(is.na(CF_match)==FALSE & match_notes=="no_cf_match", true = "sep_match", false = match_notes)) %>%
    #  mutate(CF_match = if_else(is.na(mean_cf_prep)==FALSE & is.na(CF_match), true = mean_cf_prep, false = CF_match),
    #         CF_max = if_else(is.na(max_cf_prep)==FALSE & is.na(CF_max), true = max_cf_prep, false = CF_max),
    #         CF_min = if_else(is.na(min_cf_prep)==FALSE & is.na(CF_min), true = min_cf_prep, false = CF_min),
    #         CF_n = if_else(is.na(n_cf_prep)==FALSE & is.na(CF_n), true = n_cf_prep, false = CF_n),
    #         match_notes = if_else(is.na(CF_match)==FALSE & match_notes=="no_cf_match", true = "prep_match", false = match_notes)) %>%
    #  select(Code, Description, Taxa, CF_match, CF_max, CF_min, CF_n, match_notes)
    
    ############################################################################################################
    # Now calculate final CF values by aggregating
    # Assumptions about Fishmeal and Fish oil production rates (Reference: IFFO, 2009)
    fm_rate <- .225
    fo_rate <- .05
    
    # Calculate/Match CF values based on match_criteria:
    hs_cf_match_calc <- hs_cf_match %>% 
      
      # ASSIGN: assign values to live fish, FMFOs, and caviar=
      mutate(CF_calc = case_when(str_detect(Code, "^0301") ~ 1, # LIVE/WHOLE FISH
                                 # str_detect(Code, "^0302") ~ 1, 
                                 # FISH OILS
                                 str_detect(Code, "^1504") ~ 0,
                                 # FISH FLOUR, MEALS, PELLETS
                                 str_detect(Code, pattern = "030510") ~ 1/(fm_rate+fo_rate) * (fm_rate/(fm_rate+fo_rate)),
                                 str_detect(Code, pattern = "0306[1-9]9") ~ 1/(fm_rate+fo_rate) * (fm_rate/(fm_rate+fo_rate)),
                                 str_detect(Code, pattern = "^03079") ~ 1/(fm_rate+fo_rate) * (fm_rate/(fm_rate+fo_rate)),
                                 str_detect(Code, pattern = "030890") ~ 1/(fm_rate+fo_rate) * (fm_rate/(fm_rate+fo_rate)),
                                 str_detect(Code, pattern = "051191") ~ 1/(fm_rate+fo_rate) * (fm_rate/(fm_rate+fo_rate)),
                                 str_detect(Code, pattern = "230120") ~ 1/(fm_rate+fo_rate) * (fm_rate/(fm_rate+fo_rate)),
                                 # CAVIAR:
                                 str_detect(Description, pattern = "caviar") ~ 0,
                                 TRUE ~ NaN)) %>%
      mutate(calc_notes = if_else(is.na(CF_calc) == FALSE, true = "assign", false = "no_CF")) %>%
      
      
      # FULL_MATCH: if a full_match CF value exists (taxa + preparation + separation) copy this to CF_calc and record this in calc_notes column 
      mutate(CF_calc = if_else(is.na(CF_calc) & match_notes == "full_match", true = CF_match, false = CF_calc),
             calc_notes = if_else(is.na(CF_calc) == FALSE & calc_notes == "no_CF", true = "full_match", false = calc_notes)) %>%
      
      # OUTPUT OPTION: if we want loose matching to be allowed, allow other CF values (not just those that were a "full_match") to be considered when calculating mean(CF_match)
      { if (match_criteria == "loose")
        # LOOSE_MATCH: calculated as the mean of all loose_match grouped by Code + Taxa
        group_by(., Code, Taxa) %>%
          mutate(CF_calc = if_else(is.na(CF_calc), true = mean(CF_match, na.rm = TRUE), false = CF_calc),
                 calc_notes = if_else(is.na(CF_calc)==FALSE & calc_notes == "no_CF", true = "loose_match", false = calc_notes)) %>%
          
          # CODE_AVERAGE: calculated as the mean of all CF values (full, sep, and/or prep match) within an HS code
          group_by(Code) %>%
          mutate(CF_calc = if_else(is.na(CF_calc), true = mean(CF_match, na.rm = TRUE), false = CF_calc),
                 calc_notes = if_else(is.na(CF_calc)==FALSE & calc_notes == "no_CF", true = "code_average", false = calc_notes)) %>%
          ungroup() 
        
        else . # else, only return the full matches 
        
      }
    
    # OUTPUT OPTION: if we only want fillets matches to be used, remove all non-fillet CF values
    if (match_criteria == "fillet") {
      
      fillet_list <- hs_cf_match_calc %>%
        filter(str_detect(Code, pattern = "^0304[1-4,6-8]")==TRUE|
                 str_detect(Code, pattern = "^0305[3-4]")) %>%
        pull(Code) %>%
        unique()
      
      hs_cf_match_calc <- hs_cf_match_calc %>%
        mutate(CF_calc = if_else(Code %in% fillet_list | calc_notes == "assign", true = CF_calc, false = NaN))
      
    }
 
    
    # Get list of missing CF values: 
    hs_cf_missing <- hs_cf_match_calc %>% filter(is.na(CF_calc)) %>% select(Code, Description) %>% unique()
    
    # Join EUMOFA data with hs_cf_match_final and use EUMOFA data to fill in missing values calculated from cf_data
    
    hs_cf_match_final <- hs_cf_match_calc %>%
      left_join(eumofa_for_matching, by = c("Code" = "CN.8")) %>%
      mutate(CF_calc = if_else(is.na(CF_calc), true = mean_EUMOFA, false = CF_calc),
             calc_notes = if_else(is.na(CF_calc) == FALSE & calc_notes == "no_CF", true = "mean_EUMOFA", false = calc_notes)) %>%
      ungroup() %>%
      mutate(CF_live_to_commod = 1/CF_calc) %>%
      select(Code, Taxa, Description, CF_calc, CF_max, CF_min, CF_n, match_notes, calc_notes, mean_EUMOFA, max_EUMOFA, min_EUMOFA, n_EUMOFA, CF_live_to_commod)
    
  } # end section: if (match_criteria %in% c("strict", "loose", "fillets")) 
  
  # Sometimes CF_live_to_commod = Inf (comes from when CF_calc = 0; i.e., 1 divided by 0); Replace CF_live_to_commod Infiniti values with 0
  hs_cf_match_final$CF_live_to_commod[hs_cf_match_final$CF_live_to_commod == Inf] <- 0
  
  ############################################################################################################
  # Output data and plots
    
  # Use this to identify code/taxa pairs that still don't have a CF value
  #hs_cf_match_final %>%
  #  filter(is.na(CF_calc)) %>%
  #  select(Code, Description) %>%
  #  unique() %>% as_tibble()
  
  # Check that "calc_notes" column no longer has a "no_calc" group
  #table(hs_cf_match_final$calc_notes)
  
  
  # plot eumofa vs cf_data for comparison to 1:1 line
  #p <- ggplot(data = (hs_cf_match_final %>% select(CF_calc, mean_EUMOFA, calc_notes) %>% filter(is.na(mean_EUMOFA)==FALSE) %>% unique()), aes(x = CF_calc, y = mean_EUMOFA)) +
  #  geom_point(aes(color = calc_notes), alpha = 0.4) +
  #  geom_abline(slope = 1, intercept = 0) + 
  #  labs(x = "compiled CF values", y = "EUMOFA CF values", title = match_criteria) #+
    #xlim(1, 4) +
    #ylim(1, 4)
  
  #pdf_name <- paste("plot_CF_values_", match_criteria, "_match_", Sys.Date(), ".pdf", sep = "")
  #pdf(file.path(outdir, pdf_name))
  #print(p)
  #dev.off()
  
  hs_cf_match_final <- hs_cf_match_final %>%
    mutate(CF_calc = case_when(
      max_EUMOFA == 0 ~ 0, # FIX IT: Need to determine whether to accept EUMOFA's definition of coproduct or not, (CF_max + CF_min) / 2,
      is.na(CF_n) ~ mean_EUMOFA,
      TRUE ~ ((CF_max + CF_min) / 2) * (CF_n / (CF_n + n_EUMOFA)) + ((max_EUMOFA + min_EUMOFA) / 2) * (n_EUMOFA / (CF_n + n_EUMOFA))
    )) %>% 
    mutate(CF_calc = case_when(str_detect(Code, "^0301") ~ 1, # LIVE/WHOLE FISH
                               # str_detect(Code, "^0302") ~ 1,
                               # FISH OILS
                               str_detect(Code, "^1504") ~ 0,
                               # FISH FLOUR, MEALS, PELLETS
                               str_detect(Code, pattern = "030510") ~ 1/(fm_rate+fo_rate) * (fm_rate/(fm_rate+fo_rate)),
                               str_detect(Code, pattern = "0306[1-9]9") ~ 1/(fm_rate+fo_rate) * (fm_rate/(fm_rate+fo_rate)),
                               str_detect(Code, pattern = "^03079") ~ 1/(fm_rate+fo_rate) * (fm_rate/(fm_rate+fo_rate)),
                               str_detect(Code, pattern = "030890") ~ 1/(fm_rate+fo_rate) * (fm_rate/(fm_rate+fo_rate)),
                               str_detect(Code, pattern = "051191") ~ 1/(fm_rate+fo_rate) * (fm_rate/(fm_rate+fo_rate)),
                               str_detect(Code, pattern = "230120") ~ 1/(fm_rate+fo_rate) * (fm_rate/(fm_rate+fo_rate)),
                               # CAVIAR:
                               str_detect(Description, pattern = "caviar") ~ 0,
                               # FIX IT: similar str detect for roe
                               TRUE ~ CF_calc)) %>%
    mutate(calc_notes = if_else(is.na(CF_calc) == FALSE, true = "assign", false = "no_CF"))
  
  # Check to make sure all CFs are non NA
  if (sum(is.na(hs_cf_match_final$CF_calc)) > 0) {
    warning("NAs in hs taxa CFs")
  }
  
  return(hs_cf_match_final)
  
}
