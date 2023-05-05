#' @import stringr
#' @import dplyr 
#' @export
clean_hs <- function(hs_data_raw){
  # Run ALL HS codes through clean_hs function to:
  # 1 correct spelling mistakes in HS code descriptions
  # 2 standardize HS code descriptions: e.g., all genera should be written as <genera spp>, species-level names should be in parentheses separated by commas, etc
  # 3 add scientific classification info to common names (e.g., add "Pleuronectiformes" to code descriptions for flatfish)
  # 4 create columns that will be used to match taxa to HS Codes by scientific classification (species, genera, family, order, and class) and Commodity Type (i.e., Fish, Mollusc, Crustacean, or Aquatic Invertebrate)
  # 5 run all species names through rfishbase and if outdated, replace with the current accepted scientific name (synonym function in rfishbase)
  
  # FIX IT - Use OR statement for the conditional: e.g., str_detect(pattern = "XXXX") | str_detect(patern = "XXXX" and use paste() instead of str_replace to append descriptions
  # See "Sharks" below as an example
  # FIX IT - convert to case_when instead of if_else for easier readability (see 030419 as example)
  
  hs_data <- hs_data_raw %>%
    filter(nchar(Code)==6) %>% # ONLY KEEP 6 digit codes for analysis
    mutate(Description = tolower(Description)) # ALL DESCRIPTIONS SET TO LOWER HERE
  # further downstream, use function str_to_sentence to format species names as (upper case) Genus + (lower case) species 
  
  #########################################################################
  # Correct spelling mistakes and other adjustments to HS Code Descriptions
  # Note: the following descriptions are cleaned manually (these names aren't recognized by synonyms function in rfishbase)
  hs_data$Description <- gsub(hs_data$Description, pattern="alaska pollack", replacement = "alaska pollock")
  hs_data$Description <- gsub(hs_data$Description, pattern = "theraga chalcogramma", replacement = "theragra chalcogramma")
  hs_data$Description <- gsub(hs_data$Description, pattern="engrails", replacement = "engraulis")
  hs_data$Description <- gsub(hs_data$Description, pattern = "holothurioidea", replacement = "holothuroidea")
  hs_data$Description <- gsub(hs_data$Description, pattern = "clupea harengas", replacement = "clupea harengus")
  hs_data$Description <- gsub(hs_data$Description, pattern = "echichinus esculentus", replacement = "echinus esculentus")
  hs_data$Description <- gsub(hs_data$Description, pattern = "aequi opercularis", replacement = "aequipecten opercularis")
  hs_data$Description <- gsub(hs_data$Description, pattern = " ephrops norvegicus", replacement = " nephrops norvegicus") # Note: need space before "ephrops" so it doesn't match with "nephrops"
  
  # Assumption in downstream code is that all genera are indicated as "<genera> spp"
  # Rewrite descriptions that are exceptions to this: "of the genus XXX" and "of the genera XXX"
  # Rewrite (Aguilla) and (Channa) as (Aguilla spp.) annd (Channa spp.)
  # check_grep <- grep("of the genus|of the genera", x = hs_data$Description)
 
  hs_data <- hs_data %>%
    mutate(Description = if_else(str_detect(string = hs_data$Description, pattern = "of the genera pecten, chlamys or placopecten"), 
                                 str_replace(hs_data$Description, pattern = "of the genera pecten, chlamys or placopecten", replacement = "of pecten spp., chlamy spp., or placopecten spp."),
                                 Description))

  hs_data <- hs_data %>%
    mutate(Description =  if_else(str_detect(string = hs_data$Description, pattern = "of the genus thunnus"), 
                                  str_replace(hs_data$Description, pattern = "of the genus thunnus", replacement = "of thunnus spp."),
                                  Description))
  
  # check_grep <- grep(regex("\\(Anguilla\\)|\\(Channa\\)", ignore_case = TRUE), x = hs_data$Description)
  hs_data <- hs_data %>%
    mutate(Description = if_else(str_detect(string = hs_data$Description, pattern = "\\(anguilla\\)"), 
                                 str_replace(hs_data$Description, pattern = "\\(anguilla\\)", replacement = "\\(anguilla spp.\\)"),
                                 Description))
  
  hs_data <- hs_data %>%
    mutate(Description = if_else(str_detect(string = hs_data$Description, pattern = "\\(channa\\)"),
                                 str_replace(hs_data$Description, pattern = "\\(channa\\)", replacement = "\\(channa spp.\\)"),
                                 Description))
  
  ###############################################################
  # Fix descriptions that are poorly written, not specific enough
  # Description for code 030249 does not include fish taxa. Look up HS code on HTS.usitc.gov to get official list of taxa that should be included. There should be a heading that list possible taxa for each 5-digit family of codes:
  # Full list: Herrings (Clupea harengus, Clupea pallasii), anchovies (Engraulis spp.), sardines (Sardina pilchardus, Sardinops spp.), sardinella (Sardinella spp.), brisling or sprats (Sprattus sprattus), 
  # mackerel (Scomber scombrus, Scomber australasicus, Scomber japonicus), Indian mackerels (Rastrelliger spp.), seerfishes (Scomberomorus spp.), jack and horse mackerel (Trachurus spp.),
  # jacks, crevalles (Caranx spp.), cobia (Rachycentron canadum), silver pomfrets (Pampus spp.), Pacific saury (Cololabis saira), scads (Decapterus spp.), capelin (Mallotus villosus), 
  # swordfish (Xiphias gladius), Kawakawa (Euthynnus affinis), bonitos (Sarda spp.), marlins, sailfishes, spearfish (Istiophoridae), excluding edible fish offal of subheadings 0302.91 to 0302.99:
  # From full taxa list above, only include those taxa not already mentioned in the 0302.4 heading
  hs_data <- hs_data %>%
    mutate(Description = if_else(str_detect(string = hs_data$Code, pattern = "030249"), 
                                 str_replace(hs_data$Description, pattern = "fresh or chilled,", 
                                             replacement = "fresh or chilled (rastrelliger spp., scomberomorus spp., caranx spp., pampus spp., 
                                             cololabis saira, decapterus spp., mallotus villosus, euthynnus affinis, sarda spp., istiophoridae),"),
                                 Description))
  
  # Description for Code 030259 does not include fish taxa. Look up HS code on HTS.usitc.gov to get official list of taxa that should be included. There should be a heading that list possible taxa for each 5-digit family of codes.
  # Note, the list of possible taxa in 2012 (H4 classification) was later split into two in 2017 (H5 classification)
  # For H5 Classification year: Bregmacerotidae, Euclichthyidae Gadidae, Macrouridae, Melanonidae, Merlucciidae, Moridae and Muraenolepididae
  hs_data <- hs_data %>%
    mutate(Description = if_else(str_detect(string = hs_data$Code, pattern = "030259") & hs_data$Classification == "H5", 
                                 str_replace(hs_data$Description, pattern = "fresh or chilled,", 
                                             replacement = "fresh or chilled, bregmacerotidae, euclichthyidae, gadidae, macrouridae, melanonidae, merlucciidae, moridae, and muraenolepididae"),
                                 Description))
  
  # For H4 Classification year, 030259 is a combination of 030249 and 030259 from H5 Classification year
  hs_data <- hs_data %>%
    mutate(Description = if_else(str_detect(string = hs_data$Code, pattern = "030259") & hs_data$Classification == "H4", 
                                 str_replace(hs_data$Description, pattern = "fresh or chilled,", 
                                             replacement = "fresh or chilled (rastrelliger spp., scomberomorus spp., caranx spp., pampus spp., cololabis saira, decapterus spp.,
                                             mallotus villosus, euthynnus affinis, sarda spp., istiophoridae, bregmacerotidae, euclichthyidae, gadidae, macrouridae, melanonidae, 
                                             merlucciidae, moridae, muraenolepididae"),
                                 Description))
  
  # Description for 030359 does not include taxa:
  # Herrings (Clupea harengus, Clupea pallasii), anchovies (Engraulis spp.), sardines (Sardina pilchardus, Sardinops spp.), sardinella (Sardinella spp.), brisling or sprats (Sprattus sprattus), 
  # mackerel (Scomber scombrus, Scomber australasicus, Scomber japonicus), Indian mackerels (Rastrelliger spp.), seerfishes (Scomberomorus spp.), jack and horse mackerel (Trachurus spp.), 
  # jacks, crevalles (Caranx spp.), cobia (Rachycentron canadum), silver pomfrets (Pampus spp.), Pacific saury (Cololabis saira), scads (Decapterus spp.), capelin (Mallotus villosus), swordfish (Xiphias gladius), 
  # Kawakawa (Euthynnus affinis), bonitos (Sarda spp.), marlins, sailfishes, spearfish (Istiophoridae), excluding edible fish offal of subheadings 0303.91 to 0303.99:
  # From full taxa list above, only include those taxa not already mentioned in the 0303.5 heading
  hs_data <- hs_data %>%
    mutate(Description = if_else(str_detect(string = hs_data$Code, pattern = "030359"), 
                                 str_replace(hs_data$Description, pattern = "frozen,", 
                                             replacement = "frozen (rastrelliger spp., scomberomorus spp., caranx spp., pampus spp., cololabis saira, decapterus spp., 
                                             mallotus villosus, euthynnus affinis, sarda spp., istiophoridae),"),
                                 Description))
  
  
  # Description for Codes 030419, 030429, and 030499 break from rest of parent group 0304 by indicating species rather than heading number
  # Heading number is needed to identify codes where remaining possible_prod_taxa should be matched and then reset
  hs_data <- hs_data %>%
    mutate(Description = case_when(Code == "030419" ~ str_replace(hs_data$Description, pattern = "swordfish \\(xiphias gladius\\) and toothfish \\(dissostichus spp.\\)", 
                                                                  replacement = "fish of heading 0304.1"),
                                   Code == "030429" ~ str_replace(hs_data$Description, pattern = "swordfish \\(xiphias gladius\\) and toothfish \\(dissostichus spp.\\)", 
                                                                  replacement = "fish of heading 0304.2"),
                                   Code == "030499" ~ str_replace(hs_data$Description, pattern = "swordfish \\(xiphias gladius\\) and toothfish \\(dissostichus spp.\\)", 
                                                                  replacement = "fish of heading 0304.9"),
                                   TRUE ~ Description))
    
    
    # mutate(Description = if_else(str_detect(string = hs_data$Code, pattern = "030419"), 
    #                              str_replace(hs_data$Description, pattern = "swordfish \\(xiphias gladius\\) and toothfish \\(dissostichus spp.\\)", 
    #                                          replacement = "fish of heading 0304.1"),
    #                              if_else(str_detect(string = hs_data$Code, pattern = "030429"), 
    #                                      str_replace(hs_data$Description, pattern = "swordfish \\(xiphias gladius\\) and toothfish \\(dissostichus spp.\\)", 
    #                                                  replacement = "fish of heading 0304.2"),
    #                                      if_else(str_detect(string = hs_data$Code, pattern = "030499"),
    #                                              str_replace())
    #                                      Description)))
  
  # Description for Codes 030559 (specific to H4 Classification) breaks from rest of parent group 0305 by indicating species rather than heading number
  # Heading number is needed to identify codes where remaining possible_prod_taxa should be matched and then reset
  # Pattern: other than cod (Gadus morhua, Gadus ogac, Gadus macrocephalus)
  # Replacement: n.e.c. in item no. 0305.5
  hs_data <- hs_data %>%
    mutate(Description = if_else(str_detect(string = hs_data$Code, pattern = "030559"), 
                                 str_replace(hs_data$Description, pattern = "other than cod \\(gadus morhua, gadus ogac, gadus macrocephalus\\)", 
                                             replacement = "n.e.c. in item no. 0305.5"),
                                 Description))
  
  
  # Description for code 030890 is a "true" NEC code in that all taxa not matched to other codes in this parent group should go here
  # Change Description to have "NEC" pattern so that it fits into matching logic
  hs_data <- hs_data %>%
    mutate(Description = if_else(str_detect(string = hs_data$Code, pattern = "030890"), 
                                 str_replace(hs_data$Description, pattern = "other than crustaceans, molluscs, sea urchins, sea cucumbers and jellyfish", 
                                             replacement = "n.e.c. in heading 0308"),
                                 Description))
  
  
  ##############################################################################################################
  # Deal with species names that are weirdly formatted (violates assumption of comma-delimited lists of species)
  hs_data <- hs_data %>%
    mutate(Description = if_else(str_detect(string = hs_data$Description, pattern = "oncorhynchus mykiss/clarki/aguabonita/gilae/apache/chrysogaster"), 
                                 str_replace(hs_data$Description, pattern = "oncorhynchus mykiss/clarki/aguabonita/gilae/apache/chrysogaster",
                                             replacement = "oncorhynchus mykiss, oncorhynchus clarki, oncorhynchus aguabonita, 
                                             oncorhynchus gilae, oncorhynchus apache, oncorhynchus chrysogaster"),
                                 
                                 if_else(str_detect(string = hs_data$Description, pattern = "oncorhynchus nerka, gorbuscha, keta, tschawytscha, kisutch, masou, rhodurus"), 
                                         str_replace(hs_data$Description, pattern = "oncorhynchus nerka, gorbuscha, keta, tschawytscha, kisutch, masou, rhodurus",
                                                     replacement = "oncorhynchus nerka, oncorhynchus gorbuscha, oncorhynchus keta, oncorhynchus tschawytscha, 
                                                     oncorhynchus kisutch, oncorhynchus masou, oncorhynchus rhodurus"),
                                         
                                         if_else(str_detect(string = hs_data$Description, pattern = "oncorhynchus gorbuscha/keta/tschawytscha/ kisutch/masou/rhodurus"), 
                                                 str_replace(hs_data$Description, pattern = "oncorhynchus gorbuscha/keta/tschawytscha/ kisutch/masou/rhodurus",
                                                             replacement = "oncorhynchus gorbuscha, oncorhynchus keta, oncorhynchus tschawytscha, 
                                                             oncorhynchus kisutch, oncorhynchus masou, oncorhynchus rhodurus"),
                                                 
                                                 if_else(str_detect(string = hs_data$Description, 
                                                                    pattern = "cyprinus/carassius/ctenopharyngodon idellus/hypophthalmichthys/cirrhinus/mylopharyngodon piceus/catla catla/labeo/osteochilus hasselti/leptobarbus hoeveni/megalobrama"), 
                                                         str_replace(hs_data$Description, 
                                                                     pattern = "cyprinus/carassius/ctenopharyngodon idellus/hypophthalmichthys/cirrhinus/mylopharyngodon piceus/catla catla/labeo/osteochilus hasselti/leptobarbus hoeveni/megalobrama",
                                                                     replacement = "cyprinus spp., carassius spp., ctenopharyngodon idellus, hypophthalmichthys spp., cirrhinus spp., mylopharyngodon piceus, catla catla, labeo spp., 
                                                                     osteochilus hasselti, leptobarbus hoeveni, megalobrama spp."),
                                                         
                                                         if_else(str_detect(string = hs_data$Description, pattern = "oncorhynchus gorbuscha/keta/tschawytscha/kisutch/masou/rhodurus"), 
                                                                 str_replace(hs_data$Description, pattern = "oncorhynchus gorbuscha/keta/tschawytscha/kisutch/masou/rhodurus",
                                                                             replacement = "oncorhynchus gorbuscha, oncorhynchus keta, oncorhynchus tschawytscha, oncorhynchus kisutch, oncorhynchus masou, oncorhynchus rhodurus"),
                                                                             Description))))))
  
   
  ################################################################################################################
  # Add scientific classification info to code descriptions to enable HS code to prod_taxa_classification matching
  
  # FLAT FISH defined as: Pleuronectiformes
  # hs_data$Description[grep("flat fish", hs_data$Description)] check which rows should be changed
  hs_data <- hs_data %>%
    mutate(Description = if_else(str_detect(string = hs_data$Description, pattern = "flat fish"), 
                                 str_replace(hs_data$Description, pattern = "flat fish", replacement = "flat fish, pleuronectiformes"),
                                 Description))
  
  # SKIPJACK (or stripe-bellied bonito) defined as: "(Katsuwonus pelamis)" - parentheses important because this is what downstream code uses to extract species 
  # hs_data$Description[grep("skipjack", hs_data$Description)] check which rows should be changed
  # Match pattern with "skipjack" instead of "stripe-bellied bonito" so that it doesn't get confused with "bonito" (Sarda spp.)
  hs_data <- hs_data %>%
    mutate(Description = if_else(str_detect(string = hs_data$Description, pattern = "skipjack"), 
                                 str_replace(hs_data$Description, pattern = "skipjack", replacement = "skipjack \\(katsuwonus pelamis\\)"),
                                 Description))
  
  # SKIPJACK, in some cases is defined as: (Euthynnus (Katsuwonus) pelamis)
  # Weirdly formatted species name, just remove this
  # hs_data$Description[grep("Katsuwonus", x = hs_data$Description)]
  hs_data <- hs_data %>%
    mutate(Description = if_else(str_detect(string = hs_data$Description, pattern = "\\(euthynnus \\(katsuwonus\\) pelamis\\)"), 
                                 str_remove(hs_data$Description, pattern = "\\(euthynnus \\(katsuwonus\\) pelamis\\)"),
                                 Description))
  
  
  # TUNA defined as part of the "Thunnini tribe", specify this by including genera: 
  # Note: need to grep Descriptions that have "tuna" but not "Thunnus" (or "thunnus") - i.e., don't already give species name
  # grep_check <- grepl("tuna", hs_data$Description)==TRUE & grepl("Thunnus|thunnus", hs_data$Description)==FALSE
  hs_data <- hs_data %>% 
    mutate(Description = if_else(grepl("tuna", hs_data$Description)==TRUE & grepl("thunnus", hs_data$Description)==FALSE, 
                                 str_replace(hs_data$Description, pattern = "tuna", replacement = "tuna (thunnus spp., allothunnus spp., auxis spp., euthynnus spp., katsuwonus spp.)"),
                                 Description))

  # SHARKS defined as: (Order %in% c("carcharhiniformes", "heterodontiformes", "hexanchiformes", "lamniformes", "orectolobiformes", "pristiophoriformes", "squaliformes", "squantiniformes"))
  # Note: Descriptions with "dogfish" already included as part of squaliformes
  # Note: Do this replacement for both "sharks" and "shark fins" 
  # OLD CODE:
  # hs_data$Description[grep("sharks|shark fins", x = hs_data$Description)]
  # hs_data <- hs_data %>% 
  #   mutate(Description = if_else(grepl("sharks", hs_data$Description), 
  #                                str_replace(hs_data$Description, pattern = "sharks", replacement = "sharks (carcharhiniformes, chimaeriformes, heterodontiformes, hexanchiformes, lamniformes, orectolobiformes, pristiophoriformes, squaliformes, squantiniformes)"),
  #                                if_else(grepl("shark fins", hs_data$Description),
  #                                        str_replace(hs_data$Description, pattern = "shark fins", replacement = "shark fins (carcharhiniformes, chimaeriformes, heterodontiformes, hexanchiformes, lamniformes, orectolobiformes, pristiophoriformes, squaliformes, squantiniformes)"),
  #                                Description)))

  # FASTER:
  hs_data <- hs_data %>% 
    mutate(Description = if_else(grepl("sharks|shark fins", hs_data$Description), 
                                 paste(hs_data$Description, " (carcharhiniformes, chimaeriformes, heterodontiformes, hexanchiformes, lamniformes, orectolobiformes, 
                                       pristiophoriformes, squaliformes, squantiniformes)", sep = ""),
                                 Description))
  
  
  # CARP defined as: "Cyprinus spp., Carassius spp., Ctenopharyngodon idellus, Hypophthalmichthys spp., Cirrhinus spp., Mylopharyngodon piceus, Catla catla, Labeo spp., Osteochilus hasselti, Leptobarbus hoeveni, Megalobrama spp."
  # Only insert definitions for those descriptions that don't already have a carp definition (i.e., Cyprinus == FALSE)
  # FIX IT Note: HS code descriptions of carp change through time, sometimes more explicit about species vs. genera, sometimes just defined as "Carp (as specified by the WCO)"
  # FIX IT One idea: write code to copy taxa definition (if it exists) from the same year
  # check_grep <- (grepl("carp", hs_data$Description)==TRUE & grepl("Cyprinus", hs_data$Description)==FALSE)
  hs_data <- hs_data %>%
    mutate(Description = if_else(str_detect(string = hs_data$Description, pattern = "carp")==TRUE & str_detect(string = hs_data$Description, pattern = "cyprinus")==FALSE, 
                                 str_replace(hs_data$Description, pattern = "carp", 
                                             replacement = "carp (cyprinus spp., carassius spp., ctenopharyngodon idellus, hypophthalmichthys spp., 
                                             cirrhinus spp., mylopharyngodon piceus, catla catla, labeo spp., osteochilus hasselti, leptobarbus hoeveni, megalobrama spp.)"),
                                 Description))
  
  # TILAPIA defined as: Oreochromis spp.
  # Some codes already have a definition so only insert definitions for those that don't already have one (i.e., Oreochromis == FALSE)
  hs_data <- hs_data %>%
    mutate(Description = if_else(str_detect(string = hs_data$Description, pattern = "tilapia")==TRUE & str_detect(string = hs_data$Description, pattern = "oreochromis")==FALSE, 
                                 str_replace(hs_data$Description, pattern = "tilapia", 
                                             replacement = "tilapia (oreochromis spp.)"),
                                 Description))
  
  # CATFISH defined as: Pangasius spp., Silurus spp., Clarias spp., Ictalurus spp.
  hs_data <- hs_data %>%
    mutate(Description = if_else(str_detect(string = hs_data$Description, pattern = "catfish")==TRUE & str_detect(string = hs_data$Description, pattern = "pangasius")==FALSE, 
                                 str_replace(hs_data$Description, pattern = "catfish", 
                                             replacement = "catfish (pangasius spp., silurus spp., clarias spp., ictalurus spp.)"),
                                 Description))
  
  # EEL defined as Anguilla sp.
  hs_data <- hs_data %>%
    mutate(Description = if_else(str_detect(string = hs_data$Description, pattern = "eel")==TRUE & str_detect(string = hs_data$Description, pattern = "anguilla")==FALSE, 
                                 str_replace(hs_data$Description, pattern = "eel", 
                                             replacement = "eel (anguilla spp.)"),
                                 Description))
  
  # NILE PERCH defined as: Lates niloticus
  hs_data <- hs_data %>%
    mutate(Description = if_else(str_detect(string = hs_data$Description, pattern = "nile perch")==TRUE & str_detect(string = hs_data$Description, pattern = "lates")==FALSE, 
                                 str_replace(hs_data$Description, pattern = "nile perch", 
                                             replacement = "nile perch (lates niloticus)"),
                                 Description))
  
  # SNAKEHEAD defined as: Channa spp.
  hs_data <- hs_data %>%
    mutate(Description = if_else(str_detect(string = hs_data$Description, pattern = "snakehead")==TRUE & str_detect(string = hs_data$Description, pattern = "channa")==FALSE, 
                                 str_replace(hs_data$Description, pattern = "snakehead", 
                                             replacement = "snakehead (channa spp.)"),
                                 Description))
  
  # HERRING defined as: Clupea harengus, Clupea pallasii
  hs_data <- hs_data %>%
    mutate(Description = if_else(str_detect(string = hs_data$Description, pattern = "herring")==TRUE & str_detect(string = hs_data$Description, pattern = "clupea")==FALSE, 
                                 str_replace(hs_data$Description, pattern = "herring", 
                                             replacement = "herring (clupea harengus, clupea pallasii)"),
                                 Description))
  
  # SARDINES defined as: Sardina pilchardus, Sardinops spp.
  hs_data <- hs_data %>%
    mutate(Description = if_else(str_detect(string = hs_data$Description, pattern = "sardine")==TRUE & str_detect(string = hs_data$Description, pattern = "sardina")==FALSE, 
                                 str_replace(hs_data$Description, pattern = "sardine", 
                                             replacement = "sardine (sardina pilchardus, sardinops spp.)"),
                                 Description))
  
  # ANCHOVY defined as: Engraulis spp.
  hs_data <- hs_data %>%
    mutate(Description = if_else(str_detect(string = hs_data$Description, pattern = "anchov")==TRUE & str_detect(string = hs_data$Description, pattern = "engraulis")==FALSE, 
                                 str_replace(hs_data$Description, pattern = "anchov", 
                                             replacement = "anchov (engraulis spp.)"),
                                 Description))
  
  # SARDINELLA defined as: Sardinella spp.
  hs_data <- hs_data %>%
    mutate(Description = if_else(str_detect(string = hs_data$Description, pattern = "sardinella")==TRUE & str_detect(string = hs_data$Description, pattern = "sardinella spp.")==FALSE, 
                                 str_replace(hs_data$Description, pattern = "sardinella", 
                                             replacement = "sardinella (sardinella spp.)"),
                                 Description))
  
  # BRISLINGS or SPRATS defined as: Sprattus sprattus
  hs_data <- hs_data %>%
    mutate(Description = if_else(str_detect(string = hs_data$Description, pattern = "sprat")==TRUE & str_detect(string = hs_data$Description, pattern = "sprattus")==FALSE, 
                                 str_replace(hs_data$Description, pattern = "sprat", 
                                             replacement = "sprat (sprattus sprattus)"),
                                 Description))
  
  # MACKEREL defined as: Scomber scombrus, Scomber australasicus, Scomber japonicus
  hs_data <- hs_data %>%
    mutate(Description = if_else(str_detect(string = hs_data$Description, pattern = "mackerel")==TRUE & str_detect(string = hs_data$Description, pattern = "scomber")==FALSE, 
                                 str_replace(hs_data$Description, pattern = "mackerel", 
                                             replacement = "mackerel (scomber scombrus, scomber australasicus, scomber japonicus)"),
                                 Description))
  
  # INDIAN, JACK, and HORSE MACKEREL defined as: Rastrelliger spp. (indian) and Trachurus spp. (jack and horse)
  hs_data <- hs_data %>%
    mutate(Description = if_else(str_detect(string = hs_data$Description, pattern = "\\(incl indian, jack, or horse\\)")==TRUE, 
                                 str_replace(hs_data$Description, pattern = "\\(incl indian, jack, or horse\\)", 
                                             replacement = ", indian, jack, or horse mackerel (rastrelliger spp., trachurus spp.)"),
                                 Description))
  
  # BONITOS defined as: Sarda spp.
  hs_data <- hs_data %>%
    mutate(Description = if_else(str_detect(string = hs_data$Description, pattern = "bonito")==TRUE & str_detect(string = hs_data$Description, pattern = "stripe-bellied")==FALSE, 
                                 str_replace(hs_data$Description, pattern = "bonito", 
                                             replacement = "bonito (sarda spp.)"),
                                 Description))
  
  # COBIA defined as: Rachycentron canadum
  hs_data <- hs_data %>%
    mutate(Description = if_else(str_detect(string = hs_data$Description, pattern = "cobia")==TRUE & str_detect(string = hs_data$Description, pattern = "rachycentron")==FALSE, 
                                 str_replace(hs_data$Description, pattern = "cobia", 
                                             replacement = "cobia (rachycentron canadum)"),
                                 Description))
  
  # SWORDFISH defined as: Xiphias gladius
  hs_data <- hs_data %>%
    mutate(Description = if_else(str_detect(string = hs_data$Description, pattern = "swordfish")==TRUE & str_detect(string = hs_data$Description, pattern = "xiphias")==FALSE, 
                                 str_replace(hs_data$Description, pattern = "swordfish", 
                                             replacement = "swordfish (xiphias gladius)"),
                                 Description))
  
  # SEERFISH defined as: Scomberomorus spp.
  hs_data <- hs_data %>%
    mutate(Description = if_else(str_detect(string = hs_data$Description, pattern = "seerfish")==TRUE & str_detect(string = hs_data$Description, pattern = "scomberomorus")==FALSE, 
                                 str_replace(hs_data$Description, pattern = "seerfish", 
                                             replacement = "seerfish (scomberomorus spp.)"),
                                 Description))
  
  # JACKS and CREVALLES defined as: Caranx spp.
  hs_data <- hs_data %>%
    mutate(Description = if_else(str_detect(string = hs_data$Description, pattern = "crevalles")==TRUE & str_detect(string = hs_data$Description, pattern = "caranx")==FALSE, 
                                 str_replace(hs_data$Description, pattern = "crevalles", 
                                             replacement = "crevalles (caranx spp.)"),
                                 Description))
  
  # SILVER POMFRETS defined as: Pampus spp.
  hs_data <- hs_data %>%
    mutate(Description = if_else(str_detect(string = hs_data$Description, pattern = "silver pomfret")==TRUE & str_detect(string = hs_data$Description, pattern = "pampus")==FALSE, 
                                 str_replace(hs_data$Description, pattern = "silver pomfret", 
                                             replacement = "silver pomfret (pampus spp.)"),
                                 Description))
  
  # PACIFIC SAURY defined as: Cololabis saira
  hs_data <- hs_data %>%
    mutate(Description = if_else(str_detect(string = hs_data$Description, pattern = "pacific saury")==TRUE & str_detect(string = hs_data$Description, pattern = "cololabis")==FALSE, 
                                 str_replace(hs_data$Description, pattern = "pacific saury", 
                                             replacement = "pacific saury (cololabis saira)"),
                                 Description))
  
  # SCADS defined as: Decapterus spp.
  hs_data <- hs_data %>%
    mutate(Description = if_else(str_detect(string = hs_data$Description, pattern = "scad")==TRUE & str_detect(string = hs_data$Description, pattern = "decapterus")==FALSE, 
                                 str_replace(hs_data$Description, pattern = "scad", 
                                             replacement = "scad (decapterus spp.)"),
                                 Description))
  
  # CAPELIN defined as: Mallotus villosus
  hs_data <- hs_data %>%
    mutate(Description = if_else(str_detect(string = hs_data$Description, pattern = "capelin")==TRUE & str_detect(string = hs_data$Description, pattern = "mallotus")==FALSE, 
                                 str_replace(hs_data$Description, pattern = "capelin", 
                                             replacement = "capelin (mallotus villosus)"),
                                 Description))
  
  # KAWAKAWA defined as: Euthynnus affinis
  hs_data <- hs_data %>%
    mutate(Description = if_else(str_detect(string = hs_data$Description, pattern = "kawakawa")==TRUE & str_detect(string = hs_data$Description, pattern = "euthynnus")==FALSE, 
                                 str_replace(hs_data$Description, pattern = "kawakawa", 
                                             replacement = "kawakawa (euthynnus affinis)"),
                                 Description))
  
  # MARLINS, SAILFISH, SPEARFISH defined as: Istiophoridae
  hs_data <- hs_data %>%
    mutate(Description = if_else(str_detect(string = hs_data$Description, pattern = "spearfish")==TRUE & str_detect(string = hs_data$Description, pattern = "istiophoridae")==FALSE, 
                                 str_replace(hs_data$Description, pattern = "spearfish", 
                                             replacement = "spearfish (istiophoridae)"),
                                 Description))
  
  
  # CRABS and SHRIMPS/PRAWNS - prod_taxa_classification does not contain INFRAORDER information, will have to match these groups by common name limtied by order DECAPODA
  
  ## LOBSTERS: all Lobster HS descriptions specify genera or species except for 160530 ("Crustacean preparations; lobster prepared or preserved")
  # Combining genera listed in 030611, 030612, and 030615
  hs_data <- hs_data %>%
    mutate(Description = if_else(str_detect(string = hs_data$Description, pattern = "lobster, prepared or preserved")==TRUE, 
                                 str_replace(hs_data$Description, pattern = "lobster, prepared or preserved", replacement = "lobster (homarus spp., palinurus spp., panulirus spp., 
                                             jasus spp., nephrops norvegicus)"), 
                                 Description))
  
  ## OYSTERS
  # hs_data$Description[grep("oyster", hs_data$Description)] check which rows should be changed
  hs_data <- hs_data %>%
    mutate(Description = if_else(str_detect(string = hs_data$Description, pattern = "oyster")==TRUE & str_detect(string = hs_data$Description, pattern = "ostreidae")==FALSE, 
                                 # Ostreidae are true oysters and Pteriidae are pearl oysters (Note: there are other obscure families of oysters, but none of these show up in prod_taxa_classification)
                                 str_replace(hs_data$Description, pattern = "oyster", replacement = "oyster (ostreidae, pteriidae)"), 
                                 Description))
  
  # CUTTLE FISH (Order = Sepiida) AND SQUID (Order = Teuthida):
  # check_grep <- grep("cuttle fish and squid", hs_data$Description) check which rows should be changed
  hs_data <- hs_data %>%
    mutate(Description = if_else(str_detect(string = hs_data$Description, pattern = "cuttle fish and squid"), 
                                 str_replace(hs_data$Description, pattern = "cuttle fish and squid", replacement = "cuttle fish \\(sepiida\\) and squid \\(teuthida\\)"), 
                                 Description))
  
  
  # SCALLOPS: 
  # Description reads: scallops, including queen scallops, of the genera pecten, chlamys or placopecten
  # Need to insert the species name for queen scallops (Aequipecten opercularis)
  # Note: format of genera fixed at the top of clean_hs.R
  # check_grep <- grep("scallop", hs_data$Description) check which rows should be changed
  hs_data <- hs_data %>%
    mutate(Description = if_else(str_detect(string = hs_data$Description, pattern = "queen scallop"), 
                                 str_replace(hs_data$Description, pattern = "queen scallop", replacement = "queen scallop \\(aequipecten opercularis\\)"), 
                                 Description))
  
  # SNAILS - Class Gastropoda
  # FIX IT - descriptions specify (other than sea snails), need to figure out how to differentiate freshwater vs marine
  # hs_data$Description[grep("snail", hs_data$Description)] check which rows should be changed
  hs_data <- hs_data %>%
    mutate(Description = if_else(str_detect(string = hs_data$Description, pattern = "snails"), 
                                 str_replace(hs_data$Description, pattern = "snails", replacement = "snails \\(gastropoda\\)"), 
                                 Description))
  
  # JELLYFISH: defined as Rhopilema spp.
  # NOTE: in some HS code descriptions, jellyfish is specifed as (Rhopilema spp.) rather than all of Scyphozoa, code below maintains this description throughout
  hs_data <- hs_data %>%
    mutate(Description = if_else(str_detect(string = hs_data$Description, pattern = "jellyfish")==TRUE & str_detect(string = hs_data$Description, pattern = "rhopilema")==FALSE, 
                                 str_replace(hs_data$Description, pattern = "jellyfish", replacement = "jellyfish \\(rhopilema spp.\\)"), 
                                 Description))
  
  # SEA URCHIN: defined as Strongylocentrotus spp., Paracentrotus lividus, Loxechinus albus, Echinus esculentus
  # Note: rather than defining as class, echinoidea, maintaining HS definition as: Strongylocentrotus spp., Paracentrotus lividus, Loxechinus albus, Echinus esculentus (note: some descriptions spell this incorrectly as Echichinus but this is corrected at the top of clean_hs.R)
  hs_data <- hs_data %>%
    mutate(Description = if_else(str_detect(string = hs_data$Description, pattern = "sea urchin")==TRUE & str_detect(string = hs_data$Description, pattern = "strongylocentrotus")==FALSE, 
                                 str_replace(hs_data$Description, pattern = "sea urchin", replacement = "sea urchin \\(strongylocentrotus spp., paracentrotus lividus, 
                                             loxechinus albus, echinus esculentus)"), 
                                 Description))
  
  # SEA CUCUMBERS: defined as Stichopus japonicus, Holothuroidea
  hs_data <- hs_data %>%
    mutate(Description = if_else(str_detect(string = hs_data$Description, pattern = "sea cucumber")==TRUE & str_detect(string = hs_data$Description, pattern = "stichopus")==FALSE, 
                                 str_replace(hs_data$Description, pattern = "sea cucumber", replacement = "sea cucumber \\(stichopus japonicus, holothuroidea\\)"), 
                                 Description))
  
  # CAVIAR: defined as from family Acipenseridae
  hs_data <- hs_data %>%
    mutate(Description = if_else(str_detect(string = hs_data$Description, pattern = "caviar")==TRUE & str_detect(string = hs_data$Description, pattern = "substitute")==FALSE, 
                                 str_replace(hs_data$Description, pattern = "caviar", replacement = "caviar, acipenseridae"), 
                                 Description))
  
  # Change Oncorhynchus clarki to clarkii:
  # check_grep <- grep("cuttle fish and squid", hs_data$Description) check which rows should be changed
  hs_data <- hs_data %>%
    mutate(Description = if_else(str_detect(string = hs_data$Description, pattern = "clarki,"), 
                                 str_replace(hs_data$Description, pattern = "clarki,", replacement = "clarkii,"), 
                                 Description))
  
  
  ###########################################################################################
  # Create commodity type column based on HS code hierarchy (instead of by text descriptions)
  # removes need for removing false positives (e.g., jellyfish, cuttlefish, crawfish matching "fish")
  fishes = c("^0301", "^0302", "^0303", "^0304", "^0305", "^0511", "^1504", "^1604", "^2301")
  crustaceans = c("^0306", "^0511", "^1605[1-4]", "^2301")
  molluscs = c("^0307", "^0511", "^16055", "^16059", "^2301")
  aquatic_invertebrates = c("^0308", "^0511", "^16056", "^16059", "^2301") # note: aquatic inverts = any non-crustacean, non-mollusc

  
  hs_data <- hs_data %>% 
    arrange(Code) %>%
    mutate(Fishes = NA,
           Crustaceans = NA,
           Molluscs = NA,
           Aquatic_invertebrates = NA) %>% 
    mutate(Fishes = if_else(grepl(Code, pattern = paste(fishes, collapse = "|")), 1, 0),
           Crustaceans = if_else(grepl(Code, pattern = paste(crustaceans, collapse = "|")), 1, 0),
           Molluscs = if_else(grepl(Code, pattern = paste(molluscs, collapse = "|")), 1, 0),
           Aquatic_invertebrates = if_else(grepl(Code, pattern = paste(aquatic_invertebrates, collapse = "|")), 1, 0))


  
  # Create a column of cleaned genus names (For descriptions with multiple listed genera, output should be an indexable character vector of genera)
  # Extract genus where appropriate (always followed by ' spp')
  
  gen <- str_split(hs_data$Description, pattern = " spp") # split description on "spp"
    for (i in 1:length(gen)){
    if (length(gen[[i]])==1){ # Insert NA (ignore) all instances where description was not split (i.e., length == 1)
      gen[[i]] <- NA
    } else {
      gen[[i]] <- gen[[i]][-length(gen[[i]])] # remove the last element (only want to retain info BEFORE each "spp")
      gen[[i]] <- stringr::word(string = gen[[i]], -1) # get the last word (-1) in each string
      gen[[i]] <- gsub(gen[[i]], pattern="\\(", replacement = "")
    }
  }
  hs_data$Genus <- gen
  
   
  # Extract families from description (ends in 'dae')
  fam <- stringr::str_extract_all(hs_data$Description, "([^\\s])*dae")
  for (i in 1:length(fam)){
    if (length(fam[[i]])==0){ # Insert NA if no family name was found
      fam[[i]] <- NA
    } else {
    fam[[i]] <- gsub(fam[[i]], pattern="\\(", replacement = "")
    fam[[i]] <- gsub(fam[[i]], pattern="\\)", replacement = "")
    }
  }
  hs_data$Family <- fam
  
  # Extract orders from description
  order_patterns <- c("([^\\s])*formes|sepiida|teuthida") # NOTE: ending in formes is only a characteristic of fish orders; invertebrate descriptions must be listed manually 
  ord <- stringr::str_extract_all(hs_data$Description, pattern = order_patterns) 
  for (i in 1:length(ord)){
    if (length(ord[[i]])==0){ # Insert NA if no order name was found
      ord[[i]] <- NA
    } else {
      # Remove parentheses
      ord[[i]] <- gsub(ord[[i]], pattern="\\(", replacement="")
      ord[[i]] <- gsub(ord[[i]], pattern="\\)", replacement="")
    }
  }
  hs_data$Order <- ord
  
  # Extract class - do this manually, can add to the list as necessary
  class_names <- c("holothuroidea|gastropoda|scyphozoa|echinoidea")
  classes <- stringr::str_extract_all(hs_data$Description, pattern = class_names) 
  for (i in 1:length(classes)){
    if (length(classes[[i]])==0){
      classes[[i]] <- NA
    } 
  }
  hs_data$Class <- classes
  
 
  # Note: even though the "Species" column isn't ultimately used in hs_commod_matching (match_and_remove_by_classification), generate here anyway so we can make sure they are accepted names (run through rfishbase function synonyms)
  # Do genus matching first (previous section of code), once these are identified, can filter these names out, assume all other two-word phrases separated by commas are species (these get passed through rfishbase to confirm they are species names)
  sp <- str_extract_all(hs_data$Description, "\\([^()]+\\)")
  for (i in 1:length(sp)){
    if (length(sp[[i]])>0) { # if something found in parentheses, identify species names
      if (is.na(hs_data[i,]$Genus)==FALSE) { # genera are also sometimes in parentheses and also two words (genus spp.), first remove these 
        genera_to_remove <- hs_data[i,]$Genus[[1]]
        for (genus_i in 1:length(genera_to_remove)){
          sp[[i]] <- str_remove(sp[[i]], genera_to_remove[genus_i])
          sp[[i]] <- str_remove(sp[[i]], "spp.")
        }
      }  
      ## This chunk formats each description into a single comma-separated character string of potential species
      # Remove parentheses
      sp[[i]] <- gsub(sp[[i]], pattern="\\(", replacement="")
      sp[[i]] <- gsub(sp[[i]], pattern="\\)", replacement="")
      # Replace and/or with comma to allow strsplit by comma
      sp[[i]] <- gsub(sp[[i]], pattern = " and|, and", replacement = ",")
      sp[[i]] <- gsub(sp[[i]], pattern = " or|, or", replacement = ",")
      # Collapse list into single string separated by commas - needed for descriptions with multiple groups of parentheticals
      sp[[i]] <- paste(sapply(unlist(sp[[i]]), toString), collapse=", ")
      
      ## This chunk pulls just species out of the strings
      sp[[i]] <- strsplit(sp[[i]], ",") # Use commas to split description into phrases
      
      word_count <- lapply(sp[[i]], str_count, '\\w+') # Count how many words are in each phrase
      two_word_phrase <- (word_count[[1]] == 2) # index of only the two word phrases
      sp[[i]] <- sp[[i]][[1]][two_word_phrase] # only keep these two word phrases 
      sp[[i]] <- str_trim(sp[[i]]) # trim leading/trailing whitespace
      #sp[[i]] <- str_to_sentence(sp[[i]]) # Format to Upper case first word and lower case second word (Genus species)
    }
  }
     # If empty, insert NA
  for (i in 1:length(sp)){
    if (length(sp[[i]])==0) { # if nothing found in parentheses
      sp[[i]] <- NA
    }   
  }
  # For Aequipectin opercularis, "pectin" gets removed because this is identified as a genus within the description, change back to Aequipectin
  for (i in 1:length(sp)){
    if(TRUE %in% str_detect(unlist(sp[i]), "aequi opercularis")){
      sp[i][str_detect(unlist(sp[i]), "aequi opercularis")] <- "aequipecten opercularis"
    }
  }
  hs_data$Species <- sp
  
  ## Use FISHBASE and SEALIFEBASE to confirm that listed species names in Descriptions are the current "accepted name"
  # Get full list of species in descriptions
  hs_species <- na.omit(unique(unlist(hs_data$Species)))
  
  # reads and cleans Fishbase and Sealifebase synonym datasets
  fb_slb_dir <- "/Volumes/jgephart/ARTIS/Data/fishbase_sealifebase"
  fb_df <- read_synonyms(file.path(fb_slb_dir, "synonyms_fishbase_20220518.csv"))
  slb_df <- read_synonyms(file.path(fb_slb_dir, "synonyms_sealifebase_20220525.csv"))
  
  
  # Create df for storing synonym queries and results
  name_switches <- data.frame(query = hs_species, accepted_name = NA, fb_status = NA, slb_status = NA)

  for (i in 1:length(hs_species)){
    fb_query_out <- query_synonyms(fb_df, hs_species[i])
    if (nrow(fb_query_out > 0)){
      name_switches$accepted_name[i] <- fb_query_out$synonym
      name_switches$fb_status[i] <- fb_query_out$status
    }
    slb_query_out <- query_synonyms(slb_df, hs_species[i])
    if (nrow(slb_query_out > 0)){
      name_switches$accepted_name[i] <- slb_query_out$synonym
      name_switches$slb_status[i] <- slb_query_out$status
    }
  }
  
  # Identify where query name was different from accepted_name
  name_switches_true <- name_switches %>%
    filter(query != accepted_name)
  # Find the index number of all descriptions that contain names that need to be switched
  for (i in which(str_detect(hs_data$Description, pattern = paste(tolower(name_switches_true$query), collapse = "|")))){
    # For those where a name switch is needed, which species name(s) need to be switched
    #print(paste("i", i))
    name_switches_i <- name_switches_true %>%
      filter(str_detect(hs_data$Description[i], tolower(name_switches_true$query)))
    for (j in 1:nrow(name_switches_i)){
      #print(paste("j", j))
      # For each Description [i], replace any query name [j] with accepted name [j]
      hs_data$Description[i] <- str_replace(string = hs_data$Description[i], pattern = tolower(name_switches_i$query[j]), replacement = tolower(name_switches_i$accepted_name[j]))
      # Same for list of species in "Species" column
      # For each species [k] in Description [i], replace any query name [j] with accepted name [j]
      for (k in 1:length(hs_data$Species[[i]])){
        #print(paste("k", k))
        hs_data$Species[[i]][k] <- str_replace(string = hs_data$Species[[i]][k], pattern = name_switches_i$query[j], replacement = name_switches_i$accepted_name[j])
      }
    }
  }
  
  # is the commodity a catch-all group for all things 'n.e.c.' or 'n.e.s.'? 
  hs_data$NEC <- 0
  hs_data$NEC[grepl(hs_data$Description, pattern="n.e.c.")] <- 1
  hs_data$NEC[grepl(hs_data$Description, pattern="n.e.s.")] <- 1 #In H0, n.e.s. is used instead of n.e.c.
  
  
  # FINALLY, keep track of how hs_codes_clean will differ from original hs_data description
  # hs_data_raw$Description vs hs_data$Description
  hs_data$Modification <- NA
  for (i in 1:length(hs_data$Description)){
    new_descript <- hs_data$Description[i]
    raw_descript <- hs_data_raw %>%
      filter(Code == hs_data$Code[i] & Classification == hs_data$Classification[i]) %>%
      pull(Description) %>%
      tolower()
    descript_diff <- setdiff(strsplit(new_descript, " ")[[1]], strsplit(raw_descript, " ")[[1]])
    descript_diff <- paste(descript_diff, collapse = " ")
    if (length(descript_diff > 0)){
      hs_data$Modification[i] <- descript_diff
    }
  }
  
  
  return(hs_data)
}
