#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom readxl read_excel
#' @import stringr
#' @export

classify_prod_dat <- function(datadir, 
                              filename, 
                              prod_data_source="FAO",
                              SAU_sci_2_common = NA,
                              fb_slb_dir = "model_inputs_raw/fishbase_sealifebase") {
  
  # NOTE: final prod_data output does not aggregate to taxa level - i.e., does not do: group_by(country_iso3, SciName) %>% summarise(quantity = sum(quantity))
  # instead retains distinctions within SciName for different inlandmarine_group, source_name_en, and ISSCAAP group
  # production data does eventually get aggregated to taxa level in standardize_countries
  

# Clean FAO Production Data -----------------------------------------------
  
  if (prod_data_source=="FAO") {
    # Read in raw FAO production data files 
    # Note: These can be downloaded from FAO website as a zip file, unzip and
    # place these in datadir folder and rename all with "common_file_extension"
    
    # FAO files were not consistent between 2020 and 2021, so there are versions
    # of the rebuild_fao_dat function corresponding to year
    time_series_join <- rebuild_fao_2024_dat(datadir = datadir, filename = filename)
    
    prod_ts <- time_series_join %>%
      # FIXIT: select only columns needed - reduce size of in memory dataframe
      # select(species_name_en, species_scientific_name, country_iso3_code,
      #        country, species_major_group, quantity, year) %>% 
      # Standardize column names between FAO and SAU datasets 
      dplyr::rename(
        CommonName = species_name_en, 
        SciName = species_scientific_name,
        country_iso3_alpha = country_iso3_code, # alpha iso code
        country_iso3_numeric = country) %>% # numeric iso code 
      mutate(CommonName=tolower(as.character(CommonName)),
             SciName=tolower(as.character(SciName))) %>%
      # Trim any leading/trailing whitespace
      mutate_all(str_trim) %>%
      # Filter out groups not considered in this analysis  
      filter(!species_major_group %in% c("PLANTAE AQUATICAE",
                                        "AMPHIBIA, REPTILIA",
                                        "MAMMALIA"),
             # includes corals, sponges, pearl oysters, shells 
             !yearbook_group_en == "Other aq. animals & products") %>%
      # FIXIT: remove unused factor levels - but no columns are factors - could be the difference of reading in with read_csv() that converts text to chr vs read.csv() which can read text as factors. fread() reads in txt as chr
      droplevels() %>%
      
      #Remove unnecessary labels
      mutate(
        SciName = gsub(SciName, 
                       pattern=" \\(\\=.*", 
                       replacement="")) 
    #%>%
      
      # THESE APPLY SPECIFICALLY TO FAO prod_ts
      # First do some cleaning of SciNames.
      # List of fixes comes from finding SciNames that do not match to either 
      # the fishbase classification database or fishbase synonyms function in 
      # downstream code
      ## Change names that list multiple taxa (hybrid crosses - e.g., 
      # "morone chrysops x m. saxatilis" or "auxis thazard, a. rochei") to their 
      # common genus, or other lowest-level common classification
      
      mutate(
        SciName = case_when(
          # choose cambaridae, larger family
          SciName == "astacidae, cambaridae" ~ "cambaridae",
          SciName == "auxis thazard, a. rochei" ~ "auxis spp",
          SciName == "clarias gariepinus x c. macrocephalus" ~ "clarias spp",
          # Colossoma macropomum x Piaractus brachypomus
          SciName == "c. macropomum x p. brachypomus" ~ "serrasalmidae",
          SciName == "loliginidae, ommastrephidae" ~ "teuthida",
          SciName == "merluccius capensis, m.paradoxus" ~ "merluccius",
          SciName == "morone chrysops x m. saxatilis" ~ "morone spp",
          SciName == "oreochromis aureus x o. niloticus" ~ "oreochromis",
          SciName == "osmerus spp, hypomesus spp" ~ "osmeridae",
          # Piaractus mesopotamicus x Colossoma macropomum
          SciName == "p. mesopotamicus x c. macropomum" ~ "serrasalmidae",
          # essentially, an unidentified shark; code currently defines sharks
          # as a list of orders, assign to carcharhiniformes for now
          SciName == "selachimorpha (pleurotremata)" ~ "carcharhiniformes",
          # Sepiidae = cuttlefish; Sepiolidae = bobtail squid; assigning to cuttlefish
          SciName == "sepiidae, sepiolidae" ~ "sepiidae",
          # two different orders of sharks; code currently defines sharks as a
          # list of orders, assign to carcharhiniformes for now
          SciName == "squalidae, scyliorhinidae" ~ "carcharhiniformes",
          SciName == "stolothrissa, limnothrissa" ~ "clupeidae",
          SciName == "stolothrissa, limnothrissa spp" ~ "clupeidae",
          SciName == "xiphopenaeus, trachypenaeus" ~ "penaeidae",
          SciName == "xiphopenaeus, trachypenaeus spp" ~ "penaeidae",
          # Matched to the larger family because genus was different
          SciName == "h. longifilis x c. gariepinus" ~ "clariidae",
          # matched by same genus
          SciName == "e. fuscoguttatus x e. lanceolatus" ~ "epinephelus",
          # common genus between both
          SciName == "alosa alosa, a. fallax" ~ "alosa spp",

          #### Manually fix outdated names:
          SciName == "branchiostegidae" ~ "malacanthidae",
          SciName == "caspialosa spp" ~ "alosa spp",
          SciName == "invertebrata" ~ "asteroidea",
          # assign to asteroidea for now; downstream code defines aquatic invertebrates
          # as list of classes (if we went by phylum, ascidians would be omitted as chordata)
          SciName == "mobulidae" ~ "myliobatidae",
          SciName == "natantia" ~ "crangonidae",
          # natantia is obsolete term for "shrimp"; assign to order = crangonidae for now
          SciName == "reptantia" ~ "cancridae",
          # reptantia is obsolete term for "crab"; multiple families of crab,
          # assign to family = "cancridae" for now
          SciName == "siluroidei" ~ "siluriformes",
          SciName == "aliger gigas" ~ "lobatus gigas",
          # Queen Conch
          SciName == "liza spp" ~ "planiliza spp",
          # Referring to mullets

          #### Incorrect Names (corrected via common name):
          SciName == "mytilus unguiculatus" ~ "mytilus coruscus",
          # Korean Mussel
          SciName == "tritia mutabilis" ~ "nassarius mutabilis",
          # Mutable/Changeable Nassa
          SciName == "tritia reticulata" ~ "nassarius reticulatus",
          # Netted Dog whelk

          ### Fix spelling errors:
          SciName == "herklotsichthys quadrimaculat." ~ "herklotsichthys quadrimaculatus",
          SciName == "pleuronectes quadrituberculat." ~ "pleuronectes quadrituberculatus",
          SciName == "pseudopleuronectes herzenst." ~ "pseudopleuronectes herzensteini",
          SciName == "salmonoidei" ~ "salmonidae",
          SciName == "mobulinae" ~ "mobulidae",
          SciName == "moroteuthopsis ingens" ~ "onykia ingens",
          SciName == "pandalus spp, pandalopsis spp" ~ "pandalus spp",
          #"pandalus spp", # prawn

          # Downstream code ID's crustacea to class level; assign to branchiopoda
          # for now; downstream code defines crustaceans as list of classes
          # c("branchiopoda", "malacostraca", "maxillopoda", "merostomata");
          # reason: assuming non-crab/lobster/shrimp crustacean
          SciName == "crustacea" ~ "branchiopoda",

          ### Genus with missing spp:
          SciName == "cantherhines" ~ "cantherhines spp",

          ### Names not recognized by sealifebase/fishbase:
          # Just go up one level in classification
          SciName == "anodonta cygnea" ~ "anodonta spp",
          # Because of its morphological variability and its wide range of distribution,
          # there are over 500 synonyms for this species, just use genus
          SciName == "astacus astacus" ~ "astacus spp",
          SciName == "austropotamobius pallipes" ~ "astacidae",
          # Sealifebase doesn't recognize the genus or species, just use family
          SciName == "cherax tenuimanus" ~ "cherax spp",
          SciName == "cipangopaludina chinensis" ~ "cipangopaludina spp",
          SciName == "clupea pallasii" ~ "clupea pallasii pallasii",
          # Match to clupea pallasii pallasii to allow match with rfishbase,
          # but then rename to clupea pallasii in the final step
          SciName == "clupeoidei" ~ "clupeiformes",
          SciName == "emmelichthys nitidus" ~ "emmelichthys spp",
          SciName == "euastacus armatus" ~ "parastacidae",
          # sealifebase doesn't recognize the genus or species, just use family
          SciName == "macrobrachium lar" ~ "macrobrachium spp",
          SciName == "macrobrachium malcolmsonii" ~ "macrobrachium spp",
          SciName == "merluccius gayi" ~ "merluccius spp",
          #SciName == "morone" ~ "morone spp",
          SciName == "mullus barbatus" ~ "mullus spp",
          SciName == "oreochromis" ~ "oreochromis spp",
          SciName == "percoidei" ~ "perciformes",
          SciName == "procambarus clarkii" ~ "procambarus spp",
          SciName == "scombroidei" ~ "perciformes",
          # fishbase doesn't list scombiformes as an order
          # (See fishbase %>% filter(Family == "scombridae"))
          SciName == "sebastes marinus" ~ "sebastes spp",
          SciName == "brachyura" ~ "decapoda",
          # Infraorder not part of fishbase database
          SciName == "cherax cainii" ~ "cherax spp",
          # maron - classified into two species both cherax cainii and cherax tenuimanus
          # however only cherax tenuimanus accepted in sealifebase synonyms but
          # does not occur in sealifebase taxa table
          SciName == "sinanodonta woodiana" ~ "anodonta spp",
          # Check to see if could be anodonta dejecta
          #SciName == "bryozoa" ~ "polyzoa spp", # bryozoa is a phylum that refers
          # to aquatic invertebrates
          SciName == "caridina denticulata" ~ "neocaridina denticulata",
          # synonym to accepted name that isn't caught by fb or slb
          SciName == "anomura" ~ "decapoda",
          # infraorder name to order name
          SciName == "corbicula manilensis" ~ "corbicula spp",
          SciName == "maguimithrax spinosissimus" ~ "mithrax spp",
          # This is a type species of mithrax, sea spiders
          SciName == "macroramphosidae" ~ "centriscidae",
          # bellowfish, macroramphosidae used to be classified as a subfamily of centriscidae
          SciName == "austrofusus glans" ~ "buccinum spp",
          # whelk

          ### Tribe to genus name
          SciName == "thunnini" ~ "thunnus spp",

          ### Keep all other SciNames as is:
          TRUE ~ SciName
        )
      ) # end of mutate case_when
    
    ### Identify taxonomic ranks 
    # prod_ts will eventually be joined with fishbase classification info, 
    # use column name Genus01 to differentiate from column Genus
    prod_ts$Species01 <- 0
    prod_ts$Genus01 <- 0
    prod_ts$Family01 <- 0
    prod_ts$Other01 <- 0
    
    ### One hot encode new taxonomic rank columns based on pattern matching
    # Code Genus01 if "spp" pattern found in SciName
    prod_ts$Genus01[grepl(prod_ts$SciName, pattern = "spp")] <- 1
    # Code Family01 if there is no space (e.g "genus species") in the SciName 
    # AND "dae" is found in the SciName value
    prod_ts$Family01[grepl(pattern = " ", prod_ts$SciName) == FALSE &
                       grepl(pattern = "([^\\s])*dae", prod_ts$SciName)] <- 1
    # Code Species01 if there is a space in SciName AND 
    # Family01 AND Genus01 are not already coded
    prod_ts$Species01[grepl(prod_ts$SciName, pattern = " ") &
                        prod_ts$Family01 == 0 &
                        prod_ts$Genus01 == 0] <- 1
    # Code Other01 if Species01, Genus01, AND Family01 are not already coded
    prod_ts$Other01[prod_ts$Species01 == 0 &
                      prod_ts$Genus01 == 0 & 
                      prod_ts$Family01 == 0] <- 1
    
    # Now the genera are identified, remove " spp" from SciName
    prod_ts <- prod_ts %>%
      mutate(SciName = gsub(SciName, 
                            pattern=" spp", 
                            replacement=""))
    
    # Finally data formatting
    prod_ts <- prod_ts %>%
      mutate(quantity = as.numeric(quantity),
             year = as.integer(year)) %>%
     # filter(year > 1995) %>%
      filter(quantity > 0)
    
  }
  

# Clean SAU production data ----------------------------------------------------

  if (prod_data_source=="SAU"){
    prod_ts <- read.csv(file.path(datadir, filename), 
                        stringsAsFactors = FALSE, 
                        header = TRUE, 
                        sep=",") %>%
      mutate(scientific_name = tolower(scientific_name)) %>%
      rename(quantity = sum,
             CommonName = common_name,
             SciName = scientific_name,
             country_name_en = fishing_entity,
      ) %>%
      mutate(SciName = tolower(SciName), 
             CommonName = tolower(CommonName))
    
    sci_2_common <- read.csv(file.path(datadir, SAU_sci_2_common), 
                             stringsAsFactors = FALSE) %>%
      mutate(scientific_name = tolower(scientific_name))
    
    sci_2_common <- sci_2_common %>% 
      # Remove retired scientific names
      filter(!grepl(comments_names, pattern="retired"))
    
    latest_taxon_keys <- sci_2_common %>% 
      group_by(scientific_name) %>% 
      summarize(taxon_key = max(taxon_key))
    
    sci_2_common <- sci_2_common %>%
      filter(taxon_key %in% latest_taxon_keys$taxon_key)
    
    prod_ts <- prod_ts %>% 
      left_join(sci_2_common %>%
                  select(-common_name), by = c("SciName" = "scientific_name")) %>% 
      # Trim any leading/trailing whitespace
      mutate_all(str_trim) %>%
      filter(is.na(SciName)==FALSE) %>%
      # THESE APPLY SPECIFICALLY TO SAU prod_ts
      # First do some cleaning of SciNames
      # List of fixes comes from finding SciNames that do not match to either the fishbase classification database or fishbase synonyms function in downstream code
      # Address non-scientific names
      mutate(SciName = case_when(SciName == "marine finfishes not identified" ~ "actinopterygii", 
                                 SciName == "marine fishes not identified" ~ "actinopterygii",
                                 SciName == "marine groundfishes not identified" ~ "actinopterygii",
                                 SciName == "marine pelagic fishes not identified" ~ "actinopterygii", 
                                 SciName == "miscellaneous aquatic invertebrates" ~ "asteroidea", # assign to asteroidea for now; downstream code defines aquatic invertebrates as list of classes (if we went by phylum, ascidians would be omitted as chordata)
                                 SciName == "miscellaneous diadromous fishes" ~ "actinopterygii",
                                 SciName == "miscellaneous marine crustaceans" ~ "malacostraca", # assuming some sort of crab/lobster/shrimp/prawn/crayfish crustacean
                                 
                                 # Names not recognized by fish/sealifebase, just go up one (in some cases, down) one level in classification
                                 SciName == "anomura" ~ "decapoda", # fish/sealifebase doesn't go to infraorder-level of classification
                                 SciName == "asterozoa" ~ "asteroidea",
                                 SciName == "batoidea" ~ "rajiformes", # several potential orders, assume rajiformes for now
                                 SciName == "brachyura" ~ "decapoda", 
                                 SciName == "dendrobranchiata" ~ "decapoda",
                                 SciName == "echinozoa" ~ "echinodermata", 
                                 SciName == "inermiidae" ~ "haemulidae", 
                                 SciName == "pteriomorphia" ~ "bivalvia", 
                                 SciName == "scombroidea" ~ "perciformes",
                                 
                                 # Edits because they are not getting matched to taxa table at the end
                                 SciName == "liza" ~ "planiliza",
                                 SciName == "scombroidei" ~ "perciformes",
                                 SciName == "pleuronectoidei" ~ "pleuronectiformes", # moving from suborder to order
                                 SciName == "valamugil" ~ "crenimugil",
                                 SciName == 'tridacnidae' ~ 'cardiidae', # Moving from subfamily to family name
                                 SciName == 'azurina cyanea' ~ 'azurina', # moving up a taxonomic level
                                 SciName == 'macrostrombus costatus' ~ 'strombidae', # Move from species to family name for identification
                                 SciName == 'phrontis vibex' ~ 'nassarius vibex',
                                 SciName == 'sinistrofulgur sinistrum' ~ 'neogastropoda', # Move from species to order
                                 TRUE ~ SciName)) 
    
    prod_ts$Species01 <- 0
    prod_ts$Genus01 <- 0
    prod_ts$Family01 <- 0
    prod_ts$Other01 <- 0
    
    prod_ts$Species01[which(prod_ts$taxon_level_id==6)] <- 1
    prod_ts$Genus01[which(prod_ts$taxon_level_id==5)] <- 1
    prod_ts$Family01[which(prod_ts$taxon_level_id==4)] <- 1
    prod_ts$Other01[which(prod_ts$taxon_level_id<4)] <- 1
    
    prod_ts <- prod_ts %>%
      mutate(
        Genus01 = case_when(
          SciName == 'centrophorus' ~ 1,
          SciName == 'sarda' ~ 1,
          SciName == 'crenimugil' ~ 1,
          SciName == 'balistes' ~ 1, # Triggerfish
          SciName == 'ophichthus' ~ 1, # snake eel
          SciName == 'tetrapturus' ~ 1, # genus of marlins called spearfish
          SciName == 'polyprion' ~ 1, # genus of ray finned fish
          SciName == 'chelidonichthys' ~ 1, # genus of ray finned fish
          SciName == 'illex' ~ 1, # shortfin squid
          SciName == 'alloteuthis' ~ 1, # pencil squid
          SciName == 'branchiostegus' ~ 1, # tilefish
          SciName == 'caulolatilus' ~ 1, # tilefish
          SciName == 'symphodus' ~ 1, # wrasses
          SciName == 'labrus' ~ 1, # wrasses
          SciName == 'ommastrephes' ~ 1, # neon flying squid
          SciName == 'aphanopus' ~ 1, #black scabbardfishes
          SciName == 'cepola' ~ 1, # red bandfish
          SciName == 'liocarcinus' ~ 1, # flying and vernal crab
          SciName == 'trisopterus' ~ 1, # small cods
          SciName == 'venerupis' ~ 1, # marine bivalve molluscs in family veneridae
          SciName == 'azurina' ~ 1,
          TRUE ~ Genus01
        ),
        Family01 = case_when(
          SciName == 'cardiidae' ~ 1, # Giant Clams
          SciName == 'merlucciidae' ~ 1, # hakes
          SciName == 'solenoceridae' ~ 1, # decapods
          SciName == 'moronidae' ~ 1, # family of perciform fishes
          SciName == 'pomatomidae' ~ 1, # bluefish within perciformes
          SciName == 'salpidae' ~ 1, # salp
          SciName == 'strombidae' ~ 1, # true conch
          TRUE ~ Family01
        ),
        Other01 = case_when(
          SciName == 'osteichthyes' ~ 1,
          SciName == 'pleuronectoidei' ~ 1,
          SciName == 'mytilida' ~ 1, # Order of molluscs
          SciName == 'rhizostomeae' ~ 1, # order of jellyfish
          SciName == 'neogastropoda' ~ 1,
          TRUE ~ Other01
        )
      )
    
    prod_ts <- prod_ts %>%
      filter(year > 1995) %>%
      filter(quantity > 0)
    
  }
  
  
# Match FAO and SAU prod to Fishbase and Sealifebase --------------

  # match all taxanames to classification info in fishbase/sealifebase
  # First use unique taxa names (prod_taxa_names) to match classification info
  # IMPORTANT: in the process, find SciNames with no classification info and replace synonyms with accepted names in prod_ts
  prod_taxa_names <- prod_ts %>% 
    select(SciName, CommonName, Species01, Genus01, Family01, Other01) %>% 
    arrange(SciName) %>%
    distinct()
  
  ########## Fishbase and Sealifebase Matching ------------------------------
  # Load Fishbase and Sealifebase Databases 
  # Fishbase and Sealifebase Taxa Datasets
  fishbase <- fread(file.path(fb_slb_dir, "fb_taxa_info.csv"))
  sealifebase <- fread(file.path(fb_slb_dir, "slb_taxa_info.csv"))
  
  # reads and cleans Fishbase and Sealifebase synonym datasets
  fishbase_syn <- fread(file.path(fb_slb_dir, "fb_synonyms_clean.csv"))
  sealifebase_syn <- fread(file.path(fb_slb_dir, "slb_synonyms_clean.csv"))
  
  # Standardize fishbase and sealifebase:
  fishbase <- fishbase %>% 
    mutate_all(tolower) %>%
    select(-SpecCode) %>% 
    as.data.frame()
  
  sealifebase <- sealifebase %>%
    mutate_all(tolower) %>%
    select(-SpecCode) %>% 
    as.data.frame()
  
  # HIERARCHICAL MATCHING TO CLASSIFICATION INFO
  # For each SciName in prod_taxa_names, use fishbase and sealifebase to add taxonomic classification
  # Original FAO and SAU data had taxonomic classification info, but use fishbase/sealifebase for this info instead (more trustworthy)
  # Need to join data hierarchically (match species to species, genus to genus, etc.)
  # NOTES on hierarchical joining: 
  # Use inner_join to get only matches; non-matches (i.e., not found in fishbase or sealifebase) will be rejoined at the end
  # Hierarchical joining: Before joining by Genus, remove fishbase species column (otherwise will join multiple species to what should be a single row for the genus)
  # Hierarchical joining: Before joining by Family, remove fishbase species, genus, subfamily columns
  # Hierarchical joining: Before etc. etc. 
  # For Other01=1, join separately for higher groups (Order, Class, and Superclass for fishbase; Order, Class, Phylum, Kingdom for Sealifebase)
  # For Other01=1, assign metadata to positive matches: Order01, Class01, Superclass01, Phylum01, Kingdom01
  
  # Only species level data in both prod and fb
  prod_fb_species <- prod_taxa_names %>%
    filter(Species01==1) %>%
    inner_join(fishbase, 
               by=c("SciName" = "Species")) 
  # FITIT: add/define expected relationship = "" arguement to join
  
  # Only includes genuses that appear in BOTH production and fishbase
  prod_fb_genus <- prod_taxa_names %>% 
    filter(Genus01==1) %>%
    inner_join((fishbase %>% select(-Species)), 
               by=c("SciName" = "Genus")) %>%
    mutate(Genus = SciName) %>% # Retain "Genus" column so that filtering by column "Genus" will include all Species with this Genus as well as SciName=Genus
    distinct()
  # FITIT: add/define expected relationship = "" arguement to join
  
  # Only includes Family level data in both prod and fb
  prod_fb_family <- prod_taxa_names %>% 
    filter(Family01==1) %>%
    inner_join((fishbase %>% select(-c(Species, Genus, Subfamily))), 
               by=c("SciName" = "Family")) %>%
    mutate(Family = SciName) %>%
    distinct()
  # FITIT: add/define expected relationship = "" arguement to join
  
  # Only includes Order data in both prod and fb - determine by matching to fb
  # order column - not coded into prod like species, genus, and family
  prod_fb_order <- prod_taxa_names %>% 
    filter(Other01==1) %>%
    inner_join((fishbase %>% select(-c(Species, Genus, Subfamily, Family))), 
               by=c("SciName" = "Order")) %>%
    mutate(Order = SciName) %>%
    distinct() %>%
    # add encoded order col
    mutate(Order01 = 1) %>%
    select(-Other01)
  # FITIT: add/define expected relationship = "" arguement to join
  
  # Only includes class data in both prod and fb - see order comment above
  prod_fb_class <- prod_taxa_names %>% 
    filter(Other01==1) %>%
    inner_join((fishbase %>% select(-c(Species, Genus, Subfamily, Family, Order))), 
               by=c("SciName" = "Class")) %>%
    mutate(Class = SciName) %>%
    distinct() %>%
    # add encoded class col
    mutate(Class01 = 1) %>%
    select(-Other01)
  # FITIT: add/define expected relationship = "" arguement to join
  
  # Only includes superclass data in both prod and fb - see order comment above
  prod_fb_superclass <- prod_taxa_names %>% 
    filter(Other01==1) %>%
    inner_join((fishbase %>% select(-c(Species, Genus, Subfamily, 
                                       Family, Order, Class))), 
               by=c("SciName" = "SuperClass")) %>%
    mutate(SuperClass = SciName) %>%
    distinct() %>%
    # add encoded superclass col
    mutate(Superclass01 = 1) %>%
    select(-Other01)
  # FITIT: add/define expected relationship = "" arguement to join
  
  # Repeat hierarchical joining with sealifebase: 
  
  # Only species level data in both prod and slb
  prod_slb_species <- prod_taxa_names %>% 
    filter(Species01==1) %>%
    inner_join(sealifebase, 
               by=c("SciName" = "Species")) 
  # FITIT: add/define expected relationship = "" arguement to join
  
  # Only genus level data in both prod and slb
  prod_slb_genus <- prod_taxa_names %>% 
    filter(Genus01==1) %>%
    inner_join((sealifebase %>% select(-Species)), 
               by=c("SciName" = "Genus")) %>%
    mutate(Genus = SciName) %>%
    distinct()
  # FITIT: add/define expected relationship = "" arguement to join
  
  # Only family level data in both prod and slb
  prod_slb_family <- prod_taxa_names %>% 
    filter(Family01==1) %>%
    inner_join((sealifebase %>% select(-c(Species, Genus, Subfamily))), 
               by=c("SciName" = "Family")) %>%
    mutate(Family = SciName) %>%
    distinct()
  # FITIT: add/define expected relationship = "" arguement to join
  
  # Only order level data in both prod and slb - inferred by matching - not encoding 
  prod_slb_order <- prod_taxa_names %>% 
    filter(Other01==1) %>%
    inner_join((sealifebase %>% select(-c(Species, Genus, Subfamily, Family))), 
               by=c("SciName" = "Order")) %>%
    mutate(Order = SciName) %>%
    distinct() %>%
    # add order col
    mutate(Order01 = 1) %>%
    select(-Other01)
  # FITIT: add/define expected relationship = "" arguement to join
  
  # Only class level data in both prod and slb - inferred by matching - not encoding 
  prod_slb_class <- prod_taxa_names %>% 
    filter(Other01==1) %>%
    inner_join((sealifebase %>% select(-c(Species, Genus, Subfamily, 
                                          Family, Order))), 
               by=c("SciName" = "Class")) %>%
    mutate(Class = SciName) %>%
    distinct() %>%
    # add class col
    mutate(Class01 = 1) %>%
    select(-Other01)
  # FITIT: add/define expected relationship = "" arguement to join
  
  # Only phylum level data in both prod and slb - inferred by matching - not encoding 
  prod_slb_phylum <- prod_taxa_names %>% 
    filter(Other01==1) %>%
    inner_join((sealifebase %>% select(-c(Species, Genus, Subfamily, 
                                          Family, Order, Class))), 
               by=c("SciName" = "Phylum")) %>%
    mutate(Phylum = SciName) %>%
    distinct() %>%
    # add phylum col
    mutate(Phylum01 = 1) %>%
    select(-Other01)
  # FITIT: add/define expected relationship = "" arguement to join
  
  # NO Kingdom matches, so stop here with taxa matching
  
  # Only needed metadata columns (Species01, Genus01, etc) to match production 
  # data to fishbase and sealifebase classification; can now remove these 
  # Bind all matched data frames together
  prod_fb_full <- prod_fb_species %>% 
    full_join(prod_fb_genus, 
              by = intersect(names(prod_fb_species), 
                             names(prod_fb_genus))) %>%
    full_join(prod_fb_family, 
              by = intersect(names(.), 
                             names(prod_fb_family))) %>%
    full_join(prod_fb_order, 
              by = intersect(names(.), 
                             names(prod_fb_order))) %>%
    full_join(prod_fb_class, 
              by = intersect(names(.), 
                             names(prod_fb_class))) %>%
    full_join(prod_fb_superclass, 
              by = intersect(names(.), 
                             names(prod_fb_superclass))) %>%
    arrange(SciName)
  
  prod_slb_full <- prod_slb_species %>% 
    full_join(prod_slb_genus,  
              by = intersect(names(prod_slb_species), 
                             names(prod_slb_genus))) %>%
    full_join(prod_slb_family, 
              by = intersect(names(.), 
                             names(prod_slb_family))) %>%
    full_join(prod_slb_order, 
              by = intersect(names(.), 
                             names(prod_slb_order))) %>%
    full_join(prod_slb_class, 
              by = intersect(names(.), 
                             names(prod_slb_class))) %>%
    full_join(prod_slb_phylum, 
              by = intersect(names(.), 
                             names(prod_slb_phylum))) %>%
    # There are no slb superclass or kingdom matches
    arrange(SciName)
  
# Species without matches - check synonyms -----------------------------------------
  # Standardize names of FAO prod taxa that did not match to fish/sealifebase
  # Are there any scinames common between fb and slb - should not be
  # common_fb_slb <- intersect(prod_fb_full$SciName, prod_slb_full$SciName) 
  
  # Figure out which species dropped out
  nomatch_fb <- prod_taxa_names$SciName[prod_taxa_names$SciName %in% prod_fb_full$SciName==FALSE] 
  nomatch_fb_and_slb <- nomatch_fb[nomatch_fb %in% prod_slb_full$SciName==FALSE] 
  nomatch_fb_and_slb <- unique(nomatch_fb_and_slb) 
  # Note: prod_taxa_names is allowed to have duplicate scinames (each has a different commonname), only need list of unique sci names for synonyms matching below
  
  # Use synonyms() function in rfishbase (currently using backed up dataset of fb and slb synonyms) to see if non-matching species is due to an outdated scientific name
  # First limit nomatch_fb_and_slb to just species names (i.e., two words, look for space)
  nomatch_species <- nomatch_fb_and_slb[grepl(nomatch_fb_and_slb, pattern = " ")]
  
  fb_switches <- 0
  slb_switches <- 0
  for (i in 1:length(nomatch_species)) {
    
    next_sciname <- nomatch_species[i]

    # Run scientific name through synonym databases in fishbase and sealifebase
    name_fb_status <- query_synonyms(fishbase_syn, next_sciname)
    name_slb_status <- query_synonyms(sealifebase_syn, next_sciname)
    
    # check if this SciName is in fishbase
    if (nrow(name_fb_status) > 0) {
      
      accepted_name <- tolower(name_fb_status$synonym)
      
      # REPLACE SYNONYM WITH ACCEPTED NAME IN BOTH CLASSIFICATION DATAFRAME AND PRODUCTION DATA
      # Get row from original prod_taxa_names, replace with accepted name, and join with classification info from fishbase
      prod_fb_full_newdat <- prod_taxa_names %>%
        filter(SciName == next_sciname) %>%
        mutate(SciName = accepted_name) %>%
        inner_join(fishbase, by=c("SciName" = "Species"))
      
      prod_ts <- prod_ts %>%
        mutate(SciName = if_else(SciName == next_sciname, 
                                 true = accepted_name, 
                                 false = SciName))
      
      # if a new row of data was successfully found, join to full classification
      # dataset and replace nomatch_species[i] with accepted species name
      if(nrow(prod_fb_full_newdat) > 0){ 
        # replace nomatch_species with accepted name to keep track further 
        # downstream which species are still missing
        nomatch_species[i]<-accepted_name 
        prod_fb_full <- prod_fb_full %>%
          full_join(prod_fb_full_newdat, 
                    by = intersect(names(prod_fb_full), 
                                   names(prod_fb_full_newdat))) # join by all columns
        fb_switches = fb_switches + 1
      }
    } # end if SciName fb check
    
    # check to see if this SciName is in Sealifebase
    if (nrow(name_slb_status) > 0) {
      
      accepted_name <- tolower(name_slb_status$synonym)
      
      prod_slb_full_newdat <- prod_taxa_names %>%
        filter(SciName == next_sciname) %>%
        mutate(SciName = accepted_name) %>%
        inner_join(sealifebase, by = c("SciName" = "Species")) 
      
      prod_ts <- prod_ts %>%
        mutate(SciName = if_else(SciName==next_sciname, 
                                 true = accepted_name, 
                                 false = SciName))
      
      if (nrow(prod_slb_full_newdat) > 0) {
        nomatch_species[i] <- accepted_name
        prod_slb_full <-prod_slb_full %>%
          full_join(prod_slb_full_newdat, 
                    intersect(names(prod_slb_full), 
                              names(prod_slb_full_newdat))) # join by all columns
        
        slb_switches = slb_switches + 1
      }
    } # end if SciName slb check
  } # end for loop for non matching species
  
  
  # Figure out which species are still missing
  post_match_missing_species <- nomatch_species[!(nomatch_species %in% prod_fb_full$SciName)]
  post_match_missing_species <- post_match_missing_species[!(post_match_missing_species %in% prod_slb_full$SciName)]
  
  # Only species names were screened for synonyms, get all the non-matching, non-species names
  nomatch_non_species <- nomatch_fb_and_slb[grepl(nomatch_fb_and_slb, 
                                                  pattern = " ") == FALSE]
  
  nomatch_and_nosynonym <- c(post_match_missing_species, nomatch_non_species)
  nomatch_and_nosynonym <- sort(nomatch_and_nosynonym) # length = 0 i.e., all taxa in prod_ts now matched to classification info in rfishbase
  
# Data Check - missing taxa ------------------------------------------------
  # Check for any species in prod_ts that are not found in prod_fb_full or
  # prod_slb_full - should be exact same result as nomatch_and_nosynonym 
  if(!any(sort(unique(prod_ts$SciName)[unique(prod_ts$SciName) %in%
                                       c(prod_fb_full$SciName, prod_slb_full$SciName) == FALSE])
          == nomatch_and_nosynonym)) {
    warning("`prod_ts` taxa missing from fb and slb matching does NOT match
  `nomatch_and_nosynonym` vector created after synonym matching. Needs to be exactly the same. Check for discrepancies.")
  }
  
  # Check if fb or slb introduced taxa not originally in prod_ts
  if (length(c(prod_fb_full$SciName, prod_slb_full$SciName)[c(prod_fb_full$SciName, prod_slb_full$SciName) %in% unique(prod_ts$SciName) == FALSE])
      != 0) {
    warning("Taxa in fb or slb matching not found in original `prod_ts`. Check for discrepancies.")
  }
  
# Join Aquarium Trade Data ------------------------------------------------
  
  # Get aquarium trade and habitat (Fresh, Brackish, Saltwater) info from
  # fishbase: use this to classify ornamental trade species
  #fb_aquarium_info <- rfishbase::species(str_to_sentence(prod_fb_full$SciName))
  fb_aquarium_info <- fread(file.path(fb_slb_dir, "fb_aquarium.csv"))
  fb_aquarium_relevant <- fb_aquarium_info %>%
    filter(SciName %in% prod_fb_full$SciName)
  
  prod_fb_full <- prod_fb_full %>%
    left_join(fb_aquarium_relevant, by = "SciName") %>%
    # to make it the same as previous version's code
    rename(Fresh01 = Fresh, 
           Brack01 = Brack, 
           Saltwater01 = Saltwater) 
  
  slb_aquarium_info <- fread(file.path(fb_slb_dir, "slb_aquarium.csv"))
  slb_aquarium_relevant <- slb_aquarium_info %>%
    filter(SciName %in% unique(prod_slb_full$SciName))
  
  prod_slb_full <- prod_slb_full %>%
    left_join(slb_aquarium_relevant, by = c("SciName")) %>%
    rename(Fresh01 = Fresh, 
           Brack01 = Brack, 
           Saltwater01 = Saltwater) # to make it the same as previous version's code


# Combine prod_fb_full, prod_slb_full -------------------------
  
  prod_taxa_classification <- prod_fb_full %>%
    full_join(prod_slb_full, 
              by = intersect(names(prod_fb_full), names(prod_slb_full))) %>%
    # rename SuperClass
    rename(Superclass = SuperClass) %>%
    # Remove all metadata columns (e.g., Species01) - these were only used to join FAO production data with fishbase and sealifebase; not needed for hs_commod_matching
    select(SciName, CommonName, Genus, 
           Subfamily, Family, Order, 
           Class, Superclass, Phylum, 
           Kingdom, Aquarium, Fresh01, 
           Brack01, Saltwater01) %>% 
    arrange(SciName)
  

# Data Check and Fix ----------------------------------------------------------
  # Check that all SciNames are unique (and if not, they should at least have different CommonNames AND identical classification schemes)
  # After removing common name all SciNames should be unique
  classification_check <- prod_taxa_classification %>%
    select(-CommonName) %>%
    distinct() 
  
  # These SciNames are not unique, even after removing Common Name - i.e., there are multiple, different rows of classification scheme for each of these SciNames
  classification_to_fix <- data.frame(table(classification_check$SciName)) %>% 
    filter(Freq > 1) %>%
    pull(Var1) #%>% 
    #as_tibble()
  
  # Check, in each of these cases, there is a discrepancy in the classification scheme: 
  # prod_taxa_classification %>% filter(SciName %in% classification_to_fix)
  # Standardize them by inserting NA for when there is a discrepancy
  prod_taxa_fix <- NULL
  for (i in 1:length(classification_to_fix)){
    prod_taxa_i <- prod_taxa_classification %>% 
      filter(SciName == classification_to_fix[i]) 
    
    test_taxa_i <- prod_taxa_classification %>% 
      filter(SciName == classification_to_fix[i]) %>%
      mutate(across(everything(), as.factor)) %>%
      mutate(across(everything(), as.numeric)) %>%
      colSums(na.rm = TRUE) %>%
      t()
    
    # if ColSums is 0, these are all NAs
    # if ColSums == nrow(prod_taxa_i), then all factors match 
    # anything other than 0 or nrow(prod_taxa_i) means there is a discrepancy in this column, so set original values for this column to NA
    fix_columns <- colnames(test_taxa_i)[test_taxa_i!=0 & test_taxa_i!=nrow(prod_taxa_i)]
    
    # Set all of these columns to NA
    prod_taxa_i[,fix_columns]<-NA
    
    # Classification should now be identical, distinct() should return single row
    prod_taxa_fix <- prod_taxa_fix %>%
      bind_rows(prod_taxa_i %>% distinct())
  } # end of prod_taxa_fix loop
  
  prod_taxa_classification_clean <- prod_taxa_classification %>%
    filter(SciName %in% classification_to_fix==FALSE)  %>% # Remove SciNames that did not have matching classification schemes
    bind_rows(prod_taxa_fix) %>%
    arrange(SciName)
  
  # Replace all empty values with NAs for consistent reporting
  prod_ts[prod_ts == ""] <- NA

# Final Formatting and appending missing -------------------------------------

  # Final Formatting of Prod TS to match previous code version's types
  if (prod_data_source == "FAO") {
    prod_ts <- prod_ts %>%
      mutate(country_iso3_numeric = as.integer(country_iso3_numeric),
             area.code = as.integer(area.code),
             country_identifier = as.integer(country_identifier),
             production_identifier = as.integer(production_identifier),
             sort = as.integer(sort),
             species_identifier = as.integer(species_identifier),
             unit_identifier = as.integer(unit_identifier),
             multiplier = as.integer(multiplier),
             alternate = as.integer(alternate),
             symbol_identifier = as.integer(symbol_identifier))
  }
  
  # Fill in Missing Phyla and Kingdom
  prod_taxa_classification_clean <- prod_taxa_classification_clean %>%
    mutate(Phylum = case_when(Class %in% c("actinopterygii", 
                                           "elasmobranchii", 
                                           "holocephali", 
                                           "myxini", 
                                           "cephalaspidomorphi", 
                                           "sarcopterygii") ~ "chordata",
                              Superclass == "osteichthyes" ~ "chordata",
                              TRUE ~ Phylum)) %>%
    mutate(Kingdom = "animalia")
  
  # Fill in missing rows for perciformes and bryozoa
  prod_taxa_classification_clean <- prod_taxa_classification_clean %>%
    bind_rows(
      data.frame(SciName = c("perciformes", 
                             "actinopterygii", 
                             "scorpaeniformes"),
                 CommonName = c("tuna-like fishes nei", 
                                "ray-finned fishes", 
                                "mail-cheeked fishes"),
                 Genus = NA,
                 Subfamily = NA,
                 Family = NA,
                 Order = c("perciformes", 
                           NA, 
                           "scorpaeniformes"),
                 Class = c("actinopterygii", 
                           "actinopterygii", 
                           "actinopterygii"),
                 Superclass = NA,
                 Phylum = c("chordata", 
                            "chordata", 
                            "chordata"),
                 Kingdom = "animalia",
                 Aquarium = NA,
                 Fresh01 = NA,
                 Brack01 = NA, 
                 Saltwater01 = NA)
    ) %>%
    # Special case Phylum for
    mutate(Phylum = case_when(
      SciName == "sipunculus nudus" ~ "annelida",
      TRUE ~ Phylum
    ))
  
  missing_scinames <- unique(prod_ts$SciName)[!(unique(prod_ts$SciName) %in% unique(prod_taxa_classification_clean$SciName))]
  if (length(missing_scinames) > 0) {
    warning("Not all scinames in prod_data are in prod_taxa_classification")
    warning(paste(missing_scinames, collapse = ", "))
  }
  
  return(list(prod_ts, prod_taxa_classification_clean))
  
  # NOTE: resulting tibble is allowed to have duplicate scientific names (but each should have a different CommonName)
  
}