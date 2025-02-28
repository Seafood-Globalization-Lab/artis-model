#' @import dplyr
#' @import tibble
#' @import data.table
#' @importFrom magrittr %>%
#' @export
#' 

clean_prod_scinames <- function(prod_df, 
                                write_corrections = TRUE) {
  
  # create tibble that contains corrections to scinames -----------------------
  
  sciname_corrections <- tribble(
    ~ sciname_original,
    ~ sciname_corrected,
    ~ correction_type,
    ~ notes,
    ~ in_prod_taxa_2024,
    ~ in_clean_hs,
    ~ in_get_fmfo_species,
    ~ in_match_hs_taxa,
     
    # fit to fishbase/sealifebase classification schema ------------------
    "percoidei",
    "perciformes/percoidei",
    "fishbase classification schema",
    "percoidei suborder not used in fishbase classification rank schema, uses Perciformes/subclass in the order rank to indicate suborder value",
    1,0,0,1,
    
    "perciformes",
    "FIXIT",
    "fishbase classification schema",
    "Fishbase is using a wierd classification schema hack to fit perciformes order and suborder information. Perciformes/Bembropoidei, Perciformes/Cottoidei, Perciformes/Gasterosteoidei, Perciformes/Notothenioidei, Perciformes/Percoidei, Perciformes/Percophoidei, Perciformes/Scorpaenoidei, Perciformes/Serranoidei, Perciformes/Uranoscopoidei, Perciformes/Zoarcoidei",
    1,0,0,1,
    
    "brachyura",
    "decapoda",
    "sealifebase classification schema",
    "matches by common name; infraorder (brachyura) not part of Fishbase database; brachyura are true crabs in the order decapoda; related to the reptania and Dendrobranchiata classification issues; could hack fishbase classification schema to fit order and infraorder in a single value in the order column (decapoda/brachyura, decapoda/dendrobranchiata etc.)",
    1,1,0,1,
    
    "anomura",
    "decapoda",
    "sealifebase classification schema",
    "use order; not matched by common name in other scripts like lobsters are in clean_hs",
    1,1,0,1,
    
    # genus missing spp --------------------
    "oreochromis",
    "oreochromis spp",
    "genus missing spp",
    "Use genus",
    1,1,0,0,
    
    "cantherhines",
    "cantherhines spp",
    "genus missing spp",
    "Genus with missing spp",
    1,0,0,0,
    
    # hybrids --------------
    "clarias gariepinus x c. macrocephalus",
    "clarias spp",
    "hybrid",
    "use common genus",
    1,0,0,0,
    
    "c. macropomum x p. brachypomus",
    "serrasalmidae",
    "hybrid",
    "use common family; colossoma macropomum and piaractus brachypomus species",
    1,0,0,0,
    
    "h. longifilis x c. gariepinus",
    "clariidae",
    "hybrid",
    "use common family; Heterobranchus longifilis and Clarias gariepinus species",
    1,0,0,0,
    
    "e. fuscoguttatus x e. lanceolatus",
    "epinephelus spp",
    "hybrid",
    "use common genus",
    1,0,0,0,
    
    "morone chrysops x m. saxatilis",
    "morone spp",
    "hybrid",
    "use common genus",
    1,0,0,0,
    
    "oreochromis aureus x o. niloticus",
    "oreochromis spp",
    "hybrid",
    "use common genus",
    1,1,0,0,
    
    "p. mesopotamicus x c. macropomum",
    "serrasalmidae",
    "hybrid",
    "use common family; species Piaractus mesopotamicus (Holmberg, 1887) and Colossoma macropomum (Cuvier, 1816)",
    1,0,0,0,
    
    # match to sealifebase ----------------
    "tritia reticulata",
    "nassarius reticulatus",
    "match to sealifebase",
    "sealifebase does not include worms accepted name tritia reticulata; unaccepted name nassarius reticulatus is in sealifebase",
    1,0,0,0,
    
    # multiple taxa -------------
    "alosa alosa, a. fallax",
    "alosa spp",
    "multiple taxa",
    "use common genus",
    1,0,0,0,
    
    "astacidae, cambaridae",
    "decapoda",
    "multiple taxa",
    "use common order; smallest parent taxa rank in Fishbase schema is order decapoda; worms",
    1,0,0,0,
    
    "auxis thazard, a. rochei",
    "auxis spp",
    "multiple taxa",
    "use common genus",
    1,1,0,0,
    
    "loliginidae, ommastrephidae",
    "teuthida",
    "multiple taxa",
    "Manual description edit in clean_hs, possibly correct unaccepted order teuthida. Sealifebase list order Teuthida. worms; teuthida status uncertain > nomen dubium (includes [Myopsida + Oegopsida] which are not demonstrated to form a clade)",
    1,1,0,1,
    
    "sepiidae, sepiolidae",
    "sepiida",
    "multiple taxa",
    "use common order; worms and in sealifebase",
    1,0,0,0,
    
    "pandalus, pandalopsis",
    NA,
    "multiple names",
    "Need more info",
    1,
    
    # outdated/unaccepted name --------------
    "selachimorpha (pleurotremata)",
    "carcharhiniformes",
    "unaccepted name",
    "worms has selachimorpha and pleurotremata as unaccepted superorders; common accepted infraclass selachii. ARTIS currently defines sharks as a sting of orders. Could using Elasmobranchii class be appropriate? exists in fishbase",
    1,1,0,1,
    
    "crustacea",
    "malacostraca",
    "unaccepted name",
    "Use class malacostaca for crustaceans, vast majority of human consumption is decapoda and krill which are both within malacostaca. Unknown if there are other groups which are important to fishmeal.",
    1,0,0,0,
    
    "liza",
    "chelon spp",
    "unaccepted name",
    "Referring to mullets I believe; worms record for liza points to genus chelon with an uncertain status; chelon name is in fishbase; Previous correction was to genus planiliza but that doesn't make sense",
    1,0,0,0,
    
    "caspialosa",
    "alosa spp",
    "unaccepted name",
    "use accepted genus; in sealifebase",
    1,0,0,0,
    
    "doryteuthis (amerigo) gahi",
    "doryteuthis gahi",
    "unaccepted name",
    "use accepted name; in sealifebase",
    1,0,0,0,
    
    "doryteuthis (amerigo) pealeii",
    "doryteuthis pealeii",
    "unaccepted name",
    "use accepted name; in sealifebase",
    1,0,0,0,
    
    "scorpaeniformes",
    "perciformes/scorpaenoidei",
    "unaccepted name",
    "use accepted suborder pointed to in worms; fishbase uses hacky perciformes/scorpaenoidei classification in order column",
    1,0,0,0,
    
    "reptantia",
    NA,
    "unaccepted name",
    "previously corrected to cancridae family, but jumping from suborder to family may be excluding taxa. Reptantia are not just crabs but also lobsters. Need to spend more time looking into this",
    1,0,0,0,
    
    # spelling error ---------------
    "herklotsichthys quadrimaculat.",
    "herklotsichthys quadrimaculatus",
    "spelling error",
    "use full scientific name to match to fishbase",
    1,0,0,0,
    
    "pseudopleuronectes herzenst.",
    "pseudopleuronectes herzensteini",
    "spelling error",
    "use full scientific name to match to fishbase",
    1,0,0,0
    
  )
  
  

# write corrections to .csv -----------------------------------------------

  if(write_corrections) {
    fwrite(sciname_corrections, file.path(outdir, "prod_sciname_corrections.csv"), 
              row.names = FALSE)
  }  
  # replace scinames with corrections ---------------------------------------------
  
  prod_cleaned_scinames <- prod_df %>%
    mutate(SciName = tolower(sciname)) %>%
    left_join(sciname_corrections, by = c("sciname" = "sciname_original")) %>%
    mutate(sciname = if_else(!is.na(sciname_corrected), sciname_corrected, sciname)) %>%
    select(-c(sciname_corrected, correction_type, notes, in_prod_taxa_2024, 
              in_clean_hs, in_get_fmfo_species, in_match_hs_taxa)) %>%
  
  return(prod_cleaned_scinames)
}

