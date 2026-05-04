#' Identify species associated with fishmeal and fish oil (FMFO) production.
#'
#' This function reads SAU production data and identifies species whose production is
#' allocated to fishmeal and fish oil based on user-defined thresholds. It returns
#' a list of species marked as "primary_fishmeal" for prioritization in downstream consumption.
#'
#' @param sau_fp File path to SAU production data (CSV format).
#' @param fishmeal_min_threshold_sp Minimum percentage of a species' total production
#'        (across all countries) that must be allocated to fishmeal for it to be included
#'        in the FMFO list. Default is 1.
#' @param fishmeal_min_threshold_global Minimum percentage of the global fishmeal production
#'        that a species must contribute in order to be included in the FMFO list. Default is 0.5.
#' @param fishmeal_primary_threshold Percentage of a species’ total production that must
#'        go to fishmeal for it to be labeled as `primary_fishmeal = 1`. These species are considered
#'        to have a strong functional association with fishmeal production. Default is 75.
#'
#' @export

get_fmfo_species <- function(sau_fp,
                             fishmeal_min_threshold_sp = 1,
                             fishmeal_min_threshold_global = 0.5,
                             fishmeal_primary_threshold = 75) {
  
  # fishmeal_min_threshold default is 1% of production going to FM: 
  #this defines the species allowed to go into FM
  
  # fishmeal_primary_threshold default is min of 75% of production is going into FM : 
  # this defines the species that are favored to go into FM
  
  #-----------------------------------------------------------------------------
  # Production Data
  sau <- fread(sau_fp)
  
  # create taxa list that assigns end_use fishmeal to all taxa - allows function flexibility
  # to match all taxa to fishmeal when thresholds set to 0
  sau_sp <- sau %>% 
    select(SciName) %>% 
    distinct() %>% 
    mutate(end_use = "Fishmeal and fish oil") 
  
  #-----------------------------------------------------------------------------
  # Get fmfo list based on percent produced (SAU)
  sau_grouped <- sau_sp %>%
    # include sau taxa and end_use catagorization
    full_join(sau,
              by = c("SciName", "end_use")) %>%   
    # set quantity to 0 for sau_sp observations
    replace_na(list(quantity = 0)) %>% 
    group_by(SciName, end_use) %>%
    summarize(quantity = sum(quantity, na.rm = TRUE)) %>%
    ungroup() %>% 
    group_by(SciName) %>%
    # Sciname total quantity across all end_uses - assigned to each row of a sciname
    mutate(total = sum(quantity, na.rm = TRUE)) %>%
    ungroup() %>% 
    filter(end_use == "Fishmeal and fish oil") %>% 
    # quantity is fishmeal only quantity
           # percent_sp is percent of species production going into FM
    mutate(percent_sp = 100 * quantity / total,
           # percent_global is percent FM prod by species of all global FM prod
           percent_global = 100 * quantity / sum(quantity)) %>% 
    arrange(desc(percent_global)) %>% 
    mutate(percent_cumulative = cumsum(percent_global))
  
  fmfo_species <- sau_grouped %>%
    filter(percent_sp >= fishmeal_min_threshold_sp | percent_global >= fishmeal_min_threshold_global) %>% 
    mutate(primary_fishmeal = case_when(
      percent_sp >= fishmeal_primary_threshold ~ 1,
      TRUE ~ 0))
  
  fmfo_species_output <- fmfo_species %>% 
    select(SciName, primary_fishmeal) %>%
    # Retain only true species names
    filter(str_detect(SciName, pattern = "\\ ")) %>%
    mutate(data_source = "SAU") %>%
    # Add species from Clawson et al. which pulls from industry reports
    bind_rows(
      data.frame(SciName = c(
        "gadus chalcogrammus", "thunnus alalunga", "strangomera bentincki",      
        "merluccius hubbsi", "thunnus thynnus", "gadus morhua",               
        "salmo salar", "thunnus obesus", "auxis rochei",               
        "oncorhynchus kisutch", "coryphaena hippurus", "gymnosarda unicolor",        
        "auxis thazard", "siganus guttatus", "scomberomorus guttatus",     
        "thunnus tonggol", "prionotus carolinus", "sarda lineolata",            
        "trachurus symmetricus", "pollachius pollachius", "oncorhynchus mykiss",        
        "sardinella aurita", "pollachius virens", "katsuwonus pelamis",         
        "ammodytes tobianus", "gymnammodytes semisquamatus", "thunnus maccoyii",           
        "eubleekeria splendens", "thunnus albacares"  
      ), 
      primary_fishmeal = 0, 
      data_source = "industry reports")
    ) %>%
    bind_rows(
      data.frame(SciName = c(
        "pandalus montagui", "lophius piscatorius", "pleoticus muelleri",          
        "illex argentinus", "artemesia longinaris",  "heterocarpus ensifer",        
        "hippoglossus hippoglossus", "solenocera membranacea", "ambassis gymnocephalus",      
        "sardinella lemuru", "okamejei kenojei", "lates calcarifer",
        "platycephalus indicus", "paralichthys olivaceus", "sebastes mentella",           
        "metapenaeopsis barbata", "selar crumenophthalmus",  "parastromateus niger",        
        "aphanopus carbo", "arbacia lixula", "spondyliosoma cantharus",  
        "lophius budegassa", "sarotherodon melanotheron", "thunnus atlanticus",          
        "acanthopagrus schlegelii", "atrobucca nibe", "doederleinia berycoides",     
        "carcharhinus limbatus", "aristeus antennatus", "macruronus novaezelandiae",   
        "molva dypterygia", "prionace glauca", "oreochromis aureus",          
        "fistularia commersonii", "harpadon nehereus", "ethmalosa fimbriata",         
        "oedalechilus labeo", "saurida undosquamis", "trachurus capensis",          
        "pampus chinensis", "oncorhynchus tshawytscha", "oncorhynchus keta",           
        "crangon crangon", "solea solea", "penaeus brevirostris",        
        "muraenesox cinereus", "parapenaeus longirostris", "soletellina diphos",          
        "konosirus punctatus", "cancer pagurus", "ilisha elongata",             
        "platichthys flesus", "merluccius merluccius", "pleuronectes platessa",      
        "dicentrarchus labrax", "loligo vulgaris", "lepidorhombus boscii",     
        "aristaeomorpha foliacea", "penaeus monodon", "sparus aurata",             
        "plesionika martia", "nemipterus virgatus", "ctenopharyngodon idella",     
        "pecten maximus", "carcinus maenas",  "reinhardtius hippoglossoides",
        "holthuispenaeopsis atlantica", "melanogrammus aeglefinus", "atherinomorus lacunosus",     
        "ariomma indica", "rastrelliger kanagurta", "istiophorus platypterus",     
        "probarbus jullieni", "seriola quinqueradiata", "todarodes pacificus",         
        "hyporhamphus sajori", "lateolabrax japonicus", "hemitriakis japanica",        
        "zeus faber", "euthynnus affinis", "larimichthys crocea",         
        "oligoplites saurus", "microstomus kitt", "molva molva",                 
        "euthynnus alletteratus", "oreochromis macrochir", "sarotherodon galilaeus",      
        "lutjanus argentimaculatus", "lepidorhombus whiffiagonis", "miichthys miiuy",             
        "mene maculata", "siganus fuscescens", "oreochromis mossambicus",     
        "cirrhinus molitorella", "scomberomorus commerson", "plesionika narval",           
        "oreochromis niloticus", "nephrops norvegicus", "sebastes viviparus",          
        "meganyctiphanes norvegica", "carcharhinus longimanus", "thunnus orientalis",          
        "gadus macrocephalus", "hippoglossus stenolepis", "doryteuthis gahi",            
        "selene peruviana", "clarias batrachus", "sphyraena jello",             
        "pasiphaea multidentata", "oncorhynchus gorbuscha", "trisopterus luscus",          
        "priacanthus macracanthus", "chelidonichthys cuculus", "urophycis chuss",             
        "pterois volitans",  "alpheus glaber", "etrumeus sadina",             
        "coptodon zillii", "coptodon rendalli", "oreochromis spilurus",        
        "ovalipes punctatus", "aristaeopsis edwardsiana", "salmo trutta",                
        "martialia hyadesi", "calappa granulata", "metapenaeus joyneri",         
        "rastrelliger brachysoma", "oligoplites refulgens", "alepes djedaba",              
        "carcharhinus falciformis", "pampus argenteus", "polydactylus sexfilis",       
        "benthosema pterotum", "gerres oblongus", "allothunnus fallai",          
        "scyliorhinus canicula", "oncorhynchus nerka", "penaeus notialis",            
        "maja squinado", "lutjanus guttatus", "amblygaster sirm",            
        "pangasianodon hypophthalmus", "kajikia audax", "aristeus varidens",           
        "plesionika edwardsii", "gerres longirostris", "mullus surmuletus",           
        "xiphias gladius", "raja clavata", "gerres filamentosus",         
        "stephanolepis cirrhifer", "oreochromis andersonii", "alopias vulpinus",            
        "oreochromis shiranus", "scophthalmus maximus", "brosme brosme",               
        "bregmaceros mcclellandi", "aluterus monoceros", "rapana venosa",               
        "necora puber",  "genyonemus lineatus", "pasiphaea sivado",            
        "urophycis tenuis", "chirocentrus nudus", "etrumeus whiteheadi",         
        "penaeus vannamei", "merlangius merlangus", "glyptocephalus cynoglossus",  
        "mulloidichthys martinicus", "acanthopagrus latus", "limanda aspera",              
        "selaroides leptolepis", "sphyraena flavicauda", "atule mate" 
      ), 
      primary_fishmeal = 0, 
      data_source = "grey literature")
    )

  return(fmfo_species_output)
}


