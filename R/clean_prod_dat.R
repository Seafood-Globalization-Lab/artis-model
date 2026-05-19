#' Clean and harmonize raw FAO or SAU production data
#'
#' @description
#' Source-specific ingestion and cleaning of FAO or SAU raw production data.
#' Produces a cleaned `prod_ts` data frame with rank-identification columns
#' (`Species01`, `Genus01`, `Family01`, `Other01`) ready for downstream
#' matching to FishBase/SeaLifeBase taxonomy.
#'
#' Commented-out correction-join blocks that previously lived in this section
#' of `classify_prod_dat()` have been intentionally removed — manual
#' corrections are now applied in [match_prod_taxa_to_fbslb()].
#'
#' @param prod_df Data frame. Raw production data (FAO or SAU).
#' @param prod_data_source Character. One of `"FAO"` or `"SAU"`.
#' @param datadir Character or `NULL`. Directory containing the SAU
#'   sci-to-common name CSV. Required when `prod_data_source == "SAU"`,
#'   ignored otherwise.
#' @param SAU_sci_2_common Character or `NA`. Filename (relative to `datadir`)
#'   of the SAU scientific → common name mapping CSV
#'   (e.g. `"TaxonFunctionalCommercial_Clean.csv"`). Used only when
#'   `prod_data_source == "SAU"`.
#'
#' @return A data frame (`prod_ts`) with cleaned production time-series data
#'   and rank-identification columns (`Species01`, `Genus01`, `Family01`,
#'   `Other01`).
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @import stringr
#' @import data.table
#' @export

clean_prod_dat <- function(
  prod_df,
  prod_data_source = "FAO",
  datadir = NULL,
  SAU_sci_2_common = NA
) {

  # FAO Production Data Cleaning -------------------------------------------
  if (prod_data_source == "FAO") {

    prod_ts <- prod_df %>%
      dplyr::rename(
        CommonName          = species_name_en,
        SciName             = species_scientific_name,
        country_iso3_alpha   = country_iso3_code,
        country_iso3_numeric = country
      ) %>%
      dplyr::mutate(
        CommonName = tolower(as.character(CommonName)),
        SciName    = tolower(as.character(SciName))
      ) %>%
      # Trim any leading/trailing whitespace
      dplyr::mutate_all(str_trim) %>%
      # Filter out groups not considered in this analysis
      dplyr::filter(
        !species_major_group %in%
          c("PLANTAE AQUATICAE", "AMPHIBIA, REPTILIA", "MAMMALIA"),
        # exclude corals, sponges, pearl oysters, shells
        # yearbook_group_en value "Other aquatic animals & products" notation
        # is subject to change between FAO prod releases — use flexible regex
        !str_detect(
          yearbook_group_en,
          regex(
            "^other\\s+aq(?:\\.|uatic)\\s+animals?\\s*(?:&|and)?\\s*products?$",
            ignore_case = TRUE
          )
        )
      ) %>%
      # Exclude copepods: does not map to any HS code considered in ARTIS
      filter(SciName != "calanus finmarchicus") %>%
      droplevels() %>%
      # Remove "(=...)" notation, e.g. "salmoniformes (=salmonoidei)"
      mutate(
        SciName = gsub(SciName, pattern = " \\(\\=.*", replacement = "")
      )

    # Identify taxonomic ranks
    prod_ts$Species01 <- 0
    prod_ts$Genus01   <- 0
    prod_ts$Family01  <- 0
    prod_ts$Other01   <- 0

    prod_ts$Genus01[grepl(prod_ts$SciName, pattern = "spp")] <- 1
    prod_ts$Family01[
      grepl(pattern = " ", prod_ts$SciName) == FALSE &
        grepl(pattern = "([^\\s])*dae", prod_ts$SciName)
    ] <- 1
    prod_ts$Species01[
      grepl(prod_ts$SciName, pattern = " ") &
        prod_ts$Family01 == 0 &
        prod_ts$Genus01  == 0
    ] <- 1
    prod_ts$Other01[
      prod_ts$Species01 == 0 &
        prod_ts$Genus01  == 0 &
        prod_ts$Family01 == 0
    ] <- 1

    # Remove " spp" suffix now that genera are identified
    prod_ts <- prod_ts %>%
      mutate(SciName = gsub(SciName, pattern = " spp", replacement = ""))

    # Final type coercion
    prod_ts <- prod_ts %>%
      mutate(quantity = as.numeric(quantity), year = as.integer(year))
  }

  # SAU Production Data Cleaning -------------------------------------------
  if (prod_data_source == "SAU") {

    prod_ts <- prod_df %>%
      mutate(scientific_name = tolower(scientific_name)) %>%
      rename(
        quantity       = sum,
        CommonName     = common_name,
        SciName        = scientific_name,
        country_name_en = fishing_entity
      ) %>%
      mutate(
        SciName    = tolower(SciName),
        CommonName = tolower(CommonName)
      )

    sci_2_common <- fread(
      file.path(datadir, SAU_sci_2_common),
      stringsAsFactors = FALSE,
      data.table = FALSE
    ) %>%
      mutate(scientific_name = tolower(scientific_name))

    # Remove retired scientific names
    sci_2_common <- sci_2_common %>%
      filter(!grepl(comments_names, pattern = "retired"))

    # Keep only the latest taxon key per scientific name
    latest_taxon_keys <- sci_2_common %>%
      group_by(scientific_name) %>%
      summarize(taxon_key = max(taxon_key))

    sci_2_common <- sci_2_common %>%
      filter(taxon_key %in% latest_taxon_keys$taxon_key)

    prod_ts <- prod_ts %>%
      left_join(
        sci_2_common %>% select(-common_name),
        by = c("SciName" = "scientific_name")
      ) %>%
      mutate_all(str_trim) %>%
      filter(!is.na(SciName))

    # Identify taxonomic ranks from taxon_level_id
    prod_ts$Species01 <- 0
    prod_ts$Genus01   <- 0
    prod_ts$Family01  <- 0
    prod_ts$Other01   <- 0

    prod_ts$Species01[which(prod_ts$taxon_level_id == 6)] <- 1
    prod_ts$Genus01[which(prod_ts$taxon_level_id == 5)]   <- 1
    prod_ts$Family01[which(prod_ts$taxon_level_id == 4)]  <- 1
    prod_ts$Other01[which(prod_ts$taxon_level_id < 4)]    <- 1

    # Manual overrides for SAU taxa that taxon_level_id misclassifies
    prod_ts <- prod_ts %>%
      mutate(
        Genus01 = case_when(
          SciName == "centrophorus"   ~ 1,
          SciName == "sarda"          ~ 1,
          SciName == "crenimugil"     ~ 1,
          SciName == "balistes"       ~ 1,  # Triggerfish
          SciName == "ophichthus"     ~ 1,  # Snake eel
          SciName == "tetrapturus"    ~ 1,  # Genus of marlins called spearfish
          SciName == "polyprion"      ~ 1,  # Genus of ray-finned fish
          SciName == "chelidonichthys" ~ 1, # Genus of ray-finned fish
          SciName == "illex"          ~ 1,  # Shortfin squid
          SciName == "alloteuthis"    ~ 1,  # Pencil squid
          SciName == "branchiostegus" ~ 1,  # Tilefish
          SciName == "caulolatilus"   ~ 1,  # Tilefish
          SciName == "symphodus"      ~ 1,  # Wrasses
          SciName == "labrus"         ~ 1,  # Wrasses
          SciName == "ommastrephes"   ~ 1,  # Neon flying squid
          SciName == "aphanopus"      ~ 1,  # Black scabbardfishes
          SciName == "cepola"         ~ 1,  # Red bandfish
          SciName == "liocarcinus"    ~ 1,  # Flying and vernal crab
          SciName == "trisopterus"    ~ 1,  # Small cods
          SciName == "venerupis"      ~ 1,  # Marine bivalve molluscs (Veneridae)
          SciName == "azurina"        ~ 1,
          TRUE ~ Genus01
        ),
        Family01 = case_when(
          SciName == "cardiidae"    ~ 1,  # Giant Clams
          SciName == "merlucciidae" ~ 1,  # Hakes
          SciName == "solenoceridae" ~ 1, # Decapods
          SciName == "moronidae"    ~ 1,  # Family of perciform fishes
          SciName == "pomatomidae"  ~ 1,  # Bluefish within Perciformes
          SciName == "salpidae"     ~ 1,  # Salp
          SciName == "strombidae"   ~ 1,  # True conch
          TRUE ~ Family01
        ),
        Other01 = case_when(
          SciName == "osteichthyes"    ~ 1,
          SciName == "pleuronectoidei" ~ 1,
          SciName == "mytilida"        ~ 1,  # Order of molluscs
          SciName == "rhizostomeae"    ~ 1,  # Order of jellyfish
          SciName == "neogastropoda"   ~ 1,
          TRUE ~ Other01
        )
      )
  }

  prod_ts
}
