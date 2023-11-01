#' @export
create_confidence_scores <- function(snet, prod, V1_long_in, snet_dir, hs_version_in, year_in) {
  
  # Format V1_long
  V1_long <- V1_long_in %>%
    separate(SciName, sep = "_", c("sciname", "habitat", "method")) %>%
    mutate(sciname = gsub("\\.", " ", sciname))
  
  dom_exp <- snet %>%
    filter(dom_source == "domestic")
  
  # Get all possible taxa produced by exporters that could go into an hs6 code in a given year
  domestic_taxa_hs6 <- dom_exp %>%
    # getting all hs6 codes by domestic exporter
    select(exporter_iso3c, hs6, year) %>%
    distinct() %>%
    # join all POSSIBLE taxa that could have been included in the hs6 code
    left_join(
      V1_long %>%
        filter(sciname %in% unique(snet$sciname)) %>%
        select(hs6, sciname) %>%
        distinct(),
      by = c("hs6")
    ) %>%
    # join all PRODUCED taxa that could have been included in the hs6 code by domestic exporter
    left_join(
      prod %>%
        rename(iso3c = country_iso3_alpha, sciname = SciName) %>%
        select(iso3c, sciname, year) %>%
        mutate(produced = 1) %>%
        distinct(),
      by = c("exporter_iso3c"="iso3c", "sciname", "year")
    ) %>%
    mutate(produced = case_when(
      is.na(produced) ~ 0,
      TRUE ~ produced
    )) %>%
    # only keep taxa that are produced by countries
    filter(produced == 1)
  
  # Domestic Export confidence scores-------------------------------------------
  domestic_taxa_counts <- domestic_taxa_hs6 %>%
    # Get possible taxa counts for each code by each domestic exporter
    group_by(exporter_iso3c, hs6, year) %>%
    summarize(taxa_count = sum(produced, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(hs_version = hs_version_in)
  
  write.csv(domestic_taxa_counts, file.path(snet_dir, "domestic_confidence_scores.csv"), row.names = FALSE)
  
  # Foreign Export confidence scores--------------------------------------------
  first_dom_exp <- read.csv(file.path(snet_dir, "first_dom_exp.csv"))
  first_foreign_exp <- read.csv(file.path(snet_dir, "first_foreign_exp.csv"))
  second_dom_exp <- read.csv(file.path(snet_dir, "second_dom_exp.csv"))
  
  first_dom_exp <- first_dom_exp %>%
    mutate(hs6_processed = as.character(hs6_processed),
           hs6_original = as.character(hs6_original)) %>%
    mutate(
      hs6_processed = case_when(
        str_length(hs6_processed) == 5 ~ paste("0", hs6_processed, sep = ""),
        TRUE ~ hs6_processed
      ),
      hs6_original = case_when(
        str_length(hs6_original) == 5 ~ paste("0", hs6_original, sep = ""),
        TRUE ~ hs6_original
      )
    )
  
  first_foreign_exp <- first_dom_exp %>%
    mutate(hs6_processed = as.character(hs6_processed),
           hs6_original = as.character(hs6_original)) %>%
    mutate(
      hs6_processed = case_when(
        str_length(hs6_processed) == 5 ~ paste("0", hs6_processed, sep = ""),
        TRUE ~ hs6_processed
      ),
      hs6_original = case_when(
        str_length(hs6_original) == 5 ~ paste("0", hs6_original, sep = ""),
        TRUE ~ hs6_original
      )
    )
  
  second_dom_exp <- first_dom_exp %>%
    mutate(hs6_processed = as.character(hs6_processed),
           hs6_original = as.character(hs6_original)) %>%
    mutate(
      hs6_processed = case_when(
        str_length(hs6_processed) == 5 ~ paste("0", hs6_processed, sep = ""),
        TRUE ~ hs6_processed
      ),
      hs6_original = case_when(
        str_length(hs6_original) == 5 ~ paste("0", hs6_original, sep = ""),
        TRUE ~ hs6_original
      )
    )
  
  
  # Finding all possible distinct taxa counts for foreign exports sourced by domestic exports (1st stage back)
  first_dom_taxa_count <- first_dom_exp %>%
    left_join(
      domestic_taxa_hs6 %>%
        select(-c("produced")),
      by = c("exporter_iso3c", "hs6_original"="hs6")
    ) %>%
    select(importer_iso3c, hs6_processed, re_exporter_iso3c, exporter_iso3c, sciname) %>%
    distinct() %>%
    group_by(importer_iso3c, hs6_processed, re_exporter_iso3c, exporter_iso3c) %>%
    summarize(taxa_count = n()) %>%
    ungroup()
  
  # Finding all possible distinct hs6 codes that went into the final hs6 code
  first_dom_hs6_count <- first_dom_exp %>%
    left_join(
      domestic_taxa_hs6 %>%
        select(-c("produced")),
      by = c("exporter_iso3c", "hs6_original"="hs6")
    ) %>%
    select(importer_iso3c, hs6_processed, re_exporter_iso3c, hs6_original, exporter_iso3c) %>%
    distinct() %>%
    group_by(importer_iso3c, hs6_processed, re_exporter_iso3c, exporter_iso3c) %>%
    summarize(hs6_count = n()) %>%
    ungroup()
  
  first_dom_counts <- first_dom_exp %>%
    select(importer_iso3c, re_exporter_iso3c, exporter_iso3c, hs6_processed) %>%
    distinct() %>%
    left_join(
      first_dom_taxa_count,
      by = c("importer_iso3c", "re_exporter_iso3c", "exporter_iso3c", "hs6_processed")
    ) %>%
    left_join(
      first_dom_hs6_count,
      by = c("importer_iso3c", "re_exporter_iso3c", "exporter_iso3c", "hs6_processed")
    )
  
  
  # Creating link between first and second stage of the supply chain
  foreign_export_original_link <- first_foreign_exp %>%
    # getting total by re exporter (intermediate exporter), current source country, hs6 original
    group_by(re_exporter_iso3c, exporter_iso3c, hs6_original) %>%
    mutate(total = sum(product_weight_t, na.rm = TRUE)) %>%
    ungroup() %>%
    # calculating proportion of flow over
    # total by re exporter (intermediate exporter), current source country, hs6 original
    mutate(prop_flow = product_weight_t / total) %>%
    select(importer_iso3c, re_exporter_iso3c, exporter_iso3c, hs6_processed, hs6_original, prop_flow) %>%
    distinct()
  
  # linking original importer re exporter and final source country
  # removes intermediate source country found in 1st foreign exports resolution
  relinked_second_dom_exp <- foreign_export_original_link %>%
    rename(hs6_final = hs6_processed) %>%
    rename(hs6_processed = hs6_original) %>%
    left_join(
      second_dom_exp %>%
        rename(source_country_iso3c = exporter_iso3c) %>%
        rename(exporter_iso3c = re_exporter_iso3c) %>%
        rename(re_exporter_iso3c = importer_iso3c),
      by = c("re_exporter_iso3c", "exporter_iso3c", "hs6_processed")
    ) %>%
    # multiply out proportion of overall re exporter and exporter flows from first foreign exp
    mutate(product_weight_t = product_weight_t * prop_flow)
  
  # Building out foreign domestic exports (overall_foreign_domestic_exports)
  overall_foreign_domestic_taxa_counts <- first_dom_exp %>%
    rename(hs6_final = hs6_processed) %>%
    bind_rows(relinked_second_dom_exp %>%
                select(importer_iso3c, re_exporter_iso3c, hs6_final, hs6_original, source_country_iso3c) %>%
                rename(exporter_iso3c = source_country_iso3c) %>%
                distinct()) %>%
    left_join(
      domestic_taxa_hs6,
      by = c("exporter_iso3c", "hs6_original"="hs6")
    ) %>%
    select(importer_iso3c, re_exporter_iso3c, exporter_iso3c, hs6_final, sciname) %>%
    distinct() %>%
    group_by(importer_iso3c, re_exporter_iso3c, exporter_iso3c, hs6_final) %>%
    summarize(taxa_count = n()) %>%
    ungroup()
  
  overall_foreign_domestic_hs6_counts <- relinked_second_dom_exp %>%
    select(importer_iso3c, re_exporter_iso3c, hs6_final, hs6_original, source_country_iso3c) %>%
    rename(exporter_iso3c = source_country_iso3c) %>%
    distinct() %>%
    group_by(importer_iso3c, re_exporter_iso3c, exporter_iso3c, hs6_final) %>%
    summarize(hs6_count = n()) %>%
    ungroup()
  
  overall_foreign_domestic_confidence <- overall_foreign_domestic_hs6_counts %>%
    full_join(
      overall_foreign_domestic_taxa_counts,
      by = c("importer_iso3c", "re_exporter_iso3c", "exporter_iso3c", "hs6_final")
    ) %>%
    filter(!is.na(exporter_iso3c)) %>%
    mutate(year = year_in,
           hs_version = hs_version_in)
  
  write.csv(overall_foreign_domestic_confidence, file.path(snet_dir, "foreign_confidence_scores.csv"), row.names = FALSE)
  
  return(NULL)
}
