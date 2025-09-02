#' @export
create_reweight_X_long <- function(country_est, V1, V2) {
  
  countries_to_analyze <- names(country_est) 
  
  reweight_X_long <- data.frame()
  for(i in 1:length(countries_to_analyze)){
    tmp <- reweight_X(country_est, countries_to_analyze[i], V1, V2)
    reweight_X_long <- reweight_X_long %>%
      bind_rows(tmp)
  }
  
  check_reweight_X_long <- reweight_X_long %>%
    group_by(iso3c, hs6) %>%
    summarize(reweighted_X = sum(reweighted_X, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(difference = 1 - reweighted_X) %>%
    filter(abs(difference) > 1e-9)

  # if (nrow(check_reweight_X_long)) {
  #   cli::cli_warn(c(
  #     "!" = "Reweighted X long proportions DO NOT sum to 1.",
  #     "Threshold: {.code abs(difference) > 1e-9}",
  #     "Problematic iso3c–hs6 pairs:",
  #     setNames(
  #       glue::glue_data(
  #         check_reweight_X_long,
  #         "{iso3c} \u2013 {hs6}: difference={format(difference, digits = 6, scientific = TRUE)}"
  #       ),
  #       rep("*", nrow(check_reweight_X_long))
  #     )
  #   ))
  #   return(NULL)
  # }

  # deps: cli, dplyr, glue
show_reweight_diag_cli <- function(reweight_X_long, tol = 1e-9, max_show = 100) {
  # Build diag_check
  diag_check <- reweight_X_long %>%
    dplyr::group_by(iso3c, hs6) %>%
    dplyr::summarize(
      sum_rew   = sum(reweighted_X, na.rm = TRUE),
      n_rows    = dplyr::n(),
      n_nonzero = sum(reweighted_X != 0, na.rm = TRUE),
      all_na    = all(is.na(reweighted_X)),
      .groups   = "drop"
    ) %>%
    dplyr::mutate(diff = 1 - sum_rew)

  zero_groups <- diag_check %>% dplyr::filter(n_nonzero == 0 | all_na)
  bad_sum_groups <- diag_check %>% dplyr::filter(n_nonzero > 0 & abs(diff) > tol)

  # Header + summary
  cli::cli_h2("Reweight X diagnostics")
  cli::cli_text("Tolerance: {.code abs(1 - sum_rew) > {format(tol, digits = 6, scientific = TRUE)}}")
  cli::cli_ul()
  cli::cli_li("{nrow(diag_check)} iso3c–hs6 groups checked")
  cli::cli_li("{nrow(zero_groups)} groups with no positive weights (sum == 0 or all NA)")
  cli::cli_li("{nrow(bad_sum_groups)} groups with non-unit sums beyond tolerance")
  cli::cli_end()

  # Helper to print a description list (name = 'iso3c – hs6', value = details)
  .emit_dl <- function(df, value_fmt, nmax = max_show) {
    if (!nrow(df)) return(invisible())
    if (nrow(df) > nmax) {
      cli::cli_text("{.em Note}: showing first {nmax} of {nrow(df)} rows")
      df <- df[seq_len(nmax), ]
    }
    keys <- glue::glue_data(df, "{iso3c} \u2013 {hs6}")
    vals <- glue::glue_data(df, value_fmt)
    names(vals) <- as.character(keys)
    cli::cli_dl(as.character(vals))
  }

  # Section A: zero/NA groups
  if (nrow(zero_groups)) {
    cli::cli_rule("Zero/NA groups")
    cli::cli_warn("Found iso3c–hs6 groups with no positive weights")
    .emit_dl(
      zero_groups,
      "sum={sprintf('%.12f', sum_rew)}; n_rows={n_rows}; n_nonzero={n_nonzero}; all_na={all_na}"
    )
  } else {
    cli::cli_alert_success("No zero/NA groups")
  }

  # Section B: groups that fail sum-to-1
  if (nrow(bad_sum_groups)) {
    cli::cli_rule("Non-unit sum groups")
    cli::cli_warn("Reweighted X proportions DO NOT sum to 1 beyond tolerance")
    .emit_dl(
      bad_sum_groups,
      "sum={sprintf('%.12f', sum_rew)}; diff={sprintf('%.12f', diff)}; n_nonzero={n_nonzero}; n_rows={n_rows}"
    )
  } else {
    cli::cli_alert_success("All remaining groups sum to 1 within tolerance")
  }

  invisible(diag_check)
}

diag_check <- show_reweight_diag_cli(reweight_X_long, tol = 1e-9)

  
  return(reweight_X_long)
}