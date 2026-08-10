#' Query FishBase/SeaLifeBase Synonym Table for Accepted Taxa Name
#'
#' Queries a FishBase or SeaLifeBase synonym table to find the accepted name for
#' a given scientific name. The input \code{sciname} is cleaned prior to matching
#' (dots and commas removed, hyphens replaced with spaces, lowercased).
#'
#' @param synonym_df A data frame containing FishBase or SeaLifeBase synonym
#'   records, expected to include columns \code{synonym}, \code{accepted_name},
#'   and \code{spec_code}. Typically produced by \code{\link{collect_fb_slb_data}}.
#' @param the_sciname A character string containing the scientific name (or
#'   potential synonym) to query against the FishBase/SeaLifeBase synonym table.
#'
#' @return A data frame with columns \code{synonym}, \code{spec_code}, and
#'   \code{status}. Returns 1 row with \code{status = "accepted"} if an accepted
#'   name is found, or 0 rows if no match is found.
#'
#' @section Assumption:
#' Each \code{sciname} must resolve to a single, unique taxonomic lineage. If a
#' \code{sciname} matches more than one row in the FishBase/SeaLifeBase synonym
#' table, the function will abort with an ARTIS assumption violation error.
#' Resolution requires a manual data correction in
#' \code{\link{clean_fb_slb_synonyms}} and rerunning \code{\link{collect_fb_slb_data}}.
#'
#' @seealso \code{\link{clean_fb_slb_synonyms}}, \code{\link{collect_fb_slb_data}}
#'
#' @import dplyr
#' @importFrom cli cli_abort
#' @export

query_synonyms <- function(synonym_df, the_sciname) {
  
  # clean the_sciname
  the_sciname <- tolower(the_sciname)
  the_sciname <- gsub('\\.', '', the_sciname) # eliminate dots
  the_sciname <- gsub(',', '', the_sciname) # eliminates commas
  the_sciname <- gsub('-', ' ', the_sciname) # replaces hyphens with spaces
  
  result <- synonym_df %>%
    filter(synonym == the_sciname)

  # Model assumption check - There is one accepted name per sciname
  if (nrow(result) > 1){
    cli::cli_h2("ARTIS Assumption Violated")
    cli::cli_abort(c(
      "Each {.field sciname} is expected to have a single hierarchical taxonomic classification scheme",
      "x" = "{.field sciname} {.val {the_sciname}} matched to a {.field {deparse(substitute(synonym_df))}} dataframe {.field synonym} value associated with more than one taxonomic scheme (i.e. more than one row)",
      "i" = "Requires developer to make a choice and add a manual data correction to {.fn clean_fb_slb_synonyms} and rerunning {.fn collect_fb_slb_data}"
    ),
    call = match.call())
  } else {

    # if there are no results return empty out_df dataframe
    out_df <- result %>%
      select(accepted_name, spec_code) %>%
      rename(synonym = accepted_name) %>%
      mutate(status = "accepted")
  
  return(out_df)

  }
}
