#' Get Visit Costs for Cohorts
#'
#' @param cdm CDM reference object
#' @param start_date Start date for filtering, format: YYYY-MM-DD (optional)
#' @param end_date End date for filtering, format: YYYY-MM-DD (optional)
#' @param exclude_unmapped Logical, whether to exclude unmapped values (Default: TRUE)
#' @param cohort_table Name of the cohort table (default: cdm$matched_cohort)
#'
#' @returns Tibble with visit cost data
#' @export
#'
#' @examples
#' \dontrun{
#' visit_costs <- get_visit_costs(cdm, cdm$matched_cohort,
#'                                 start_date = "2012-01-01",
#'                                 end_date = "2024-12-31")
#' }
get_visit_costs <- function(cdm, cohort_table = cdm$matched_cohort, start_date = NULL, end_date = NULL, exclude_unmapped = TRUE){

  # Validate inputs
  if (!inherits(cdm, "cdm_reference")){
    stop("cdm must be a CDM reference object from CDMConnector")
  }

  # Get visit costs
  visit_costs <- cohort_table %>%
    dplyr::inner_join(cdm$visit_occurrence, by = c("subject_id" = "person_id")) %>%
    dplyr::inner_join(cdm$cost, by = c("visit_occurrence_id" = "cost_event_id")) %>%
    dplyr::inner_join(cdm$concept, by = c("visit_concept_id" = "concept_id")) %>%
    dplyr::select(subject_id, cohort, total_paid, paid_by_payer, paid_by_patient, cost_domain_id, visit_start_date, visit_concept_id, concept_name) %>%
    dplyr::collect() %>%
    dplyr::mutate(year = lubridate::year(visit_start_date))

  # Date filter
  if (!is.null(start_date)) {
    visit_costs <- visit_costs %>%
      dplyr::filter(visit_start_date >= as.Date(start_date))
  }

  if (!is.null(end_date)) {
    visit_costs <- visit_costs %>%
      dplyr::filter(visit_start_date <= as.Date(end_date))
  }

  # Remove unmapped concepts
  if (exclude_unmapped){
    visit_costs <- visit_costs %>%
      dplyr::filter(visit_concept_id != 0)
  }

  return(visit_costs)
}

#' Get Procedure Costs for Cohorts
#'
#' @param cdm CDM reference object
#' @param start_date Start date for filtering, format: YYYY-MM-DD (optional)
#' @param end_date End date for filtering, format: YYYY-MM-DD (optional)
#' @param exclude_unmapped Logical, whether to exclude unmapped values (Default: TRUE)
#' @param cohort_table Name of the cohort table (default: cdm$matched_cohort)
#'
#' @returns Tibble with procedure cost data
#' @export
#'
#' @examples
#' \dontrun{
#' procedure_costs <- get_procedure_costs(cdm, cdm$matched_cohort,
#'                                 start_date = "2012-01-01",
#'                                 end_date = "2024-12-31")
#' }
get_procedure_costs <- function(cdm, cohort_table, start_date = NULL, end_date = NULL, exclude_unmapped = TRUE) {

  # Validate inputs
  if (!inherits(cdm, "cdm_reference")){
    stop("cdm must be a CDM reference object from CDMConnector")
  }


  # Get procedure costs
  procedure_costs <- cohort_table %>%
    dplyr::inner_join(cdm$procedure_occurrence, by = c("subject_id" = "person_id")) %>%
    dplyr::inner_join(cdm$cost, by = c("procedure_occurrence_id" = "cost_event_id")) %>%
    dplyr::inner_join(cdm$concept, by = c("procedure_concept_id" = "concept_id")) %>%
    dplyr::select(subject_id, cohort, total_paid, paid_by_payer, paid_by_patient, cost_domain_id, procedure_date, procedure_concept_id, concept_name) %>%
    dplyr::collect() %>%
    dplyr::mutate(year = lubridate::year(procedure_date))

  # Date filters
  if (!is.null(start_date)) {
    procedure_costs <- procedure_costs %>%
      dplyr::filter(procedure_date >= as.Date(start_date))
  }

  if (!is.null(end_date)) {
    procedure_costs <- procedure_costs %>%
      dplyr::filter(procedure_date <= as.Date(end_date))
  }

  # Remove unmapped concepts
  if (exclude_unmapped){
    procedure_costs <- procedure_costs %>%
      dplyr::filter(procedure_concept_id != 0)
  }

  return(procedure_costs)
}


#' Get Drug Costs for Cohorts
#'
#' @param cdm CDM reference object
#' @param start_date Start date for filtering, format: YYYY-MM-DD (optional)
#' @param end_date End date for filtering, format: YYYY-MM-DD (optional)
#' @param exclude_unmapped Logical, whether to exclude unmapped values (Default: TRUE)
#' @param cohort_table Name of the cohort table (default: cdm$matched_cohort)
#'
#' @returns Tibble with drug cost data
#' @export
#'
#' @examples
#' \dontrun{
#' drug_costs <- get_drug_costs(cdm, cdm$matched_cohort,
#'                                 start_date = "2012-01-01",
#'                                 end_date = "2024-12-31")
#' }
get_drug_costs <- function(cdm, cohort_table, start_date = NULL, end_date = NULL, exclude_unmapped = TRUE) {

  # Validate inputs
  if (!inherits(cdm, "cdm_reference")){
    stop("cdm must be a CDM reference object from CDMConnector")
  }

  # Get drug costs
  drug_costs <- cohort_table %>%
    dplyr::inner_join(cdm$drug_exposure, by = c("subject_id" = "person_id")) %>%
    dplyr::inner_join(cdm$cost, by = c("drug_exposure_id" = "cost_event_id")) %>%
    dplyr::inner_join(cdm$concept, by = c("drug_concept_id" = "concept_id")) %>%
    dplyr::select(subject_id, cohort, total_paid, paid_by_payer, paid_by_patient, cost_domain_id, drug_exposure_start_date, drug_concept_id, concept_name) %>%
    dplyr::collect() %>%
    dplyr::mutate(year = lubridate::year(drug_exposure_start_date))

  # Date filters
  if (!is.null(start_date)) {
    drug_costs <- drug_costs %>%
      dplyr::filter(drug_exposure_start_date >= as.Date(start_date))
  }

  if (!is.null(end_date)) {
    drug_costs <- drug_costs %>%
      dplyr::filter(drug_exposure_start_date <= as.Date(end_date))
  }

  # Remove unmapped concepts
  if (exclude_unmapped){
    drug_costs <- drug_costs %>%
      dplyr::filter(drug_concept_id != 0)
  }

  return(drug_costs)
}

#' Get All Costs for Cohorts
#'
#' Convenience function to get all cost types at once
#'
#' @param cdm CDM reference object
#' @param start_date Start date for filtering, format: YYYY-MM-DD (optional)
#' @param end_date End date for filtering, format: YYYY-MM-DD (optional)
#' @param exclude_unmapped Logical, whether to exclude unmapped values (Default: TRUE)
#' @param cohort_table Name of the cohort table (default: cdm$matched_cohort)
#' @param cost_types Vector of cost types to retrieve: "visit", "procedure", "drug"
#'
#' @return Tibble with all cost data combined with a 'type' column
#' @export
#' @examples
#' \dontrun{
#' all_costs <- get_all_costs(cdm, cdm$matched_cohort,
#'                            start_date = "2012-01-01",
#'                            end_date = "2024-12-31")
#' }
get_all_costs <- function(cdm, cohort_table, start_date = NULL, end_date = NULL, exclude_unmapped = TRUE,
                          cost_types = c("visit", "procedure", "drug")) {

  cost_list <- list()

  if ("visit" %in% cost_types) {
    message("Fetching visit costs...")
    cost_list$visit <- get_visit_costs(cdm, cohort_table, start_date, end_date, exclude_unmapped) %>%
      dplyr::mutate(type = "Visit")
  }

  if ("procedure" %in% cost_types) {
    message("Fetching procedure costs...")
    cost_list$procedure <- get_procedure_costs(cdm, cohort_table, start_date, end_date, exclude_unmapped) %>%
      dplyr::mutate(type = "Procedure")
  }

  if ("drug" %in% cost_types) {
    message("Fetching drug costs...")
    cost_list$drug <- get_drug_costs(cdm, cohort_table, start_date, end_date, exclude_unmapped) %>%
      dplyr::mutate(type = "Drug")
  }

  all_costs <- dplyr::bind_rows(cost_list)

  message("Done!")

  return(all_costs)
}
