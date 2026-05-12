#' Calculate Annual Costs by Person
#'
#' @param cost_data Cost data from get_*_costs functions
#'
#' @return Tibble with annual costs per person
#' @export
calculate_annual_costs <- function(cost_data) {

  result <- cost_data %>%
    dplyr::group_by(subject_id, year, cohort) %>%
    dplyr::summarise(
      annual_cost = sum(total_paid, na.rm = TRUE),
      annual_payer_cost = sum(paid_by_payer, na.rm = TRUE),
      annual_patient_cost = sum(paid_by_patient, na.rm = TRUE),
      .groups = "drop"
    )

  return(result)
}

#' Calculate Cost Per Person Per Year
#'
#' @param annual_cost_data Annual cost data from calculate_annual_costs()
#'
#' @return Tibble with cost per person per year metrics
#' @export
#' @examples
#' \dontrun{
#' # Get all costs
#' all_costs <- get_all_costs(cdm, cdm$matched_cohort,
#'                            start_date = "2012-01-01",
#'                            end_date = "2024-12-31")
#'
#' # Calculate annual costs
#' annual_costs <- calculate_annual_costs(all_costs)
#'
#' # Calculate average costs
#' avg_per_person_year <- calculate_average_cost_per_person_year(annual_costs)
#'
#' }
calculate_average_cost_per_person_year <- function(annual_cost_data) {

  result <- annual_cost_data %>%
    dplyr::group_by(year, cohort) %>%
    dplyr::summarise(
      n_patients = n(),
      average_cost_per_person = mean(annual_cost, na.rm = TRUE),
      average_cost_payer_per_person = mean(annual_payer_cost, na.rm = TRUE),
      average_cost_patient = mean(annual_patient_cost, na.rm = TRUE),
      sd_cost = sd(annual_cost, na.rm = TRUE),
      total_cost = sum(annual_cost, na.rm = TRUE),
      .groups = "drop"
    )

  return(result)
}

#' Calculate Summary Statistics
#'
#' @param cost_data Cost data tibble from calculate_annual_costs function
#' @param group_vars Character vector of variables to group by
#'
#' @return Tibble with summary statistics
#' @export
calculate_cost_summary <- function(cost_data, group_vars = "cohort") {

  result <- cost_data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
    dplyr::summarise(
      n_patients = dplyr::n_distinct(subject_id),
      mean_cost = mean(annual_cost, na.rm = TRUE),
      median_cost = median(annual_cost, na.rm = TRUE),
      mean_cost_insurance = mean(annual_payer_cost, na.rm = TRUE),
      mean_cost_patient = mean(annual_patient_cost, na.rm = TRUE),
      sd_cost = sd(annual_cost, na.rm = TRUE),
      total_cost = sum(annual_cost, na.rm = TRUE),
      .groups = "drop"
    )

  return(result)
}

#' Prepare Data for Shiny Application
#'
#' Creates 2 tables: aggregated summary by cost type, year, and cohort; person-level costs
#'
#' @param all_costs Combined cost data from get_all_costs()
#'
#' @return List
#' @export
prepare_types_data <- function(all_costs) {

# Get unique patients
  unique_patients <- all_costs %>%
    dplyr::group_by(cohort, type) %>%
    dplyr::summarise(
      unique_patients = dplyr::n_distinct(subject_id),
      .groups = "drop"
    )

# Calculate person-level annual costs
  person_year_costs <- all_costs %>%
    dplyr::group_by(cohort, type, year, subject_id)%>%
    dplyr::summarise(
      person_annual_cost = sum(total_paid, na.rm = TRUE),
      paid_by_payer = sum(paid_by_payer, na.rm = TRUE),
      paid_by_patient = sum(paid_by_patient, na.rm = TRUE),
      .groups = "drop"
      )

  # Calculate annual costs by_cohort table
  person_costs <- calculate_annual_costs(all_costs)


  # Calculate costs for graph and other tables
  types <- person_year_costs %>%
    dplyr::group_by(cohort, type, year) %>%
    dplyr::summarise(
      n_patients = dplyr::n_distinct(subject_id),
      total_cost = sum(person_annual_cost, na.rm = TRUE),
      paid_by_payer = sum(paid_by_payer, na.rm = TRUE),
      paid_by_patient = sum(paid_by_patient, na.rm = TRUE),
      mean_per_person = mean(person_annual_cost, na.rm = TRUE),
      median_per_person = median(person_annual_cost, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::left_join(unique_patients, by = c("cohort", "type"))

  return(list(
    aggregated = types,
    person_level = person_costs,
    person_type = person_year_costs
  ))
}

#' Prepare Data for Analysis by Type and Year
#'
#' Creates aggregated summary by cost type, year, and cohort
#'
#' @param all_costs Combined cost data from get_all_costs()
#'
#' @return Tibble ready for visualization
#' @export
prepare_graph_data <- function(costs) {

  graph_data <- costs %>%
    dplyr::group_by(cohort, year) %>%
    dplyr::summarise(
      n_patients = dplyr::n_distinct(subject_id),
      total_cost = sum(person_annual_cost, na.rm = TRUE),
      paid_by_payer = sum(paid_by_payer, na.rm = TRUE),
      paid_by_patient = sum(paid_by_patient, na.rm = TRUE),
      .groups = "drop"
    )

  return(graph_data)
}

#' Prepare Data for Analysis by Type and Year
#'
#' Creates aggregated summary by cost type, year, and cohort
#'
#' @param all_costs Combined cost data from get_all_costs()
#'
#' @return Tibble ready for visualization
#' @export
prepare_type_graph_data <- function(all_costs) {

  graph_data <- all_costs %>%
    dplyr::group_by(cohort, type, year) %>%
    dplyr::summarise(
      n_patients = dplyr::n_distinct(subject_id),
      total_cost = sum(person_annual_cost, na.rm = TRUE),
      paid_by_payer = sum(paid_by_payer, na.rm = TRUE),
      paid_by_patient = sum(paid_by_patient, na.rm = TRUE),
      .groups = "drop"
    )

  return(graph_data)
}
