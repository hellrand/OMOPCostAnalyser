#' Prepare Cohorts for Matching
#'
#' Creates a temporary cohort table containing only the specified cohorts
#' This is required before matching cohorts
#'
#' @param cdm CDM reference object
#' @param target_cohort_id Target cohort definition ID
#' @param control_cohort_id Control cohort definition ID
#' @param write_schema Name of schema user has write access to
#' @param cohort Name of the source cohort table (default: "cohort")
#' @param temp_name Name for the temporary cohort table (default: "temp_cohort_match")
#'
#' @return CDM object with temporary cohort table added
#' @export
#' @examples
#' \dontrun{
#' cdm <- prepare_cohorts_for_matching(cdm, "user_write_schema",
#'                                      target_cohort_id = 18,
#'                                      control_cohort_id = 19)
#' }
prepare_cohorts_for_matching <- function(cdm, write_schema, cohort = cohort,
                                         target_cohort_id,
                                         control_cohort_id,
                                         temp_name = "temp_cohort_match"
                                         ) {

  if (!inherits(cdm, "cdm_reference")) {
    stop("cdm must be a CDM reference object from CDMConnector")
  }

  # Filter to only the cohorts we want to match
  message("Creating temporary cohort table with cohorts ",
          target_cohort_id, " and ", control_cohort_id)

  cdm[[temp_name]] <- cohort %>%
    dplyr::filter(cohort_definition_id %in% c(target_cohort_id, control_cohort_id)) %>%
    CDMConnector::computeQuery(
      name = temp_name,
      temporary = FALSE,
      schema = write_schema,
      overwrite = TRUE
    )

  # Convert to cohort table class
  if (requireNamespace("CDMConnector", quietly = TRUE)) {
    cdm[[temp_name]] <- omopgenerics::newCohortTable(
      table = cdm[[temp_name]],
      .softValidation = TRUE
    )
  }

  message("Temporary cohort table '", temp_name, "' created successfully")

  return(cdm)
}

#' Match Cohorts by Age and Index Date
#'
#' Matches a target cohort to a control cohort based on demographics
#'
#' @param cdm CDM reference object (should have temp cohort table from prepare_cohorts_for_matching)
#' @param target_cohort_id Target cohort definition ID
#' @param cohort_table_name Name of the cohort table to use (default: "temp_cohort_match")
#' @param match_sex Logical, whether to match on sex (default: TRUE)
#' @param match_year_of_birth Logical, whether to match on year of birth (default: TRUE)
#' @param ratio Number of allowed matches per individual in the target cohort (default 1:1)
#' @param matched_name Name for the matched cohort table (default: "matched_cohort")
#' @param soft_validation Logical, whether to perform a soft validation of consistency. If set to FALSE four additional checks will be performed: 1) a check that cohort end date is not before cohort start date, 2) a check that there are no missing values in required columns, 3) a check that cohort duration is all within observation period, and 4) that there are no overlapping cohort entries (default: FALSE)
#'
#' @return CDM object with matched cohorts added
#' @export
#' @examples
#' \dontrun{
#' # First prepare cohorts
#' cdm <- prepare_cohorts_for_matching(cdm, "user_write_schema",
#'                                      target_cohort_id = 18,
#'                                      control_cohort_id = 19)
#'
#' # Then match them
#' cdm <- match_cohorts(cdm, 18)
#'
#' }
match_cohorts <- function(cdm,
                          target_cohort_id,
                          cohort_table_name = "temp_cohort_match",
                          match_sex = TRUE,
                          match_year_of_birth = TRUE,
                          ratio = 1,
                          soft_validation = FALSE,
                          matched_name = "matched_cohort") {

  if (!requireNamespace("CohortConstructor", quietly = TRUE)) {
    stop("CohortConstructor package is required for matching. Install with:\n",
         "remotes::install_github('OHDSI/CohortConstructor')")
  }

  if (!inherits(cdm, "cdm_reference")) {
    stop("cdm must be a CDM reference object")
  }

  # Check if cohort table exists
  if (!cohort_table_name %in% names(cdm)) {
    stop(paste("Cohort table", cohort_table_name, "not found in CDM.\n",
               "Did you run prepare_cohorts_for_matching() first?"))
  }

  message("Matching cohorts...")
  message("Target cohort: ", target_cohort_id)
  message("Matching on year of birth: ", match_year_of_birth)
  message("Matching on sex: ", match_sex)
  message("Ratio: 1:", ratio)
  message("Soft validation: ", soft_validation)

  # Perform matching
  matched <- CohortConstructor::matchCohorts(
    cohort = cdm[[cohort_table_name]],
    name = matched_name,
    cohortId = target_cohort_id,
    matchSex = match_sex,
    matchYearOfBirth = match_year_of_birth,
    ratio = ratio,
    .softValidation = soft_validation
  )

  # Add matched cohort to CDM
  cdm[[matched_name]] <- omopgenerics::newCohortTable(
    table = matched,
    .softValidation = TRUE
  )

  # Add cohort names
  cdm[[matched_name]] <- cdm[[matched_name]] %>%
    mutate(cohort = ifelse(cohort_definition_id == 1, "Target", "Control"))

  # Print summary with match status
  if (requireNamespace("CohortConstructor", quietly = TRUE)) {
    message("\n========== Matching Complete! ==========")

    tryCatch({
      # Get cohort counts
      count_summary <- CohortConstructor::cohortCount(matched) %>%
        dplyr::collect()

      # Get settings to identify target vs control
      settings <- CohortConstructor::settings(matched) %>%
        dplyr::collect()

      # Combine counts with settings to show status
      result <- count_summary %>%
        dplyr::left_join(settings, by = "cohort_definition_id") %>%
        dplyr::select(cohort_definition_id, cohort_name,
                      number_records, number_subjects)

      message("\nMatched Cohort Summary:")
      print(result, n = Inf)

      # Also print which is target and which is control
      target_info <- result %>%
        dplyr::filter(grepl("target", tolower(cohort_name)) |
                        cohort_definition_id == min(cohort_definition_id))

      control_info <- result %>%
        dplyr::filter(grepl("control", tolower(cohort_name)) |
                        cohort_definition_id == max(cohort_definition_id))

      message("\n--- Match Status ---")
      if (nrow(target_info) > 0) {
        message("Target (ID ", target_info$cohort_definition_id[1], "): ",
                target_info$number_subjects[1], " subjects")
      }
      if (nrow(control_info) > 0) {
        message("Control (ID ", control_info$cohort_definition_id[1], "): ",
                control_info$number_subjects[1], " subjects")
      }
      message("========================================\n")

    }, error = function(e) {
      message("Could not retrieve detailed cohort information")
      message("Error: ", e$message)

      # Fallback to simple count
      simple_count <- cdm$matched_name %>%
        dplyr::group_by(cohort_definition_id) %>%
        dplyr::summarise(n_subjects = dplyr::n_distinct(subject_id),
                         .groups = "drop") %>%
        dplyr::collect()

      message("\nMatched cohort counts:")
      print(simple_count)
    })
  }

  return(cdm)
}

#' Complete Cohort Matching Workflow
#'
#' Convenience function that prepares and matches cohorts in one step
#'
#' @param cdm CDM reference object
#' @param target_cohort_id Target cohort definition ID
#' @param control_cohort_id Control cohort definition ID
#' @param match_sex Logical, whether to match on sex
#' @param match_year_of_birth Logical, whether to match on year of birth
#' @param ratio Matching ratio
#' @param write_schema Name of schema user has write access to
#' @param cohort Name of the source cohort table (default: "cohort")
#' @param matched_name Name for matched cohort table (default: "matched_cohort")
#'
#' @return CDM object with matched cohorts
#' @export
#' @examples
#' \dontrun{
#' cdm <- match_cohorts_workflow(cdm, "user_write_schema", cohort, 18, 19)
#'
#' # Get costs for matched cohorts
#' matched_costs <- get_all_costs(cdm, cdm$matched_cohort)
#' }
match_cohorts_workflow <- function(cdm, write_schema, cohort = cohort,
                                   target_cohort_id,
                                   control_cohort_id,
                                   match_sex = TRUE,
                                   match_year_of_birth = TRUE,
                                   ratio = 1,
                                   matched_name = "matched_cohort") {

  # Step 1: Prepare cohorts
  cdm <- prepare_cohorts_for_matching(
    cdm = cdm,
    write_schema = write_schema,
    cohort = cohort,
    target_cohort_id = target_cohort_id,
    control_cohort_id = control_cohort_id,
    temp_name = "temp_cohort_match"
  )

  # Step 2: Match cohorts
  cdm <- match_cohorts(
    cdm = cdm,
    target_cohort_id = target_cohort_id,
    cohort_table_name = "temp_cohort_match",
    match_sex = match_sex,
    match_year_of_birth = match_year_of_birth,
    ratio = ratio,
    matched_name = matched_name
  )


  message("\nMatching workflow complete!")
  message("Matched cohorts are available in cdm$", matched_name)
  message("Use cdm$", matched_name, " as cohort table")

  return(cdm)
}
