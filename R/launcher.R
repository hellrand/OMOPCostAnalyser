#' Launch Interactive Cost Analysis Shiny App
#'
#' @param types_data Pre-computed types data (required)
#' @export
#' @examples
#' \dontrun{
#' # Prepare data
#' types_data <- prepare_types_data(all_costs)
#'
#' # Launch app
#' launch_cost_app(types_data)
#' }
launch_cost_app <- function(types_data) {

  if (is.null(types_data$aggregated) || is.null(types_data$person_level) || is.null(types_data$person_type)) {
    stop("types_data is required. Use prepare_types_data() to create it from your cost data.")
  }

  appDir <- system.file("shiny-app", package = "OMOPCostAnalyser")

  if (appDir == "") {
    # fallback for devtools::load_all()
    appDir <- "inst/shiny-app"
  }

  if (!dir.exists(appDir)) {
    stop("Could not find Shiny app.")
  }

  # Pass data to global environment for the app
  .GlobalEnv$aggregated <- types_data$aggregated
  .GlobalEnv$person_level <- types_data$person_level
  .GlobalEnv$person_type <- types_data$person_type

  message("Launching Cost Analysis app...")
  message("Data loaded: ", nrow(aggregated), ", ", nrow(person_level), " and ", nrow(person_type)," rows")


  shiny::runApp(appDir, display.mode = "normal")
}

