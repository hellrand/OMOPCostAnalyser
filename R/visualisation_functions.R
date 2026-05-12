#' Plot Costs by Type
#'
#' @param type_graph_data Data from prepare_type_graph_data()
#'
#' @return ggplot object
#' @export
plot_costs_by_type <- function(type_graph_data) {


  summary_data <- type_graph_data %>%
    dplyr::group_by(cohort, type) %>%
    dplyr::summarise(total_cost = sum(total_cost), .groups = "drop")

  p <- ggplot(summary_data, aes(x = type, y = total_cost, fill = cohort)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.7) +
    scale_fill_manual(values = c("Target" = "royalblue4", "Control" = "steelblue1")) +
    scale_y_continuous(labels = scales::comma) +
    labs(title = "Total Costs for Drugs, Procedures, and Visits",
         x = "Type",
         y = "Total Cost (€)",
         fill = "Cohort") +
    theme_minimal()

  return(p)
}

#' Plot Annual Costs Over Time
#'
#' @param graph_data Data from prepare_graph_data()
#'
#' @return ggplot object
#' @export
plot_annual_costs <- function(graph_data) {

  p <- ggplot(graph_data, aes(x = year, y = total_cost, fill = cohort)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = c("Target" = "royalblue4", "Control" = "steelblue1")) +
    scale_y_continuous(labels = scales::comma) +
    labs(title = "Annual Costs for Target and Control Group",
         x = "Year",
         y = "Total Cost (€)") +
    theme_minimal()

  return(p)
}

#' Plot Costs by Type and Year
#'
#' @param type_graph_data Data from prepare_type_graph_data()
#'
#' @return ggplot object
#' @export
plot_costs_by_type_year <- function(type_graph_data) {

  p <- ggplot(type_graph_data, aes(x = factor(year), y = total_cost, fill = cohort)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~ type, scales = "free_y") +
    scale_fill_manual(values = c("Target" = "royalblue4", "Control" = "steelblue1")) +
    scale_y_continuous(labels = scales::comma) +
    labs(title = "Total Costs by Year",
         x = "Year",
         y = "Total Cost (€)",
         fill = "Cohort") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  return(p)
}

#' Plot Cost Trends by Type or Patient Number
#'
#' Line chart showing cost trends over time by type or number of patients over time
#'
#' @param type_graph_data Data from prepare_type_graph_data()
#' @param cost_var Variable to plot: total_cost/n_patients (default: "total_cost")
#'
#' @return ggplot object
#' @export
plot_cost_trends <- function(type_graph_data,
                             cost_var = "total_cost") {

  y_label <- case_when(
    cost_var == "total_cost" ~ "Total Cost (€)",
    cost_var == "n_patients" ~ "Number of Patients",
    TRUE ~ cost_var
  )

  p <- ggplot(type_graph_data, aes(x = year, y = !!sym(cost_var),
                              color = cohort, linetype = type,
                              group = interaction(cohort, type))) +
    geom_line(size = 1.2) +
    geom_point(size = 2) +
    scale_color_manual(values = c("Target" = "royalblue4", "Control" = "steelblue1")) +
    scale_y_continuous(labels = scales::comma) +
    labs(title = paste(y_label, "Over Time"),
         x = "Year",
         y = y_label,
         color = "Cohort",
         linetype = "Type") +
    theme_minimal(base_size = 14)

  return(p)
}
