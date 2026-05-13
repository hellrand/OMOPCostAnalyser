library(shiny)
library(ggplot2)
library(dplyr)
library(scales)
library(plotly)
library(DT)

# Check for data
if (!exists("types")) {
  stop("No data provided. Use launch_cost_app(types_data = your_data)")
}

# Detect available years and types
year_range <- range(aggregated$year, na.rm = TRUE)
available_types <- unique(aggregated$type)

# UI
ui <- fluidPage(
  titlePanel(
    windowTitle = "costAnalyser",
    div(
      h1("OMOPCostAnalyser", style = "margin-bottom: 0px;")
    )
  ),

      sidebarLayout(
        sidebarPanel(
          # Filters
          checkboxGroupInput("cost_types",
                             "Select Cost Types:",
                             choices = available_types,
                             selected = available_types),

          radioButtons("cohort_filter",
                       "Select Cohort:",
                       choices = c("Both" = "both",
                                   "Target Cohort" = "Target",
                                   "Control Cohort" = "Control"),
                       selected = "both"),

          sliderInput("year_range",
                      "Year Range:",
                      min = year_range[1],
                      max = year_range[2],
                      value = year_range,
                      step = 1,
                      sep = ""),

          hr(),

          selectInput("metric",
                      "Metric to Display:",
                      choices = c("Total Cost" = "total_cost",
                                  "Mean Cost per Patient" = "mean_per_patient"),
                      selected = "total_cost"),

          checkboxInput("show_stats",
                        "Show Summary Statistics",
                        value = TRUE),

          # Toggle for summary table grouping
          conditionalPanel(
            condition = "input.show_stats == true",
            radioButtons("summary_grouping",
                         "Summary Table View:",
                         choices = c("Total (All Years)" = "total",
                                     "By Year" = "by_year",
                                     "By Cohort Only" = "by_cohort",
                                     "Statistical Comparison" = "statistical_comparison"),
                         selected = "total")
          )
        ),

        mainPanel(
          plotlyOutput("cost_plot", height = "500px"),
          hr(),
          conditionalPanel(
            condition = "input.show_stats == true",
            h4("Summary Statistics"),
            DTOutput("summary_table")
          )
        )
      )
    )

# Server
server <- function(input, output, session) {

  # Reactive filtered data
  filtered_data <- reactive({

    res_agg <- aggregated %>%
      filter(
        type %in% input$cost_types,
        year >= input$year_range[1],
        year <= input$year_range[2]
      )

    res_type <- person_type %>%
      filter(
        type %in% input$cost_types,
        year >= input$year_range[1],
        year <= input$year_range[2]
      )

    res_raw <- person_level %>%
      filter(
        year >= input$year_range[1],
        year <= input$year_range[2]
      )

    if (input$cohort_filter != "both") {
      res_agg <- res_agg %>% filter(cohort == input$cohort_filter)
      res_type <- res_type %>% filter(cohort == input$cohort_filter)
      res_raw <- res_raw %>% filter(cohort == input$cohort_filter)
    }

    res_agg <- res_agg %>%
      mutate(mean_per_patient = total_cost / n_patients)

    list(
      aggregated = res_agg,
      person_level = res_raw,
      person_type = res_type
    )
  })

  # Main plot
  output$cost_plot <- renderPlotly({
    plot_data <- filtered_data()$aggregated

    # Select the metric to plot
    y_var <- input$metric
    y_label <- case_when(
      y_var == "total_cost" ~ "Total Cost (€)",
      y_var == "mean_per_patient" ~ "Mean Cost per Patient (€)"
    )

    p <- ggplot(plot_data, aes(x = year, y = !!sym(y_var),
                            color = cohort, linetype = type,
                            group = interaction(cohort, type),
                            text = paste0("Year: ", year,
                                          "\n", y_label, ": ", comma(!!sym(y_var)),
                                          "\nCohort: ", cohort,
                                          "\nType: ", type))) +
        geom_line(linewidth = 1.2) +
        geom_point(size = 2) +
        scale_color_manual(values = c("Target" = "royalblue4", "Control" = "steelblue1")) +
        scale_x_continuous(breaks = function(x) seq(floor(min(x)), ceiling(max(x)), by = 1)) +
        scale_y_continuous(labels = comma) +
        labs(title = paste(y_label, "Over Time"),
             x = "Year",
             y = y_label,
             color = "Cohort",
             linetype = "Type") +
        theme_minimal(base_size = 14)

    ggplotly(p, tooltip = "text")
  })


  # Summary Table
  output$summary_table <- renderDT({

    # TITLES
    table_title <- switch(
      input$summary_grouping,

      "total" = "Total Cost Summary",
      "by_year" = "Annual Cost Summary",
      "by_cohort" = "Cohort Cost Summary",
      "statistical_comparison" = "Statistical Comparison of Mean Costs"
    )

    summary_df <- switch(

      input$summary_grouping,

      # =========================================================
      # TOTAL
      # =========================================================
      "total" = {

        plot_data <- filtered_data()$person_type

        plot_data %>%
          group_by(Type = type, Cohort = cohort) %>%
          summarise(
            `Total Patients` = n_distinct(subject_id),

            `Total Cost` =
              sum(person_annual_cost, na.rm = TRUE),

            `Paid by Payer` =
              sum(paid_by_payer, na.rm = TRUE),

            `Paid by Patient` =
              sum(paid_by_patient, na.rm = TRUE),

            `Mean per Patient` =
              mean(person_annual_cost, na.rm = TRUE),

            `Median per Patient` =
              median(person_annual_cost, na.rm = TRUE),

            .groups = "drop"
          )
      },

      # =========================================================
      # BY YEAR
      # =========================================================
      "by_year" = {

        plot_data <- filtered_data()$person_type

        plot_data %>%
          group_by(
            Type = type,
            Cohort = cohort,
            Year = year
          ) %>%
          summarise(

            `Total Patients` =
              n_distinct(subject_id),

            `Total Cost` =
              sum(person_annual_cost, na.rm = TRUE),

            `Paid by Payer` =
              sum(paid_by_payer, na.rm = TRUE),

            `Paid by Patient` =
              sum(paid_by_patient, na.rm = TRUE),

            `Mean per Patient` =
              mean(person_annual_cost, na.rm = TRUE),

            `Median per Patient` =
              median(person_annual_cost, na.rm = TRUE),

            .groups = "drop"
          )
      },

      # =========================================================
      # BY COHORT
      # =========================================================
      "by_cohort" = {

        plot_data <- filtered_data()$person_level

        plot_data %>%
          group_by(Cohort = cohort) %>%
          summarise(

            `Total Patients` =
              n_distinct(subject_id),

            `Total Cost` =
              sum(annual_cost, na.rm = TRUE),

            `Paid by Payer` =
              sum(annual_payer_cost, na.rm = TRUE),

            `Paid by Patient` =
              sum(annual_patient_cost, na.rm = TRUE),

            `Mean per Patient` =
              mean(annual_cost, na.rm = TRUE),

            `Median per Patient` =
              median(annual_cost, na.rm = TRUE),

            .groups = "drop"
          )
      },

      # =========================================================
      # STATISTICAL COMPARISON
      # =========================================================
      "statistical_comparison" = {

        # Ensure that every patient contributes to every type
        all_patients <- filtered_data()$person_level %>%
          distinct(subject_id, cohort)

        all_types <- tibble(
          type = unique(filtered_data()$person_type$type)
        )

        complete_data <- tidyr::crossing(
          all_patients,
          all_types
        ) %>%
          left_join(
            filtered_data()$person_type,
            by = c("subject_id", "cohort", "type")
          ) %>%
          mutate(
            person_annual_cost =
              tidyr::replace_na(person_annual_cost, 0)
          )

        plot_data <- complete_data

        plot_data %>%
          group_by(Type = type) %>%
          summarise(

            `Mean per Person (Target)` =
              mean(
                person_annual_cost[cohort == "Target"],
                na.rm = TRUE
              ),

            `Mean per Person (Control)` =
              mean(
                person_annual_cost[cohort == "Control"],
                na.rm = TRUE
              ),

            `Mean Cost Difference` =
              `Mean per Person (Target)` -
              `Mean per Person (Control)`,

            ci_low = suppressWarnings(
              t.test(
                person_annual_cost[cohort == "Target"],
                person_annual_cost[cohort == "Control"]
              )$conf.int[1]
            ),

            ci_high = suppressWarnings(
              t.test(
                person_annual_cost[cohort == "Target"],
                person_annual_cost[cohort == "Control"]
              )$conf.int[2]
            ),

            `95% CI` = paste0(
              round(ci_low, 2),
              " - ",
              round(ci_high, 2)
            ),

            p_value_numeric = suppressWarnings(
              t.test(
                person_annual_cost[cohort == "Target"],
                person_annual_cost[cohort == "Control"]
              )$p.value
            ),

            `p-value` = case_when(
              is.na(p_value_numeric) ~ "",
              p_value_numeric < 0.01 ~ "p < 0.01",
              TRUE ~ sprintf("%.4f", p_value_numeric)
            ),

            .groups = "drop"
          ) %>%
          select(
            Type,
            `Mean per Person (Target)`,
            `Mean per Person (Control)`,
            `Mean Cost Difference`,
            `95% CI`,
            `p-value`,
            p_value_numeric
          )
      }
    )

    # =========================================================
    # DATATABLE
    # =========================================================

    dt <- datatable(
      summary_df,

      caption = htmltools::tags$caption(
        style = 'caption-side: top;
             text-align: left;
             font-size: 18px;
             font-weight: bold;',
        table_title
      ),

      options = list(
        pageLength = 20,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel'),
        ordering = TRUE,

        columnDefs = list(
          list(
            targets =
              which(names(summary_df) == "p_value_numeric") - 1,
            visible = FALSE
          )
        )
      ),

      extensions = 'Buttons',
      rownames = FALSE,
      class = 'stripe hover compact'
    )

    # =========================================================
    # CURRENCY FORMATTING
    # =========================================================

    money_cols <- grep(
      "Cost|Paid|Mean|Median|Difference",
      names(summary_df),
      value = TRUE
    )

    money_cols <- setdiff(
      money_cols,
      c("p_value_numeric")
    )

    if (length(money_cols) > 0) {

      dt <- dt %>%
        formatCurrency(
          columns = money_cols,
          currency = "€",
          digits = 2,
          before = FALSE,
          mark = ","
        )
    }

    # =========================================================
    # TOTAL PATIENTS ROUNDING
    # =========================================================

    if ("Total Patients" %in% names(summary_df)) {

      dt <- dt %>%
        formatRound(
          columns = "Total Patients",
          digits = 0,
          mark = ","
        )
    }

    # =========================================================
    # P-VALUE COLORS
    # =========================================================

    if ("p-value" %in% names(summary_df)) {

      dt <- dt %>%
        formatStyle(
          "p-value",

          valueColumns = "p_value_numeric",

          backgroundColor = styleInterval(
            c(0.01, 0.05),
            c("lightgreen", "khaki", NA)
          )
        )
    }

    dt

  }, server = FALSE)
}

# Run the app
shinyApp(ui = ui, server = server)
