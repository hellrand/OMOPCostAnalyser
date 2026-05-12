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
                                     "By Cohort Only" = "by_cohort"),
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

    add_stats <- function(df, group_vars) {
      summary_df <- df %>%
        group_by(across(all_of(c(group_vars, "cohort")))) %>%
        summarise(
          `Total Patients` = n_distinct(subject_id),
          `Total Cost` = sum(cost, na.rm = TRUE),
          #`Paid by Payer` = sum(payer_cost, na.rm = TRUE),
          #`Paid by Patient` = sum(patient_cost, na.rm = TRUE),
          `Mean per Patient` = mean(cost, na.rm = TRUE),
          `Median per Patient` = median(cost, na.rm = TRUE),
          SD = sd(cost, na.rm = TRUE),
          N = sum(!is.na(cost)),
          SE = SD / sqrt(N),
          `CI Lower` = `Mean per Patient` - qt(0.975, pmax(N - 1, 1)) * SE,
          `CI Upper` = `Mean per Patient` + qt(0.975, pmax(N - 1, 1)) * SE,
          .groups = "drop"
        ) %>%
        mutate(
          `95% CI` = paste0(
            round(`CI Lower`, 2),
            " to ",
            round(`CI Upper`, 2)
          )
        )

      tests <- df %>%
        group_by(across(all_of(group_vars))) %>%
        summarise(
          `p-value` = {
            target <- cost[cohort == "Target"]
            control <- cost[cohort == "Control"]
            if (length(target) > 1 && length(control) > 1) {
              wilcox.test(target, control)$p.value
            } else {
              NA_real_
            }
          },
          .groups = "drop"
        )

      summary_df %>%
        left_join(tests, by = group_vars) %>%
        select(
          all_of(group_vars),
          Cohort = cohort,
          `Total Patients`,
          `Total Cost`,
          `Paid by Payer`,
          `Paid by Patient`,
          `Mean per Patient`,
          `Median per Patient`,
          `95% CI`,
          `p-value`
        )
    }

    summary_df <- switch(
      input$summary_grouping,

      "by_year" = {
        filtered_data()$person_type %>%
          transmute(
            Type = type,
            Year = year,
            cohort,
            subject_id,
            cost = person_annual_cost,
            payer_cost = paid_by_payer,
            patient_cost = paid_by_patient
          ) %>%
          add_stats(c("Type", "Year"))
      },

      "total" = {
        filtered_data()$person_type %>%
          group_by(Type = type, cohort, subject_id) %>%
          summarise(
            cost = sum(person_annual_cost, na.rm = TRUE),
            payer_cost = sum(paid_by_payer, na.rm = TRUE),
            patient_cost = sum(paid_by_patient, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          add_stats("Type")
      },

      "by_cohort" = {
        plot_data <- filtered_data()$person_level

        summary_df <- plot_data %>%
          group_by(Cohort = cohort) %>%
          summarise(
            `Total Patients` = n_distinct(subject_id),
            `Total Cost` = sum(annual_cost, na.rm = TRUE),
            `Paid by Payer` = sum(annual_payer_cost, na.rm = TRUE),
            `Paid by Patient` = sum(annual_patient_cost, na.rm = TRUE),
            `Mean per Patient` = mean(annual_cost, na.rm = TRUE),
            `Median per Patient` = median(annual_cost, na.rm = TRUE),
            SD = sd(annual_cost, na.rm = TRUE),
            N = sum(!is.na(annual_cost)),
            SE = SD / sqrt(N),
            `CI Lower` = `Mean per Patient` - qt(0.975, pmax(N - 1, 1)) * SE,
            `CI Upper` = `Mean per Patient` + qt(0.975, pmax(N - 1, 1)) * SE,
            `95% CI` = paste0(
              round(`CI Lower`, 2),
              " to ",
              round(`CI Upper`, 2)
            ),
            .groups = "drop"
          ) %>%
          select(-N, -SE, -SD, -`CI Lower`, -`CI Upper`)

        test_res <- plot_data %>%
          summarise(
            `p-value` = if (
              sum(cohort == "Target") > 1 &&
              sum(cohort == "Control") > 1
            ) {
              wilcox.test(
                annual_cost[cohort == "Target"],
                annual_cost[cohort == "Control"]
              )$p.value
            } else {
              NA_real_
            }
          )

        bind_cols(summary_df, test_res)
      }
    )

    datatable(
      summary_df,
      options = list(
        pageLength = 20,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel'),
        ordering = TRUE
      ),
      extensions = 'Buttons',
      rownames = FALSE,
      class = 'stripe hover compact'
    ) %>%
      formatCurrency(
        columns = grep("Cost|Paid|Mean per Patient|Median|Difference",
                       names(summary_df)),
        currency = "€",
        digits = 2,
        before = FALSE,
        mark = ","
      ) %>%
      formatRound(columns = "Total Patients", digits = 0, mark = ",") %>%
      formatRound(columns = "p-value", digits = 4) %>%
      formatStyle(
        "p-value",
        backgroundColor = styleInterval(
          c(0.01, 0.05),
          c("lightgreen", "khaki", NA)
        )
      )

  }, server = FALSE)
}

# Run the app
shinyApp(ui = ui, server = server)
