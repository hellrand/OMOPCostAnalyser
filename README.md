
<!-- README.md is generated from README.Rmd. Please edit that file -->

# OMOPCostAnalyser

<!-- badges: start -->

<!-- badges: end -->

The goal of OMOPCostAnalyser is to provide a framework for analysing
direct healthcare costs in cohort-based studies. The package assumes
that cohorts are defined using ATLAS and that the underlying data
conforms to the OMOP CDM.

## Installation

You can install the development version of OMOPCostAnalyser from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("hellrand/OMOPCostAnalyser")
```

## Example

This is the basic workflow for launching the Shiny app:

``` r
library(OMOPCostAnalyser)
# Connect to database and create CDM object

# Match cohorts
# cdm <- match_cohorts_workflow(cdm, "user_write_schema", cohort, 18, 19)

# Get costs for matched cohorts
# all_costs <- get_all_costs(cdm, cdm$matched_cohort)

# Get data for Shiny app
# types_data <- prepare_types_data(all_costs)
 
# Launch Shiny app
# launch_cost_app(types_data)
 
```
