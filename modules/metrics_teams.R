# Metric module with summary

import("shiny")
import("dplyr")
import("htmltools")
import("glue")

export("ui")
export("init_server")

expose("constants.R")

ui <- function(id) {
  ns <- NS(id)
  choices <- getMetricsChoicesByCategory(id)
  
  tagList(
    selectInput(
      ns("summary_metric"), "Metric",
      choices,
      width = NULL,
      selectize = TRUE,
      selected = choices[[1]]
    ),
    uiOutput(ns("summary"))
  )
}

init_server <- function(id, monthly_df, yearly_df, y, m, previous_time_range) {
  callModule(server, id, monthly_df, yearly_df, y, m, previous_time_range)
}

server <- function(input, output, session, monthly_df, yearly_df, y, m, previous_time_range) {
  
  
}