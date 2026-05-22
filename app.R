library(shiny)
library(bslib)
library(markdown)
# library(safegen)
# to load functions locally instead of from package
sapply(list.files("./safegen", full.names=TRUE), source)


## TODO:
## - Change theme for messaging about study success
## - Save parameters button resets all objects/outputs in other tabs
## - Changing inputs on Start of Study after processing causes the output to be
##    greyed out until another button press
## - Include card to change interim timing mechanism

# Helper functions ------
measures = c("sensitivity", "specificity", "both")

param_info <- function(label, tooltip_text) {
  tagList(
    label,
    tooltip(
      bsicons::bs_icon("info-circle", size = "0.6em"),
      tooltip_text,
      placement = "right"
    )
  )
}

create_beta_prior <- function(a, b) {
  if (is.na(a) || is.na(b)) {
    return(c())
  } else {
    return(beta_hyparam(a, b))
  }
}

study_details <- function(analysis_params, measure) {
  ap <- analysis_params

  lines <- c(
    # max n
    paste0("No more than ", ap$max_n, " patients should be recruited for this study.")
  )

  # Success criteria line
  if (measure == "sensitivity" | measure == "specificity") {
    lines <- c(lines, paste0(
      "The study is a success if the ", (1 - ap$alpha) * 100,
      "% credible interval for ", measure,
      " has a width < ", ap$target_width, "."
    ))
  } else if (measure == "both") {
    lines <- c(lines, paste0(
      "The study is a success if the joint ", (1 - ap$alpha) * 100,
      "% credible interval for both sensitivity and specificity has a width < ",
      ap$target_width, "."
    ))
  }


  lines <- c(lines,
    # early stopping
    paste0("If this is achieved at any of the interim analyses, the study can stop
    early for efficacy."),
    paste0("If at any of the interim analyses the assurance is < ",
           ap$futility_assurance, ", success is unlikely and the study can be
           stopped early for futility.")
    )

  return(lines)
}

data_validate <- function(measure, n11, n22, nT1, nT2) {
  validate(
    need(!is.na(nT1),
         "Please enter nT1."),
    need(!is.na(nT2),
         "Please enter nT2."),
    need(measure == "specificity" || !is.na(n11),
         "Please enter n\u2081\u2081."),
    need(measure == "sensitivity" || !is.na(n22),
         "Please enter n\u2082\u2082.")
  )
  validate(
    need(measure == "specificity" || isTRUE(n11 <= nT1),
         "n\u2081\u2081 cannot exceed nT1."),
    need(measure == "sensitivity" || isTRUE(n22 <= nT2),
         "n\u2082\u2082 cannot exceed nT2.")
  )
}


data_input_card = function(tab, header="Input Data", new_data=FALSE){
  card(
    card_header(param_info(header, "Observed count data from contingency table")),
    # p("Enter observed counts from the 2x2 table.", class = "text-muted small"),
    layout_columns(
      col_widths = c(6, 6),
      numericInput(paste0("n11",tab),
                   param_info(withMathJax("True positives (\\(n_{11}\\))"),"Test positive, with disease "),
                   value = 21, min = 0, step = 1),
      numericInput(paste0("nT1",tab), "Total with disease (\\(n_{T1}\\))",
                   value = 24, min = 0, step = 1)
    ),
    layout_columns(
      col_widths = c(6, 6),
      numericInput(paste0("n22",tab),
                   param_info(withMathJax("True negatives (\\(n_{22}\\))"),"Test negative, no disease "),
                   value = 58, min = 0, step = 1),
      numericInput(paste0("nT2",tab), "Total no disease (\\(n_{T2}\\))",
                   value = 76, min = 0, step = 1)
    ),

    if (new_data){
      radioButtons(paste0("new",tab),
                   "This data contains:",
                   choiceNames = c("all patients","only new patients since interim"),
                   choiceValues = c("all","new"),
                   inline = TRUE)
    },

    fill = FALSE
  )

}

data_display = function(data, header="Observed Data" ){

  n11 = data$n11
  n22 = data$n22
  nT1 = data$nT1
  nT2 = data$nT2

  card(
    card_header(header),
    div(
      tags$table(
        class = "table table-sm",
        tags$thead(
          tags$tr(
            tags$th(""),
            tags$th("Diseased"),
            tags$th("Not diseased")
          )
        ),
        tags$tbody(
          tags$tr(
            tags$td("Test positive"),
            tags$td(n11),
            tags$td(nT2 - n22)
          ),
          tags$tr(
            tags$td("Test negative"),
            tags$td(nT1 - n11),
            tags$td(n22)
          ),
          tags$tr(
            tags$td(strong("Total")),
            tags$td(strong(nT1)),
            tags$td(strong(nT2))
          )
        )
      ),

      p("Total sample size: ", strong(nT1 + nT2))
    ),
    fill = FALSE
  )

}

box_theme <- function(success){
  if (is.na(success)) {
    value_box_theme(bg = "#f0f4fd", fg = "#2b4fa3")
  } else if (success) {
    value_box_theme(bg = "#f0faf3", fg = "#276b3d")
  } else {
    value_box_theme(bg = "#fdf0f0", fg = "#a33333")
  }
}

interim_tab = function(i) {
  nav_panel(
    paste("Interim", i),
    data_input_card(paste0("_int", i), new_data = (i>1)),

    if (i > 1) {
      uiOutput(paste0("prev_data_card_", i))
    },

    input_task_button(paste0("run_interim_", i),
                      paste("Run Interim", i),
                      class = "btn-success"),
    uiOutput(paste0("interim_results_", i))
    #verbatimTextOutput(paste0("interim_results_", i))
  )
}

interim_status = function(result){

  if (result$stop_for_efficacy) {
    value_box(
      title = "Stop for efficacy",
      value = "Success",
      theme = box_theme(TRUE),
      min_height = 150
    )
  } else if (result$stop_for_futility) {
    value_box(
      title = "Stop for futility",
      value = "Futility",
      theme = box_theme(FALSE),
      min_height = 150
    )
  } else {
    value_box(
      title = "Continue recruitment",
      value = paste0("+ ", result$additional_n, " participants"),
      theme = box_theme(NA),
      min_height = 150
    )
  }
}

final_tab = function() {
  nav_panel(
    "Final Analysis",
    data_input_card("_final", new_data = TRUE),
    uiOutput("prev_data_card_final"),
    input_task_button("run_final", "Run Final Analysis", class = "btn-success"),
    uiOutput("final_results")
  )
}

study_results <- function(r, params, is_final = FALSE) {
  result <- r$result
  alpha_label <- paste0((1 - params$alpha) * 100, "% Credible Interval")

  criteria_box <- div(
    class = "alert alert-info",
    icon("info-circle"),
    span(paste0(
      "The study is successful if the ",
      alpha_label, " for ",
      r$measure, " has a width less than ", params$target_width, "."
    ))
  )

  success_box <- if (r$success) {
    div(class = "alert alert-success",
        icon("check-circle"),
        strong("Success — "),
        span(paste0("the study was successful based on ", r$final_n, " participants."))
    )
  } else {
    div(class = "alert alert-danger",
        icon("times-circle"),
        strong("Not successful — "),
        span(paste0("the study did not meet its success criteria based on ",
                    r$final_n, " participants.")),
        if (!is_final && isTRUE(r$check_futility && result$stop_for_futility)) {
          tagList(
            tags$br(),
            span(icon("exclamation-triangle"),
                 strong("Stop for futility — "),
                 "based on existing data, it will not be possible to achieve success without exceeding the maximum sample size.")
          )
        }
    )
  }

  ci_section <- if (r$measure == "both") {
    layout_columns(
      col_widths = c(6, 6),
      value_box(
        title = "Sensitivity",
        value = sprintf("(%.3f, %.3f)", result$sens_CI[1], result$sens_CI[2]),
        p(sprintf("Width: %.3f", result$sens_CI[2] - result$sens_CI[1]), class = "small"),
        min_height = 150,
        theme = box_theme(r$success)
      ),
      value_box(
        title = "Specificity",
        value = sprintf("(%.3f, %.3f)", result$spec_CI[1], result$spec_CI[2]),
        p(sprintf("Width: %.3f", result$spec_CI[2] - result$spec_CI[1]), class = "small"),
        min_height = 150,
        theme = box_theme(r$success)
      )
    )
  } else {
    ci <- result$credible_interval
    layout_columns(
      col_widths = c(6, 6),
      value_box(
        title = tools::toTitleCase(result$measure),
        value = sprintf("(%.3f, %.3f)", ci[1], ci[2]),
        p(alpha_label, class = "small", style = "margin: 0;"),
        p(sprintf("Width: %.3f", ci[2] - ci[1]), class = "small"),
        min_height = 150,
        theme = box_theme(r$success)
      ),
      success_box
    )
  }

  tagList(
    criteria_box,
    if (r$measure == "both") success_box,
    ci_section
  )
}



# App ------
source("ui.R")
source("server.R")
shinyApp(ui, server)
