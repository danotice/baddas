library(shiny)
library(shinyWidgets)
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
## - add a way  to abandon operation
## - add note that calculating the assurance (sample size re-estimation)
##    for "both" is slower

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

  lines <- list(
    # max n
    paste0("No more than ", ap$max_n, " patients should be recruited for this study.")
  )

  # Success criteria line
  if (measure == "sensitivity") {
    criteria <- list(span(withMathJax(paste0(
      "The study is a success if the central two-sided ", (1 - ap$alpha) * 100,
      "% credible interval for sensitivity at analysis \\(i \\,\\, (\\lambda_L^{(i)},\\lambda_U^{(i)}) \\,\\, \\) ",
      "  has a width \\( w_\\lambda^{(i)} = \\lambda_U^{(i)} - \\lambda_L^{(i)} < \\quad \\) ", ap$target_width, "."
    ))))
  } else if (measure == "specificity") {
    criteria <- list(span(withMathJax(paste0(
      "The study is a success if the central two-sided ", (1 - ap$alpha) * 100,
      "% credible interval for specificity at analysis \\(i \\,\\, (\\theta_L^{(i)},\\theta_U^{(i)}) \\,\\, \\) ",
      "  has a width \\( w_\\theta^{(i)} = \\theta_U^{(i)} - \\theta_L^{(i)} < \\quad \\) ", ap$target_width, "."
    ))))
  } else if (measure == "both") {
    #lines <- c(lines, paste0(
    criteria <- list(span(withMathJax(paste0(
      "The study is a success if, at analysis \\(i \\,\\, \\),  the central two-sided ", (1 - ap$alpha) * 100,
      "% credible intervals for both sensitivity \\( (\\lambda_L^{(i)},\\lambda_U^{(i)}) \\,\\, \\) and specificity \\( (\\theta_L^{(i)},\\theta_U^{(i)}) \\,\\, \\) have widths < ",
      ap$target_width, "."
    ))))
  }

  lines <- c(lines, criteria)

  lines <- c(lines,
    # early stopping
    paste0("If this is achieved at any of the interim analyses, the study can stop
    early for efficacy."),
    paste0("If at any of the interim analyses the assurance is less than ",
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
                   param_info(withMathJax("True negatives (\\(n_{22}\\))"),"Test negative, without disease "),
                   value = 58, min = 0, step = 1),
      numericInput(paste0("nT2",tab), "Total without disease (\\(n_{T2}\\))",
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

  if (is.na(n11)) {
    n11 = "?"
    n1T = "?"
  } else {
    n1T = nT1 - n11
  }

  if (is.na(n22)) {
    n22 = "?"
    n2T = "?"
  } else {
    n2T = nT2 - n22
  }

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
            tags$td(n2T)
          ),
          tags$tr(
            tags$td("Test negative"),
            tags$td(n1T),
            tags$td(n22)
          ),
          tags$tr(
            tags$td(strong("Total")),
            tags$td(strong(nT1)),
            tags$td(strong(nT2))
          )
        )
      ),

      p("Current total sample size: ", strong(nT1 + nT2))
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




# UI --------

ui <- page_sidebar(
  title = "Bayesian Adaptive Design Diagnostic Accuracy Study",

  # Add custom CSS to style disabled button
  tags$head(
    tags$style(HTML("
      .btn-disabled-grey {
        opacity: 0.5;
        cursor: not-allowed;
        pointer-events: none;
      }
    "))
  ),

  tags$head(
    tags$style(HTML("
    .MathJax {
      font-size: 0.9em !important;
    }
  "))
  ),

  ## Sidebar - display saved parameters -----------
  sidebar = sidebar(
    h4("Current Parameters"),
    uiOutput("params_display"),
    hr(),
    uiOutput("params_link"),

    #uiOutput("reset_button_ui")
    actionButton("resetAll", "Reset",
                 class = "btn-outline-danger btn-sm w-100",
                 icon = icon("rotate-left"))
  ),

  navset_card_tab(
    id = "main_tabs",

    ## Tab - Instructions -----------
    nav_panel(
      "Instructions",
      card(
        #card_header("How to use this app"),
        includeMarkdown("instructions0.md"),
        hr(),
        includeMarkdown("instructions.md"),
        #hr(),
        h5("Contingency Table"),
        p("Below is the layout of a contingency table. Only the highlighted cells are requested."),
        tags$table(
          class = "table table-sm table-bordered",
          style = "max-width: 500px;",
          tags$thead(
            tags$tr(
              tags$th(""),
              tags$th("Diseased"),
              tags$th("Not diseased")
            )
          ),
          tags$tbody(
            tags$tr(
              tags$td(strong("Test positive")),
              tags$td(style = "background-color: #d4edda; color: #155724;",
                      "True positives ", withMathJax("(\\(n_{11}\\))")),
              tags$td("False positives")
            ),
            tags$tr(
              tags$td(strong("Test negative")),
              tags$td("False negatives"),
              tags$td(style = "background-color: #d4edda; color: #155724;",
                      "True negatives (\\(n_{22}\\))")
            ),
            tags$tr(
              style = "border-top: 2px solid #dee2e6;",
              tags$td(strong("Total")),
              tags$td(style = "background-color: #d4edda; color: #155724;",
                      "Total diseased (\\(n_{T1}\\))"),
              tags$td(style = "background-color: #d4edda; color: #155724;",
                      "Total not diseased (\\(n_{T2}\\))")
            )
          )
        ),
        tags$ul(
          tags$li("The totals \\(n_{T1}\\) and \\(n_{T2}\\) are always needed."),
          tags$li("Sensitivity is estimated from \\(n_{11}\\)."),
          tags$li("Specificity is estimated from \\(n_{22}\\).")
        ),
        fill = FALSE

      )
    ),

    ## Tab - set study parameters ----------
    nav_panel(
      "Parameters",

      card(
        #  card_header("Study Design Parameters"),
        h4("Study Design Paramaters"),

        card(
          card_header("Accuracy Measure"),
          radioButtons("measure", NULL, choices = measures, selected = character(),
                       inline = TRUE),
          fill = FALSE
        ),

        # Study Design Parameters
        layout_columns(
          col_widths = c(6, 6),
          numericInput("target_width", param_info("Target Width", "The maximum acceptable credible interval width for the study to be a success."),
                       value = 0.2, min = 0, step = 0.01),
          numericInput("alpha", param_info("Alpha", withMathJax("Used to define the central two-sided \\(100(1-\\alpha)\\%\\) credible interval")),
                       value = 0.05, min = 0, max = 1, step = 0.01)
        ),

        # sliderInput("n_range", param_info("Sample Size Range","The minimum and maximum number of participants to recruit."),
        #             min = 0, max = 2000, value = c(100, 800), step = 1),
        numericRangeInput("n_range", param_info("Sample Size Range","The minimum and maximum number of participants to recruit."),
                          min = 0, max = 2000, value = c(100, 800), step = 1),
        # numericInput("max_n", "Maximum N:", value = 800, min = 1, step = 10),
        # numericInput("min_n", "Minimum N:", value = 100, min = 1, step = 10),

        layout_columns(
          col_widths = c(6, 6),
          numericInput("target_assurance", param_info("Target Assurance","The desired probability of success."),
                       value = 0.8, min = 0, max = 1, step = 0.01),
          numericInput("futility_assurance", param_info("Futility Assurance", "Assurance threshold for early stopping for futility at an interim."),
                       value = 0.2, min = 0, max = 1, step = 0.01)
        ),

        numericInput("num_interims", "Number of Interims:", value = 2, min = 0, step = 1),

        fill = FALSE
      ),


      # Prior Parameters
      card(
        h4(param_info("Beta Priors - Hyperparameters",
                      "Specify shape and rate hyperparameters for all model parameters.")),

        accordion(
          open = "Prevalence",

          accordion_panel(
            "Sensitivity \\( (\\lambda) \\)",
            h6(param_info("Design Prior",
                  "\\( \\pi_D(\\lambda) = \\text{Beta}(\\lambda | a_\\lambda^D, b_\\lambda^D) \\)  ")),
            layout_columns(
              col_widths = c(6, 6),
              numericInput("sens_design_a", "\\(a_\\lambda^D\\) :", value = 7, min = 0, step = 1),
              numericInput("sens_design_b", "\\(b_\\lambda^D\\) :", value = 3, min = 0, step = 1)
            ),
            h6(param_info("Analysis Prior",
                  "\\( \\pi_A(\\lambda) = \\text{Beta}(\\lambda | a_\\lambda^A, b_\\lambda^A) \\)")),
            layout_columns(
              col_widths = c(6, 6),
              numericInput("sens_analysis_a", "\\(a_\\lambda^A\\) :", value = 1, min = 0, step = 1),
              numericInput("sens_analysis_b", "\\(b_\\lambda^A\\) :", value = 1, min = 0, step = 1)
            )
          ),
          accordion_panel(
            "Specificity \\( (\\theta) \\)",
            h6(param_info("Design Prior",
                          "\\( \\pi_D(\\theta) = \\text{Beta}(\\theta | a_\\theta^D, b_\\theta^D) \\)  ")),
            layout_columns(
              col_widths = c(6, 6),
              numericInput("spec_design_a", "\\(a_\\theta^D\\) :", value = NA, min = 0, step = 1),
              numericInput("spec_design_b", "\\(b_\\theta^D\\) :", value = NA, min = 0, step = 1)
            ),
            h6(param_info("Analysis Prior",
                          "\\( \\pi_A(\\theta) = \\text{Beta}(\\theta | a_\\theta^A, b_\\theta^A) \\)")),
            layout_columns(
              col_widths = c(6, 6),
              numericInput("spec_analysis_a", "\\(a_\\theta^A\\) :", value = 1, min = 0, step = 1),
              numericInput("spec_analysis_b", "\\(b_\\theta^A\\) :", value = 1, min = 0, step = 1)
            )
          ),
          accordion_panel(
            "Prevalence \\( (\\rho) \\)",
            #h6("Prior"),
            layout_columns(
              col_widths = c(6, 6),
              numericInput("prev_design_a", "\\(a_\\rho\\) :", value = 2, min = 0, step = 1),
              numericInput("prev_design_b", "\\(b_\\rho\\) :", value = 8, min = 0, step = 1)
            )
          )
        ),

        fill = FALSE
      ),

      actionButton("saveParams", "Save Parameters", class = "btn-primary"),

      uiOutput("params_error"),

      # card(
      #   card_header("Analysis Parameters Object"),
      #   verbatimTextOutput("params_output")
      #   )
    ),

    ## Tab - start of study summary ------------
    nav_panel(
      "Start of Study",

      # card(
      #   card_header("Accuracy Measure"),
      #   radioButtons("measure_s", NULL, choices = measures, selected = "sensitivity",
      #                inline = TRUE),
      #   fill = FALSE
      # ),

      card(
        card_header("Planned Sample Size"),
        radioButtons("planned_n_option", NULL,
                     choices = c("Calculate from parameters" = "calculate",
                                 "Specify value" = "specify"),
                     selected = "calculate"),
        conditionalPanel(
          condition = "input.planned_n_option == 'specify'",
          numericInput("planned_n_value", NULL, value = 500, min = 1, step = 10),
        ),
        fill = FALSE
        # actionButton("update_params", "Update Parameters with Planned N", class = "btn-info")
      ),

      # input_task_button("run_summary", "Start of Study Summary", class = "btn-success"),
      uiOutput("summary_button_ui"),

      # verbatimTextOutput("summary_results")
      htmlOutput("summary_results")

    ),

    ## Tab - Interim Analyses -----------
    nav_panel(
      "Interim Analyses",

      # card(
      #   card_header("Accuracy Measure"),
      #   radioButtons("measure_int", NULL, choices = measures, selected = "sensitivity",
      #                inline = TRUE),
      #   fill = FALSE
      # ),

      uiOutput("interim_panel_ui")
    ),

    ## Tab - check study -----------
    nav_panel(
      "Check Study",

      # card(
      #   card_header("Accuracy Measure"),
      #   radioButtons("measure_check", NULL, choices = measures, selected = "sensitivity",
      #                inline = TRUE),
      #   fill = FALSE
      # ),

      data_input_card(""),

      checkboxInput("check_futility", "Check for futility", value = FALSE),

      uiOutput("check_button_ui"),

      uiOutput("check_study_results"),
      #tableOutput("ci_table")

    ),


  )
)


# Server -----------
server <- function(input, output, session) {

  # Tab & Sidebar - parameters --------

  # # for reset upon new save
  # reset_trigger <- reactiveVal(0)

  # Navigate to Parameters tab when link is clicked
  output$params_link <- renderUI({
    label <- if (input$saveParams == 0) "Set parameters →" else "Change parameters →"
    p(class = "text-muted small",
      actionLink("goto_params", label))
  })

  observeEvent(input$resetAll, {
    # reset the saveParams counter equivalent by reloading session
    session$reload()
  })

  # Reactive value to store analysis parameters at button click
  analysis_params_obj <- eventReactive(input$saveParams, {
    # Create analysis_parameters object
    tryCatch({

      base_params = analysis_parameters(
        target_width = if(is.na(input$target_width)) NA_real_ else input$target_width,
        alpha = if(is.na(input$alpha)) NA_real_ else input$alpha,
        max_n = as.integer(input$n_range[2]),
        min_n = as.integer(input$n_range[1]),
        # planned_n = if(is.na(input$planned_n)) NA_integer_ else as.integer(input$planned_n),
        num_interims = if(is.na(input$num_interims)) NA_integer_ else as.integer(input$num_interims),
        # interim_timing = input$interim_timing,
        sens_design_prior = create_beta_prior(input$sens_design_a, input$sens_design_b),
        sens_analysis_prior = create_beta_prior(input$sens_analysis_a, input$sens_analysis_b),
        spec_design_prior = create_beta_prior(input$spec_design_a, input$spec_design_b),
        spec_analysis_prior = create_beta_prior(input$spec_analysis_a, input$spec_analysis_b),
        prevalence_design_prior = create_beta_prior(input$prev_design_a, input$prev_design_b),
        target_assurance = if(is.na(input$target_assurance)) NA_real_ else input$target_assurance,
        futility_assurance = if(is.na(input$futility_assurance)) NA_real_ else input$futility_assurance
      )

      validate(
        need(!is.na(input$measure), "An accuracy measure must be specified."),
        need(length(length(base_params$sens_design_prior) > 0),
             "Prevalence design prior must be specified."),
        # need(input$n_range[1] > 0, "Minimum sample size cannot be 0.")
      )

      validate(
        need(!(input$measure == "sensitivity" && length(base_params$sens_design_prior) == 0),
             "Sensitivity priors must be specified for this choice of accuracy measure."),
        need(!(input$measure == "specificity" && length(base_params$spec_design_prior) == 0),
             "Specificity priors must be specified for this choice of accuracy measure."),
        need(!(input$measure == "both" &&
                 (length(base_params$sens_design_prior) == 0 || length(base_params$spec_design_prior) == 0)),
             "Both sensitivity and specificity priors must be specified for this choice of accuracy measure.")
      )

      base_params$measure = input$measure

      base_params
    }, error = function(e) {
      return(paste("Error creating analysis parameters:", e$message))
    })
  })


  # Display parameters in sidebar
  output$params_display <- renderUI({
    if (input$saveParams == 0) {
      return(p(class = "text-muted", em("No parameters saved yet.")))
    }

    req(analysis_params_obj())

    params <- analysis_params_obj()

    if (is.character(params)) {
      return(div(style = "color: red; font-size: 0.85em;", params))
    }

    sens_priors = if (params$measure == "specificity") {NULL} else {
      div(class = "mb-3",
          strong("Sensitivity Priors"),
          tags$ul(class = "list-unstyled small ms-2",
                  tags$li(ifelse(length(params$sens_design_prior) > 0,
                                 sprintf("Design: Beta(%.1f, %.1f)", input$sens_design_a, input$sens_design_b),
                                 "Design: Not specified")),
                  tags$li(ifelse(length(params$sens_analysis_prior) > 0,
                                 sprintf("Analysis: Beta(%.1f, %.1f)", input$sens_analysis_a, input$sens_analysis_b),
                                 "Analysis: Not specified"))
          )
      )
    }

    spec_priors = if (params$measure == "sensitivity") {NULL} else {
      div(class = "mb-3",
          strong("Specificity Priors"),
          tags$ul(class = "list-unstyled small ms-2",
                  tags$li(ifelse(length(params$spec_design_prior) > 0,
                                 sprintf("Design: Beta(%.1f, %.1f)", input$spec_design_a, input$spec_design_b),
                                 "Design: Not specified")),
                  tags$li(ifelse(length(params$spec_analysis_prior) > 0,
                                 sprintf("Analysis: Beta(%.1f, %.1f)", input$spec_analysis_a, input$spec_analysis_b),
                                 "Analysis: Not specified"))
          )
      )
    }

    tagList(
      div(class = "mb-3",
          strong("Study Design"),
          tags$ul(class = "list-unstyled small ms-2",
                  tags$li(sprintf("Target Width: %.2f", params$target_width)),
                  tags$li(sprintf("Alpha: %.2f", params$alpha)),
                  tags$li(sprintf("Maximum Sample Size: %d", params$max_n)),
                  tags$li(sprintf("Minimum Sample Size: %d", params$min_n)),
                  #tags$li(sprintf("Sample Size: %d - %d", params$min_n, params$max_n)),
                  tags$li(sprintf("Target Assurance: %.2f", params$target_assurance)),
                  tags$li(sprintf("Futility Assurance: %.2f", params$futility_assurance)),
                  tags$li(sprintf("Number of Interim Analyses: %d", params$num_interims))
          )
      ),
      div(class = "mb-3",
          strong("Accuracy Measure"),
          tags$ul(class = "list-unstyled small ms-2",
                  tags$li(sprintf(tools::toTitleCase(params$measure)))
          )
      ),

      sens_priors,
      spec_priors,

      div(class = "mb-3",
          strong("Prevalence Prior"),
          tags$ul(class = "list-unstyled small ms-2",
                  tags$li(ifelse(length(params$prevalence_design_prior) > 0,
                                 sprintf("Beta(%.1f, %.1f)", input$prev_design_a, input$prev_design_b),
                                 "Not specified"))
          )
      )
    )
  })

  # Display errors
  output$params_error <- renderUI({

    req(analysis_params_obj())
    params <- analysis_params_obj()

    if (is.character(params)) {
      # Parameters not saved yet - show disabled button with message
      tagList(
        div(class = "alert alert-warning", role = "alert",
            icon("exclamation-triangle"),
            params
        )
      )
    }
  })

  # Tab - start of study summary --------

  # Render the summary button conditionally
  output$summary_button_ui <- renderUI({
    if (input$saveParams == 0 || is.character(analysis_params_obj())) {
      # Parameters not saved yet - show disabled button with message
      tagList(
        div(class = "alert alert-warning", role = "alert",
            icon("exclamation-triangle"),
            "Save parameters before trying to generate the study summary."
        ),
        input_task_button("run_summary", "Start of Study Summary",
                          class = "btn-success btn-disabled-grey"
        )
      )
    } else {
      # warning for accuracy measure == both
      params <- analysis_params_obj()
      both_warning = if (params$measure == "both") {
        div(class = "alert alert-warning", role = "alert",
            icon("exclamation-triangle"),
            "Both sensitivity and specificity are being considered.
            Calculations may take a while."
        )
      } else {NULL}

      # Parameters saved - show enabled button
      tagList(
        both_warning,
        input_task_button("run_summary", "Start of Study Summary", class = "btn-success")
      )
    }
  })

  # Run start_of_study_summary
  summary_computed <- eventReactive(input$run_summary, {
    req(analysis_params_obj())

    params <- analysis_params_obj()
    if (is.character(params)) return(list(error = params))

    params$planned_n <- if (input$planned_n_option == "specify") {
      as.integer(input$planned_n_value)
    } else {
      NA_integer_
    }

    tryCatch({
      capture.output({
        result <- start_of_study_summary(params, params$measure)
        # result <- start_of_study_summary(params, input$measure_s)
      })

      list(
        error        = NULL,
        params       = params,
        planned_n    = result$planned_n,
        interims_at  = result$interims_at,
        first_interim = if (length(result$interims_at) > 0) result$interims_at[1] else NA
      )
    }, error = function(e) {
      list(error = e$message)
    })
  })

  # Display results
  output$summary_results <- renderUI({
    r <- summary_computed()

    if (!is.null(r$error)) {
      return(HTML(paste0("<p style='color: red;'>Error: ", r$error, "</p>")))
    }

    params        <- r$params
    planned_n     <- r$planned_n
    first_interim <- r$first_interim

    # Build subtitle for planned_n box
    n_subtitle <- if (!is.na(params$planned_n)) {
      "(user-specified)"
    } else if (is.na(planned_n)) {
      paste0("No sample size in ", params$min_n, "\u2013", params$max_n,
             " achieves target assurance of ", params$target_assurance)
    } else {
      paste0("(minimum to achieve target assurance of ", params$target_assurance, ")")
    }

    n_value_display <- if (is.na(planned_n)) "N/A" else as.character(planned_n)

    # Build subtitle for first interim box
    interim_label <- if (params$num_interims > 1) {
      "First interim analysis"
    } else {
      "Interim analysis"
    }

    interim_subtitle <- if (params$num_interims > 1) {
      remaining <- params$num_interims - 1
      p(paste0("Sample size for the remaining ", remaining,
               " interim analys", if (remaining > 1) "es" else "is",
               " will be recalculated."),
        class = "text-muted small")
    } else if (is.na(first_interim)) {
      p("No interims planned", class = "text-muted small")
    } else {
      NULL
    }

    interim_value_display <- if (is.na(first_interim)) "\u2014" else as.character(first_interim)

    # other details
    details <- study_details(params, params$measure)
    # details <- study_details(params, input$measure_s)

    # display
    tagList(
      layout_columns(
        col_widths = c(6, 6),
        value_box(
          title = "Planned sample size",
          value = n_value_display,
          #showcase = bsicons::bs_icon("people-fill"),
          p(n_subtitle, class = "small"),
          theme = "primary"
        ),
        value_box(
          title = tagList(
            p("Sample size at", class = "small", style = "margin: 0;"),
            interim_label),
          value = interim_value_display,
          #showcase = bsicons::bs_icon("flag-fill"),
          interim_subtitle,
          theme = "secondary"
        )
      ),
      h5("Study Details", style = "margin-top: 1.5rem;"),
      tags$ul(lapply(details, tags$li))
    )
  })

  # Tab - interim analyses --------

  output$interim_panel_ui <- renderUI({
    if (input$saveParams == 0 || is.character(analysis_params_obj())) {
      return(div(class = "alert alert-warning", role = "alert",
                 icon("exclamation-triangle"),
                 "Save parameters before running interim analyses."))
    }

    req(analysis_params_obj())
    params <- analysis_params_obj()
    if (is.character(params)) return(NULL)

    num_interims <- params$num_interims

    if (num_interims == 0) {
      return(div(class = "alert alert-info", role = "alert",
                 "No interim analyses are planned for this study."))
    }

    interim_tabs <- lapply(seq_len(num_interims), interim_tab)
    do.call(navset_pill_list, c(interim_tabs, list(final_tab(), id = "interim_select")))
  })

  # interim panels
  observeEvent(input$saveParams, {
    req(analysis_params_obj())
    params <- analysis_params_obj()
    if (is.character(params)) return(NULL)

    num_interims <- params$num_interims

    lapply(seq_len(num_interims), function(i) {
      tab <- paste0("_int", i)
      btn_id <- paste0("run_interim_", i)
      out_id <- paste0("interim_results_", i)

      # conditional previous data card
      if (i > 1) {
        output[[paste0("prev_data_card_", i)]] <- renderUI({
          new_data_id <- paste0("new_int", i)
          req(input[[new_data_id]])

          if (input[[new_data_id]] == "new") {
            data_input_card(paste0("_prev", i), header = "Previous Data")
          }
        })
      }

      interim_computed <- eventReactive(input[[btn_id]], {

        # input data
        n11 <- input[[paste0("n11", tab)]]
        n22 <- input[[paste0("n22", tab)]]
        nT1 <- input[[paste0("nT1", tab)]]
        nT2 <- input[[paste0("nT2", tab)]]
        data_validate(params$measure, n11, n22, nT1, nT2)
        # data_validate(input$measure_int, n11, n22, nT1, nT2)

        # conditional prev data
        if (i > 1 && input[[paste0("new_int", i)]] == "new") {

          prev_tab <- paste0("_prev", i)
          prev_n11 <- input[[paste0("n11", prev_tab)]]
          prev_n22 <- input[[paste0("n22", prev_tab)]]
          prev_nT1 <- input[[paste0("nT1", prev_tab)]]
          prev_nT2 <- input[[paste0("nT2", prev_tab)]]
          data_validate(params$measure, prev_n11, prev_n22, prev_nT1, prev_nT2)
          # data_validate(input$measure_int, prev_n11, prev_n22, prev_nT1, prev_nT2)

          combined <- update_data(
            previous_data = list(n11 = prev_n11, n22 = prev_n22,
                                 nT1 = prev_nT1, nT2 = prev_nT2),
            new_data = list(n11 = n11, n22 = n22, nT1 = nT1, nT2 = nT2)
          )

          n11 <- combined$n11
          n22 <- combined$n22
          nT1 <- combined$nT1
          nT2 <- combined$nT2
        }


        tryCatch({
          printed <- capture.output({
            result <- interim_analysis(
              n11 = as.integer(n11),
              n22 = as.integer(n22),
              nT1 = as.integer(nT1),
              nT2 = as.integer(nT2),
              analysis_params = params,
              measure = params$measure,
              # measure = input$measure_int,
              print = TRUE
            )
          })

          list(error = NULL,
               printed = printed,
               result = result,
               data = list(n11=n11,n22=n22,nT1=nT1,nT2=nT2))

        }, error = function(e) {
          list(error = e$message)
        })
      })


      output[[out_id]] <- renderUI({
        req(input[[btn_id]])
        r <- interim_computed()

        if (!is.null(r$error)) {
          return(HTML(paste0("<p style='color: red;'>Error: ", r$error, "</p>")))
        }

        result <- r$result
        current_n <- r$data$nT1 + r$data$nT2

        # observed data table
        obs_table = data_display(r$data, "Current Sample")

        status_box = interim_status(result)


        # CI details
        alpha_label <- paste0((1 - params$alpha) * 100, "% credible interval")

        ci_details <- if (result$measure == "both") {
          tagList(
            p(strong("Sensitivity CI: "),
              sprintf("(%.3f, %.3f)", result$sens_CI[1], result$sens_CI[2]),
              sprintf(" — width %.3f", result$sens_CI[2] - result$sens_CI[1])),
            p(strong("Specificity CI: "),
              sprintf("(%.3f, %.3f)", result$spec_CI[1], result$spec_CI[2]),
              sprintf(" — width %.3f", result$spec_CI[2] - result$spec_CI[1]))
          )
        } else {
          ci <- result$credible_interval
          tagList(
            p(strong(paste0(tools::toTitleCase(result$measure), " ", alpha_label, ": "))),
            p(sprintf("(%.3f, %.3f)", ci[1], ci[2]),
              sprintf(" — width %.3f", ci[2] - ci[1]))
          )
        }

        details <- div(
          if (result$stop_for_efficacy) {
            p(sprintf(
              "CI width of %.3f is below the target of %.3f. No further recruitment necessary.",
                      result$final_width, params$target_width))

          } else if (result$stop_for_futility) {
            p(sprintf("Based on existing data, it will not be possible to achieve
                      success without exceeding the maximum sample size of %d.",
                      params$max_n))

          } else {
            rem_interims = params$num_interims - i
            tagList(
              p(sprintf("The re-estimated total sample size is %d.",
                        current_n + result$additional_n)),

              # lower assurance message
              if (isTRUE(result$lower_assurance)) {
                p(class = "text-warning",
                  icon("exclamation-triangle"),
                  " Assurance may be lower than target at end of study.")
              },

              # next interim analysis message
              if (rem_interims > 0) {
                # assuming even spacing
                interims <- interim_timings(result$additional_n, rem_interims)
                #params$interim_timing)

                if (rem_interims == 1) {
                  tagList(
                    p("1 planned interim analysis remaining."),
                    p(sprintf("This interim analysis should take place halfway through the remaining recruitment
                      - after recruiting %d additional participants.", interims$interims_at[1]))
                  )
                } else {
                  tagList(
                    p(sprintf("%d planned interim analyses remaining,
                      which are to be evenly spaced throughout the remaining recruitment.", rem_interims)),
                    p(sprintf("The next interim analysis should take place after recruiting
                          %d additional participants.", interims$interims_at[1]))
                  )
                }
              } else {
                tagList(
                  p("There are no remaining interim analyses."),
                  p("The end of study analysis should be conducted after completing the additional recruitment.")
                )

              }
            )
          },

          #hr(),
          #ci_details
        )

        tagList(
          obs_table,
          layout_columns(
            col_widths = c(6,6),
            status_box,
            details
          ),
          ci_details
        )
      })

    })


  })

  ## final analysis panel
  observeEvent(input$saveParams, {
    req(analysis_params_obj())
    params <- analysis_params_obj()
    if (is.character(params)) return(NULL)

    output$prev_data_card_final <- renderUI({
      req(input$new_final)
      if (input$new_final == "new") {
        data_input_card("_prev_final", header = "Previous Data")
      }
    })

    final_computed <- eventReactive(input$run_final, {

      # input data
      n11 <- input$n11_final
      n22 <- input$n2_final
      nT1 <- input$nT1_final
      nT2 <- input$nT2_final
      data_validate(params$measure, n11, n22, nT1, nT2)
      # data_validate(input$measure_int, n11, n22, nT1, nT2)

      # conditional prev data
      if (input$new_final == "new") {

        prev_tab <- "_prev_final"
        prev_n11 <- input[[paste0("n11", prev_tab)]]
        prev_n22 <- input[[paste0("n22", prev_tab)]]
        prev_nT1 <- input[[paste0("nT1", prev_tab)]]
        prev_nT2 <- input[[paste0("nT2", prev_tab)]]
        data_validate(params$measure, prev_n11, prev_n22, prev_nT1, prev_nT2)
        # data_validate(input$measure_int, prev_n11, prev_n22, prev_nT1, prev_nT2)

        combined <- update_data(
          previous_data = list(n11 = prev_n11, n22 = prev_n22,
                               nT1 = prev_nT1, nT2 = prev_nT2),
          new_data = list(n11 = n11, n22 = n22, nT1 = nT1, nT2 = nT2)
        )

        n11 <- combined$n11
        n22 <- combined$n22
        nT1 <- combined$nT1
        nT2 <- combined$nT2
      }


      tryCatch({
        printed <- capture.output({
          result <- check_study_success(
            n11 = as.integer(n11),
            n22 = as.integer(n22),
            nT1 = as.integer(nT1),
            nT2 = as.integer(nT2),
            analysis_params = params,
            measure = params$measure,
            # measure = input$measure_int,
            print = TRUE
          )
        })

        list(error = NULL,
             printed = printed,
             result = result,
             success = result$success,
             measure = params$measure,
             # measure = input$measure_int,
             final_n = nT1 + nT2)
      }, error = function(e) {
        list(error = e$message)
      })
    })

    output$final_results <- renderUI({
      req(input$run_final)
      r <- final_computed()
      if (!is.null(r$error)) {
        return(HTML(paste0("<p style='color: red;'>Error: ", r$error, "</p>")))
      }

      study_results(r, params, is_final = TRUE)
    })

  })

  # Tab - Check study -----
  output$check_button_ui <- renderUI({
    if (input$saveParams == 0 || is.character(analysis_params_obj())) {
      tagList(
        div(class = "alert alert-warning", role = "alert",
            icon("exclamation-triangle"),
            "Save parameters before checking study success."),
        input_task_button("run_check", "Check Study Success",
                          class = "btn-success btn-disabled-grey")
      )
    } else {

      # warning for accuracy measure == both
      params <- analysis_params_obj()
      both_warning = if (params$measure == "both") {
        div(class = "alert alert-warning", role = "alert",
            icon("exclamation-triangle"),
            "Both sensitivity and specificity are being considered.
            Calculations may take a while."
        )
      } else {NULL}

      tagList(
        both_warning,
        input_task_button("run_check", "Check Study Success", class = "btn-success")
      )
    }
  })

  check_study_computed <- eventReactive(input$run_check, {
    req(analysis_params_obj())
    params <- analysis_params_obj()
    if (is.character(params)) return(list(error = params))

    data_validate(params$measure, input$n11, input$n22, input$nT1, input$nT2)
    # data_validate(input$measure_check, input$n11, input$n22, input$nT1, input$nT2)

    tryCatch({
      result <- if (input$check_futility) {
        interim_analysis(
          n11 = as.integer(input$n11),
          n22 = as.integer(input$n22),
          nT1 = as.integer(input$nT1),
          nT2 = as.integer(input$nT2),
          analysis_params = params,
          measure = params$measure,
          # measure = input$measure_check,
          print = FALSE
        )
      } else {
        check_study_success(
          n11 = as.integer(input$n11),
          n22 = as.integer(input$n22),
          nT1 = as.integer(input$nT1),
          nT2 = as.integer(input$nT2),
          analysis_params = params,
          measure = params$measure,
          # measure = input$measure_check,
          print = FALSE
        )
      }

      # normalise success field
      # in interim analysis stop_for_efficacy is success
      success <- if (input$check_futility) result$stop_for_efficacy else result$success

      list(
        error          = NULL,
        result         = result,
        success        = success,
        final_n        = input$nT1 + input$nT2,
        measure = params$measure,
        # measure = input$measure_check,
        alpha          = params$alpha,
        check_futility = input$check_futility
      )
    }, error = function(e) {
      list(error = e$message)
    })
  })

  output$check_study_results <- renderUI({
    r <- check_study_computed()
    params <- analysis_params_obj()

    if (is.character(params)) {
      return(HTML(paste0("<p style='color: red;'>", params, "</p>")))
    }
    if (!is.null(r$error)) {
      return(HTML(paste0("<p style='color: red;'>Error: ", r$error, "</p>")))
    }

    study_results(r, params, is_final = FALSE)

    # result <- r$result
    #
    # alpha_label <- paste0((1 - r$alpha) * 100, "% Credible Interval")
    #
    # criteria_box <- div(
    #   class = "alert alert-info",
    #   icon("info-circle"),
    #   span(paste0(
    #     "The study is successful if the ",
    #     alpha_label, " for ",
    #     r$measure, " has a width less than ", params$target_width, "."
    #   ))
    # )
    #
    # success_box = if (r$success) {
    #   div(class = "alert alert-success",
    #       icon("check-circle"),
    #       strong("Success — "),
    #       span(paste0("the study was successful based on ", r$final_n, " participants."))
    #   )
    # } else {
    #   div(class = "alert alert-danger",
    #       icon("times-circle"),
    #       strong("Not successful — "),
    #       span(paste0("the study did not meet its success criteria based on ",
    #                   r$final_n, " participants.")),
    #
    #       if (r$check_futility && result$stop_for_futility) {
    #         tagList(
    #           tags$br(),
    #           span(icon("exclamation-triangle"),
    #                strong("Stop for futility — "),
    #                "based on existing data, it will not be possible to achieve success without exceeding the maximum sample size.")
    #         )
    #       }
    #   )
    # }
    #
    # # section includes success_box in else
    # ci_section <- if (r$measure == "both") {
    #   layout_columns(
    #     col_widths = c(6, 6),
    #     value_box(
    #       title = "Sensitivity",
    #       value = sprintf("(%.3f, %.3f)", result$sens_CI[1], result$sens_CI[2]),
    #       p(sprintf("Width: %.3f", result$sens_CI[2] - result$sens_CI[1]), class = "small"),
    #       p(sprintf("Width: %.3f", ci[2] - ci[1]), class = "small"),
    #       min_height = 150,
    #       theme = box_theme(NA) #(r$success)
    #     ),
    #     value_box(
    #       title = paste("Specificity", alpha_label),
    #       value = sprintf("(%.3f, %.3f)", result$spec_CI[1], result$spec_CI[2]),
    #       p(sprintf("Width: %.3f", result$spec_CI[2] - result$spec_CI[1]), class = "small"),
    #       p(sprintf("Width: %.3f", ci[2] - ci[1]), class = "small"),
    #       min_height = 150,
    #       theme = box_theme(NA) #(r$success)
    #     )
    #   )
    # } else {
    #   ci <- result$credible_interval
    #
    #   layout_columns(
    #     col_widths = c(6, 6),
    #     value_box(
    #       title = tools::toTitleCase(result$measure),
    #       value = sprintf("(%.3f, %.3f)", ci[1], ci[2]),
    #       p(alpha_label, class = "small", style = "margin: 0;"),
    #       p(sprintf("Width: %.3f", ci[2] - ci[1]), class = "small"),
    #       min_height = 150,
    #       theme = box_theme(NA) #(r$success)
    #     ),
    #     success_box
    #   )
    #
    # }
    #
    # tagList(
    #   criteria_box,
    #   if (r$measure == "both") success_box,
    #   ci_section
    #   )
  })


  # CI - not rendering table right now
  output$ci_table <- renderTable({

    r <- check_study_computed()
    req(!is.null(r) && is.null(r$error))

    result <- r$result

    if (r$measure == "both") {
      data.frame(
        Measure = c("Sensitivity", "Specificity"),
        `Credible Interval` = c(sprintf("[%.3f, %.3f]", result$sens_CI[1], result$sens_CI[2]),
                                sprintf("[%.3f, %.3f]", result$spec_CI[1], result$spec_CI[2])),
        Width = c(result$sens_CI[2] - result$sens_CI[1],
                  result$spec_CI[2] - result$spec_CI[1])
      )
    } else {
      ci <- result$credible_interval
      data.frame(
        Measure = tools::toTitleCase(result$measure),
        `Credible Interval` = sprintf("[%.3f, %.3f]", ci[1], ci[2]),
        Width = ci[2] - ci[1]
      )
    }
  }, digits = 3, colnames = TRUE)


}


# App ------
shinyApp(ui, server)
