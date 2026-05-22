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

    ## Tab - set study parameters ----------
    nav_panel(
      "Parameters",

      card(
        #  card_header("Study Design Parameters"),
        h4("Study Design Paramaters"),

        # Study Design Parameters
        layout_columns(
          col_widths = c(6, 6),
          numericInput("target_width", param_info("Target Width", "The maximum acceptable credible interval width for the study to be a success."),
                       value = 0.2, min = 0, step = 0.01),
          numericInput("alpha", param_info("Alpha","For the credible interval level"),
                       value = 0.05, min = 0, max = 1, step = 0.01)
        ),

        sliderInput("n_range", param_info("Sample Size Range","The minimum and maximum number of participants to recruit."),
                    min = 0, max = 2000, value = c(100, 800), step = 10),
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

      card(
        card_header("Accuracy Measure"),
        radioButtons("measure", NULL, choices = measures, selected = character(),
                     inline = TRUE),
        fill = FALSE
      ),

      # Prior Parameters
      card(
        h4(param_info("Beta Priors - Hyperparameters", "")),

        accordion(
          open = "Prevalence",

          accordion_panel(
            "Sensitivity",
            h6("Design Prior"),
            layout_columns(
              col_widths = c(6, 6),
              numericInput("sens_design_a", "a:", value = 7, min = 0, step = 1),
              numericInput("sens_design_b", "b:", value = 3, min = 0, step = 1)
            ),
            h6("Analysis Prior"),
            layout_columns(
              col_widths = c(6, 6),
              numericInput("sens_analysis_a", "a:", value = 1, min = 0, step = 1),
              numericInput("sens_analysis_b", "b:", value = 1, min = 0, step = 1)
            )
          ),
          accordion_panel(
            "Specificity",
            h6("Design Prior"),
            layout_columns(
              col_widths = c(6, 6),
              numericInput("spec_design_a", "a:", value = NA, min = 0, step = 1),
              numericInput("spec_design_b", "b:", value = NA, min = 0, step = 1)
            ),
            h6("Analysis Prior"),
            layout_columns(
              col_widths = c(6, 6),
              numericInput("spec_analysis_a", "a:", value = 1, min = 0, step = 1),
              numericInput("spec_analysis_b", "b:", value = 1, min = 0, step = 1)
            )
          ),
          accordion_panel(
            "Prevalence",
            #h6("Prior"),
            layout_columns(
              col_widths = c(6, 6),
              numericInput("prev_design_a", "a:", value = 2, min = 0, step = 1),
              numericInput("prev_design_b", "b:", value = 8, min = 0, step = 1)
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

    ## Tab - Instructions -----------
    nav_panel(
      "Instructions",
      card(
        #card_header("How to use this app"),
        includeMarkdown("instructions.md"),
        hr(),
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
    )
  )
)
