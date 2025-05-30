# MLE-4


####################################### WEILCOME TO THE SHINY APP ###########################################
####################################### from Xufan Dong (2025) ##############################################
#############################################################################################################


####################################### Setup & Dependencies ############################################

# setwd("D:\\project\\R\\Praxisprojekt")
data.basis.path <- getwd()
# source("main.R")
source(paste0(data.basis.path, "/main.R"))
par(mar = c(3,3,3,3)) # Fix the problem of not being able to display pictures because the window is too small

if (require("shiny")) {
    library(shiny)
} else {
    install.packages("shiny")
    library(shiny)
}
if (require("ggplot2")) {
    library(ggplot2)
} else {
    install.packages("ggplot2")
    library(ggplot2)
}
# if (require("plotly")) {
#     library(plotly)
# } else {
#     install.packages("plotly")
#     library(plotly)
# }
# if (require("dplyr")) {
#     library(dplyr)
# } else {
#     install.packages("dplyr")
#     library(dplyr)
# }
if (require("Hmisc")) {
    library(Hmisc)
} else {
    install.packages("Hmisc")
    library(Hmisc)
}
if (require("reflimR")) {
    library(reflimR)
} else {
    install.packages("reflimR")
    library(reflimR)
}
if (require("shinyjs")) {
    library(shinyjs)
} else {
    install.packages("shinyjs")
    library(shinyjs)
}
if (require("truncnorm")) {
    library(truncnorm)
} else {
    install.packages("truncnorm")
    library(truncnorm)
}
# if (require("grid")) {
#     library(grid)
# } else {
#     install.packages("grid")
#     library(grid)
# }

if (require("shinycssloaders")) {
    library(shinycssloaders)
} else {
    install.packages("shinycssloaders")
    library(shinycssloaders)
}




####################################### Sample Dataset ############################################

x <- reflimR::livertests$ALB
t <- reflimR::livertests$Age


####################################### User interface ############################################

ui <- fluidPage(
    useShinyjs(),
    titlePanel("reflimR_Sliding"),

    sidebarLayout(
        sidebarPanel(
            selectInput("distribution",
                        label = div(icon("wave-square"), "Choose weight function:"),
                        choices = c("Truncated gaussian" = "truncated_gaussian",
                                    "Gaussian" = "gaussian",
                                    "Triangular" = "triangular",
                                    "Trapezoidal" = "trapezoidal"),
                        selected = "truncated_gaussian"),
            fileInput("datafile",div(icon("file-csv"), "Upload CSV File"), accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
            actionButton('reset', 'Reset Input', icon = icon("trash")),
            selectInput("separator", "Select Separator:", choices = c("Comma(,)" = ",", "Semicolon(;)" = ";"), selected = ";"),
            
            selectInput("xcol", "Select x column:", choices = NULL),
            selectInput("tcol", "Select covariate column:", choices = NULL),

            sliderInput("segment",
                        "Choose Segment:",
                        min = 1,
                        max = 1, 
                        value = 1,
                        step = 1,
                        animate = animationOptions(interval = 1000, loop = TRUE)),

            # for truncated_gaussian
            conditionalPanel(
                condition = "input.distribution == 'truncated_gaussian'",
                textInput("window_size_truncated", "Window Size:", value = ""),
                textInput("step_width_truncated", "Step Width:", value = ""),
                numericInput("standard_deviation_truncated", "Standard deviation:", value = 5)
                # radioButtons(
                #     "log_scale",
                #     "Log Scale:",
                #     choices = c("No" = "no", "Yes" = "yes"),
                #     selected = "no"
                # )
            ), 
            # for gaussian
            conditionalPanel(
                condition = "input.distribution == 'gaussian'",
                numericInput("standard_deviation_gaussian",
                             "Standard deviation:",
                             value = 5)
                # radioButtons("log_scale", "Log Scale:", choices = c("No" = "no", "Yes" = "yes"), selected = "no")
            ),
            # for triangular
            conditionalPanel(
                condition = "input.distribution == 'triangular'",
                textInput("window_size_triangular", "Window Size:", value = ""),
                textInput("step_width_triangular", "Step Width:", value = ""),
                # numericInput("a", "Parameter a:", value = 0),
                numericInput("vertex1", "Vertex:", value = 0.5)
                # numericInput("c", "Paramater c:", value = 1)
                # radioButtons(
                #     "log_scale",
                #     "Log Scale:",
                #     choices = c("No" = "no", "Yes" = "yes"),
                #     selected = "no"
                # )
            ), 
            # for trapezoidal
            conditionalPanel(
                condition = "input.distribution == 'trapezoidal'",
                textInput("window_size_trapezoidal", "Window Size:", value = ""),
                textInput("step_width_trapezoidal", "Step Width:", value = ""),
                # numericInput("a_trap", "Parameter a:", value = 0),
                numericInput("vertex1_trap", "Top left vertex:", value = 0.3),
                numericInput("vertex2_trap", "Top right vertex:", value = 0.6)
                # numericInput("d_trap", "Parameter d:", value = 1)
                # radioButtons(
                #     "log_scale",
                #     "Log Scale:",
                #     choices = c("No" = "no", "Yes" = "yes"),
                #     selected = "no"
                # )
            ),
            radioButtons(
                "log_scale",
                "Log Scale:",
                choices = c("No" = "no", "Yes" = "yes"),
                selected = "no"
            ),
            radioButtons(
                "method_choice",
                "Choose Method:",
                choices = c("reflmR" = "reflimR", "reflimLOD" = "reflimLOD"),
                selected = "reflimR"
            )
        ),

        mainPanel(
            tabsetPanel(id = "tabs",
                tabPanel("Dataset", icon = icon("database"),
                         fluidRow(
                             column(12,  
                                    h3("Reference Interval Estimation with Sliding Windows"),
                                    p("This Shiny application implements reference interval estimation using sliding windows with various weighting functions. 
                                     It supports multiple distribution types including truncated Gaussian, Gaussian, triangular, and trapezoidal distributions."),
                                    p("Key features:"),
                                    tags$ul(
                                        tags$li("Flexible data input through CSV file upload"),
                                        tags$li("Multiple weighting function options"),
                                        tags$li("Visualization of reference intervals"),
                                        tags$li("Parameter comparison capabilities"),
                                        tags$li("Customizable window sizes and step widths")
                                    ),
                                    p("The application helps prevent abrupt jumps between age groups by implementing smooth transitions 
                                     through weighted calculations, ensuring more reliable and clinically meaningful reference intervals."),
                                    hr(),
                                    plotOutput("uploadedScatterPlot"))
                        )
                ),
                tabPanel("Limit", icon = icon("chart-line"),
                         fluidRow(
                             column(12, withSpinner(plotOutput("scatterPlot2"))),
                             column(12, div(style = "color: red;", textOutput("errorMessage"))),
                             column(12, plotOutput("scatterPlot")),
                             column(12, plotOutput("weightDistPlot"))
                         )
                ),
                tabPanel("Comparison", icon = icon("balance-scale"),
                         fluidRow(
                             conditionalPanel(
                                 condition = "input.tabs == 'Comparison'",
                                 # column(12, div(style = "color: red;", textOutput("falseDistribution"))),
                                 # column(12, div(style = "color: red;", textOutput("errorMessage"))),
                                 
                                 # column(12, numericInput("comparison_sd", "Comparison standard deviation :", value = 6)),
                                 
                                 conditionalPanel(
                                     condition = "input.distribution == 'truncated_gaussian'",
                                     column(12, textInput("comparison_window_size_truncated", "Window Size:", value = "")),
                                     column(12, textInput("comparison_step_width_truncated", "Step Width:", value = "")),
                                     column(12, numericInput("comparison_sd", "Comparison standard deviation:", value = 6))
                                 ),
                                 conditionalPanel(
                                     condition = "input.distribution == 'gaussian'",
                                     column(12, numericInput("comparison_sd", "Comparison standard deviation:", value = 6))
                                 ),
                                 conditionalPanel(
                                     condition = "input.distribution == 'triangular'",
                                     column(12, textInput("comparison_window_size_triangular", "Window Size:", value = "")),
                                     column(12, textInput("comparison_step_width_triangular", "Step Width:", value = "")),
                                     column(12, numericInput("comparison_vertex", "Comparison vertex:", value = 0.5))
                                 ),
                                 conditionalPanel(
                                     condition = "input.distribution == 'trapezoidal'",
                                     column(12, textInput("comparison_window_size_trapezoidal", "Window Size:", value = "")),
                                     column(12, textInput("comparison_step_width_trapezoidal", "Step Width:", value = "")),
                                     column(12, numericInput("comparison_vertex1", "Comparison vertex1:", value = 0.3)),
                                     column(12, numericInput("comparison_vertex2", "Comparison vertex2:", value = 0.6))
                                 ),
                                 column(12, actionButton("compare", "Compare")),
                                 column(12, div(style = "color: red;", textOutput("paError"))),
                                 column(12, plotOutput("comparisonPlot"))
                             )
                         )
                ),
                tabPanel("Settings", icon = icon("cog"),
                         fluidRow(
                             column(12, textInput("weight_threshold", "Weight threshold:", value = NULL)),
                             column(12, actionButton("reset", "Reset Input")),
                             column(12, br()),
                             column(12, br()),
                             column(12, radioButtons("showMoreInfo", 
                                                     "Show more information in console:", 
                                                     choices = c("No" = "no", "Yes" = "yes"),
                                                     selected = "no", inline = TRUE))
                        )
                )
            )
        )
    )
)


############################################ Server logic ############################################

server <- function(input, output, session) {
    values <- reactiveValues(upload_state = NULL)
    weight_threshold_value <- reactiveVal(NULL)
    show_more_info <- reactiveVal(FALSE)
    
    observeEvent(input$weight_threshold, {
        if (nzchar(input$weight_threshold)) {
            tryCatch({
                value <- as.numeric(input$weight_threshold)
                if (is.na(value)) {
                    showNotification("Please enter a valid number", type = "error")
                } else {
                    weight_threshold_value(value)
                    showNotification(paste("Weight threshold value set to:", value), type = "message")
                }
            }, warning = function(w) {
                showNotification("Please enter a valid number", type = "error")
            }, error = function(e) {
                showNotification("Please enter a valid number", type = "error")
            })
        }
    })
    
    observeEvent(input$reset, {
        weight_threshold_value(NULL)
        updateTextInput(session, "weight_threshold", value = "")
        showNotification("Weight threshold value has been reset.", type = "message")
    })
    
    observeEvent(input$showMoreInfo, {
        show_more_info(input$showMoreInfo == "yes")
        if (input$showMoreInfo == "yes") {
            showNotification("More information will be shown in the console.", type = "message")
        } else {
            showNotification("Less information will be shown in the console.", type = "message")
        }
    })
    
    # Reset inputs when switching the distribution
    observeEvent(input$distribution, {
        updateTextInput(session, "window_size_truncated", value = "")
        updateTextInput(session, "step_width_truncated", value = "")
        updateTextInput(session, "window_size_triangular", value = "")
        updateTextInput(session, "step_width_triangular", value = "")
        updateTextInput(session, "window_size_trapezoidal", value = "")
        updateTextInput(session, "step_width_trapezoidal", value = "")
        
        updateTextInput(session, "comparison_window_size_truncated", value = "")
        updateTextInput(session, "comparison_step_width_truncated", value = "")
        updateTextInput(session, "comparison_window_size_triangular", value = "")
        updateTextInput(session, "comparison_step_width_triangular", value = "")
        updateTextInput(session, "comparison_window_size_trapezoidal", value = "")
        updateTextInput(session, "comparison_step_width_trapezoidal", value = "")
        updateNumericInput(session, "comparison_sd", value = 6)
        updateNumericInput(session, "comparison_vertex", value = 0.5)
        updateNumericInput(session, "comparison_vertex1", value = 0.3)
        updateNumericInput(session, "comparison_vertex2", value = 0.6)
        
        if (input$distribution == "truncated_gaussian") {
            # updateTextInput(session, "window_size", value = "")
            # updateTextInput(session, "step_width", value = "")
            updateNumericInput(session, "standard_deviation_truncated", value = 5)
            updateRadioButtons(session, "log_scale", selected = "no")
        } else if (input$distribution == "gaussian") {
            updateNumericInput(session, "standard_deviation_gaussian", value = 5)
            updateRadioButtons(session, "log_scale", selected = "no")
        } else if (input$distribution == "triangular") {
            # updateTextInput(session, "window_size", value = "")
            # updateTextInput(session, "step_width", value = "")
            updateNumericInput(session, "vertex1", value = 0.5)
            updateRadioButtons(session, "log_scale", selected = "no")
            showNotification("The vertex is the peak position of the triangular distribution function, with a value range of (0, 1]. 
                             When the input value is 0.5, the triangle is an isosceles triangle.",duration = 10, type = "message")
        } else if (input$distribution == "trapezoidal") {
            # updateTextInput(session, "window_size", value = "")
            # updateTextInput(session, "step_width", value = "")
            updateNumericInput(session, "vertex1_trap", value = 0.3)
            updateNumericInput(session, "vertex2_trap", value = 0.6)
            updateRadioButtons(session, "log_scale", selected = "no")
            showNotification("The top left and top right vertices of the trapezoidal distribution have a value range of (0, 1]. 
                             When 0.3 and 0.6 are entered respectively, the trapezoid is an isosceles trapezoid.",duration = 10, type = "message")
        }
    })
    
    observeEvent(input$datafile, {
        values$upload_state <- 'uploaded'
    })
    
    observeEvent(input$xcol, {
        file_data <- dataset_input()
        if (!is.null(input$xcol)) {
            x_values <- file_data[[input$xcol]]
            if (any(x_values == 0, na.rm = TRUE) && input$method_choice == "reflimR") {
                showNotification("Dataset contains zero values, recommend using reflimLOD method", type = "warning", duration = 10)
            }
        }
    })
    
    observeEvent(input$reset, {
        values$upload_state <- 'reset'
        shinyjs::reset("datafile")
    })
    
    dataset_input <- reactive({
        if (is.null(values$upload_state)) {
            return(list(x = x, t = t))
        } else if (values$upload_state == 'uploaded') {
            return(read.csv(input$datafile$datapath, sep = input$separator))
        } else if (values$upload_state == 'reset') {
            return(list(x = x, t = t))
        }
    })
    
    # uploaded_data <- reactive({
    #   if (is.null(input$datafile)) {
    #     return(NULL)
    #   } else {
    #     uploaded <- read.csv(input$datafile$datapath)
    #     return(uploaded)
    #   }
    # })

    observe({
        # file_data <- uploaded_data()
        file_data <- dataset_input()
        
        if (!is.null(file_data)) {
            col_names <- names(file_data)
            updateSelectInput(session,
                              "xcol",
                              choices = col_names,
                              selected = col_names[1])
            updateSelectInput(session,
                              "tcol",
                              choices = col_names,
                              selected = col_names[2])
        }
        
    })
    

    reactive_data <- reactive({
        # file_data <- uploaded_data()
        file_data <- dataset_input()
        
        if (is.null(file_data)) {
            return(list(x = x, t = t))  # If not uploaded, use standard x/t
        }
        selected_x <- file_data[[input$xcol]]
        selected_t <- file_data[[input$tcol]]
        
        return(list(x = selected_x, t = selected_t))
    })

    # Calculate res and segment_indices
    data <- reactive({
        user_data <- reactive_data()
        # print(user_data)
        user_x <- user_data$x
        user_t <- user_data$t

        
        window_size <- switch(input$distribution,
                              "truncated_gaussian" = if (nzchar(input$window_size_truncated)) as.numeric(input$window_size_truncated) else NULL,
                              "triangular" = if (nzchar(input$window_size_triangular)) as.numeric(input$window_size_triangular) else NULL,
                              "trapezoidal" = if (nzchar(input$window_size_trapezoidal)) as.numeric(input$window_size_trapezoidal) else NULL,
                              NULL
        )
        
        step_width <- switch(input$distribution,
                             "truncated_gaussian" = if (nzchar(input$step_width_truncated)) as.numeric(input$step_width_truncated) else NULL,
                             "triangular" = if (nzchar(input$step_width_triangular)) as.numeric(input$step_width_triangular) else NULL,
                             "trapezoidal" = if (nzchar(input$step_width_trapezoidal)) as.numeric(input$step_width_trapezoidal) else NULL,
                             NULL
        )
        
        standard_deviation <- switch(input$distribution,
                                     "truncated_gaussian" = input$standard_deviation_truncated,
                                     "gaussian" = input$standard_deviation_gaussian,
                                     NULL
        )

        # standard_deviation <- input$standard_deviation
        # a <- input$a
        vertex1 <- input$vertex1
        # c <- input$c
        # a_trap <- input$a_trap
        vertex1_trap <- input$vertex1_trap
        vertex2_trap <- input$vertex2_trap
        # d_trap <- input$d_trap
        
        
        result <- tryCatch({
            if (input$distribution == "truncated_gaussian") {
                print(
                    paste(
                        "------------Input values:",
                        "window_size:",
                        window_size,
                        "step_width:",
                        step_width,
                        "standard_deviation:",
                        standard_deviation,
                        "------------"
                    )
                )
                res <- w_sliding.reflim.plot(
                    user_x,
                    user_t,
                    distribution = input$distribution,
                    standard_deviation = standard_deviation,
                    window.size = window_size,
                    step.width = step_width,
                    weight_threshold = weight_threshold_value(),
                    verbose = show_more_info()
                )
            } else if (input$distribution == "gaussian") {
                print(
                    paste(
                        "------------Input values:",
                        "window_size:",
                        window_size,
                        "step_width:",
                        step_width,
                        "standard_deviation:",
                        standard_deviation,
                        "------------"
                    )
                )
                res <- w_sliding.reflim.plot(
                    user_x,
                    user_t,
                    distribution = input$distribution,
                    standard_deviation = standard_deviation,
                    window.size = NULL,
                    step.width = NULL,
                    weight_threshold = weight_threshold_value(),
                    verbose = show_more_info()
                )
            } else if (input$distribution == "triangular") {
                print(
                    paste(
                        "------------Input values:",
                        "window_size:",
                        window_size,
                        "step_width:",
                        step_width,
                        "vertex:",
                        vertex1,
                        "------------"
                    )
                )
                res <- w_sliding.reflim.plot(
                    user_x,
                    user_t,
                    distribution = input$distribution,
                    vertex1 = vertex1,
                    window.size = window_size,
                    step.width = step_width,
                    weight_threshold = weight_threshold_value(),
                    verbose = show_more_info()
                )
            } else if (input$distribution == "trapezoidal") {
                print(
                    paste(
                        "------------Input values:",
                        "window_size:",
                        window_size,
                        "step_width:",
                        step_width,
                        "vertex1:",
                        vertex1_trap,
                        "vertex2:",
                        vertex2_trap,
                        "------------"
                    )
                )
                res <- w_sliding.reflim.plot(
                    user_x,
                    user_t,
                    distribution = input$distribution,
                    vertex1 = vertex1_trap,
                    vertex2 = vertex2_trap,
                    window.size = window_size,
                    step.width = step_width,
                    weight_threshold = weight_threshold_value(),
                    verbose = show_more_info()
                )
            }
            

            # Compute segment index
            segment_indices <- which(res$x == "---") - 1
            
            if (length(segment_indices) == 0) {
                segment_indices <- nrow(res)
                segment_count <- 1
            } else {
                if (tail(segment_indices, 1) < nrow(res)) {
                    segment_indices <- c(segment_indices, nrow(res))
                }
                segment_count <- length(segment_indices)
            }
            
            # segment_count <- length(segment_indices)

            # Update the maximum value of the slider
            updateSliderInput(session, "segment", max = segment_count)

            list(res = res, segment_indices = segment_indices, segment_count = segment_count, error = NULL)
        }, error = function(e) {
            # Returns an error message
            list(res = NULL, segment_indices = NULL, segment_count = 0, error = e$message)
        })
        return(result)  # caliper: result -> null
    })

    plot_data <- reactive({
        user_data <- reactive_data()
        user_x <- user_data$x
        user_t <- user_data$t

        # if (is.na(input$standard_deviation)) {
        #     standard_deviation <- NULL
        # } else {
        #     standard_deviation <- input$standard_deviation
        # }
        
        window_size <- switch(input$distribution,
                              "truncated_gaussian" = if (nzchar(input$window_size_truncated)) as.numeric(input$window_size_truncated) else NULL,
                              "triangular" = if (nzchar(input$window_size_triangular)) as.numeric(input$window_size_triangular) else NULL,
                              "trapezoidal" = if (nzchar(input$window_size_trapezoidal)) as.numeric(input$window_size_trapezoidal) else NULL,
                              NULL
        )
        
        step_width <- switch(input$distribution,
                             "truncated_gaussian" = if (nzchar(input$step_width_truncated)) as.numeric(input$step_width_truncated) else NULL,
                             "triangular" = if (nzchar(input$step_width_triangular)) as.numeric(input$step_width_triangular) else NULL,
                             "trapezoidal" = if (nzchar(input$step_width_trapezoidal)) as.numeric(input$step_width_trapezoidal) else NULL,
                             NULL
        )
        
        standard_deviation <- switch(input$distribution,
                                     "truncated_gaussian" = input$standard_deviation_truncated,
                                     "gaussian" = input$standard_deviation_gaussian,
                                     NULL
        )

        # standard_deviation <- input$standard_deviation
        # a <- input$a
        vertex1 <- input$vertex1
        # c <- input$c
        # a_trap <- input$a_trap
        vertex1_trap <- input$vertex1_trap
        vertex2_trap <- input$vertex2_trap
        # d_trap <- input$d_trap

        result <- tryCatch({
            log_scale_bool <- ifelse(input$log_scale == "yes", TRUE, FALSE)
            use_mle <- ifelse(input$method_choice == "reflimR", FALSE, TRUE)
            if (input$distribution == "truncated_gaussian") {
                if (show_more_info()) {
                    print(paste("log scale:", log_scale_bool))
                    print(paste("Using method:", input$method_choice))
                } 
                reflimR_Sliding(user_x, user_t, distribution = input$distribution, log.scale = log_scale_bool,
                    standard_deviation = standard_deviation,
                    window.size = window_size,
                    step.width = step_width,
                    weight_threshold = weight_threshold_value(), 
                    verbose = show_more_info(),
                    MLE = use_mle)
            } else if (input$distribution == "gaussian") {
                if (show_more_info()) {
                    print(paste("log scale:", log_scale_bool))
                    print(paste("Using method:", input$method_choice))
                } 
                reflimR_Sliding(user_x, user_t, distribution = input$distribution, log.scale = log_scale_bool,
                    standard_deviation = standard_deviation,
                    window.size = NULL,
                    step.width = NULL,
                    weight_threshold = weight_threshold_value(), 
                    verbose = show_more_info(),
                    MLE = use_mle)
            } else if (input$distribution == "triangular") {
                if (show_more_info()) {
                    print(paste("log scale:", log_scale_bool))
                    print(paste("Using method:", input$method_choice))
                } 
                reflimR_Sliding(user_x, user_t, distribution = input$distribution, log.scale = log_scale_bool,
                    vertex1 = vertex1,
                    window.size = window_size,
                    step.width = step_width,
                    weight_threshold = weight_threshold_value(),
                    verbose = show_more_info(),
                    MLE = use_mle)
            } else if (input$distribution == "trapezoidal") {
                if (show_more_info()) {
                    print(paste("log scale:", log_scale_bool))
                    print(paste("Using method:", input$method_choice))
                } 
                reflimR_Sliding(user_x, user_t, distribution = input$distribution, log.scale = log_scale_bool,
                     vertex1 = vertex1_trap, vertex2 = vertex2_trap,
                    window.size = window_size,
                    step.width = step_width,
                    weight_threshold = weight_threshold_value(),
                    verbose = show_more_info(),
                    MLE = use_mle)
            }
        }, error = function(e) {
            message("Error: ", e$message)
            return(NULL)
        })
        return(result)
    })

    # show Error Messages in UI (in Limit)
    output$errorMessage <- renderText({
        data_info <- data()
        plot_info <- plot_data()
        
        if (!is.null(data_info$error)) {
            return("Error: Disallowed Parameters. Please change!")  # show this error in trapzoidal
            # return(paste("Error in first plot:", data_info$error))
        }
        if (!is.null(plot_info$error)) {
            return(paste("Error: ", plot_info$error))
        }
        
        # if (!is.null(data_info$error) || !is.null(plot_info$error)) {
        #     return("Error: Disallowed Parameters. Please change!")
        # }
        return(NULL)
    })
    
    output$uploadedScatterPlot <- renderPlot({
        user_data <- reactive_data()
        
        if (is.null(user_data) || all(is.na(user_data$t)) || all(is.na(user_data$x))) {
            return(NULL)
        }

        plot(user_data$t, user_data$x,
             xlab = input$tcol,
             ylab = input$xcol,
             main = "Scatter Plot of Uploaded Data",
             pch = 16,
             col = "darkblue")
    })

    output$scatterPlot <- renderPlot({  # first plot
        par(mar = c(3,3,3,8))
        data_info <- data()
        # options(max.print = 2000)
        # print(data_info$res)
        
        if (!is.null(data_info$error)) {
            return(NULL)
        }
        
        res <- data_info$res

        segment_indices <- data_info$segment_indices
        segment_count <- data_info$segment_count
        
        segment_index <- input$segment

        if (segment_index == 1) {
            start_row <- 1
        } else {
            start_row <- segment_indices[segment_index - 1] + 2
        }
        end_row <- segment_indices[segment_index]
        
        segment_data <- res[start_row:end_row, ]
        
        calculated_window_size <- max(as.numeric(segment_data$t)) - min(as.numeric(segment_data$t))
        calculated_step_width <- if (segment_count > 1 && segment_index > 1) {
            current_start_time <- min(as.numeric(segment_data$t))
            prev_start_time <- min(as.numeric(res[if (segment_index == 2) 1 else segment_indices[segment_index - 2] + 2, "t"]))
            current_start_time - prev_start_time
        } else 0

        if (show_more_info())
        print(paste("window.size (point):", end_row - start_row + 1, "from:", start_row, "to:", end_row, ", window.size (long):", as.numeric(res[end_row, "t"]) - as.numeric(res[start_row, "t"]), 
                          "from:", as.numeric(res[start_row, "t"]), "to", as.numeric(res[end_row, "t"])))
        w_values <- as.numeric(segment_data$w)
        
        total_w <- sum(w_values, na.rm = TRUE)
        
        # Creating a continuous color gradient with colorRampPalette()
        color_palette <- colorRampPalette(c("blue", "red"))(100)
        w_colors <- color_palette[cut(w_values, breaks = 100, labels = FALSE)]

        # par(mar = c(3, 3, 3, 8))
        
        plot(as.numeric(segment_data$t),
             as.numeric(segment_data$x),
             xlab = "t",
             ylab = "x",
             main = paste("Scatter Plot for Segment", segment_index, "with Distribution:", input$distribution),
             pch = 16,
             col = w_colors)
        
        legend("topright",
               inset = c(-0.15, 0),
               legend = c("w = 1", "w = 0.8", "w = 0.5"),
               col = color_palette[c(100, 80, 50)],
               pch = 16,
               title = "Weighting",
               xpd = TRUE,
               bty = "n")
        
        legend("bottomright",
               inset = c(-0.25, 0),
               legend = paste("Window size:", calculated_window_size, "\nStep width:", calculated_step_width),
               col = "black",
               pch = NA,
               title = "",
               xpd = TRUE,
               bty = "n")
    })
    
    
    # output$textBelowPlot <- renderUI({
    #     HTML("<p style='color:red;'>Weighting = 1</p>
    #           <p style='color:blue;'>Weighting >= 0.8 and < 1</p>
    #           <p style='color:green;'>Weighting >= 0.5 and < 0.8</p>
    #           <p style='color:black;'>Weighting < 0.5</p>")
    # })
    
    output$weightDistPlot <- renderPlot({
        par(mar = c(3,3,3,8))
        data_info <- data()
        
        if (!is.null(data_info$error)) {
            return(NULL)
        }
        
        res <- data_info$res    # x t w
        segment_indices <- data_info$segment_indices    # the end of each segment
        segment_count <- data_info$segment_count    # the number of segments
        
        segment_index <- input$segment
        
        if (segment_index == 1) {
            start_row <- 1
        } else {
            start_row <- segment_indices[segment_index - 1] + 2
        }
        end_row <- segment_indices[segment_index]   # error: (200 10) end_row is NA
        
        segment_data <- res[start_row:end_row, ]
        interval_cov <- as.numeric(segment_data$t)
        www <- as.numeric(segment_data$w)
        www_sum <- sum(www, na.rm = TRUE)
        
        plot(interval_cov, www,
             type = "l",
             col = "blue",
             lwd = 2,
             main = paste("Weight Distribution - Segment", segment_index),
             xlab = "Interval Covariate",
             ylab = "Weight")
        points(interval_cov, www, col = "red")


        # legend("bottomright",
        #        inset = c(-0.25, 0),
        #        legend = paste(round(www_sum, 2)),
        #        col = "black",
        #        pch = NA,
        #        title = "Total weight:",
        #        xpd = TRUE,
        #        bty = "o")

        legend("topright",
               legend = paste("Total weight:", round(www_sum, 2)),
               col = "black",
               pch = NA,
               bty = "o",
               bg = "white",
               cex = 0.8,
               inset = 0.02)
        
        # weight_plot <- ggplot(plot_data, aes(x = covariate, y = weight)) +
        #     geom_line(color = "blue", size = 1) +
        #     geom_point(color = "red", size = 2) +
        #     labs(
        #         title = paste("Weight Distribution - Segment", segment_index),
        #         x = "Interval Covariate",
        #         y = "Weight"
        #     ) +
        #     annotate("text",
        #              x = max(interval_cov),
        #              y = max(www),
        #              label = paste("Total weight:", round(www_sum, 2)),
        #              hjust = 1,
        #              vjust = 1,
        #              color = "black",
        #              size = 4,
        #              fontface = "bold",
        #              bg.color = "white") +
        #     theme_minimal() +
        #     theme(
        #         plot.title = element_text(hjust = 0.5, size = 14),
        #         axis.title = element_text(size = 12),
        #         axis.text = element_text(size = 10)
        #     )
        # 
        # print(weight_plot)
        
        
        # plot_data <- data.frame(
        #     covariate = interval_cov,
        #     weight = www
        # )
        # 
        # weight_plot <- ggplot(plot_data, aes(x = covariate, y = weight)) +
        #     geom_line(color = "blue", size = 1) +
        #     geom_point(color = "red", size = 2) +
        #     labs(
        #         title = paste("Weight Distribution - Segment", segment_index),
        #         x = "Interval Covariate",
        #         y = "Weight"
        #     ) +
        #     theme_minimal() +
        #     theme(
        #         plot.title = element_text(hjust = 0.5, size = 14),
        #         axis.title = element_text(size = 12),
        #         axis.text = element_text(size = 10)
        #     )
        # 
        # print(weight_plot)
    })

    output$scatterPlot2 <- renderPlot({
        alistplot <- plot_data()

        if (is.null(alistplot)) {
            return(NULL)
        }
        # res <- alistplot
        # print(alistplot)


        method_name <- if(input$method_choice == "reflimR") "reflimR" else "reflimLOD"
        plot_title <- paste("Reference Limits Plot using", method_name, "method")
        
        alistplot + ggtitle(plot_title) + 
            theme(plot.title = element_text(face = "bold", hjust = 0.5))
    })
    
    observeEvent(input$method_choice, {
        method_name <- if(input$method_choice == "reflimR") "reflimR" else "reflimLOD.MLE"
        showNotification(
            paste("Reference Limits Comparison using", method_name, "method"),
            type = "message",
            duration = 5
        )
    })
    
    observeEvent(input$compare, {
        if (show_more_info() == TRUE)
        print("Use comparison feature.")
        user_data <- reactive_data()
        user_x <- user_data$x
        user_t <- user_data$t
        
        output$comparisonPlot <- renderPlot({
            params <- switch(input$distribution,
                             "truncated_gaussian" = list(
                                 window.size = if (nzchar(input$comparison_window_size_truncated)) as.numeric(input$comparison_window_size_truncated) else NULL,
                                 step.width = if (nzchar(input$comparison_step_width_truncated)) as.numeric(input$comparison_step_width_truncated) else NULL,
                                 standard_deviation = input$comparison_sd
                             ),
                             "gaussian" = list(
                                 standard_deviation = input$comparison_sd
                             ),
                             "triangular" = list(
                                 window.size = if (nzchar(input$comparison_window_size_triangular)) as.numeric(input$comparison_window_size_triangular) else NULL,
                                 step.width = if (nzchar(input$comparison_step_width_triangular)) as.numeric(input$comparison_step_width_triangular) else NULL,
                                 vertex1 = input$comparison_vertex
                             ),
                             "trapezoidal" = list(
                                 window.size = if (nzchar(input$comparison_window_size_trapezoidal)) as.numeric(input$comparison_window_size_trapezoidal) else NULL,
                                 step.width = if (nzchar(input$comparison_step_width_trapezoidal)) as.numeric(input$comparison_step_width_trapezoidal) else NULL,
                                 vertex1 = input$comparison_vertex1,
                                 vertex2 = input$comparison_vertex2
                             )
            )

            original_params <- switch(input$distribution,
                                      "truncated_gaussian" = list(
                                          window.size = if (nzchar(input$window_size_truncated)) as.numeric(input$window_size_truncated) else NULL,
                                          step.width = if (nzchar(input$step_width_truncated)) as.numeric(input$step_width_truncated) else NULL,
                                          standard_deviation = input$standard_deviation_truncated
                                      ),
                                      "gaussian" = list(
                                          standard_deviation = input$standard_deviation_gaussian
                                      ),
                                      "triangular" = list(
                                          window.size = if (nzchar(input$window_size_triangular)) as.numeric(input$window_size_triangular) else NULL,
                                          step.width = if (nzchar(input$step_width_triangular)) as.numeric(input$step_width_triangular) else NULL,
                                          vertex1 = input$vertex1
                                      ),
                                      "trapezoidal" = list(
                                          window.size = if (nzchar(input$window_size_trapezoidal)) as.numeric(input$window_size_trapezoidal) else NULL,
                                          step.width = if (nzchar(input$step_width_trapezoidal)) as.numeric(input$step_width_trapezoidal) else NULL,
                                          vertex1 = input$vertex1_trap,
                                          vertex2 = input$vertex2_trap
                                      )
            )

            tryCatch({
                log_scale_bool <- ifelse(input$log_scale == "yes", TRUE, FALSE)
                use_mle <- ifelse(input$method_choice == "reflimR", FALSE, TRUE)
                
                if (show_more_info()){
                    print(paste("log_scale_bool:", log_scale_bool))
                    print(paste("Using method:", if (use_mle) "reflimLOD" else "reflimR"))
                }
                

                res1 <- do.call(w_sliding.reflim,
                                c(list(user_x, user_t,
                                       distribution = input$distribution,
                                       plot.weight = FALSE,
                                       weight_threshold = weight_threshold_value(),
                                       verbose = show_more_info(),
                                       MLE = use_mle
                                       ),
                                  original_params))

                res2 <- do.call(w_sliding.reflim,
                                c(list(user_x, user_t,
                                       distribution = input$distribution,
                                       plot.weight = FALSE,
                                       weight_threshold = weight_threshold_value(),
                                       verbose = show_more_info(),
                                       MLE = use_mle
                                       ),
                                  params))

                alist_custom_sd_plot <- gg_alist_compare(res1, res2, log.scale = log_scale_bool)
                plot(alist_custom_sd_plot)

                output$paError <- renderText({""})
            }, error = function(e) {
                output$paError <- renderText({
                    return("Error: Disallowed Parameters. Please change!")
                })
            })
        })
        
        
    })
    


}

# Warning in regularize.values(x, y, ties, missing(ties), na.rm = na.rm) :collapsing to unique 'x' values


####################################### Run the application ############################################

shinyApp(ui = ui, server = server)



##### function #####

#' w_sliding.reflim.plot
#' 
#' @description 
#' This function is similar to the w_sliding.reflim function, but only records information about the weights of each point in each loop

# w_sliding.reflim.plot <- function(x,covariate,distribution = "truncated_gaussian", standard_deviation = 5, vertex1 = NULL, vertex2 = NULL, window.size=NULL,step.width=NULL,lognormal=NULL,perc.trunc=2.5,n.min.window=200,n.min=100,apply.rounding=FALSE)
# {
#     is.nona <- !is.na(x) & !is.na(covariate)
#     xx <- x[is.nona]
#     covcomp <- covariate[is.nona]
#     
#     ord.cov <- order(covcomp)
#     xx <- xx[ord.cov]
#     covcomp <- covcomp[ord.cov]
#     
#     if(!is.numeric(xx)){stop("(reflim) x must be numeric.")}
#     if(min(xx) <= 0){stop("(reflim) only positive values allowed.")}
#     n <- length(xx)
#     if(n < 39){stop(paste0("(iboxplot) n = ", n, ". The length of x should be 200 or more. The absolute minimum for reference limit estimation is 39."))}
#     if(n < n.min){  # Determine enough points
#         print(noquote(paste("n =", n, "where a minimum of", n.min, "is required. You may try to reduce n.min at the loss of accuracy.")))
#         return(c(mean = NA, sd = NA, lower.lim = NA, upper.lim = NA))
#     }
#     
#     cov.unique <- covcomp[!duplicated(covcomp)]
#     n.steps <- length(cov.unique)
#     print(paste("n.steps =", n.steps))
#     if(n.steps==1){stop("The covariate is constant.")}
#     
#     if (!is.null(window.size) & !is.null(step.width)){
#         n.steps <- ceiling(max(c(1,(covcomp[length(covcomp)]-covcomp[1]-window.size)/step.width)))
#         print(paste("get new n.steps =", n.steps))
#     }
#     
#     x.interval <- list()
#     t.interval <- list()
#     w.interval <- list()
#     
#     sum.www <- rep(NA, n.steps)
#     
#     loop <- 0
#     
#     if (distribution == "gaussian") {
#         print("gaussian")
#         w_function <- makeWeightFunction("gaussian", sigma = standard_deviation)
#         step_index <- 1
#         for (i in seq(min(covcomp), max(covcomp), length.out = n.steps)) {  # Generate an equally spaced sequence from the minimum to the maximum value of covcomp.
#             www <- w_function(covcomp, mean = i)
# 
#             res.reflim <- w_reflim(xx, www, n.min = n.min, apply.rounding = apply.rounding, lognormal = lognormal, plot.all = TRUE)
#             loop <- loop + 1
# 
#             x.interval[[step_index]] <- xx
#             t.interval[[step_index]] <- covcomp
#             w.interval[[step_index]] <- www
#             
#             step_index <- step_index + 1
#         }
#     } else {
#         if (!is.null(window.size) & !is.null(step.width)) {
#             print("window.size & step.width not null")
#             window.left <- covcomp[1]
#             window.right <- window.left + window.size
#             
#             covariate.left <- numeric(n.steps)
#             covariate.right <- numeric(n.steps)
#             covariate.n <- numeric(n.steps)
#             for (i in 1:n.steps) {
#                 
#                 is.in.interval <- covcomp >= window.left & covcomp <= window.right
#                 if (sum(is.in.interval) >= n.min) {
#                     
#                     interval_cov <- covcomp[is.in.interval]
#                     t.interval[[i]] <- interval_cov
#                     
#                     xxx <- xx[is.in.interval]
#                     x.interval[[i]] <- xxx 
#                     
#                     
#                     if (distribution == "truncated_gaussian") {
#                         w_function <- makeWeightFunction(distribution, sigma = standard_deviation)
#                         www <- w_function(interval_cov, mean = (min(interval_cov) + max(interval_cov)) / 2)
#                     } else if (distribution == "triangular") {
#                         vertex1 <- if (is.null(vertex1)) 0.5 else vertex1
#                         
#                         start_point.value <- min(interval_cov)
#                         end_point.value <- max(interval_cov)
#                         vertex1.value <- (end_point.value - start_point.value) * vertex1 + start_point.value
# 
#                         w_function <- makeWeightFunction(distribution, a = start_point.value, b = vertex1.value, c = end_point.value)
#                         www <- w_function(interval_cov)
#                     } else if (distribution == "trapezoidal") {
#                         vertex1 <- if (is.null(vertex1)) 0.3 else vertex1
#                         vertex2 <- if (is.null(vertex2)) 0.6 else vertex2
#                         
#                         start_point.value <- min(interval_cov)
#                         end_point.value <- max(interval_cov)
#                         vertex1.value <- (end_point.value - start_point.value) * vertex1 + start_point.value
#                         vertex2.value <- (end_point.value - vertex1.value) * vertex2 + start_point.value
#                         
#                         w_function <- makeWeightFunction(distribution = distribution, a = start_point.value, b = vertex1.value, c = vertex2.value, d = end_point.value)
#                         www <- w_function(interval_cov)
#                     }
#                     w.interval[[i]] <- www
# 
#                     www_sum <- sum(www)
# 
#                     sum.www[i] <- www_sum
#                     
#                         res.reflim <- w_reflim(xxx, www, n.min = n.min, apply.rounding = apply.rounding, lognormal = lognormal, plot.all = TRUE)
#                         loop <- loop + 1
#                     
#                 } else {
#                     covariate.left[i] <- window.left
#                     covariate.right[i] <- window.right
#                     covariate.n[i] <- sum(is.in.interval)
#                 }
#                 window.left <- window.left + step.width
#                 window.right <- window.right + step.width
#             }
#         } else {
#             print("window.size & step.width is null")
#             ind <- 1
#             indl <- 1
#             indr <- 2
#             while(indr <= length(cov.unique)) {
#                 is.in.interval <- covcomp >= cov.unique[indl] & covcomp < cov.unique[indr]
# 
#                 if (sum(is.in.interval) >= n.min.window) {
# 
#                     interval_cov <- covcomp[is.in.interval]
#                     t.interval[[ind]] <- interval_cov
#                     
#                     xxx <- xx[is.in.interval]
#                     x.interval[[ind]] <- xxx
#                     
#                     if (distribution == "truncated_gaussian") {
#                         w_function <- makeWeightFunction(distribution = distribution, sigma = standard_deviation)
#                         www <- w_function(interval_cov, mean = (min(interval_cov) + max(interval_cov)) / 2)
#                     } else if (distribution == "triangular") {
#                         vertex1 <- if (is.null(vertex1)) 0.5 else vertex1
#                         
#                         start_point.value <- min(interval_cov)
#                         end_point.value <- max(interval_cov)
#                         vertex1.value <- (end_point.value - start_point.value) * vertex1 + start_point.value
# 
#                         w_function <- makeWeightFunction(distribution, a = start_point.value, b = vertex1.value, c = end_point.value)
#                         www <- w_function(interval_cov)
#                     } else if (distribution == "trapezoidal") {
#                         vertex1 <- if (is.null(vertex1)) 0.3 else vertex1
#                         vertex2 <- if (is.null(vertex2)) 0.6 else vertex2
#                         
#                         start_point.value <- min(interval_cov)
#                         end_point.value <- max(interval_cov)
#                         vertex1.value <- (end_point.value - start_point.value) * vertex1 + start_point.value
#                         vertex2.value <- (end_point.value - vertex1.value) * vertex2 + start_point.value
# 
#                         w_function <- makeWeightFunction(distribution = distribution, a = start_point.value, b = vertex1.value, c = vertex2.value, d = end_point.value)
# 
#                         www <- w_function(interval_cov)
#                     }
#                     w.interval[[ind]] <- www 
#                     
#                     www_sum <- sum(www)
# 
#                     sum.www[ind] <- www_sum
# 
#                     
#                         res.reflim <- w_reflim(xxx, www, n.min = n.min, apply.rounding = apply.rounding, lognormal = lognormal, plot.all = TRUE)
#                         loop <- loop + 1
#                         
#                         indl <- indl + 1
#                         indr <- indr + 1
#                         ind <- ind + 1
#                     
#                 } else {
#                     indr <- indr + 1
#                 }
#             }
#         }
#     }
# 
#     
#     
#     print(paste("Number of loops = ", loop))
#     res <- data.frame()
#     
#     last_was_separetor <- TRUE
# 
#     for (i in 1:loop) {
#         temp_df <- data.frame(
#             x = x.interval[[i]],
#             t = t.interval[[i]],
#             w = w.interval[[i]]
#         )
# 
#         res <- rbind(res, temp_df)
#         
#         if (nrow(temp_df) > 0) {
#             res <- rbind(res, temp_df)
#             
#             if (!last_was_separetor) {
#                 separator <- data.frame(x = "---", t = "---", w = "---")
#                 res <- rbind(res, separator)
#             }
#             last_was_separetor <- FALSE
#         } else {
#             last_was_separetor <- TRUE
#         }
# 
#     }
#     
#     return(res)
# }