#debug-CALIPER-2


####################################### WEILCOM TO THE SHINY APP ############################################
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
if (require("plotly")) {
    library(plotly)
} else {
    install.packages("plotly")
    library(plotly)
}
if (require("dplyr")) {
    library(dplyr)
} else {
    install.packages("dplyr")
    library(dplyr)
}
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
    titlePanel("Scatter Plot by Segment"),

    sidebarLayout(
        sidebarPanel(
            selectInput("distribution",
                        "Choose Distribution:",
                        choices = c("Truncated gaussian" = "truncated_gaussian",
                                    "Gaussian" = "gaussian",
                                    "Triangular" = "triangular",
                                    "Trapezoidal" = "trapezoidal"),
                        selected = "truncated_gaussian"),
            fileInput("datafile", "Upload CSV File", accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
            actionButton('reset', 'Reset Input', icon = icon("trash")),
            selectInput("separator", "Select Separator:", choices = c("Comma(,)" = ",", "Semicolon(;)" = ";"), selected = ";"),
            
            selectInput("xcol", "Select X Column", choices = NULL),
            selectInput("tcol", "Select T Column", choices = NULL),

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
                numericInput("window_size", "Window Size:", value = NULL),
                numericInput("step_width", "Step Width:", value = NULL),
                numericInput("standard_deviation", "Standard deviation:", value = 5),
                radioButtons(
                    "log_scale",
                    "Log Scale:",
                    choices = c("No" = "no", "Yes" = "yes"),
                    selected = "no"
                )
            ), 
            # for gaussian
            conditionalPanel(
                condition = "input.distribution == 'gaussian'",
                numericInput("standard_deviation",
                             "Standard deviation:",
                             value = 5),
                radioButtons("log_scale", "Log Scale:", choices = c("No" = "no", "Yes" = "yes"), selected = "no")
            ),
            # for triangular
            conditionalPanel(
                condition = "input.distribution == 'triangular'",
                numericInput("window_size", "Window Size:", value = NULL),
                numericInput("step_width", "Step Width:", value = NULL),
                # numericInput("a", "Parameter a:", value = 0),
                numericInput("vertex1", "Vertex:", value = 0.5),
                # numericInput("c", "Paramater c:", value = 1)
                radioButtons(
                    "log_scale",
                    "Log Scale:",
                    choices = c("No" = "no", "Yes" = "yes"),
                    selected = "no"
                )
            ), 
            # for trapezoidal
            conditionalPanel(
                condition = "input.distribution == 'trapezoidal'",
                numericInput("window_size", "Window Size:", value = NULL),
                numericInput("step_width", "Step Width:", value = NULL),
                # numericInput("a_trap", "Parameter a:", value = 0),
                numericInput("vertex1_trap", "Top left vertex:", value = 0.3),
                numericInput("vertex2_trap", "Top right vertex:", value = 0.6),
                # numericInput("d_trap", "Parameter d:", value = 1)
                radioButtons(
                    "log_scale",
                    "Log Scale:",
                    choices = c("No" = "no", "Yes" = "yes"),
                    selected = "no"
                )
            )
        ),

        mainPanel(
            tabsetPanel(id = "tabs",
                tabPanel("Dataset",
                         fluidRow(
                             column(12, plotOutput("uploadedScatterPlot"))
                        )
                ),
                tabPanel("Limit",
                         fluidRow(
                             column(12, plotOutput("scatterPlot")),
                             column(12, div(style = "color: red;", textOutput("errorMessage"))),
                             column(12, withSpinner(plotOutput("scatterPlot2")))
                         )
                ),
                tabPanel("Comparison",
                         fluidRow(
                             conditionalPanel(
                                 condition = "input.tabs == 'Comparison'",
                                 column(12, div(style = "color: red;", textOutput("falseDistribution"))),
                                 # column(12, div(style = "color: red;", textOutput("errorMessage"))),
                                 column(12, numericInput("comparison_sd", "Comparison SD :", value = 6)),
                                 column(12, actionButton("compare", "Compare")),
                                 column(12, div(style = "color: red;", textOutput("paError"))),
                                 column(12, plotOutput("comparisonPlot"))
                             )
                         )
                )
            )
        )
    )
)


############################################ Server logic ############################################

server <- function(input, output, session) {
    values <- reactiveValues(upload_state = NULL)
    
    # Reset inputs when switching the distribution
    observeEvent(input$distribution, {
        if (input$distribution == "truncated_gaussian") {
            updateNumericInput(session, "window_size", value = NULL)
            updateNumericInput(session, "step_width", value = NULL)
            updateNumericInput(session, "standard_deviation", value = 5)
            updateRadioButtons(session, "log_scale", selected = "no")
        } else if (input$distribution == "gaussian") {
            updateNumericInput(session, "standard_deviation", value = 5)
            updateRadioButtons(session, "log_scale", selected = "no")
        } else if (input$distribution == "triangular") {
            updateNumericInput(session, "window_size", value = NULL)
            updateNumericInput(session, "step_width", value = NULL)
            updateNumericInput(session, "vertex1", value = 0.5)
            updateRadioButtons(session, "log_scale", selected = "no")
            showNotification("The vertex is the peak position of the triangular distribution function, with a value range of (0, 1]. 
                             When the input value is 0.5, the triangle is an isosceles triangle.",duration = 10, type = "message")
        } else if (input$distribution == "trapezoidal") {
            updateNumericInput(session, "window_size", value = NULL)
            updateNumericInput(session, "step_width", value = NULL)
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
    
    observeEvent(input$reset, {
        values$upload_state <- 'reset'
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
        print(user_data)
        user_x <- user_data$x
        user_t <- user_data$t

        if (is.na(input$window_size)) {
            window_size <- NULL
        } else {
            window_size <- input$window_size
        }
        if (is.na(input$step_width)) {
            step_width <- NULL
        } else {
            step_width <- input$step_width
        }
        

        standard_deviation <- input$standard_deviation
        # a <- input$a
        vertex1 <- input$vertex1
        # c <- input$c
        # a_trap <- input$a_trap
        vertex1_trap <- input$vertex1_trap
        vertex2_trap <- input$vertex2_trap
        # d_trap <- input$d_trap
        
        
        result <- tryCatch({
            print("Debug: Starting data processing")
            print(
                paste(
                    "Input values:",
                    "window_size:",
                    window_size,
                    "step_width:",
                    step_width,
                    "standard_deviation:",
                    standard_deviation
                )
            )
            if (input$distribution == "truncated_gaussian") {
                print("Debug: Processing truncated_gaussian")
                res <- w_sliding.reflim.plot(
                    user_x,
                    user_t,
                    distribution = input$distribution,
                    standard_deviation = standard_deviation,
                    window.size = window_size,
                    step.width = step_width
                )
                print("Debug: Completed w_sliding.reflim.plot for truncated_gaussian")
            } else if (input$distribution == "gaussian") {
                res <- w_sliding.reflim.plot(
                    user_x,
                    user_t,
                    distribution = input$distribution,
                    standard_deviation = standard_deviation,
                    window.size = NULL,
                    step.width = NULL
                )
            } else if (input$distribution == "triangular") {
                res <- w_sliding.reflim.plot(
                    user_x,
                    user_t,
                    distribution = input$distribution,
                    vertex1 = vertex1,
                    window.size = window_size,
                    step.width = step_width
                )
            } else if (input$distribution == "trapezoidal") {
                res <- w_sliding.reflim.plot(
                    user_x,
                    user_t,
                    distribution = input$distribution,
                    vertex1 = vertex1_trap,
                    vertex2 = vertex2_trap,
                    window.size = window_size,
                    step.width = step_width
                )
            }
            

            # Compute segment index
            segment_indices <- which(res$x == "---") - 1
            segment_count <- length(segment_indices)

            # Update the maximum value of the slider
            updateSliderInput(session, "segment", max = segment_count)

            list(res = res, segment_indices = segment_indices, segment_count = segment_count, error = NULL)
        }, error = function(e) {
            # Returns an error message
            print(paste("Debug: Error ocurred: ", e$message))
            print(paste("Debug: Error call:", deparse(e$call)))
            list(res = NULL, segment_indices = NULL, segment_count = 0, error = e$message)
        })
        return(result)  # caliper: result -> null
    })

    plot_data <- reactive({
        user_data <- reactive_data()
        user_x <- user_data$x
        user_t <- user_data$t

        if (is.na(input$standard_deviation)) {
            standard_deviation <- NULL
        } else {
            standard_deviation <- input$standard_deviation
        }
        if (is.na(input$window_size)) {
            window_size <- NULL
        } else {
            window_size <- input$window_size
        }
        if (is.na(input$step_width)) {
            step_width <- NULL
        } else {
            step_width <- input$step_width
        }
        
        standard_deviation <- input$standard_deviation
        # a <- input$a
        vertex1 <- input$vertex1
        # c <- input$c
        # a_trap <- input$a_trap
        vertex1_trap <- input$vertex1_trap
        vertex2_trap <- input$vertex2_trap
        # d_trap <- input$d_trap

        result <- tryCatch({
            log_scale_bool <- ifelse(input$log_scale == "yes", TRUE, FALSE)
            if (input$distribution == "truncated_gaussian") {
                run(user_x, user_t, distribution = input$distribution, log.scale = log_scale_bool,
                    standard_deviation = standard_deviation,
                    window.size = window_size,
                    step.width = step_width)
            } else if (input$distribution == "gaussian") {
                run(user_x, user_t, distribution = input$distribution, log.scale = log_scale_bool,
                    standard_deviation = standard_deviation,
                    window.size = NULL,
                    step.width = NULL)
            } else if (input$distribution == "triangular") {
                run(user_x, user_t, distribution = input$distribution, log.scale = log_scale_bool,
                    vertex1 = vertex1,
                    window.size = window_size,
                    step.width = step_width)
            } else if (input$distribution == "trapezoidal") {
                run(user_x, user_t, distribution = input$distribution, log.scale = log_scale_bool,
                     vertex1 = vertex1_trap, vertex2 = vertex2_trap,
                    window.size = window_size,
                    step.width = step_width)
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
            # return("Error: Disallowed Parameters. Please change!")  # show this error in trapzoidal
            return(paste("Error in first plot:", data_info$error))
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
        if (is.null(user_data)) {
            return(NULL)
        }
        
        plot(user_data$t, user_data$x,
             xlab = input$tcol, 
             ylab = input$xcol, 
             main = "Scatter Plot of Uploaded Data",
             pch = 16, 
             col = "darkblue")
    })

    output$scatterPlot <- renderPlot({  # first plot    Error in first plot: 'by' argument is much too small
        print("Debug: Rendering Scatter-Plot ----- by argument")
        par(mar = c(3,3,3,8))
        data_info <- data() # Error: data_info is null
        
        # if (!is.null(data_info$error)) {
        #     return(NULL)
        # }
        
        res <- data_info$res
        print(data_info)
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
        w_values <- as.numeric(segment_data$w)
        
        total_w <- sum(w_values, na.rm = TRUE)
        
        # Creating a continuous color gradient with colorRampPalette()
        color_palette <- colorRampPalette(c("blue", "red"))(100)
        w_colors <- color_palette[cut(w_values, breaks = 100, labels = FALSE)]

        par(mar = c(3, 3, 3, 8))
        print("Debug: begin plot ----- by argument")
        
        plot(as.numeric(segment_data$t),
             as.numeric(segment_data$x),
             xlab = "t",
             ylab = "x",
             main = paste("Scatter Plot for Segment", segment_index, "with Distribution:", input$distribution),
             pch = 16,
             col = w_colors)
        
        legend("topright", 
               inset = c(-0.2, 0), 
               legend = c("w = 1", "w = 0.8", "w = 0.5"), 
               col = color_palette[c(100, 80, 50)],  
               pch = 16,       
               title = "Weighting",     
               xpd = TRUE,
               bty = "n")   
        
        text(x = par("usr")[2] - 0.3 * diff(par("usr")[1:2]),  
             y = par("usr")[3] + 0.05 * diff(par("usr")[3:4]),
             labels = paste("Total w:", round(total_w, 2)), 
             pos = 4, 
             col = "black",
             cex = 1.2)
    })
    
    
    # output$textBelowPlot <- renderUI({
    #     HTML("<p style='color:red;'>Weighting = 1</p>
    #           <p style='color:blue;'>Weighting >= 0.8 and < 1</p>
    #           <p style='color:green;'>Weighting >= 0.5 and < 0.8</p>
    #           <p style='color:black;'>Weighting < 0.5</p>")
    # })

    output$scatterPlot2 <- renderPlot({
        alistplot <- plot_data()

        if (is.null(alistplot)) {
            return(NULL)
        }
        res <- alistplot
        print(alistplot)

        # plot(res$t, res$x, xlab = "t", ylab = "x",
        #      main = paste("Scatter"),
        #      pch = 16, col = "blue")
    })
    
    
    # whether supported comparison types
    output$falseDistribution <- renderText({
        if (input$distribution != "truncated_gaussian" && input$distribution != "gaussian") {
            return("This distribution does not support comparison!")
        }
        return(NULL)
    })
    
    # Calculation/Drawing/ErrorReporting in Comparison
    observeEvent(input$compare, {
        if (input$distribution == "truncated_gaussian" || input$distribution == "gaussian") {
            print("begin to compare")
            user_data <- reactive_data()
            user_x <- user_data$x
            user_t <- user_data$t
            
            output$comparisonPlot <- renderPlot({
                tryCatch({
                    log_scale_bool <- ifelse(input$log_scale == "yes", TRUE, FALSE)
                    print(paste("log_scale_bool:", log_scale_bool))
                        
                    res1 <- w_sliding.reflim(user_x, user_t, distribution = input$distribution, standard_deviation = input$standard_deviation, plot.weight = FALSE)  # Error: Disallowed Parameters. Please change!
                    res2 <- w_sliding.reflim(user_x, user_t, distribution = input$distribution, standard_deviation = input$comparison_sd, plot.weight = FALSE)
                    alist_custom_sd_plot <- gg_alist_custom_sd(res1, res2, log.scale = log_scale_bool)
                    plot(alist_custom_sd_plot)
                    
                    output$paError <- renderText({""})
                }, error = function(e) {
                    output$paError <- renderText({
                        return("Error: Disallowed Parameters. Please change!")
                    })
                })
                
            })
        } else {
            showNotification("Comparison only available for Gaussian or Truncated Gaussian distributions", type = "error")
        }
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