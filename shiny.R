#shiny-improvement
setwd("D:\\project\\R\\Praxisprojekt")
source("main.R")
# source("D:/AppData/OneDrive - lelelelele/Studium/Bachelor/WiSe24-25/Praxisprojekt/code/main.R")
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

if (require("shinycssloaders")) {
    library(shinycssloaders)
} else {
    install.packages("shinycssloaders")
    library(shinycssloaders)
}





par(mar = c(3,3,3,3)) # Fix the problem of not being able to display pictures because the window is too small

x <- reflimR::livertests$ALB
t <- reflimR::livertests$Age

ui <- fluidPage(
    titlePanel("Scatter Plot by Segment"),

    sidebarLayout(
        sidebarPanel(
            selectInput("verteilung",
                        "Choose Distribution:",
                        choices = c("Truncated gaussian" = "truncated_gaussian",
                                    "Gaussian" = "gaussian",
                                    "Triangular" = "triangular",
                                    "Trapezoidal" = "trapezoidal"),
                        selected = "truncated_gaussian"),
            fileInput("datafile", "Upload CSV File", accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
            actionButton('reset', 'Reset Input', icon = icon("trash")),
            
            selectInput("xcol", "Select X Column", choices = NULL),
            selectInput("tcol", "Select T Column", choices = NULL),

            sliderInput("segment",
                        "Choose Segment:",
                        min = 1,
                        max = 1, 
                        value = 1,
                        step = 1,
                        animate = animationOptions(interval = 1000, loop = TRUE)),

            conditionalPanel(
              condition = "input.verteilung == 'truncated_gaussian'",
              numericInput("window_size",
                           "Window Size:",
                           value = NULL),
              numericInput("step_width",
                           "Step Width:",
                           value = NULL),
              numericInput("standardabweichung",
                           "Standard deviation:",
                           value = 5)
            ),
            # for gaussian
            conditionalPanel(
                condition = "input.verteilung == 'gaussian'",
                numericInput("standardabweichung",
                             "Standard deviation:",
                             value = 5)
            ),
            # for triangular
            conditionalPanel(
              condition = "input.verteilung == 'triangular'",
              numericInput("window_size",
                           "Window Size:",
                           value = NULL),
              numericInput("step_width",
                           "Step Width:",
                           value = NULL),
              # numericInput("a", "Parameter a:", value = 0),
              numericInput("b", "Vertex:", value = 0.5)
              # numericInput("c", "Paramater c:", value = 1)
            ),
            # for trapezoidal
            conditionalPanel(
              condition = "input.verteilung == 'trapezoidal'",
              numericInput("window_size",
                           "Window Size:",
                           value = NULL),
              numericInput("step_width",
                           "Step Width:",
                           value = NULL),
              # numericInput("a_trap", "Parameter a:", value = 0),
              numericInput("b_trap", "Top left angle:", value = 0.3),
              numericInput("c_trap", "Top right angle:", value = 0.6)
              # numericInput("d_trap", "Parameter d:", value = 1)
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


server <- function(input, output, session) {
    values <- reactiveValues(upload_state = NULL)
    
    # Reset inputs when switching the distribution
    observeEvent(input$verteilung, {
        if (input$verteilung == "truncated_gaussian") {
            updateNumericInput(session, "window_size", value = NULL)
            updateNumericInput(session, "step_width", value = NULL)
            updateNumericInput(session, "standardabweichung", value = 5)
        } else if (input$verteilung == "gaussian") {
            updateNumericInput(session, "standardabweichung", value = 5)
        } else if (input$verteilung == "triangular") {
            updateNumericInput(session, "window_size", value = NULL)
            updateNumericInput(session, "step_width", value = NULL)
            updateNumericInput(session, "b", value = 0.5)
        } else if (input$verteilung == "trapezoidal") {
            updateNumericInput(session, "window_size", value = NULL)
            updateNumericInput(session, "step_width", value = NULL)
            updateNumericInput(session, "b_trap", value = 0.3)
            updateNumericInput(session, "c_trap", value = 0.6)
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
            return(read.csv(input$datafile$datapath))
        } else if (values$upload_state == 'reset') {
            return(list(x = x, t = t))
        }
    })
    
    uploaded_data <- reactive({
      if (is.null(input$datafile)) {
        return(NULL)
      } else {
        uploaded <- read.csv(input$datafile$datapath)
        return(uploaded)
      }
    })

    observe({
      # file_data <- uploaded_data()
      file_data <- dataset_input()

      if (!is.null(file_data)) {
        col_names <- names(file_data)
        updateSelectInput(session, "xcol", choices = col_names, selected = col_names[1])
        updateSelectInput(session, "tcol", choices = col_names, selected = col_names[2])
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
        

        standardabweichung <- input$standardabweichung
        # a <- input$a
        b <- input$b
        # c <- input$c
        # a_trap <- input$a_trap
        b_trap <- input$b_trap
        c_trap <- input$c_trap
        # d_trap <- input$d_trap
        
        

        result <- tryCatch({
          if (input$verteilung == "truncated_gaussian") {
            res <- w_sliding.reflim.plot(user_x, user_t, verteilung = input$verteilung,
                                         standard_deviation = standardabweichung,
                                         window.size = window_size,
                                         step.width = step_width)
          } else if (input$verteilung == "gaussian") {
              res <- w_sliding.reflim.plot(user_x, user_t, verteilung = input$verteilung,
                                           standard_deviation = standardabweichung,
                                           window.size = NULL,
                                           step.width = NULL)
          } else if (input$verteilung == "triangular") {
            res <- w_sliding.reflim.plot(user_x, user_t, verteilung = input$verteilung,
                                         b = b,
                                         window.size = window_size,
                                         step.width = step_width)
          } else if (input$verteilung == "trapezoidal") {
            res <- w_sliding.reflim.plot(user_x, user_t, verteilung = input$verteilung,
                                         b = b_trap, c = c_trap,
                                         window.size = window_size,
                                         step.width = step_width)
          }
            

            # Compute segment index
            segment_indices <- which(res$x == "---") - 1
            segment_count <- length(segment_indices)

            # Update the maximum value of the slider
            updateSliderInput(session, "segment", max = segment_count)

            list(res = res, segment_indices = segment_indices, segment_count = segment_count, error = NULL)
        }, error = function(e) {
            # Returns an error message
            list(res = NULL, segment_indices = NULL, segment_count = 0, error = e$message)
        })

        return(result)
    })

    plot_data <- reactive({
        user_data <- reactive_data()
        user_x <- user_data$x
        user_t <- user_data$t

        if (is.na(input$standardabweichung)) {
            standardabweichung <- NULL
        } else {
            standardabweichung <- input$standardabweichung
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
        
        standardabweichung <- input$standardabweichung
        # a <- input$a
        b <- input$b
        # c <- input$c
        # a_trap <- input$a_trap
        b_trap <- input$b_trap
        c_trap <- input$c_trap
        # d_trap <- input$d_trap

        result <- tryCatch({
            if (input$verteilung == "truncated_gaussian") {
                run(user_x, user_t, verteilung = input$verteilung,
                    standardabweichung = standardabweichung,
                    window.size = window_size,
                    step.width = step_width)
            } else if (input$verteilung == "gaussian") {
                run(user_x, user_t, verteilung = input$verteilung,
                    standardabweichung = standardabweichung,
                    window.size = NULL,
                    step.width = NULL)
            } else if (input$verteilung == "triangular") {
                run(user_x, user_t, verteilung = input$verteilung,
                    b = b,
                    window.size = window_size,
                    step.width = step_width)
            } else if (input$verteilung == "trapezoidal") {
                run(user_x, user_t, verteilung = input$verteilung,
                     b = b_trap, c = c_trap,
                    window.size = window_size,
                    step.width = step_width)
            }
            
        }, error = function(e) {
            message("Error: ", e$message)
            return(NULL)
        })
        return(result)
    })

    # show Error Messages in UI
    output$errorMessage <- renderText({
        data_info <- data()
        plot_info <- plot_data()
        
        # if (!is.null(data_info$error)) {
        #     # return(paste("Error: Error in function w_sliding.reflim.plot")) # error
        #   return(paste("Error:", data_info$error))
        # }
        # if (!is.null(plot_info$error)) {
        #     return(paste("Error: ", plot_info$error))
        # }
        if (!is.null(data_info$error) || !is.null(plot_info$error)) {
            return("Error: Disallowed Parameters. Please change!")
        }
        
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

    output$scatterPlot <- renderPlot({  # first plot
        data_info <- data()
        
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
        w_values <- as.numeric(segment_data$w)
        
        total_w <- sum(w_values, na.rm = TRUE)
        
        # Creating a continuous color gradient with colorRampPalette()
        color_palette <- colorRampPalette(c("blue", "red"))(100)
        w_colors <- color_palette[cut(w_values, breaks = 100, labels = FALSE)]

        par(mar = c(3, 3, 3, 8))
        
        plot(as.numeric(segment_data$t),
             as.numeric(segment_data$x),
             xlab = "t",
             ylab = "x",
             main = paste("Scatter Plot for Segment", segment_index, "with Distribution:", input$verteilung),
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
    
    
    # Comparison
    output$falseDistribution <- renderText({
        if (input$verteilung != "truncated_gaussian" && input$verteilung != "gaussian") {
            return("This distribution does not support comparison!")
        }
        return(NULL)
    })
    
    observeEvent(input$compare, {
        if (input$verteilung == "truncated_gaussian" || input$verteilung == "gaussian") {
            user_data <- reactive_data()
            user_x <- user_data$x
            user_t <- user_data$t
            
            output$comparisonPlot <- renderPlot({
                tryCatch({
                    res1 <- w_sliding.reflim(user_x, user_t, verteilung = input$verteilung, standard_deviation = input$standardabweichung)
                    res2 <- w_sliding.reflim(user_x, user_t, verteilung = input$verteilung, standard_deviation = input$comparison_sd)
                    alist_custom_sd_plot <- gg_alist_custom_sd(res1, res2)
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


























##### function #####

#' w_sliding.reflim.plot
#' 
#' @description 
#' This function is similar to the w_sliding.reflim function, but only records information about the weights of each point in each loop

w_sliding.reflim.plot <- function(x,covariate,verteilung = "truncated_gaussian", standard_deviation = 5, b = NULL, c = NULL, window.size=NULL,step.width=NULL,lognormal=NULL,perc.trunc=2.5,n.min.window=200,n.min=100,apply.rounding=FALSE)
{
    is.nona <- !is.na(x) & !is.na(covariate)
    xx <- x[is.nona]
    covcomp <- covariate[is.nona]
    
    ord.cov <- order(covcomp)
    xx <- xx[ord.cov]
    covcomp <- covcomp[ord.cov]
    
    if(!is.numeric(xx)){stop("(reflim) x must be numeric.")}
    if(min(xx) <= 0){stop("(reflim) only positive values allowed.")}
    n <- length(xx)
    if(n < 39){stop(paste0("(iboxplot) n = ", n, ". The length of x should be 200 or more. The absolute minimum for reference limit estimation is 39."))}
    if(n < n.min){  # Determine enough points
        print(noquote(paste("n =", n, "where a minimum of", n.min, "is required. You may try to reduce n.min at the loss of accuracy.")))
        return(c(mean = NA, sd = NA, lower.lim = NA, upper.lim = NA))
    }
    
    cov.unique <- covcomp[!duplicated(covcomp)]
    n.steps <- length(cov.unique)
    print(paste("n.steps =", n.steps))
    if(n.steps==1){stop("The covariate is constant.")}
    
    if (!is.null(window.size) & !is.null(step.width)){
        n.steps <- ceiling(max(c(1,(covcomp[length(covcomp)]-covcomp[1]-window.size)/step.width)))
        print(paste("get new n.steps =", n.steps))
    }
    
    x.interval <- list()
    t.interval <- list()
    w.interval <- list()
    
    sum.www <- rep(NA, n.steps)
    
    loop <- 0
    
    if (verteilung == "gaussian") {
        print("gaussian")
        w_function <- makeWeightFunction("gaussian", sigma = standard_deviation)
        step_index <- 1
        for (i in seq(min(covcomp), max(covcomp), length.out = n.steps)) {  # Generate an equally spaced sequence from the minimum to the maximum value of covcomp.
            www <- w_function(covcomp, mean = i)

            res.reflim <- w_reflim(xx, www, n.min = n.min, apply.rounding = apply.rounding, lognormal = lognormal, plot.all = TRUE)
            loop <- loop + 1

            x.interval[[step_index]] <- xx
            t.interval[[step_index]] <- covcomp
            w.interval[[step_index]] <- www
            
            step_index <- step_index + 1
        }
    } else {
        if (!is.null(window.size) & !is.null(step.width)) {
            print("window.size & step.width not null")
            window.left <- covcomp[1]
            window.right <- window.left + window.size
            for (i in 1:n.steps) {
                
                is.in.interval <- covcomp >= window.left & covcomp <= window.right
                if (sum(is.in.interval) >= n.min) {
                    
                    interval_cov <- covcomp[is.in.interval]
                    t.interval[[i]] <- interval_cov
                    
                    xxx <- xx[is.in.interval]
                    x.interval[[i]] <- xxx 
                    
                    
                    if (verteilung == "truncated_gaussian") {
                        w_function <- makeWeightFunction(verteilung, sigma = standard_deviation)
                        www <- w_function(interval_cov, mean = (min(interval_cov) + max(interval_cov)) / 2)
                    } else if (verteilung == "triangular") {
                        b <- if (is.null(b)) 0.5 else b
                        
                        a.value <- min(interval_cov)
                        c.value <- max(interval_cov)
                        b.value <- (c.value - a.value) * b + a.value

                        w_function <- makeWeightFunction(verteilung, a = a.value, b = b.value, c = c.value)
                        www <- w_function(interval_cov)
                    } else if (verteilung == "trapezoidal") {
                        b <- if (is.null(b)) 0.3 else b
                        c <- if (is.null(c)) 0.6 else c
                        
                        a.value <- min(interval_cov)
                        d.value <- max(interval_cov)
                        b.value <- (d.value - a.value) * b + a.value
                        c.value <- (d.value - b.value) * c + a.value
                        
                        w_function <- makeWeightFunction(distribution = verteilung, a = a.value, b = b.value, c = c.value, d = d.value)
                        www <- w_function(interval_cov)
                    }
                    w.interval[[i]] <- www

                    www_sum <- sum(www)

                    sum.www[i] <- www_sum
                    
                        res.reflim <- w_reflim(xxx, www, n.min = n.min, apply.rounding = apply.rounding, lognormal = lognormal, plot.all = TRUE)
                        loop <- loop + 1
                    
                } else {
                    covariate.left[i] <- window.left
                    covariate.right[i] <- window.right
                    covariate.n[i] <- sum(is.in.interval)
                }
                window.left <- window.left + step.width
                window.right <- window.right + step.width
            }
        } else {
            print("window.size & step.width is null")
            ind <- 1
            indl <- 1
            indr <- 2
            while(indr <= length(cov.unique)) {
                is.in.interval <- covcomp >= cov.unique[indl] & covcomp < cov.unique[indr]

                if (sum(is.in.interval) >= n.min.window) {

                    interval_cov <- covcomp[is.in.interval]
                    t.interval[[ind]] <- interval_cov
                    
                    xxx <- xx[is.in.interval]
                    x.interval[[ind]] <- xxx
                    
                    if (verteilung == "truncated_gaussian") {
                        w_function <- makeWeightFunction(distribution = verteilung, sigma = standard_deviation)
                        www <- w_function(interval_cov, mean = (min(interval_cov) + max(interval_cov)) / 2)
                    } else if (verteilung == "triangular") {
                        b <- if (is.null(b)) 0.5 else b
                        
                        a.value <- min(interval_cov)
                        c.value <- max(interval_cov)
                        b.value <- (c.value - a.value) * b + a.value

                        w_function <- makeWeightFunction(verteilung, a = a.value, b = b.value, c = c.value)
                        www <- w_function(interval_cov)
                    } else if (verteilung == "trapezoidal") {
                        b <- if (is.null(b)) 0.3 else b
                        c <- if (is.null(c)) 0.6 else c
                        
                        a.value <- min(interval_cov)
                        d.value <- max(interval_cov)
                        b.value <- (d.value - a.value) * b + a.value
                        c.value <- (d.value - b.value) * c + a.value

                        w_function <- makeWeightFunction(distribution = verteilung, a = a.value, b = b.value, c = c.value, d = d.value)

                        www <- w_function(interval_cov)
                    }
                    w.interval[[ind]] <- www 
                    
                    www_sum <- sum(www)

                    sum.www[ind] <- www_sum

                    
                        res.reflim <- w_reflim(xxx, www, n.min = n.min, apply.rounding = apply.rounding, lognormal = lognormal, plot.all = TRUE)
                        loop <- loop + 1
                        
                        indl <- indl + 1
                        indr <- indr + 1
                        ind <- ind + 1
                    
                } else {
                    indr <- indr + 1
                }
            }
        }
    }

    
    
    print(paste("Number of loops = ", loop))
    res <- data.frame()
    
    last_was_separetor <- TRUE

    for (i in 1:loop) {
        temp_df <- data.frame(
            x = x.interval[[i]],
            t = t.interval[[i]],
            w = w.interval[[i]]
        )

        res <- rbind(res, temp_df)
        
        if (nrow(temp_df) > 0) {
            res <- rbind(res, temp_df)
            
            if (!last_was_separetor) {
                separator <- data.frame(x = "---", t = "---", w = "---")
                res <- rbind(res, separator)
            }
            last_was_separetor <- FALSE
        } else {
            last_was_separetor <- TRUE
        }

    }
    
    return(res)
}


shinyApp(ui = ui, server = server)
