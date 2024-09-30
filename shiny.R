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





par(mar = c(1,1,1,1)) # Fix the problem of not being able to display pictures because the window is too small

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

            selectInput("xcol", "Select X Column", choices = NULL),
            selectInput("tcol", "Select T Column", choices = NULL),

            sliderInput("segment",
                        "Choose Segment:",
                        min = 1,
                        max = 1, 
                        value = 1,
                        step = 1,
                        animate = animationOptions(interval = 1000, loop = TRUE)),
            # numericInput("standardabweichung",
            #              "Standardabweichung:",
            #              value = 5),
            
                          
            # numericInput("window_size",
            #              "Window Size:",
            #              value = NULL),
            # numericInput("step_width",
            #              "Step Width:",
            #              value = NULL),
            # for truncated_gaussian
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
              numericInput("b", "Paramater b:", value = 0.5)
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
              numericInput("b_trap", "Parameter b:", value = 0.3),
              numericInput("c_trap", "Parameter c:", value = 0.6)
              # numericInput("d_trap", "Parameter d:", value = 1)
            )
        ),

        mainPanel(
            fluidRow(
                column(12, plotOutput("scatterPlot"), uiOutput("textBelowPlot")),
                column(12, withSpinner(plotOutput("scatterPlot2")))
            ),
            textOutput("errorMessage")
        )
    )
)


server <- function(input, output, session) {
    uploaded_data <- reactive({
      if (is.null(input$datafile)) {
        return(NULL)
      } else {
        uploaded <- read.csv(input$datafile$datapath)
        return(uploaded)
      }
    })

    observe({
      file_data <- uploaded_data()

      if (!is.null(file_data)) {
        col_names <- names(file_data)
        updateSelectInput(session, "xcol", choices = col_names, selected = col_names[1])
        updateSelectInput(session, "tcol", choices = col_names, selected = col_names[2])
      }
    })

    reactive_data <- reactive({
      file_data <- uploaded_data()

      if (is.null(file_data)) {
        return(list(x = x, t = t))  # 没有上传，使用standard x/t
      }
      selected_x <- file_data[[input$xcol]]
      selected_t <- file_data[[input$tcol]]

      return(list(x = selected_x, t = selected_t))
    })

    # 计算 res 和 segment_indices
    data <- reactive({
        user_data <- reactive_data()
        user_x <- user_data$x
        user_t <- user_data$t

        # if (is.na(input$standardabweichung)) {
        #     standardabweichung <- NULL
        # } else {
        #     standardabweichung <- input$standardabweichung
        # }
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
        a <- input$a
        b <- input$b
        c <- input$c
        a_trap <- input$a_trap
        b_trap <- input$b_trap
        c_trap <- input$c_trap
        d_trap <- input$d_trap
        
        

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
            

            # 计算段索引
            segment_indices <- which(res$x == "---") - 1
            segment_count <- length(segment_indices)

            # 更新滑块的最大值
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
        a <- input$a
        b <- input$b
        c <- input$c
        a_trap <- input$a_trap
        b_trap <- input$b_trap
        c_trap <- input$c_trap
        d_trap <- input$d_trap

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
            return(NULL)
        })
        return(result)
    })

    # show Error Messages in UI
    output$errorMessage <- renderText({
        data_info <- data()
        if (!is.null(data_info$error)) {
            # return(paste("Error: Error in function w_sliding.reflim.plot")) # error
          return(paste("Error:", data_info$error))
        }
        return(NULL)
    })

    output$scatterPlot <- renderPlot({
        # 获取计算结果
        data_info <- data()

        # 如果有错误，不生成图表
        if (!is.null(data_info$error)) {
            return(NULL)
        }

        res <- data_info$res
        segment_indices <- data_info$segment_indices
        segment_count <- data_info$segment_count

        # 获取当前选择的段索引
        segment_index <- input$segment

        # 获取当前段的起始和结束位置
        if (segment_index == 1) {
            start_row <- 1
        } else {
            start_row <- segment_indices[segment_index - 1] + 2
        }
        end_row <- segment_indices[segment_index]

        # 提取该段的数据
        segment_data <- res[start_row:end_row, ]

        # color
        w_values <- as.numeric(segment_data$w)
        color_palette <- colorRampPalette(c("white", "black"))(100)
        w_colors <- color_palette[cut(w_values, breaks = 100)]

        w_colors[w_values == 1] <- "red"
        w_colors[w_values >= 0.8 & w_values < 1] <- "blue"
        w_colors[w_values >= 0.5 & w_values < 0.8] <- "green"

        plot(as.numeric(segment_data$t),
             as.numeric(segment_data$x),
             xlab = "t",
             ylab = "x",
             main = paste("Scatter Plot for Segment", segment_index, "with Distribution:", input$verteilung),
             pch = 16,
             col = w_colors)
    })
    
    output$textBelowPlot <- renderUI({
        HTML("<p style='color:red;'>Weighting = 1</p>
              <p style='color:blue;'>Weighting >= 0.8 and < 1</p>
              <p style='color:green;'>Weighting >= 0.5 and < 0.8</p>
              <p style='color:black;'>Weighting < 0.5</p>")
    })

    output$scatterPlot2 <- renderPlot({
        data_info1 <- plot_data()

        if (is.null(data_info1)) {
            return(NULL)
        }
        # res <- data_info1$result.sliding.reflim     # ERROR
        res <- data_info1

        plot(res$t, res$x, xlab = "t", ylab = "x",
             main = paste("Scatter"),
             pch = 16, col = "blue")    # error 只有gaussian不行
    })
}



# shinyApp(ui = ui, server = server)

# Warning in regularize.values(x, y, ties, missing(ties), na.rm = na.rm) :collapsing to unique 'x' values
# 输入的 x 值中有重复的数值，而这些函数通常要求 x 值是唯一的。在这种情况下，R会自动将这些重复的 x 值进行“折叠”，即取唯一值，并相应地处理 y 值。


























##### function #####

#' w_sliding.reflim.plot
#' 
#' @description 
#' This function is similar to the w_sliding.reflim function, but only records information about the weights of each point in each loop

w_sliding.reflim.plot <- function(x,covariate,verteilung = "truncated_gaussian", standard_deviation = 5, b = NULL, c = NULL, window.size=NULL,step.width=NULL,lognormal=NULL,perc.trunc=2.5,n.min.window=200,n.min=100,apply.rounding=FALSE)
{
    # print(paste("sd = ", standard_deviation))
    
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
    if(n < n.min){  # 判断点数
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
    
    # lower.lim <- rep(NA,n.steps)
    # upper.lim <- rep(NA,n.steps)
    # ci.lower.lim.l <- rep(NA,n.steps)
    # ci.lower.lim.u <- rep(NA,n.steps)
    # ci.upper.lim.l <- rep(NA,n.steps)
    # ci.upper.lim.u <- rep(NA,n.steps)
    # distribution.type <- rep(NA,n.steps)
    
    # covariate.left <- rep(NA,n.steps)
    # covariate.right <- rep(NA,n.steps)
    # covariate.mean <- rep(NA,n.steps)
    # covariate.median <- rep(NA,n.steps)
    # covariate.n <- rep(NA,n.steps)
    
    sum.www <- rep(NA, n.steps)
    
    loop <- 0
    
    if (verteilung == "gaussian") {
        print("gaussian")
        w_function <- makeWeightFunction("gaussian", sigma = standard_deviation)
        # print("after makeweight")
        for (i in seq(min(covcomp), max(covcomp), length.out = n.steps)) {  # 生成从 covcomp 的最小值到最大值之间的一个等间距序列
            www <- w_function(covcomp, mean = i)
            
            # if (any(www == 1)) {
            #     print("true")
            # } else {
            #     print("false")
            # }

            res.reflim <- w_reflim(xx, www, n.min = n.min, apply.rounding = apply.rounding, lognormal = lognormal, plot.all = TRUE) # warning
            # 输入的 x 值中有重复的数值，而这些函数通常要求 x 值是唯一的。在这种情况下，R会自动将这些重复的 x 值进行“折叠”，即取唯一值，并相应地处理 y 值。
            loop <- loop + 1

            # lower.lim[i] <- res.reflim$limits[1]
            # upper.lim[i] <- res.reflim$limits[2]
            # ci.lower.lim.l[i] <- res.reflim$confidence.int[1]
            # ci.lower.lim.u[i] <- res.reflim$confidence.int[2]
            # ci.upper.lim.l[i] <- res.reflim$confidence.int[3]
            # ci.upper.lim.u[i] <- res.reflim$confidence.int[4]
            # sum.www[i] <- sum(www)
            # 
            # distribution.type[i] <- ifelse(names(res.reflim)[1] == "mean", "normal", "lognormal")
            # 
            # covariate.left[i] <- min(covcomp)  # 因为不需要区间，所以用最小值
            # covariate.right[i] <- max(covcomp)  # 同样，用最大值
            # # covariate.mean[i] <- mean(covcomp)
            # covariate.mean[i] <- covcomp[which.max(www)]    # 用mean参数来记录权值最大点
            # covariate.median[i] <- median(covcomp)
            # covariate.n[i] <- length(covcomp)  # 统计所有协变量的数量
            # 
            # plot(covcomp, www, type = "l", col = "blue", lwd = 2, main = paste("Gaussian Weight Function at i =", i))
            # points(covcomp, www, col = "red")
            # www_sum <- sum(www)

            x.interval[[i]] <- xx
            t.interval[[i]] <- covcomp
            w.interval[[i]] <- www
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
                        # www <- w_function(interval_cov, mean = median(interval_cov))
                        www <- w_function(interval_cov, mean = (min(interval_cov) + max(interval_cov)) / 2)
                    } else if (verteilung == "triangular") {
                        # a <- if (is.null(a)) 0 else a
                        # b <- if (is.null(b)) 0.5 else b
                        # c <- if (is.null(c)) 1 else c
                        # 
                        # a.value <- quantile(interval_cov, a)
                        # b.value <- quantile(interval_cov, b)
                        # c.value <- quantile(interval_cov, c)
                        
                        b <- if (is.null(b)) 0.5 else b
                        print(b)
                        
                        a.value <- min(interval_cov)
                        c.value <- max(interval_cov)
                        b.value <- (c.value - a.value) * b + a.value
                        
                        # a.value <- min(interval_cov)
                        # b.value <- median(interval_cov)
                        # c.value <- max(interval_cov)
                        w_function <- makeWeightFunction(verteilung, a = a.value, b = b.value, c = c.value)
                        www <- w_function(interval_cov)
                    } else if (verteilung == "trapezoidal") {
                        # a <- if (is.null(a)) 0 else a
                        # b <- if (is.null(b)) 0.25 else b
                        # c <- if (is.null(c)) 0.75 else c
                        # d <- if (is.null(d)) 1 else d
                        # 
                        # a.value <- quantile(interval_cov, a)
                        # b.value <- quantile(interval_cov, b)
                        # c.value <- quantile(interval_cov, c)
                        # d.value <- quantile(interval_cov, d)
                        
                        b <- if (is.null(b)) 0.3 else b
                        c <- if (is.null(c)) 0.6 else c
                        print(b)
                        print(c)
                        
                        a.value <- min(interval_cov)
                        d.value <- max(interval_cov)
                        b.value <- (d.value - a.value) * b + a.value
                        c.value <- (d.value - b.value) * c + a.value
                        
                        # a.value <- min(interval_cov)
                        # b.value <- quantile(interval_cov, 0.25)
                        # c.value <- quantile(interval_cov, 0.75)
                        # d.value <- max(interval_cov)
                        
                        w_function <- makeWeightFunction(distribution = verteilung, a = a.value, b = b.value, c = c.value, d = d.value)
                        www <- w_function(interval_cov)
                    }
                    w.interval[[i]] <- www
                    
                    # loop <- loop + 1
                    
                    # plot(interval_cov, www, type = "l", col = "blue", lwd = 2, main = "www VS interval_cov")
                    # points(interval_cov, www, col = "red")
                    www_sum <- sum(www)
                    # text(x = mean(interval_cov), y = mean(www), 
                    #      labels = paste("Sum of www =", round(www_sum, 2)),
                    #      col = "darkgreen", cex = 1.5, font = 2)
                    sum.www[i] <- www_sum
                    
                    
                    
                    if (sum.www[i] > 100 || TRUE) {
                        res.reflim <- w_reflim(xxx, www, n.min = n.min, apply.rounding = apply.rounding, lognormal = lognormal, plot.all = TRUE)
                        loop <- loop + 1
                        
                    }
                    
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
            # loop <- 0
            while(indr <= length(cov.unique)) {
                # print("in while")
                is.in.interval <- covcomp >= cov.unique[indl] & covcomp < cov.unique[indr]
                
                # print(sum(is.in.interval) >= n.min.window)
                if (sum(is.in.interval) >= n.min.window) {
                    
                    # print(covcomp[is.in.interval])
                    interval_cov <- covcomp[is.in.interval]
                    t.interval[[ind]] <- interval_cov
                    
                    xxx <- xx[is.in.interval]
                    x.interval[[ind]] <- xxx
                    
                    if (verteilung == "truncated_gaussian") {
                        w_function <- makeWeightFunction(distribution = verteilung, sigma = standard_deviation)
                        # www <- w_function(interval_cov, mean = as.numeric(median(interval_cov)))
                        www <- w_function(interval_cov, mean = (min(interval_cov) + max(interval_cov)) / 2)
                    } else if (verteilung == "triangular") {
                        # a <- if (is.null(a)) 0 else a
                        # b <- if (is.null(b)) 0.5 else b
                        # c <- if (is.null(c)) 1 else c
                        # 
                        # a.value <- quantile(interval_cov, a)
                        # b.value <- quantile(interval_cov, b)
                        # c.value <- quantile(interval_cov, c)
                        
                        b <- if (is.null(b)) 0.5 else b
                        print(b)
                        
                        a.value <- min(interval_cov)
                        c.value <- max(interval_cov)
                        b.value <- (c.value - a.value) * b + a.value
                        
                        # a.value <- min(interval_cov)
                        # b.value <- median(interval_cov)
                        # c.value <- max(interval_cov)
                        w_function <- makeWeightFunction(verteilung, a = a.value, b = b.value, c = c.value) # 需要加权函数参数
                        www <- w_function(interval_cov)
                    } else if (verteilung == "trapezoidal") {
                        # a <- if (is.null(a)) 0 else a
                        # b <- if (is.null(b)) 0.25 else b
                        # c <- if (is.null(c)) 0.75 else c
                        # d <- if (is.null(d)) 1 else d
                        
                        # print(paste("a,", a))
                        # print(paste("b,", b))
                        # print(paste("c,", c))
                        # print(paste("d,", d))
                        
                        # a.value <- quantile(interval_cov, a)
                        # b.value <- quantile(interval_cov, b)
                        # c.value <- quantile(interval_cov, c)
                        # d.value <- quantile(interval_cov, d)
                        
                        b <- if (is.null(b)) 0.3 else b
                        c <- if (is.null(c)) 0.6 else c
                        print(b)
                        print(c)
                        
                        a.value <- min(interval_cov)
                        d.value <- max(interval_cov)
                        b.value <- (d.value - a.value) * b + a.value
                        c.value <- (d.value - b.value) * c + a.value
                        
                        
                        # print(paste("a.value ", a.value))
                        # print(paste("b.value ", b.value))
                        # print(paste("c.value ", c.value))
                        # print(paste("d.value ", d.value))
                        
                        # a.value <- min(interval_cov)
                        # b.value <- quantile(interval_cov, 0.25)
                        # c.value <- quantile(interval_cov, 0.75)
                        # d.value <- max(interval_cov)
                        
                        w_function <- makeWeightFunction(distribution = verteilung, a = a.value, b = b.value, c = c.value, d = d.value)
                        # print(w_function)
                        www <- w_function(interval_cov)
                        # print(www)
                    }
                    w.interval[[ind]] <- www 
                    
                    
                    
                    
                    # print(length(x.interval[[ind]]))
                    # print(length(t.interval[[ind]]))
                    # print(length(w.interval[[ind]]))
                    # print("--")
                    
                    # loop <- loop + 1
                    
                    # www <- w_function(interval_cov, mean = median(interval_cov))
                    
                    
                    # plot(interval_cov, www, type = "l", col = "blue", lwd = 2, main = "www VS interval_cov")
                    # points(interval_cov, www, col = "red")
                    www_sum <- sum(www)
                    # print(paste("www_sum", www_sum))
                    # text(x = mean(interval_cov), y = mean(www), 
                    #      labels = paste("Sum of www =", round(www_sum, 2)),
                    #      col = "darkgreen", cex = 1.5, font = 2)
                    sum.www[ind] <- www_sum
                    
                    
                    
                    # while(sum.www[ind] < 7) {
                    #     indr <- indr + 1
                    # }
                    
                    if (sum.www[ind] > 100 || TRUE) { # ERROR -> null
                        res.reflim <- w_reflim(xxx, www, n.min = n.min, apply.rounding = apply.rounding, lognormal = lognormal, plot.all = TRUE)
                        loop <- loop + 1
                        # print(res.reflim)
                        
                        indl <- indl + 1
                        indr <- indr + 1
                        ind <- ind + 1
                        # print(paste("ind", ind))
                    } else {
                        indr <- indr + 1
                    }
                } else {
                    indr <- indr + 1
                }
            }
        }
    }

    
    
    
    # res <- data.frame(lower.lim,upper.lim,ci.lower.lim.l,ci.lower.lim.u,ci.upper.lim.l,ci.upper.lim.u,distribution.type,covariate.left,covariate.right,covariate.mean,covariate.median,covariate.n,sum.www)
    # res <- data.frame(x.interval, t.interval, w.interval)
    # # 去除数据框中含有 NA 的行
    # res <- res[!is.na(covariate.n),]
    # return(res)
    
    print(paste("Schleifenzeiten = ", loop))
    res <- data.frame()
    
    last_was_separetor <- TRUE

    for (i in 1:loop) {
        temp_df <- data.frame(
            x = x.interval[[i]],
            t = t.interval[[i]],
            w = w.interval[[i]]
        )
        # print(i)
        # print(temp_df)
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
        
        # separator <- data.frame(x = "---", t = "---", w = "---")
        # res <- rbind(res, separator)
    }
    
    return(res)
}


shinyApp(ui = ui, server = server)
