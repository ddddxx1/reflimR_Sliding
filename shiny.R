setwd("C:\\Users\\12591\\Desktop")
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











x <- reflimR::livertests$ALB
t <- reflimR::livertests$Age

ui <- fluidPage(
    titlePanel("Scatter Plot by Segment"),

    sidebarLayout(
        sidebarPanel(
            selectInput("verteilung",
                        "Choose Distribution:",
                        choices = c("Gaussian" = "gaussian",
                                    "Triangular" = "triangular",
                                    "Trapezoidal" = "trapezoidal"),
                        selected = "gaussian"),
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
            numericInput("standardabweichung",
                         "Standardabweichung:",
                         value = 5),
            numericInput("window_size",
                         "Window Size:",
                         value = NULL),
            numericInput("step_width",
                         "Step Width:",
                         value = NULL)
        ),

        mainPanel(
            fluidRow(
                column(12, plotOutput("scatterPlot")),
                column(12, plotOutput("scatterPlot2"))
            ),
            textOutput("errorMessage")
        )
    )
)

server <- function(input, output, session) {

    # uploaded_data <- reactive({
    #     if (is.null(input$datafile)) {
    #         return(list(x = x, t = t))
    #     } else {
    #         uploaded <- read.csv(input$datafile$datapath)
    #         return(list(x = uploaded$x, t = uploaded$t))
    #     }
    # })

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

        result <- tryCatch({
            res <- w_sliding.reflim.plot(user_x, user_t, verteilung = input$verteilung,
                                         standard_deviation = standardabweichung,
                                         window.size = window_size,
                                         step.width = step_width)

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

        result <- tryCatch({
            run(user_x, user_t, verteilung = input$verteilung,
                standardabweichung = standardabweichung,
                window.size = window_size,
                step.width = step_width)
        }, error = function(e) {
            return(NULL)
        })
        return(result)
    })

    # show Error Messages in UI
    output$errorMessage <- renderText({
        data_info <- data()
        if (!is.null(data_info$error)) {
            return(paste("Error: Error in function w_sliding.reflim.plot")) # error
          # return(paste("Error:", data_info$error))
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

        # 将 w 转换为灰度颜色
        w_values <- as.numeric(segment_data$w)
        color_palette <- colorRampPalette(c("white", "black"))(100)
        w_colors <- color_palette[cut(w_values, breaks = 100)]

        w_colors[w_values == 1] <- "red"

        # 绘制散点图，点颜色根据 w 值确定
        plot(as.numeric(segment_data$t),
             as.numeric(segment_data$x),
             xlab = "t",
             ylab = "x",
             main = paste("Scatter Plot for Segment", segment_index, "with Distribution:", input$verteilung),
             pch = 16,
             col = w_colors)
    })

    output$scatterPlot2 <- renderPlot({
        data_info1 <- plot_data()

        if (is.null(data_info1)) {
            return(NULL)
        }
        res <- data_info1$result.sliding.reflim

        plot(res$t, res$x, xlab = "t", ylab = "x",
             main = paste("Scatter"),
             pch = 16, col = "blue")
    })
}

shinyApp(ui = ui, server = server)































##### function #####

w_sliding.reflim.plot <- function(x,covariate,verteilung = "gaussian", standard_deviation = 5, window.size=NULL,step.width=NULL,lognormal=NULL,perc.trunc=2.5,n.min.window=200,n.min=100,apply.rounding=FALSE)
{
    print(paste("sd = ", standard_deviation))
    
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
    
    a <- 0

    
    if (!is.null(window.size) & !is.null(step.width)) {
        print("!is.null(window.size) & !is.null(step.width)")
        window.left <- covcomp[1]
        window.right <- window.left + window.size
        for (i in 1:n.steps) {

            is.in.interval <- covcomp >= window.left & covcomp <= window.right
            if (sum(is.in.interval) >= n.min) {
                
                interval_cov <- covcomp[is.in.interval]
                t.interval[[i]] <- interval_cov
                
                xxx <- xx[is.in.interval]
                x.interval[[i]] <- xxx 

                
                if (verteilung == "gaussian") {
                    w_function <- makeWeightFunction(verteilung, sigma = standard_deviation)
                    www <- w_function(interval_cov, mean = median(interval_cov))
                } else if (verteilung == "triangular") {
                    a.value <- min(interval_cov)
                    b.value <- median(interval_cov)
                    c.value <- max(interval_cov)
                    w_function <- makeWeightFunction(verteilung, a = a.value, b = b.value, c = c.value)
                    www <- w_function(interval_cov)
                } else if (verteilung == "trapezoidal") {
                    a.value <- min(interval_cov)
                    b.value <- quantile(interval_cov, 0.25)
                    c.value <- quantile(interval_cov, 0.75)
                    d.value <- max(interval_cov)
                    
                    w_function <- makeWeightFunction(distribution = verteilung, a = a.value, b = b.value, c = c.value, d = d.value)
                    www <- w_function(interval_cov)
                }
                w.interval[[i]] <- www
                
                a <- a + 1
                
                # plot(interval_cov, www, type = "l", col = "blue", lwd = 2, main = "www VS interval_cov")
                # points(interval_cov, www, col = "red")
                www_sum <- sum(www)
                # text(x = mean(interval_cov), y = mean(www), 
                #      labels = paste("Sum of www =", round(www_sum, 2)),
                #      col = "darkgreen", cex = 1.5, font = 2)
                sum.www[i] <- www_sum
                
                
                
                if (sum.www[i] > 100) {
                    res.reflim <- w_reflim(xxx, www, n.min = n.min, apply.rounding = apply.rounding, lognormal = lognormal, plot.all = TRUE)
                    
                    
                    # lower.lim[i] <- res.reflim$limits[1]
                    # upper.lim[i] <- res.reflim$limits[2]
                    # ci.lower.lim.l[i] <- res.reflim$confidence.int[1]
                    # ci.lower.lim.u[i] <- res.reflim$confidence.int[2]
                    # ci.upper.lim.l[i] <- res.reflim$confidence.int[3]
                    # ci.upper.lim.u[i] <- res.reflim$confidence.int[4]
                    
                    
                    # if (names(res.reflim$stats)[1] == "mean") {
                    #     distribution.type[i] <- "normal"
                    # } else {
                    #     distribution.type[i] <- "lognormal"
                    # }
                    
                    # covals <- covcomp[is.in.interval]
                    # covariate.left[i] <- window.left
                    # covariate.right[i] <- window.right
                    # covariate.mean[i] <- mean(covals)
                    # covariate.median[i] <- median(covals)
                    # covariate.n[i] <- sum(is.in.interval)
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
        print("ELSE")
        ind <- 1
        indl <- 1
        indr <- 2
        # a <- 0
        while(indr <= length(cov.unique)) {
          print("loading...")
            is.in.interval <- covcomp >= cov.unique[indl] & covcomp < cov.unique[indr]

            if (sum(is.in.interval) >= n.min.window) {
                
                # print(covcomp[is.in.interval])
                interval_cov <- covcomp[is.in.interval]
                t.interval[[ind]] <- interval_cov
                
                xxx <- xx[is.in.interval]
                x.interval[[ind]] <- xxx
                
                if (verteilung == "gaussian") {
                    w_function <- makeWeightFunction(verteilung, sigma = standard_deviation)
                    www <- w_function(interval_cov, mean = as.numeric(median(interval_cov)))
                } else if (verteilung == "triangular") {
                    a.value <- min(interval_cov)
                    b.value <- median(interval_cov)
                    c.value <- max(interval_cov)
                    w_function <- makeWeightFunction(verteilung, a = a.value, b = b.value, c = c.value)
                    www <- w_function(interval_cov)
                } else if (verteilung == "trapezoidal") {
                    a.value <- min(interval_cov)
                    b.value <- quantile(interval_cov, 0.25)
                    c.value <- quantile(interval_cov, 0.75)
                    d.value <- max(interval_cov)
                    
                    w_function <- makeWeightFunction(distribution = verteilung, a = a.value, b = b.value, c = c.value, d = d.value)  # todo
                    www <- w_function(interval_cov)
                }
                w.interval[[ind]] <- www 

                
                
                
                # print(length(x.interval[[ind]]))
                # print(length(t.interval[[ind]]))
                # print(length(w.interval[[ind]]))
                # print("--")
                a <- a + 1
                
                # www <- w_function(interval_cov, mean = median(interval_cov))
                
                
                # plot(interval_cov, www, type = "l", col = "blue", lwd = 2, main = "www VS interval_cov")
                # points(interval_cov, www, col = "red")
                www_sum <- sum(www)
                # text(x = mean(interval_cov), y = mean(www), 
                #      labels = paste("Sum of www =", round(www_sum, 2)),
                #      col = "darkgreen", cex = 1.5, font = 2)
                sum.www[ind] <- www_sum
                
                
                
                # while(sum.www[ind] < 7) {
                #     indr <- indr + 1
                # }
                
                if (sum.www[ind] > 100) {
                    res.reflim <- w_reflim(xxx, www, n.min = n.min, apply.rounding = apply.rounding, lognormal = lognormal, plot.all = TRUE)
                    # print(res.reflim)
                    
                    # lower.lim[ind] <- res.reflim$limits[1]
                    # upper.lim[ind] <- res.reflim$limits[2]
                    # ci.lower.lim.l[ind] <- res.reflim$confidence.int[1]
                    # ci.lower.lim.u[ind] <- res.reflim$confidence.int[2]
                    # ci.upper.lim.l[ind] <- res.reflim$confidence.int[3]
                    # ci.upper.lim.u[ind] <- res.reflim$confidence.int[4]
                    
                    # if (names(res.reflim$stats)[1] == "mean") {
                    #     distribution.type[ind] <- "normal"
                    # } else {
                    #     distribution.type[ind] <- "lognormal"
                    # }
                    
                    # covals <- covcomp[is.in.interval]
                    # covariate.left[ind] <- min(covals)
                    # covariate.right[ind] <- max(covals)
                    # covariate.mean[ind] <- mean(covals)
                    # covariate.median[ind] <- median(covals)
                    # covariate.n[ind] <- sum(is.in.interval)
                    
                    indl <- indl + 1
                    indr <- indr + 1
                    ind <- ind + 1
                } else {
                    indr <- indr + 1
                }
            } else {
                indr <- indr + 1
            }
        }
    }
    
    # res <- data.frame(lower.lim,upper.lim,ci.lower.lim.l,ci.lower.lim.u,ci.upper.lim.l,ci.upper.lim.u,distribution.type,covariate.left,covariate.right,covariate.mean,covariate.median,covariate.n,sum.www)
    # res <- data.frame(x.interval, t.interval, w.interval)
    # # 去除数据框中含有 NA 的行
    # res <- res[!is.na(covariate.n),]
    # return(res)
    
    print(paste("Schleifenzeiten = ", a))
    res <- data.frame()
    

    for (i in 1:a) {
        temp_df <- data.frame(
            x = x.interval[[i]],
            t = t.interval[[i]],
            w = w.interval[[i]]
        )
        # print(i)
        # print(temp_df)
        res <- rbind(res, temp_df)
        
        separator <- data.frame(x = "---", t = "---", w = "---")
        res <- rbind(res, separator)
    }
    
    return(res)
}

