# 数据量不足时的平滑处理：在某些区间内数据点较少的情况下，未加权的滑动窗口可能会给出不可靠的参考区间。加权滑动窗口能够将附近数据点的影响考虑进来，从而平滑结果，避免某些区间数据量不足导致的波动。
# 边界效应：在窗口边界处，未加权的滑动窗口可能会受到边界数据稀疏或异常值的影响较大。加权处理可以减少这种边界效应，将更多的注意力放在中心数据点上，而对边界数据点的权重降低。
# 连续性和稳定性：加权滑动窗口可以使得参考区间在随窗口移动时更加连续和平滑，减少剧烈波动，从而提供更稳定的参考区间。
# 灵活性：通过调整权重函数的形状和参数，可以灵活控制不同数据点对参考区间计算的影响，从而适应不同的应用场景和数据分布特点。
# 
# 
# 更新：09.01
# 1. 增加了加权函数的灵活性，可以使用三种不同的分布函数
# 2.确保尺度无关skalenunabhängig，加权函数的在分布的中心点标准化为1
# 
# 问题：

main <- function() {
  # x <- reflimR::livertests$ALB[1:100]
  # t <- reflimR::livertests$Age[1:100]
  # x <- abs(rnorm(400))
  # w <- dnorm(x, mean = median(x), sd = sd(x)/2) # ? sapply(t, dnorm)
  
  x <- reflimR::livertests$ALB
  t <- reflimR::livertests$Age
  w <- dnorm(t, mean = median(t), sd = sd(t)/2)
  run(x, t)
  
  csvdata <- data.frame(x = x, covariate = t)
  write.csv(csvdata, "output-sliding.csv", row.names = FALSE)
  
  w <- dnorm(t, mean = median(t), sd = sd(t)/2) # todo
  
  res <- w_sliding.reflim(x, t)
  alist(result.sliding.reflim = res)
  
  sd = 10
  if (sd == 5) {
    res <- w_sliding.reflim(x, t)
    alist(result.sliding.reflim = res)
  } else {
    res <- w_sliding.reflim(x, t)
    res1 <- w_sliding.reflim(x, t, standard_deviation = sd)
    # alist(res)
    alist1(res, res1)
  }
  
  
  # dnorm todo
  # w_bowley(x, ww)
  lognormal <- w_lognorm(x, w)$lognormal
  res.ibox <- w_iboxplot(x, w, lognormal)
  w_truncated_qqplot(res.ibox$trunc, res.ibox$w_trunc, lognormal = lognormal)
  
  is.nona <- !is.na(x) & !is.na(t) & !is.na(w)
  xa <- x[is.nona]
  t <- t[is.nona]
  w <- w[is.nona]
  
}


#' run
#' 
#' @description 
#' Window slider main function
#' complete the entire process of the program
#' 
#' @param x [numeric]
#' @param t [integer]
#' 
#' @example 
#' run(x, t, verteilung = "truncated_gaussian")
#' 
#' @export

run <- function(x, t, verteilung = "truncated_gaussian", standardabweichung = 5, a = NULL, b = NULL, c = NULL, d = NULL, window.size=NULL,step.width=NULL,lognormal=NULL,perc.trunc=2.5,n.min.window=200,n.min=100,apply.rounding=FALSE) {
    if (verteilung == "truncated_gaussian") {
        if (standardabweichung == 5) {
            res <- w_sliding.reflim(x, t,verteilung = verteilung, window.size = window.size, step.width = step.width, lognormal = lognormal)
            alist(result.sliding.reflim = res)
        } else {
            res <- w_sliding.reflim(x, t, verteilung = verteilung, window.size = window.size, step.width = step.width, lognormal = lognormal)
            res1 <- w_sliding.reflim(x, t, verteilung = verteilung, standard_deviation = standardabweichung, window.size = window.size, step.width = step.width, lognormal = lognormal)
            # alist(res)
            alist1(res, res1)
        }
    } else if (verteilung == "gaussian") {  # todo
        if (standardabweichung == 5) {
            res <- w_sliding.reflim(x, t, verteilung = verteilung, window.size = NULL, step.width = NULL)
            alist(result.sliding.reflim = res)
            print("alist finish")
        } else {
            res <- w_sliding.reflim(x, t, verteilung = verteilung, window.size = NULL, step.width = NULL, lognormal = lognormal)
            res1 <- w_sliding.reflim(x, t, verteilung = verteilung, standard_deviation = standardabweichung, window.size = NULL, step.width = NULL, lognormal = lognormal)
            alist1(res, res1)
        }
      
    } else {
        res <- w_sliding.reflim(x, t, verteilung = verteilung, a = a, b = b, c = c, d = d, window.size = window.size, step.width = step.width, lognormal = lognormal)
        alist(result.sliding.reflim = res)
    }
}



#' alist
#' 
#' @description 
#' Plot Sliding Reference Limits with Confidence Intervals
#' 
#' @param result.sliding.reflim 
#' A data frame or list containing the sliding reference limits and their associated confidence intervals
#' 
#' @example 
#' alist(result.sliding.reflim = res)
#' 
#' @export

alist <- function(result.sliding.reflim,use.mean=T,xlim=NULL,ylim=NULL,xlab=NULL,ylab=NULL,col.low=c(0,0,1),col.upp=c(1,0,0),lwd=2,transparency=0.8,draw.cis=T,grid.col=NULL,log="",cut.at=1){
    
    # 变量初始化
    rsr <- result.sliding.reflim
    
    cova <- rsr$covariate.mean
    if (!use.mean){
        cova <- rsr$covariate.mean
    }
    
    # 确定y轴范围
    ylim.mod <- ylim
    if (is.null(ylim)){
        ylim.mod <- c(min(rsr$lower.lim),max(rsr$upper.lim))
        if (draw.cis){
            ylim.mod <- c(min(rsr$ci.lower.lim.l),max(rsr$ci.upper.lim.u))
            if (log=="y" | log=="xy"){
                ylim.mod[1] <- max(c(cut.at,ylim.mod[1]))
            }
        }
    }
    
    
    # 绘制下限曲线
    loli <- rsr$lower.lim
    if (log=="y" | log=="xy"){
        loli <- sapply(loli,cut.at.l,l=cut.at)
    }
    
    plot(cova,loli,xlim=xlim,ylim=ylim.mod,type="l",lwd=lwd,col=rgb(col.low[1],col.low[2],col.low[3]),xlab=xlab,ylab=ylab,log=log)
    if (!is.null(grid.col)){
        grid(col=grid.col)
    }
    
    # 绘制上限曲线
    points(cova,rsr$upper.lim,type="l",lwd=lwd,col=rgb(col.upp[1],col.upp[2],col.upp[3]))
    
    # 绘制置信区间
    cilloli <- rsr$ci.lower.lim.l
    ciuloli <- rsr$ci.lower.lim.u
    if (log=="y" | log=="xy"){
        cilloli <- sapply(cilloli,cut.at.l,l=cut.at)
        ciuloli <- sapply(ciuloli,cut.at.l,l=cut.at)
    }
    
    
    if (draw.cis){
        collot <- rgb(col.low[1],col.low[2],col.low[3],1-transparency)
        colupt <- rgb(col.upp[1],col.upp[2],col.upp[3],1-transparency)
        polygon(c(cova,rev(cova)),c(cilloli,rev(ciuloli)),col=collot,border=collot)
        polygon(c(cova,rev(cova)),c(rsr$ci.upper.lim.l,rev(rsr$ci.upper.lim.u)),col=colupt,border=colupt)
    }
    
    
}












#' alist1
#' 
#' @description 
#' Plot 2 Sliding Reference Limits with Confidence Intervals
#' (when default parameters are not used)
#' 
#' @param result.sliding.reflim1 
#' A data frame or list containing the sliding reference limits and their associated confidence intervals
#' 
#' @param result.sliding.reflim2
#' Another data frame or list containing the sliding reference limits and their associated confidence intervals
#'  
#' @example 
#' alist1(res, res1)
#' 
#' @export

alist1 <- function(result.sliding.reflim1, result.sliding.reflim2, use.mean=T, xlim=NULL, ylim=NULL, 
                  xlab=NULL, ylab=NULL, col.low1=c(0,0,1), col.upp1=c(1,0,0), col.low2=c(0,1,0), 
                  col.upp2=c(0,0,0), lwd=2, transparency=0.8, draw.cis=T, grid.col=NULL, log="", 
                  cut.at=1){
  
  # 变量初始化
  rsr1 <- result.sliding.reflim1
  rsr2 <- result.sliding.reflim2
  
  cova1 <- rsr1$covariate.mean
  cova2 <- rsr2$covariate.mean
  if (!use.mean){
    cova1 <- rsr1$covariate.mean
    cova2 <- rsr2$covariate.mean
  }
  
  # 确定y轴范围
  ylim.mod <- ylim
  if (is.null(ylim)){
    ylim.mod <- c(min(c(rsr1$lower.lim, rsr2$lower.lim)), max(c(rsr1$upper.lim, rsr2$upper.lim)))
    if (draw.cis){
      ylim.mod <- c(min(c(rsr1$ci.lower.lim.l, rsr2$ci.lower.lim.l)), max(c(rsr1$ci.upper.lim.u, rsr2$ci.upper.lim.u)))
      if (log=="y" | log=="xy"){
        ylim.mod[1] <- max(c(cut.at, ylim.mod[1]))
      }
    }
  }
  
  # 绘制rsr1下限曲线
  loli1 <- rsr1$lower.lim
  if (log=="y" | log=="xy"){
    loli1 <- sapply(loli1, cut.at.l, l=cut.at)
  }
  plot(cova1, loli1, xlim=xlim, ylim=ylim.mod, type="l", lwd=lwd, col=rgb(col.low1[1], col.low1[2], col.low1[3]), 
       xlab=xlab, ylab=ylab, log=log)
  
  # 绘制rsr1上限曲线
  points(cova1, rsr1$upper.lim, type="l", lwd=lwd, col=rgb(col.upp1[1], col.upp1[2], col.upp1[3]))
  
  # 绘制rsr2下限曲线
  loli2 <- rsr2$lower.lim
  if (log=="y" | log=="xy"){
    loli2 <- sapply(loli2, cut.at.l, l=cut.at)
  }
  points(cova2, loli2, type="l", lwd=lwd, col=rgb(col.low2[1], col.low2[2], col.low2[3]))
  
  # 绘制rsr2上限曲线
  points(cova2, rsr2$upper.lim, type="l", lwd=lwd, col=rgb(col.upp2[1], col.upp2[2], col.upp2[3]))
  
  if (!is.null(grid.col)){
    grid(col=grid.col)
  }
  
  # 绘制置信区间
  if (draw.cis){
    # rsr1置信区间
    cilloli1 <- rsr1$ci.lower.lim.l
    ciuloli1 <- rsr1$ci.lower.lim.u
    if (log=="y" | log=="xy"){
      cilloli1 <- sapply(cilloli1, cut.at.l, l=cut.at)
      ciuloli1 <- sapply(ciuloli1, cut.at.l, l=cut.at)
    }
    collot1 <- rgb(col.low1[1], col.low1[2], col.low1[3], 1-transparency)
    colupt1 <- rgb(col.upp1[1], col.upp1[2], col.upp1[3], 1-transparency)
    polygon(c(cova1, rev(cova1)), c(cilloli1, rev(ciuloli1)), col=collot1, border=collot1)
    polygon(c(cova1, rev(cova1)), c(rsr1$ci.upper.lim.l, rev(rsr1$ci.upper.lim.u)), col=colupt1, border=colupt1)
    
    # rsr2置信区间
    cilloli2 <- rsr2$ci.lower.lim.l
    ciuloli2 <- rsr2$ci.lower.lim.u
    if (log=="y" | log=="xy"){
      cilloli2 <- sapply(cilloli2, cut.at.l, l=cut.at)
      ciuloli2 <- sapply(ciuloli2, cut.at.l, l=cut.at)
    }
    collot2 <- rgb(col.low2[1], col.low2[2], col.low2[3], 1-transparency)
    colupt2 <- rgb(col.upp2[1], col.upp2[2], col.upp2[3], 1-transparency)
    polygon(c(cova2, rev(cova2)), c(cilloli2, rev(ciuloli2)), col=collot2, border=collot2)
    polygon(c(cova2, rev(cova2)), c(rsr2$ci.upper.lim.l, rev(rsr2$ci.upper.lim.u)), col=colupt2, border=colupt2)
  }
}




#' dtriang
#' 
#' @description 
#' Triangular Probability Density Function
#' 
#' @param a [int] left endpoint of a triangle
#' @param b [int] Vertex of a triangle
#' @param c [int] Right endpoint of the triangle
#' 
#' @return [int] Probability density at point x
#' 
#' @export

dtriang <- function(x, a, b, c) {
    # if (x < a || x > c) return(0)
    # if (x <= b) {
    #     return(2 * (x - a) / ((b - a) * (c - a)))
    # } else {
    #     return(2 * (c - x) / ((c - b) * (c - a)))
    # }
    y <- ifelse(x < a | x > c, 0,
                ifelse(x <= b, 2 * (x - a) / ((b - a) * (c - a)),
                       2 * (c - x) / ((c - b) * (c - a))))
    return(y)
}



#' dtrapezoid
#' 
#' @description 
#' Trapezoidal Probability Density Function
#' 
#' @param a [int] left endpoint of a trapezoid
#' @param b [int] Trapezoid left horizontal point
#' @param c [int] Trapezoid right horizontal point
#' @param d [int] Trapezoid right endpoint
#' 
#' @return [int] Probability density
#' 
#' @export

dtrapezoid <- function(x, a, b, c, d) {
    # if (x < a || x > d) return(0)
    # if (x <= b) {
    #     return((x - a) / ((b - a) * (d - a)))
    # } else if (x <= c) {
    #     return(1 / (d - a))
    # } else {
    #     return((d - x) / ((d - c) * (d - a)))
    # }
    y <- ifelse(x < a | x > d, 0,
                ifelse(x <= b, (x - a) / ((b - a) * (d - a)),
                       ifelse(x <= c, 1 / (d - a),
                              (d - x) / ((d - c) * (d - a)))))
    return(y)
}

#' makeWeightFunction
#' 
#' @description 
#' Generate a weighting function based on the specified distribution type
#' 
#' @param distribution [character] distribution type
#' 
#' @return [function] weighting function
#' 
#' @export

makeWeightFunction <- function(distribution = "truncated_gaussian", ...) {
  # print(distribution)
    if (distribution == "truncated_gaussian") {
        sigma <- list(...)$sigma
        if (is.null(sigma)) {
            sigma <- 5
        }
        return(function(x, mean) { # mean将区间内x的中位数传入，不需要用户自己更改
            dnorm(x, mean = mean, sd = sigma) / dnorm(mean, mean = mean, sd = sigma)
        })
    } else if (distribution == "gaussian") {    # todo
        sigma <- list(...)$sigma
        if (is.null(sigma)) {
            sigma <- 5
        }
        return(function(x, mean) {
            dnorm(x, mean = mean, sd = sigma) / dnorm(mean, mean = mean, sd = sigma)
        })
    } else if (distribution == "triangular") {
        a <- list(...)$a
        b <- list(...)$b
        c <- list(...)$c
        return(function(x) {
            dtriang(x, a = a, b = b, c = c) / dtriang(b, a = a, b = b, c = c)
        })
    } else if (distribution == "trapezoidal") {
        a <- list(...)$a
        b <- list(...)$b
        c <- list(...)$c
        d <- list(...)$d
        return(function(x) {
            dtrapezoid(x, a = a, b = b, c = c, d = d) / dtrapezoid((b + c) / 2, a = a, b = b, c = c, d = d)
        })
    } else {
        stop("Unsupported distribution type")
    }
}






#' w_sliding.reflim
#' 
#' @description 
#' Estimate reference limits using sliding windows with weighting functions
#' 
#' This function estimates reference limits using a sliding window approach, applying different types of weighting functions 
#' (truncated Gaussian, triangular, trapezoidal) to the covariate data. It is designed to handle missing values, 
#' perform ordered computations, and calculate weighted reference limits for each windowed interval.
#' 
#' @return lower.lim 参考区间下限
#'          upper.lim 参考区间上限
#' 
#' @export

w_sliding.reflim <- function(x,covariate,verteilung = "truncated_gaussian", standard_deviation = 5, a = NULL, b = NULL, c = NULL, d = NULL, window.size=NULL,step.width=NULL,lognormal=NULL,perc.trunc=2.5,n.min.window=200,n.min=100,apply.rounding=FALSE)
{
    # print("x")
    # print(x)
    # 
    # print("t")
    # print(covariate)
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
    
    lower.lim <- rep(NA,n.steps)
    upper.lim <- rep(NA,n.steps)
    ci.lower.lim.l <- rep(NA,n.steps) 
    ci.lower.lim.u <- rep(NA,n.steps) 
    ci.upper.lim.l <- rep(NA,n.steps)
    ci.upper.lim.u <- rep(NA,n.steps)
    distribution.type <- rep(NA,n.steps)
    
    covariate.left <- rep(NA,n.steps)
    covariate.right <- rep(NA,n.steps)
    covariate.mean <- rep(NA,n.steps)
    covariate.median <- rep(NA,n.steps)
    covariate.n <- rep(NA,n.steps)
    
    sum.www <- rep(NA, n.steps)
    
    # print("xx")
    # print(xx)
    
    if (verteilung == "gaussian") { # todo
        print("gaussian")
        w_function <- makeWeightFunction("gaussian", sigma = standard_deviation)
        for (i in seq(min(covcomp), max(covcomp), length.out = n.steps)) {
            www <- w_function(covcomp, mean = i)
            
            res.reflim <- w_reflim(xx, www, n.min = n.min, apply.rounding = apply.rounding, lognormal = lognormal, plot.all = TRUE)
            
            lower.lim[i] <- res.reflim$limits[1]
            upper.lim[i] <- res.reflim$limits[2]
            ci.lower.lim.l[i] <- res.reflim$confidence.int[1]
            ci.lower.lim.u[i] <- res.reflim$confidence.int[2]
            ci.upper.lim.l[i] <- res.reflim$confidence.int[3]
            ci.upper.lim.u[i] <- res.reflim$confidence.int[4]
            sum.www[i] <- sum(www)
            
            distribution.type[i] <- ifelse(names(res.reflim)[1] == "mean", "normal", "lognormal")
            
            covariate.left[i] <- min(covcomp)  # 因为不需要区间，所以用最小值
            covariate.right[i] <- max(covcomp)  # 同样，用最大值
            covariate.mean[i] <- mean(covcomp)
            covariate.median[i] <- median(covcomp)
            covariate.n[i] <- length(covcomp)  # 统计所有协变量的数量
        }
    } else {
        if (!is.null(window.size) & !is.null(step.width)) {
            print("!is.null(window.size) & !is.null(step.width)")
            window.left <- covcomp[1]
            window.right <- window.left + window.size
            for (i in 1:n.steps) {
                # print(window.left)
                # print(window.right)
                # print(covcomp)
                is.in.interval <- covcomp >= window.left & covcomp <= window.right
                if (sum(is.in.interval) >= n.min) {
                    
                    interval_cov <- covcomp[is.in.interval]
                    
                    xxx <- xx[is.in.interval]
                    # www <- ww[is.in.interval]
                    # www <- dnorm(interval_cov, mean = median(interval_cov), sd = standard_deviation)
                    
                    # gaussian <- makeGaussian(standard_deviation)
                    # www <- gaussian(interval_cov, mean = median(interval_cov))
                    
                    
                    if (verteilung == "truncated_gaussian") {
                        w_function <- makeWeightFunction(verteilung, sigma = standard_deviation)
                        www <- w_function(interval_cov, mean = median(interval_cov))
                    } else if (verteilung == "triangular") {
                        a <- if (is.null(a)) 0 else a
                        b <- if (is.null(b)) 0.5 else b
                        c <- if (is.null(c)) 1 else c
                        
                        a.value <- quantile(interval_cov, a)
                        b.value <- quantile(interval_cov, b)
                        c.value <- quantile(interval_cov, c)
                        
                        # print(paste("a.value =", a.value))
                        # print(paste("b.value =", b.value))
                        # print(paste("c.value =", c.value))
                        # print("---")
                        
                        # a.value <- min(interval_cov)
                        # b.value <- median(interval_cov)
                        # c.value <- max(interval_cov)
                        w_function <- makeWeightFunction(verteilung, a = a.value, b = b.value, c = c.value)  # 这里需要分布函数参数
                        www <- w_function(interval_cov)
                    } else if (verteilung == "trapezoidal") {
                        a <- if (is.null(a)) 0 else a
                        b <- if (is.null(b)) 0.25 else b
                        c <- if (is.null(c)) 0.75 else c
                        d <- if (is.null(d)) 1 else d
                        
                        a.value <- quantile(interval_cov, a)
                        b.value <- quantile(interval_cov, b)
                        c.value <- quantile(interval_cov, c)
                        d.value <- quantile(interval_cov, d)
                        
                        # a.value <- min(interval_cov)
                        # b.value <- quantile(interval_cov, 0.25) 
                        # c.value <- quantile(interval_cov, 0.75)
                        # d.value <- max(interval_cov)     
                        
                        w_function <- makeWeightFunction(distribution = verteilung, a = a.value, b = b.value, c = c.value, d = d.value)
                        www <- w_function(interval_cov)
                    }
                    
                    plot(interval_cov, www, type = "l", col = "blue", lwd = 2, main = "www VS interval_cov")
                    points(interval_cov, www, col = "red")
                    www_sum <- sum(www)
                    text(x = mean(interval_cov), y = mean(www), 
                         labels = paste("Sum of www =", round(www_sum, 2)),
                         col = "darkgreen", cex = 1.5, font = 2)
                    sum.www[i] <- www_sum
                    
                    
                    
                    if (sum.www[i] > 100) {
                        res.reflim <- w_reflim(xxx, www, n.min = n.min, apply.rounding = apply.rounding, lognormal = lognormal, plot.all = TRUE)
                        
                        
                        lower.lim[i] <- res.reflim$limits[1]
                        upper.lim[i] <- res.reflim$limits[2]
                        ci.lower.lim.l[i] <- res.reflim$confidence.int[1]
                        ci.lower.lim.u[i] <- res.reflim$confidence.int[2]
                        ci.upper.lim.l[i] <- res.reflim$confidence.int[3]
                        ci.upper.lim.u[i] <- res.reflim$confidence.int[4]
                        
                        
                        if (names(res.reflim$stats)[1] == "mean") {
                            distribution.type[i] <- "normal"
                        } else {
                            distribution.type[i] <- "lognormal"
                        }
                        
                        covals <- covcomp[is.in.interval]
                        covariate.left[i] <- window.left
                        covariate.right[i] <- window.right
                        covariate.mean[i] <- mean(covals)
                        covariate.median[i] <- median(covals)
                        covariate.n[i] <- sum(is.in.interval)
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
            while(indr <= length(cov.unique)) {
                is.in.interval <- covcomp >= cov.unique[indl] & covcomp < cov.unique[indr]
                
                if (sum(is.in.interval) >= n.min.window) {
                    
                    # print(covcomp[is.in.interval])
                    interval_cov <- covcomp[is.in.interval]
                    
                    xxx <- xx[is.in.interval]
                    # www <- ww[is.in.interval]
                    # www <- dnorm(interval_cov, mean = median(interval_cov), sd = standard_deviation)
                    
                    # gaussian <- makeGaussian(standard_deviation)
                    # www <- gaussian(interval_cov, mean = median(interval_cov))
                    
                    
                    if (verteilung == "truncated_gaussian") {
                        w_function <- makeWeightFunction(verteilung, sigma = standard_deviation)
                        www <- w_function(interval_cov, mean = median(interval_cov))
                    } else if (verteilung == "triangular") {
                        a <- if (is.null(a)) 0 else a
                        b <- if (is.null(b)) 0.5 else b
                        c <- if (is.null(c)) 1 else c
                        
                        a.value <- quantile(interval_cov, a)
                        b.value <- quantile(interval_cov, b)
                        c.value <- quantile(interval_cov, c)
                        
                        # print(paste("a.value =", a.value))
                        # print(paste("b.value =", b.value))
                        # print(paste("c.value =", c.value))
                        # print("---")
                        
                        # a.value <- min(interval_cov)
                        # b.value <- median(interval_cov)
                        # c.value <- max(interval_cov)
                        w_function <- makeWeightFunction(verteilung, a = a.value, b = b.value, c = c.value)
                        www <- w_function(interval_cov)
                    } else if (verteilung == "trapezoidal") {
                        a <- if (is.null(a)) 0 else a
                        b <- if (is.null(b)) 0.25 else b
                        c <- if (is.null(c)) 0.75 else c
                        d <- if (is.null(d)) 1 else d
                        
                        a.value <- quantile(interval_cov, a)
                        b.value <- quantile(interval_cov, b)
                        c.value <- quantile(interval_cov, c)
                        d.value <- quantile(interval_cov, d)
                        
                        # a.value <- min(interval_cov)
                        # b.value <- quantile(interval_cov, 0.25)
                        # c.value <- quantile(interval_cov, 0.75)
                        # d.value <- max(interval_cov)
                        
                        w_function <- makeWeightFunction(distribution = verteilung, a = a.value, b = b.value, c = c.value, d = d.value)  # todo
                        www <- w_function(interval_cov)
                    }
                    
                    # www <- w_function(interval_cov, mean = median(interval_cov))
                    
                    
                    plot(interval_cov, www, type = "l", col = "blue", lwd = 2, main = "www VS interval_cov")
                    points(interval_cov, www, col = "red")
                    www_sum <- sum(www)
                    text(x = mean(interval_cov), y = mean(www), 
                         labels = paste("Sum of www =", round(www_sum, 2)),
                         col = "darkgreen", cex = 1.5, font = 2)
                    sum.www[ind] <- www_sum
                    
                    # while(sum.www[ind] < 7) {
                    #     indr <- indr + 1
                    # }
                    
                    if (sum.www[ind] > 100) {
                        res.reflim <- w_reflim(xxx, www, n.min = n.min, apply.rounding = apply.rounding, lognormal = lognormal, plot.all = TRUE)
                        # print(res.reflim)
                        
                        lower.lim[ind] <- res.reflim$limits[1]
                        upper.lim[ind] <- res.reflim$limits[2]
                        ci.lower.lim.l[ind] <- res.reflim$confidence.int[1]
                        ci.lower.lim.u[ind] <- res.reflim$confidence.int[2]
                        ci.upper.lim.l[ind] <- res.reflim$confidence.int[3]
                        ci.upper.lim.u[ind] <- res.reflim$confidence.int[4]
                        
                        if (names(res.reflim$stats)[1] == "mean") {
                            distribution.type[ind] <- "normal"
                        } else {
                            distribution.type[ind] <- "lognormal"
                        }
                        
                        covals <- covcomp[is.in.interval]
                        covariate.left[ind] <- min(covals)
                        covariate.right[ind] <- max(covals)
                        covariate.mean[ind] <- mean(covals)
                        covariate.median[ind] <- median(covals)
                        covariate.n[ind] <- sum(is.in.interval)
                        
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
    }
    
    # print(length(lower.lim))
    # print(length(upper.lim))
    # print(length(ci.lower.lim.l))
    # print(length(ci.lower.lim.u))
    # print(length(ci.upper.lim.l))
    # print(length(ci.upper.lim.u))
    # print(length(distribution.type))
    # print(length(covariate.left))
    # print(length(covariate.right))
    # print(length(covariate.mean))
    # print(length(covariate.median))
    # print(length(covariate.n))
    # print(length(sum.www))
    
    res <- data.frame(lower.lim,upper.lim,ci.lower.lim.l,ci.lower.lim.u,ci.upper.lim.l,ci.upper.lim.u,distribution.type,covariate.left,covariate.right,covariate.mean,covariate.median,covariate.n,sum.www)
    # 去除数据框中含有 NA 的行
    res <- res[!is.na(covariate.n),]
    return(res)
}


#' w_reflim
#' 
#' @description 
#' 使用qq图方法，计算上下限

# targets提供目标上下限
w_reflim <- function (x, x_weight, lognormal = NULL, targets = NULL, perc.trunc = 2.5,
             n.min = 200, apply.rounding = TRUE, plot.it = TRUE, plot.all = TRUE, 
             print.n = TRUE, main = "reference limits", xlab = "x") 
{
    # reflimR für conf_int95
    if (require("reflimR")) {
        library(reflimR)
    } else {
        install.packages("reflimR")
        library(reflimR)
    }
    
    na_indices <- is.na(x)
    x_clean <- x[!na_indices]
    ww_clean <- x_weight[!na_indices]
    
    
    result <- list(stats = c(mean = NA, sd = NA, n.total = NA, 
                             n.trunc = NA), lognormal = lognormal, limits = c(lower.lim = NA, 
                                                                              upper.lim = NA, lower.lim.low = NA, lower.lim.upp = NA, 
                                                                              upper.lim.low = NA, upper.lim.upp = NA), targets = c(lower.lim = NA, 
                                                                                                                                   upper.lim = NA, lower.lim.low = NA, lower.lim.up = NA, 
                                                                                                                                   upper.lim.low = NA, upper.lim.upp = NA), perc.norm = NA, 
                   confidence.int = c(lower.lim.low = NA, lower.lim.upp = NA, 
                                      upper.lim.low = NA, upper.lim.upp = NA, n = NA), 
                   interpretation = c(lower.limit = NA, upper.limit = NA), 
                   remarks = NA)
    
    if (!is.numeric(x_clean)) {
        warning("x must be numeric. Non-numeric values removed.")
        x_clean <- as.numeric(x_clean)
        
        na_indices <- is.na(x_clean)
        x_clean <- x_clean[!na_indices]
        ww_clean <- ww_clean[!na_indices]
    }
    if (min(x_clean) <= 0) {
        warning("Only positive values allowed. values <= 0 removed.")
        x_clean <- x_clean[x_clean > 0]
        ww_clean <- ww_clean[x_clean > 0]
        result$remarks <- "Values <= 0 removed"
    }
    
    if (!is.null(targets)) {
        targets <- na.omit(as.numeric(targets))
        if (length(targets) != 2) {
            warning("targets must be a vector with length 2. NA not allowed. Targets removed.")
            targets = NULL
            result$remarks <- "Unsuitable target values removed"
        }
    }
    if (!is.null(targets)) {
        if (is.na(targets[1]) | is.na(targets[2])) {
            warning("Targets must be numeric. NA not allowed. Targets removed.")
            targets = NULL
            result$remarks <- "Unsuitable target values removed"
        }
    }
    if (!is.null(targets)) {
        if (targets[1] >= targets[2]) {
            warning("The upper target limit must be greater than the lower target limit. Targets removed. ")
            targets = NULL
            result$remarks <- "Unsuitable target values removed"
        }
    }
    if (!is.null(targets)) {
        if (targets[1] <= 0 | targets[2] <= 0) {
            warning("Only positive target values allowed. Targets removed.")
            targets = NULL
            result$remarks <- "Unsuitable target values removed"
        }
    }
    n <- length(x_clean)
    if (n < 40) {
        warning(paste("n = ", n, ". The absolute minimum for reference limit estimation is 40. NAs returned."))
        result$stats[3] <- n
        result$remarks <- "Total n < 40"
        return(result)
    }
    if (n < n.min) {
        warning(paste("n = ", n, "where a minimum of ", n.min, 
                      "is required. n.min has been set to 40 at a potential loss of accuracy."))
        result$stats[3] <- n
        result$remarks <- "Attention: low.n"
        n.min <- 40
    }
    digits <- adjust_digits(median(x_clean))$digits
    if (is.null(lognormal)) {
        plot.logtype <- TRUE
        lognormal <- w_lognorm(x_clean, ww_clean)$lognormal
    } else {
        plot.logtype <- FALSE
    }
    # Error zeigen
    # print(paste("is x == x_clean ?", all(x, x_clean)))
    
    res.lognorm <- w_lognorm(x_clean, ww_clean, plot.it = FALSE)
    res.trunc <- w_iboxplot(x_clean, ww_clean, lognormal = lognormal, perc.trunc = perc.trunc,
                            apply.rounding = apply.rounding, plot.it = FALSE)
    # Error zeigen
    # print(res.trunc$progress)
    
    n.trunc <- length(res.trunc$trunc)
    if (n.trunc < 40) {
        warning(paste("n = ", n.trunc, "after truncation. The absolute minimum for reference limit estimation is 40. NAs returned."))
        result$stats[3] <- n
        result$stats[4] <- n.trunc
        result$remarks <- "n < 40 after truncation."
        return(result)
    }
    if (n.trunc < n.min) {
        warning(paste("n.trunc =", n.trunc, "where a minimum of", 
                      n.min, "is required. n.min has been set to 40 at a potential loss of accuracy."))
        result$stats[3] <- n
        result$stats[4] <- n.trunc
        result$remarks <- "Low n after truncation."
        n.min <- 40
    }
    
    res.qq <- w_truncated_qqplot(res.trunc$trunc, res.trunc$w_trunc, lognormal = lognormal,
                                 perc.trunc = perc.trunc, n.min = n.min, apply.rounding = apply.rounding,
                                 plot.it = FALSE)$result
    res.ci <- conf_int95(n = n, lower.limit = as.numeric(res.qq[3]),
                         upper.limit = as.numeric(res.qq[4]), lognormal = lognormal,
                         apply.rounding = apply.rounding)
    if (res.qq[3] > 0) {
        res.pu <- permissible_uncertainty(lower.limit = as.numeric(res.qq[3]),
                                          upper.limit = as.numeric(res.qq[4]), apply.rounding = apply.rounding)
    } else {
        warning("Estimated lower limit <- 0. No tolerance limits calculated. No graphics produced.")
        res.pu <- rep(NA, 4)
        targets = NULL
        result$remarks <- "Lower limit <= 0"
    }
    res.lim <- c(as.numeric(res.qq[3:4]), as.numeric(res.pu))
    names(res.lim) <- c("lower.lim", "upper.lim", "lower.lim.low",
                        "lower.lim.upp", "upper.lim.low", "upper.lim.upp")
    if (apply.rounding) {
        res.lim <- round(res.lim, digits)
    }
    
    res.tar <- c(lower.lim = NA, upper.lim = NA, lower.lim.low = NA, 
                 lower.lim.up = NA, upper.lim.low = NA, upper.lim.upp = NA)
    dev.lim <- c(lower.limit = NA, upper.limit = NA)
    
    if (!is.null(targets)) {
        ip <- interpretation(res.lim[1:2], targets) # 检查给定的限制值和目标值是否在容许的不确定性范围内
        res.tar[1:2] <- targets
        res.tar[3:6] <- ip$tol.tar
        if (apply.rounding) {
            res.tar <- round(res.tar, digits)
        }
        dev.lim <- ip$dev.lim
    }
    
    if (res.qq[3] > 0) {
        if (plot.all) {
            plot.it <- TRUE
        }
        if (plot.all) {
            oldpar <- par(mfrow = c(2, 2))
            on.exit(par(oldpar))
        }
        if (plot.it) {
            rh <- ri_hist(x_clean, lognormal = lognormal, stats = res.qq[1:2],
                          limits = res.qq[3:4], targets = targets, perc.norm = res.trunc$perc.norm,
                          main = main, xlab = xlab)
            if (print.n) {
                legend("topright", legend = paste("n = ", n.trunc,
                                                  "after truncation"), bty = "n", cex = 0.75)
            }
        }
        if (plot.all) {
            w_lognorm(x_clean, ww_clean, main = "Step 1: w_Bowley skewness", xlab = "",
                      plot.logtype = plot.logtype)
            w_iboxplot(x_clean, ww_clean, lognormal = lognormal, perc.trunc = perc.trunc,
                       apply.rounding = apply.rounding, main = "Step 2: w_iBoxplot",
                       xlab = "")
            w_truncated_qqplot(res.trunc$trunc, res.trunc$w_trunc, lognormal = lognormal,
                               perc.trunc = perc.trunc, n.min = n.min, apply.rounding = apply.rounding,
                               main = "Step 3: w_Q-Q plot", xlab = "", ylab = "")
        }
    }
    
    result$stats = c(res.qq[1:2], n.total = n, n.trunc = n.trunc)
    result$lognormal = lognormal
    result$limits = res.lim
    result$targets = res.tar
    result$perc.norm = res.trunc$perc.norm
    result$confidence.int = res.ci[1:4]
    result$interpretation = dev.lim
    return(result)
} 








#' w_bowley
#' 
#' @description 
#' 计算偏度度量，判断对称性
#' 
#' @export

w_bowley <- function(x, x_weight) {
  # Hmisc für wtd.quantile
  if (require("Hmisc")) {
    library(Hmisc)
  } else {
    install.packages("Hmisc")
    library(Hmisc)
  }
  
  w_quantiles <- wtd.quantile(x, x_weight, probs = c(0.25, 0.5, 0.75))
  return((w_quantiles[3] + w_quantiles[1] - 2* w_quantiles[2])/(w_quantiles[3]-w_quantiles[2]))
}


w_IQR <- function(x, x_weight) {
  w_quantiles <- wtd.quantile(x, x_weight, probs = c(0.25, 0.5, 0.75))
  iqr <- w_quantiles[3] - w_quantiles[1]
  return(iqr)
}

#' w_lognorm
#' 
#' @description 
#' 计算是否应该使用lognorm或者norm
#' 
#' @export

w_lognorm <- function(x, x_weight, cutoff = 0.05, digits = 3, plot.it = FALSE, xlab = "x",
                      plot.logtype = TRUE, main = "W_Bowley skewness") {
  if (!is.numeric(x)) {
    stop("x must be numeric.")
  }
  if (length(x) < 2) {
    stop("x must be a vector of at least 2 numeric values.")
  }
  if (min(x) <= 0) {
    stop("Negative values not allowed.")
  }
  bs <- rep(NA, 2)
  bs[1] <- w_bowley(x, x_weight)
  bs[2] <- w_bowley(log(x), x_weight)
  
  # delta kann bestimmen, ob die logarithmische Transformation die Schiefe der Daten erheblich verringert
  if (bs[1] < 0) {
    lognormal <- FALSE
  } else {
    lognormal <- (bs[1] - bs[2]) >= cutoff
  }
  
  if (!is.na(digits)) {
    bs <- round(bs, digits)
  }
  if (plot.it) {
    xx <- x[x < median(x) + 6 * w_IQR(x, x_weight = x_weight)]
    df.x <- data.frame(lin = xx, log = log(xx))
    df.quant <- cbind(lin = wtd.quantile(df.x[, 1], x_weight, c(0.1, 0.5)),
                      log = wtd.quantile(df.x[, 2], x_weight, c(0.1, 0.5)))
    # print(df.quant)
    
    # Berechnen die Steigung und den Achsenabschnitt der linearen Transformation,
    # damit die logarithmisch transformierten Daten mit den ursprünglichen Daten in 
    # Übereinstimmung gebracht werden können.
    
    # 斜率 
    slope <- (df.quant[2, 1] - df.quant[1, 1])/(df.quant[2, 
                                                         2] - df.quant[1, 2])
    # 截距
    intercept <- df.quant[1, 1] - slope * df.quant[1, 2]
    df.x <- cbind(df.x, transformed = intercept + slope * df.x[, 2])
    # print(df.x)
    
    # 绘制概率密度函数 Wahrscheinlichkeitsdichte
    d1 <- density(df.x[, 1])
    d2 <- density(df.x[, 3])
    ymax <- max(max(d1$y), max(d2$y)) * 1.4
    plot(d1$x, d1$y, main = main, xlab = xlab, ylab = "", 
         type = "l", lwd = 2, yaxt = "n", xlim = c(0.9 * min(df.x[, 
                                                                  1]), max(max(d1$x), max(d2$x))), ylim = c(0, 
                                                                                                            ymax))
    lines(d2$x, d2$y, col = "blue", lwd = 2)
    base.scale <- c(1, 1.5, 2, 3, 5, 7)
    num.scale <- c(base.scale * 10^-1, base.scale * 10^0, 
                   base.scale * 10^1, base.scale * 10^2, base.scale * 
                     10^3, base.scale * 10^4)
    log.scale <- intercept + slope * log(num.scale)
    axis(3, at = log.scale, labels = as.character(num.scale), 
         col.axis = "blue", mgp = c(3, 0.1, 0), tcl = -0.1)
    boxplot(df.x[, 1], horizontal = TRUE, at = 0.75 * ymax, 
            boxwex = ymax/20, col = "lightgrey", pch = 20, add = TRUE)
    boxplot(df.x[, 3], horizontal = TRUE, at = 0.85 * ymax, 
            boxwex = ymax/20, col = "blue", pch = 20, add = TRUE)
    text(median(df.x[, 1]), 0, paste("delta =", bs[1] - bs[2]), 
         pos = 3)
    if (plot.logtype) {
      tx <- ifelse(lognormal, "lognormal distribution", 
                   "normal distribution")
      text(0.9 * min(df.x[, 1]), 0.95 * ymax, tx, pos = 4)
    }
  }
  
  return(list(lognormal = lognormal, BowleySkewness = c(normal = bs[1], 
                                                   lognormal = bs[2],
                                                   delta = bs[1] - bs[2])))
  
}








#' w_iboxplot
#' 
#' @description 
#' 箱线图法进行迭代截断，以获取中心95%的可能不显著结果
#' 
#' @export

w_iboxplot <- function(x, x_weight, lognormal = NULL, perc.trunc = 2.5,
                       apply.rounding = TRUE, plot.it = TRUE, main = "w_iBoxplot", xlab = "x") {
  # reflimR für adjust_digits
  if (require("reflimR")) {
    library(reflimR)
  } else {
    install.packages("reflimR")
    library(reflimR)
  }
  # Hmisc für wtd.quantile
  if (require("Hmisc")) {
    library(Hmisc)
  } else {
    install.packages("Hmisc")
    library(Hmisc)
  }
  
  na_indices <- is.na(x)
  x_clean <- x[!na_indices]
  ww_clean <- x_weight[!na_indices]
  
  # x_clean <- na.omit(x)
  # ww_clean <- na.omit(x_weight)
  
  if (!is.numeric(x_clean)) {
    stop("x must be numeric.")
  }
  if (min(x_clean) <= 0) {
    stop("x must be a vector of positive numbers.")
  }
  digits <- adjust_digits(median(x_clean))$digits
  # digits <- adjust_digits(wtd.quantile(x_clean, weights = ww_clean, probs = 0.5))$digits
  n <- length(x_clean)
  if (n < 40) {
    stop(paste0("n = ", n, ". The absolute minimum for reference limit estimation is 40."))
  }
  progress <- data.frame(n = n, min = min(x_clean), max = max(x_clean), w = length(x_weight))
  
  if (is.null(lognormal)) {
    lognormal <- w_lognorm(x_clean, ww_clean, plot.it = FALSE)$lognormal
  }
  if (lognormal) {
    x_clean <- log(x_clean)
  }
  q.trunc <- perc.trunc/100
  w_truncate_x <- function(x, x_weight, i) {
    qf <- ifelse(i == 1, qf <- qnorm(q.trunc) / qnorm(0.25),
                 qf <- qnorm(q.trunc) / qnorm(0.25 * (1 - q.trunc / 50)
                                              + q.trunc))
    # ?
    Q <- wtd.quantile(x, x_weight, c(0.25, 0.5, 0.75))
    var1 <- Q[2] - Q[1]
    var2 <- Q[3] - Q[2]
    var <- min(var1, var2)
    lim <- c(Q[2] - qf * var, Q[2] + qf * var)
    indices <- which(x >= lim[1] & x <= lim[2])
    # return(subset(x, x >= lim[1] & x <= lim[2]))
    return(list(x = x[indices], x_weight = x_weight[indices]))
  }
  print.progress <- function(x, x_weight, lognormal = FALSE) {
    if (lognormal) {
      x <- exp(x)
    }
    return(c(length(x), min(x), max(x), length(x_weight)))
  }

  n0 <- 1
  n1 <- 0
  i <- 0
  while (n0 > n1) {
    i <- i + 1
    n0 <- length(x_clean)
    result <- w_truncate_x(x_clean, ww_clean, i)
    x_clean <- result$x
    ww_clean <- result$x_weight
    n1 <- length(x_clean)
    progress <- rbind(progress, print.progress(x_clean, ww_clean, lognormal = lognormal))
  }
  if (lognormal) {
    x_clean <- exp(x_clean)
  }
  lim <- c(lower = min(x_clean), upper = max(x_clean))
  if (apply.rounding) {
    lim <- round(lim, digits)
  }
  prop <- round(length(x_clean) * 100/0.95/n, 1)
  if (prop > 100) {
    prop <- 100
  }

  if (plot.it) {

    # 选择需要绘制的数据范围
    # x1 <- x[x < median(x) + 8 * w_IQR(x, x_weight)]
    x1 <- x[x < median(x) + 8 * IQR(x)]
    # 计算加权密度估计
    d0 <- density(x1)
    # 确定绘图数据的范围
    d1 <- data.frame(d0$x, d0$y)
    d <- subset(d1, d1[, 1] >= lim[1] & d1[, 1] <= lim[2])
    # 根据数据量选择合适的直方图断点
    if (n < 200) {
      breaks <- "Sturges"
    } else {
      delta <- max(x1) - min(x1)
      breaks <- seq(from = min(x1) - 0.1 * delta, to = max(x1) + 0.1 * delta,
                    by = (lim[2] - lim[1]) / 10)
    }
    # 绘制加权直方图
    hist(x1, freq = FALSE, breaks = breaks, yaxt = "n", ylim = c(0, max(d[, 2]) * 1.5),
         main = main, xlab = xlab, ylab = "", col = "white", border = "grey")
    # 绘制加权密度曲线
    lines(d[, 1], d[, 2], col = "blue", lwd = 2)  # 曲线
    lines(rep(lim[1], 2), c(0, d[1, 2]), col = "blue", lwd = 2) # 左边界
    lines(rep(lim[2], 2), c(0, d[nrow(d), 2]), col = "blue", lwd = 2)  # 右边界
    # 添加文本标签
    text(lim[1], 0, round(lim[1], digits), pos = 3)
    text(lim[2], 0, round(lim[2], digits), pos = 3)
    # 绘制加权箱线图
    boxplot(x1, at = max(d[, 2]) * 1.4, boxwex = max(d[, 2]) / 10, col = "white", pch = 20,
            horizontal = TRUE, add = TRUE)
    boxplot(x_clean, at = max(d[, 2]) * 1.25, boxwex = max(d[, 2]) / 10, col = "blue", pch = 20,
            horizontal = TRUE, add = TRUE)
  }

  progress[, 2:3] <- round(progress[, 2:3], digits)
  row.names(progress) <- paste0("cycle", 0:i)
  return(list(trunc = x_clean, w_trunc = ww_clean, truncation.points = lim, lognormal = lognormal,
              perc.norm = prop, progress = progress))
}



#' w_truncated_qqplot
#' 
#' @description 
#' qq图用于验证正态性，以及比较任意两种分布
#' 
#' @export

w_truncated_qqplot <- function(x, x_weight, lognormal = NULL, perc.trunc = 2.5, n.min = 200,
                               apply.rounding = TRUE, plot.it = TRUE, main = "w_Q-Q plot",
                               xlab = "theoretical quantiles", ylab = "sample quantiles") 
{
  # Hmisc für wtd.quantile
  if (require("Hmisc")) {
    library(Hmisc)
  } else {
    install.packages("Hmisc")
    library(Hmisc)
  }

    # reflimR für adjust_digits
    if (require("reflimR")) {
        library(reflimR)
    } else {
        install.packages("reflimR")
        library(reflimR)
    }
  
  na_indices <- is.na(x)
  x_clean <- x[!na_indices]
  ww_clean <- x_weight[!na_indices]
  
  if (!is.numeric(x_clean)) {
    stop("x must be numeric.")
  }
  if (min(x_clean) < 0) {
    stop("Only positive values allowed.")
  }
  n <- length(x_clean)
  if (n < 40) {
    stop(paste0("n = ", n, ". The absolute minimum for reference limit estimation is 40."))
  }
  if (n < n.min) {
    warning(paste("(w_truncated_qqplot) n =", n, "where a minimum of",
                  n.min, "is required. You may try to reduce n.min at the loss of accuracy."))
    return(list(result = NULL, lognormal = NULL))
  }
  
  # 设定要计算的分位点数为100或n个
  n.quantiles <- 100
  if (n < n.quantiles) {
    n.quantiles <- n
  }
  digits <- adjust_digits(median(x_clean))$digits
  if (is.null(lognormal)) {
    lognormal <- w_lognorm(x_clean, ww_clean, plot.it = FALSE)$lognormal
  }
  if (lognormal) {
    x_clean <- log(x_clean)
  }
  p1 <- seq(from = perc.trunc/100, to = 1 - perc.trunc/100,
            length.out = n.quantiles)
  p2 <- seq(from = 0, to = 1, length.out = n.quantiles)
  x.ax <- qnorm(p1) # 理论分位数 theoretical quantiles
  y.ax <- wtd.quantile(x_clean, ww_clean, p2) # 样本分位数 sample quantiles
  central.part <- floor(0.05 * n.quantiles) : ceiling(0.95 * n.quantiles)
  reg <- lm(y.ax[central.part] ~ x.ax[central.part])
  a <- reg$coefficients[2]  # 线性回归计算斜率 Regression Steigung slope
  b <- reg$coefficients[1]  # 截距 Achsenabschnitt intercept
  
  result <- c(b, a, b - 1.96 * a, b + 1.96 * a)
  result <- setNames(result, c("mean", "sd", "lower.lim", "upper.lim"))
  
  if (lognormal) {
    names(result)[1:2] <- paste0(names(result)[1:2], "log") # meanlog  sdlog
    result[1:2] <- round(result[1:2], 3)
    result[3:4] <- exp(result[3:4]) # e的次方
  }
  if (result[3] < 0) {
      result[3] <- 0
  }
  if (apply.rounding) {
      result[3:4] <- round(result[3:4], digits)
  }
  
  if (plot.it) {
      if (!lognormal) {
          ll <- result[3]
          ul <- result[4]
          diff <- ul - ll
          plot(y.ax ~ x.ax, pch = 20, col = "blue", xlim = c(-3, 
                                                             3), ylim = c(ll - 0.2 * diff, ul + 0.2 * diff), 
               main = main, xlab = xlab, ylab = ylab)
      } else {
          ll <- log(result[3])
          ul <- log(result[4])
          diff <- ul - ll
          plot(y.ax ~ x.ax, yaxt = "n", xlim = c(-3, 3), ylim = c(ll - 
                                                                      0.2 * diff, ul + 0.2 * diff), main = main, xlab = xlab, 
               ylab = ylab)
          y.pos <- c(50, 100, 150, 200, 300, 400, 500, 1000, 
                     1500)/(10^digits)
          axis(2, at = log(y.pos), labels = y.pos)
      }
      abline(v = 0)
      abline(v = c(-1.96, 1.96), lty = 2)
      abline(h = c(ll, ul), col = "green", lwd = 2)
      abline(reg$coefficients, lwd = 2, col = "blue")
      points(c(-1.96, 1.96), c(ll, ul), pch = 19, col = "green")
      text(-2, ll, formatC(result[3], digits, format = "f"), 
           pos = 1)
      text(2, ul, formatC(result[4], digits, format = "f"), 
           pos = 3)
      
  }
  
  return(list(result = result, lognormal = lognormal))
}