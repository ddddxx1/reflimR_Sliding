# MLE-4

# source("stats-3200273-supplementary.R")

MLE <- function(data, weights = NULL, verbose = FALSE) {
    # is.nona <- !is.na(covariate)
    # xx <- x[is.nona]
    # covcomp <- covariate[is.nona]
    # 
    # ord.cov <- order(covcomp)
    # xx <- xx[ord.cov]
    # covcomp <- covcomp[ord.cov]
    # 
    # lod = min(xx, na.rm = TRUE)
    # n.lod  <- sum(!is.na(covcomp) & is.na(xx))
    # print(paste("lod =", lod, "n.lod =", n.lod))
    # 
    # xxx <- xx[!is.na(xx)]
    # 
    # print(reflimLOD.MLE(xxx, lod, n.lod))
    # browser()
    data <- data[!is.na(data)]

    lod_value <- min(data[data > 0])

    n_at_lod <- sum(data == lod_value)
    
    # cat("Estimated LOD value: ", lod_value, "\n")
    # cat("Number of data points replaced by LOD: ", n_at_lod, "\n")
    
    # return(reflimLOD.MLE(data, lod_value, n_at_lod))
    
    return(w_reflimLOD.MLE(measured.values = data, weights = weights, lod = lod_value, n.lod = n_at_lod, verbose = verbose))
}

MLE_o <- function(data) {
    # is.nona <- !is.na(covariate)
    # xx <- x[is.nona]
    # covcomp <- covariate[is.nona]
    # 
    # ord.cov <- order(covcomp)
    # xx <- xx[ord.cov]
    # covcomp <- covcomp[ord.cov]
    # 
    # lod = min(xx, na.rm = TRUE)
    # n.lod  <- sum(!is.na(covcomp) & is.na(xx))
    # print(paste("lod =", lod, "n.lod =", n.lod))
    # 
    # xxx <- xx[!is.na(xx)]
    # 
    # print(reflimLOD.MLE(xxx, lod, n.lod))
    # browser()
    data <- data[!is.na(data)]
    
    lod_value <- min(data[data > 0])
    
    n_at_lod <- sum(data == lod_value)
    
    cat("Estimated LOD value: ", lod_value, "\n")
    cat("Number of data points replaced by LOD: ", n_at_lod, "\n")
    
    return(reflimLOD.MLE(data, lod_value, n_at_lod))
    
    # return(w_reflimLOD.MLE(data, weights = weights, lod_value, n_at_lod))
}


reflimLOD.MLE <- function(measured.values,lod,n.lod,lambda=0,right.quantile=0.75){
    transformed.measured.values <- box.cox.trans(measured.values,lambda=lambda)
    transformed.lod <- box.cox.trans(lod,lambda=lambda)
    
    normal.result <- modTrunc(transformed.measured.values,transformed.lod,n.lod,right.quantile=right.quantile)
    
    if (is.na(normal.result$upper.truncation)){
        return(list(lower.limit=NA,upper.limit=NA,mu.log=NA,sigma.log=NA,upper.truncation=NA,selected.values=NA,lod=lod,n.lod=n.lod))
    }
    
    
    
    obj.fun <- function(pars){
        pnorm.lod <- ptruncnorm(transformed.lod,b=normal.result$upper.truncation,mean=pars[1],sd=pars[2])
        
        print(paste("obj result", -n.lod*log(pnorm.lod) - length(normal.result$selected.values)*log(1-pnorm.lod) - sum(log(dtruncnorm(normal.result$selected.values,a=transformed.lod,b=normal.result$upper.truncation,mean=pars[1],sd=pars[2])))))
        return(-n.lod*log(pnorm.lod) - length(normal.result$selected.values)*log(1-pnorm.lod) - sum(log(dtruncnorm(normal.result$selected.values,a=transformed.lod,b=normal.result$upper.truncation,mean=pars[1],sd=pars[2]))))
        # <LOD      >LOD        each actual observation
    }
    
    sigma <- (normal.result$upper.truncation - transformed.lod)/(qnorm(0.975)-qnorm(0.975*n.lod/(length(normal.result$selected.values)+n.lod)))
    mu <- normal.result$upper.truncation - sigma*qnorm(0.975)
    
    pars.initial <- c(mu,sigma)
    optim.result <- optim(pars.initial,obj.fun)
    
    lims <- box.cox.inv.trans(qnorm(c(0.025,0.975),mean=optim.result$par[1],sd=optim.result$par[2]),lambda=lambda)
    if (is.nan(lims[1])){
        lims[1] <- 0
    }
    lims1perc <- box.cox.inv.trans(qnorm(c(0.01,0.99),mean=optim.result$par[1],sd=optim.result$par[2]),lambda=lambda)
    if (is.nan(lims1perc[1])){
        lims1perc[1] <- 0
    }
    return(list(lower.limit=lims[1],upper.limit=lims[2],percentile1=lims1perc[1],percentile99=lims1perc[2],mu.log=unname(optim.result$par[1]),sigma.log=unname(optim.result$par[2]),upper.truncation=unname(box.cox.inv.trans(normal.result$upper.truncation,lambda=lambda)),selected.values=box.cox.inv.trans(normal.result$selected.values,lambda=lambda),minus.log.likelihood=optim.result$value,lod=lod,n.lod=n.lod,lambda=lambda))
    
}



w_reflimLOD.MLE <- function(measured.values, lod, n.lod, weights = NULL, lambda = 0, right.quantile = 0.75, verbose = FALSE) {
    if (is.null(weights)) {
        weights <- rep(1, length(measured.values))
        print("weights are not provided, using equal weights")
    }
    # browser()
    
    transformed.measured.values <- box.cox.trans(measured.values, lambda = lambda)
    transformed.lod <- box.cox.trans(lod, lambda = lambda)
    
    normal.result <- modTrunc(transformed.measured.values, transformed.lod, n.lod, right.quantile = right.quantile)
    # print(normal.result)
    
    if (is.na(normal.result$upper.truncation)){
        return(list(lower.limit=NA, upper.limit=NA, mu.log=NA, sigma.log=NA, 
                    upper.truncation=NA, selected.values=NA, lod=lod, n.lod=n.lod))
    }
    
    
    # obj.fun <- function(pars){
    #     pnorm.lod <- ptruncnorm(transformed.lod, b=normal.result$upper.truncation,
    #                             mean=pars[1], sd=pars[2])
    #     # print(paste("pnorm.lod", pnorm.lod))
    # 
    #     lod_weights <- weights[measured.values <= lod]
    # 
    #     selected_weights <- weights[normal.result$selected.values.index]
    #     # print(paste("obj result", -sum(lod_weights) * log(pnorm.lod) -
    #     #                 sum(selected_weights) * log(1-pnorm.lod) -
    #     #                 sum(selected_weights * log(dtruncnorm(normal.result$selected.values,
    #     #                                                       a=transformed.lod, b=normal.result$upper.truncation,
    #     #                                                       mean=pars[1], sd=pars[2])))))
    #     return(
    #         -sum(lod_weights) * log(pnorm.lod) -
    #             sum(selected_weights) * log(1-pnorm.lod) -
    #             sum(selected_weights * log(dtruncnorm(normal.result$selected.values,
    #                                                   a=transformed.lod, b=normal.result$upper.truncation,
    #                                                   mean=pars[1], sd=pars[2])))
    #     )
    # }
    
    obj.fun <- function(pars){
        # sigma must >0
        if (pars[2] <= 0) return(1e10)  # Returns a very large penalty value to prevent the optimizer from continuing in this direction
        result <- tryCatch({
            pnorm.lod <- ptruncnorm(transformed.lod, b=normal.result$upper.truncation, mean=pars[1], sd=pars[2])
            # Prevent probability of 0 or 1
            if (is.nan(pnorm.lod) || pnorm.lod <= 1e-10 || pnorm.lod >= 1 - 1e-10) return(1e10)
            # print(lod)
            lod_weights <- weights[measured.values <= lod]
            selected_weights <- weights[normal.result$selected.values.index]
            # print(paste("lod_weights", sum(lod_weights), "selected_weights", sum(selected_weights)))
            d_trunc <- dtruncnorm(normal.result$selected.values, a=transformed.lod, b=normal.result$upper.truncation, mean=pars[1], sd=pars[2])
            if (any(is.nan(d_trunc)) || any(d_trunc <= 1e-10)) return(1e10)
            # val <- -n.lod * log(pnorm.lod) -
            #     length(normal.result$selected.values) * log1p(-pnorm.lod) -
            #     sum(log(d_trunc))
            
            val <-  (-sum(lod_weights) * log(pnorm.lod) -
                sum(selected_weights) * log(1-pnorm.lod) -
                sum(selected_weights * log(dtruncnorm(normal.result$selected.values,
                                                      a=transformed.lod, b=normal.result$upper.truncation,
                                                      mean=pars[1], sd=pars[2]))))
            if (is.nan(val) || is.infinite(val)) return(1e10)
            val
        }, error = function(e) 1e10)
        return(result)
    }
    
    sigma <- (normal.result$upper.truncation - transformed.lod)/(qnorm(0.975)-qnorm(0.975*n.lod/(length(normal.result$selected.values)+n.lod)))
    mu <- normal.result$upper.truncation - sigma*qnorm(0.975)
    
    pars.initial <- c(mu,sigma)
    # if (verbose) print(paste("initial mu: ", mu, "initial sigma: ", sigma))
    # print(paste("initial pars", pars.initial))
    # optim.result <- optim(pars.initial,obj.fun) # -10774595   9083789 
    # browser()
    # print("before optim")
    optim.result <- optim(
        pars.initial, obj.fun,
        method = "L-BFGS-B",
        lower = c(mu - 5 * sigma, 0.001),   # Sigma minimum 0.001
        upper = c(mu + 5 * sigma, 5 * sigma),
        control = list(maxit = 1000)
    )
    # optim.result <- optim(pars.initial,obj.fun)
    # print("optim.result")
    
    # print(qnorm(c(0.025,0.975), mean=optim.result$par[1], sd=optim.result$par[2]))
    lims <- box.cox.inv.trans(qnorm(c(0.025,0.975),mean=optim.result$par[1],sd=optim.result$par[2]),lambda=lambda)  # 0 Inf
    # if (verbose) print(paste("lowerlimt: ", lims[1], "upperlimit: ", lims[2]))
    # if (is.nan(lims[1])){
    #     lims[1] <- 0
    # }
    lims1perc <- box.cox.inv.trans(qnorm(c(0.01,0.99),mean=optim.result$par[1],sd=optim.result$par[2]),lambda=lambda)
    # if (is.nan(lims1perc[1])){
    #     lims1perc[1] <- 0
    # }
    # print(paste("lims", lims[1], lims[2]))
    return(list(lower.limit=lims[1],upper.limit=lims[2],percentile1=lims1perc[1],percentile99=lims1perc[2],mu.log=unname(optim.result$par[1]),
                sigma.log=unname(optim.result$par[2]),upper.truncation=unname(box.cox.inv.trans(normal.result$upper.truncation,lambda=lambda)),
                selected.values=box.cox.inv.trans(normal.result$selected.values,lambda=lambda),minus.log.likelihood=optim.result$value,lod=lod,
                n.lod=n.lod,lambda=lambda))
}

box.cox.trans <- function(x,lambda=1){
    if (lambda==0){
        return(log(x))
    }else{
        return((x^lambda - 1)/lambda)
    }
}

modTrunc <- function(measured.values, lod, n.lod, right.quantile=0.75){
    
    n.min.values <- 2*n.lod+1
    quantile.factor <- qnorm(0.975)/qnorm(right.quantile)

    idx <- c(rep(NA, n.lod), seq_along(measured.values))
    x <- c(rep(lod-1, n.lod), measured.values)
    
    if (length(x) < n.min.values){
        return(list(selected.values=NA, selected.values.index=NA, upper.truncation=NA, median=NA, lod=lod, n.lod=n.lod))
    }
    
    x.qmr <- quantile(x, probs=c(0.5, right.quantile))
    upper.truncation <- x.qmr[1] + (x.qmr[2]-x.qmr[1])*quantile.factor
    
    x.length.old <- length(x)
    keep <- x <= upper.truncation
    x <- x[keep]
    idx <- idx[keep]
    
    if (length(x) < n.min.values){
        return(list(selected.values=NA, selected.values.index=NA, upper.truncation=NA, median=NA, lod=lod, n.lod=n.lod))
    }
    
    if (length(x) == x.length.old){
        return(list(selected.values=x[-(1:n.lod)], selected.values.index=idx[-(1:n.lod)], upper.truncation=upper.truncation, median=x.qmr[1], lod=lod, n.lod=n.lod))
    }
    
    median.mod <- 0.5/0.975
    right.quantile.mod <- right.quantile/0.975
    
    while(length(x) < x.length.old){
        x.length.old <- length(x)
        x.qmr <- quantile(x, probs=c(median.mod, right.quantile.mod))
        upper.truncation <- x.qmr[1] + (x.qmr[2]-x.qmr[1])*quantile.factor
        
        keep <- x <= upper.truncation
        x <- x[keep]
        idx <- idx[keep]
        
        if (length(x) < n.min.values){
            return(list(selected.values=NA, selected.values.index=NA, upper.truncation=NA, median=NA, lod=lod, n.lod=n.lod))
        }
    }  
    
    return(list(selected.values=x[-(1:n.lod)], selected.values.index=idx[-(1:n.lod)], upper.truncation=upper.truncation, median=x.qmr[1], lod=lod, n.lod=n.lod))
}

box.cox.inv.trans <- function(x,lambda=1){
    if (lambda==0){
        return(exp(x))
    }else{
        return((x*lambda + 1)^(1/lambda))
    }
}

#' reflimR_Sliding
#' 
#' @description 
#' Window slider main function that performs sliding window analysis with different weight functions.
#' The function supports multiple distribution types and comparison between different parameter settings.
#' 
#' @param x [numeric] Input measurement values
#' @param t [integer] Time points or covariate values
#' @param distribution [character] Weight function type: "truncated_gaussian", "gaussian", "triangular", or "trapezoidal"
#' @param log.scale [logical] If TRUE, applies logarithmic scaling to the x-axis
#' @param standard_deviation [numeric] Standard deviation for Gaussian distributions (default: 5)
#' @param standard_deviation_compare [numeric] Standard deviation for comparison plot
#' @param vertex1,vertex2 [numeric] Shape parameters for triangular/trapezoidal distributions (0-1)
#' @param vertex1_com,vertex2_com [numeric] Shape parameters for comparison plot
#' @param window.size,step.width [numeric] Window size and step width for sliding window
#' @param window.size_com,step.width_com [numeric] Window parameters for comparison plot
#' @param lognormal [logical] If TRUE, assumes log-normal distribution
#' @param weight_threshold [numeric] Minimum weight sum threshold
#' @param verbose [logical] If TRUE, prints additional information during execution
#' 
#' @return A plot object showing reference limits with confidence intervals
#' 
#' @example 
#' # Basic usage with truncated Gaussian distribution
#' reflimR_Sliding(x, t, distribution = "truncated_gaussian")
#' 
#' # Compare two different standard deviations
#' reflimR_Sliding(x, t, distribution = "gaussian", 
#'                standard_deviation = 3, 
#'                standard_deviation_compare = 5)
#' 
#' # Using triangular distribution with comparison
#' reflimR_Sliding(x, t, distribution = "triangular",
#'                vertex1 = 0.3, 
#'                vertex1_com = 0.7)
#' 
#' @export

reflimR_Sliding <- function(x, t, distribution = "truncated_gaussian", log.scale = FALSE, standard_deviation = 5, 
                standard_deviation_compare = NULL, vertex1 = NULL, vertex2 = NULL, vertex1_com = NULL, vertex2_com = NULL, window.size=NULL,step.width=NULL, window.size_com = NULL, step.width_com = NULL,
                lognormal=NULL,perc.trunc=2.5,n.min.window=200,n.min=100,apply.rounding=FALSE, weight_threshold = NULL, verbose = TRUE, MLE = FALSE) {
    # if (distribution == "truncated_gaussian") {
    #     if (standard_deviation == 5) {
    #         res <- w_sliding.reflim(x, t,distribution = distribution, window.size = window.size, step.width = step.width, lognormal = lognormal)
    #         gg_alist(result.sliding.reflim = res)
    #     } else {
    #         res <- w_sliding.reflim(x, t, distribution = distribution, window.size = window.size, step.width = step.width, lognormal = lognormal)
    #         res1 <- w_sliding.reflim(x, t, distribution = distribution, standard_deviation = standard_deviation, window.size = window.size, step.width = step.width, lognormal = lognormal)
    #         gg_alist_compare(res, res1)
    #     }
    # } else if (distribution == "gaussian") {
    #     if (standard_deviation == 5) {
    #         res <- w_sliding.reflim(x, t, distribution = distribution, window.size = NULL, step.width = NULL)
    #         gg_alist(result.sliding.reflim = res)
    #     } else {
    #         res <- w_sliding.reflim(x, t, distribution = distribution, window.size = NULL, step.width = NULL, lognormal = lognormal)
    #         res1 <- w_sliding.reflim(x, t, distribution = distribution, standard_deviation = standard_deviation, window.size = NULL, step.width = NULL, lognormal = lognormal)
    #         gg_alist_compare(res, res1)
    #     }
    #   
    # } else {
    #     res <- w_sliding.reflim(x, t, distribution = distribution, a = a, b = b, c = c, d = d, window.size = window.size, step.width = step.width, lognormal = lognormal)
    #     gg_alist(result.sliding.reflim = res)
    
    # x <- as.numeric(x)
    # t <- as.integer(t)
    par(mar = c(3, 3, 3, 3))
    if (is.null(standard_deviation_compare) && is.null(vertex1_com) && is.null(vertex2_com) && (is.null(window.size_com) || is.null(step.width_com))) {  # no comparison
        # print("no comparison")
        res <- w_sliding.reflim(x, t, distribution = distribution, standard_deviation = standard_deviation, 
                                vertex1 = vertex1, vertex2 = vertex2, 
                                window.size = window.size, step.width = step.width, lognormal = lognormal, 
                                weight_threshold = weight_threshold, verbose = verbose, MLE = MLE)
        if(verbose) print(res[, 1:2])
        gg_alist(result.sliding.reflim = res, log.scale = log.scale)
    } else {
        res1 <- w_sliding.reflim(x, t, distribution = distribution, standard_deviation = standard_deviation, 
                                 vertex1 = vertex1, vertex2 = vertex2, 
                                 window.size = window.size, step.width = step.width, lognormal = lognormal, 
                                 weight_threshold = weight_threshold, verbose = verbose, MLE = MLE)
        res2 <- w_sliding.reflim(x, t, distribution = distribution, standard_deviation = standard_deviation_compare, 
                                 vertex1 = vertex1_com, vertex2 = vertex2_com, 
                                 window.size = window.size_com, step.width = step.width_com, lognormal = lognormal, 
                                 weight_threshold = weight_threshold, verbose = verbose, MLE = MLE)
        gg_alist_compare(result.sliding.reflim1 = res1, result.sliding.reflim2 = res2, log.scale = log.scale)
    }
    
}

#' gg_alist
#' 
#' @description 
#' Plot Sliding Reference Limits with Confidence Intervals
#' 
#' @param result.sliding.reflim A data frame or list containing the sliding reference limits and their associated confidence intervals
#' @param log.scale Used to determine if logarithmic scaling of time is required
#' 
#' @import ggplot2
#' 
#' @example 
#' gg_alist(result.sliding.reflim = res, log.scale = TRUE)
#' 
#' @export

gg_alist <- function(result.sliding.reflim, log.scale = FALSE, use.mean = TRUE, xlim = NULL, ylim = NULL, 
                     xlab = NULL, ylab = NULL, col.low = c(0, 0, 1), col.upp = c(1, 0, 0), 
                     lwd = 1, transparency = 0.2, draw.cis = TRUE, grid.col = NULL, log = "", 
                     cut.at = 1) {
    rsr <- result.sliding.reflim
    
    cova <- rsr$covariate.mean
    if (!use.mean) {
        cova <- rsr$covariate.mean
    }
    
    df <- data.frame(
        covariate = cova,
        lower_lim = rsr$lower.lim,
        upper_lim = rsr$upper.lim,
        ci_lower_lim_low = rsr$ci.lower.lim.l,
        ci_lower_lim_up = rsr$ci.lower.lim.u,
        ci_upper_lim_low = rsr$ci.upper.lim.l,
        ci_upper_lim_up = rsr$ci.upper.lim.u
    )
    
    p <- ggplot(df, aes(x = covariate)) +
        geom_ribbon(aes(ymin = ci_lower_lim_low, ymax = ci_lower_lim_up),
                    fill = rgb(col.low[1], col.low[2], col.low[3], transparency),
                    alpha = transparency) +
        geom_ribbon(aes(ymin = ci_upper_lim_low, ymax = ci_upper_lim_up),
                    fill = rgb(col.upp[1], col.upp[2], col.upp[3], transparency),
                    alpha = transparency) +
        geom_line(aes(y = lower_lim, color = "Lower Limit"), linewidth = lwd) +
        geom_line(aes(y = upper_lim, color = "Upper Limit"), linewidth = lwd) +
        labs(x = xlab, y = ylab) +
        theme_minimal() +
        
        
        scale_color_manual(
            values = c("Upper Limit" = rgb(col.upp[1], col.upp[2], col.upp[3]), 
                       "Lower Limit" = rgb(col.low[1], col.low[2], col.low[3])),
            limits = c("Upper Limit", "Lower Limit")
        ) +
        labs(color = "Limits") +
        theme(legend.position = "right")
    
    # logarithmic scaling
    if (log.scale) {
        p <- p + scale_x_log10()
    }

    return(p)
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

# alist <- function(result.sliding.reflim,use.mean=T,xlim=NULL,ylim=NULL,xlab=NULL,ylab=NULL,col.low=c(0,0,1),col.upp=c(1,0,0),lwd=2,transparency=0.8,draw.cis=T,grid.col=NULL,log="",cut.at=1){
#     rsr <- result.sliding.reflim
#     
#     cova <- rsr$covariate.mean
#     if (!use.mean){
#         cova <- rsr$covariate.mean
#     }
#     
#     ylim.mod <- ylim
#     if (is.null(ylim)){
#         ylim.mod <- c(min(rsr$lower.lim),max(rsr$upper.lim))
#         if (draw.cis){
#             ylim.mod <- c(min(rsr$ci.lower.lim.l),max(rsr$ci.upper.lim.u))
#             if (log=="y" | log=="xy"){
#                 ylim.mod[1] <- max(c(cut.at,ylim.mod[1]))
#             }
#         }
#     }
#     
#     par(mar = c(3, 3, 3, 8))
#     
#     
#     # Plotting the lower limit curve
#     loli <- rsr$lower.lim
#     if (log=="y" | log=="xy"){
#         loli <- sapply(loli,cut.at.l,l=cut.at)
#     }
#     
#     plot(cova,loli,xlim=xlim,ylim=ylim.mod,type="l",lwd=lwd,col=rgb(col.low[1],col.low[2],col.low[3]),xlab=xlab,ylab=ylab,log=log)
#     if (!is.null(grid.col)){
#         grid(col=grid.col)
#     }
#     
#     # Plotting the upper limit curve
#     points(cova,rsr$upper.lim,type="l",lwd=lwd,col=rgb(col.upp[1],col.upp[2],col.upp[3]))
#     
#     # Plotting confidence intervals
#     cilloli <- rsr$ci.lower.lim.l
#     ciuloli <- rsr$ci.lower.lim.u
#     if (log=="y" | log=="xy"){
#         cilloli <- sapply(cilloli,cut.at.l,l=cut.at)
#         ciuloli <- sapply(ciuloli,cut.at.l,l=cut.at)
#     }
#     
#     
#     if (draw.cis){
#         collot <- rgb(col.low[1],col.low[2],col.low[3],1-transparency)
#         colupt <- rgb(col.upp[1],col.upp[2],col.upp[3],1-transparency)
#         polygon(c(cova,rev(cova)),c(cilloli,rev(ciuloli)),col=collot,border=collot)
#         polygon(c(cova,rev(cova)),c(rsr$ci.upper.lim.l,rev(rsr$ci.upper.lim.u)),col=colupt,border=colupt)
#     }
#     
#     legend("topright",
#            inset = c(-0.2, 0), 
#            legend = c("Upper Limit", "Lower Limit"), 
#            col = c(rgb(col.upp[1], col.upp[2], col.upp[3]), rgb(col.low[1], col.low[2], col.low[3])), 
#            lwd = lwd, 
#            title = "Limits", 
#            xpd = TRUE,
#            cex = 0.8,
#            bty = "n")
# }





#' gg_alist_compare
#' 
#' @description 
#' Plot 2 Sliding Reference Limits with Confidence Intervals, Used to compare results for two different parameters
#' 
#' @param result.sliding.reflim1 
#' A data frame or list containing the sliding reference limits and their associated confidence intervals
#' 
#' @param result.sliding.reflim2
#' Another data frame or list containing the sliding reference limits and their associated confidence intervals
#'  
#' @import ggplot2
#' 
#' @example 
#' gg_alist_compare(res, res1, log.scale = FALSE)
#' 
#' @export

gg_alist_compare <- function(result.sliding.reflim1, result.sliding.reflim2, log.scale = FALSE, use.mean = TRUE, 
                            xlim = NULL, ylim = NULL, xlab = NULL, ylab = NULL, 
                            col.low1 = c(0, 0, 1), col.upp1 = c(1, 0, 0), col.low2 = c(0, 1, 0), 
                            col.upp2 = c(0, 0, 0), lwd = 1, transparency = 0.2, 
                            draw.cis = TRUE, grid.col = NULL, log = "", cut.at = 1) {
    # Extract data from both results
    rsr1 <- result.sliding.reflim1
    rsr2 <- result.sliding.reflim2
    
    cova1 <- if (use.mean) rsr1$covariate.mean else rsr1$covariate.sd
    cova2 <- if (use.mean) rsr2$covariate.mean else rsr2$covariate.sd
    
    # Create data frames for plotting
    df1 <- data.frame(
        covariate = cova1,
        lower_lim = rsr1$lower.lim,
        upper_lim = rsr1$upper.lim,
        ci_lower_lim_low = rsr1$ci.lower.lim.l,
        ci_lower_lim_up = rsr1$ci.lower.lim.u,
        ci_upper_lim_low = rsr1$ci.upper.lim.l,
        ci_upper_lim_up = rsr1$ci.upper.lim.u
    )
    
    df2 <- data.frame(
        covariate = cova2,
        lower_lim = rsr2$lower.lim,
        upper_lim = rsr2$upper.lim,
        ci_lower_lim_low = rsr2$ci.lower.lim.l,
        ci_lower_lim_up = rsr2$ci.lower.lim.u,
        ci_upper_lim_low = rsr2$ci.upper.lim.l,
        ci_upper_lim_up = rsr2$ci.upper.lim.u
    )
    
    # Set ylim if not provided
    if (is.null(ylim)) {
        ylim <- range(c(df1$ci_lower_lim_low, df1$ci_upper_lim_up, df2$ci_lower_lim_low, df2$ci_upper_lim_up), na.rm = TRUE)
        if (log == "y" || log == "xy") {
            ylim[1] <- max(c(cut.at, ylim[1]))
        }
    }
    
    # Start plotting
    p <- ggplot() +
        geom_ribbon(data = df1, aes(x = covariate, ymin = ci_lower_lim_low, ymax = ci_lower_lim_up),
                    fill = rgb(col.low1[1], col.low1[2], col.low1[3], transparency), alpha = transparency) +
        geom_ribbon(data = df1, aes(x = covariate, ymin = ci_upper_lim_low, ymax = ci_upper_lim_up),
                    fill = rgb(col.upp1[1], col.upp1[2], col.upp1[3], transparency), alpha = transparency) +
        geom_line(data = df1, aes(x = covariate, y = lower_lim, color = "Lower Limit 1(standard)"), linewidth = lwd) +
        geom_line(data = df1, aes(x = covariate, y = upper_lim, color = "Upper Limit 1(standard)"), linewidth = lwd) +
        
        geom_ribbon(data = df2, aes(x = covariate, ymin = ci_lower_lim_low, ymax = ci_lower_lim_up),
                    fill = rgb(col.low2[1], col.low2[2], col.low2[3], transparency), alpha = transparency) +
        geom_ribbon(data = df2, aes(x = covariate, ymin = ci_upper_lim_low, ymax = ci_upper_lim_up),
                    fill = rgb(col.upp2[1], col.upp2[2], col.upp2[3], transparency), alpha = transparency) +
        geom_line(data = df2, aes(x = covariate, y = lower_lim, color = "Lower Limit 2(comparison)"), linewidth = lwd) +
        geom_line(data = df2, aes(x = covariate, y = upper_lim, color = "Upper Limit 2(comparison)"), linewidth = lwd) +
        
        labs(x = xlab, y = ylab, color = "Limits") +
        scale_color_manual(values = c("Upper Limit 1(standard)" = rgb(col.upp1[1], col.upp1[2], col.upp1[3]), 
                                      "Lower Limit 1(standard)" = rgb(col.low1[1], col.low1[2], col.low1[3]),
                                      "Upper Limit 2(comparison)" = rgb(col.upp2[1], col.upp2[2], col.upp2[3]),
                                      "Lower Limit 2(comparison)" = rgb(col.low2[1], col.low2[2], col.low2[3])),
                           limits = c("Upper Limit 1(standard)", "Upper Limit 2(comparison)", "Lower Limit 1(standard)", "Lower Limit 2(comparison)")) +
        theme_minimal() +
        theme(legend.position = "right")
    
    # Apply xlim and ylim if provided
    if (!is.null(xlim)) {
        p <- p + xlim(xlim)
    }
    
    if (!is.null(ylim)) {
        p <- p + ylim(ylim)
    }
    
    # Optionally add grid lines
    if (!is.null(grid.col)) {
        p <- p + theme(panel.grid.major = element_line(color = grid.col))
    }
    
    if (log.scale) {
        p <- p + scale_x_log10()
    }
    
    return(p)
}



#' alist_custom_sd
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
#' alist_custom_sd(res, res1)
#' 
#' @export

# alist_custom_sd <- function(result.sliding.reflim1, result.sliding.reflim2, use.mean=T, xlim=NULL, ylim=NULL, 
#                   xlab=NULL, ylab=NULL, col.low1=c(0,0,1), col.upp1=c(1,0,0), col.low2=c(0,1,0), 
#                   col.upp2=c(0,0,0), lwd=2, transparency=0.8, draw.cis=T, grid.col=NULL, log="", 
#                   cut.at=1){
#   
#   rsr1 <- result.sliding.reflim1
#   rsr2 <- result.sliding.reflim2
#   
#   cova1 <- rsr1$covariate.mean
#   cova2 <- rsr2$covariate.mean
#   if (!use.mean){
#     cova1 <- rsr1$covariate.mean
#     cova2 <- rsr2$covariate.mean
#   }
#   
#   ylim.mod <- ylim
#   if (is.null(ylim)){
#     ylim.mod <- c(min(c(rsr1$lower.lim, rsr2$lower.lim)), max(c(rsr1$upper.lim, rsr2$upper.lim)))
#     if (draw.cis){
#       ylim.mod <- c(min(c(rsr1$ci.lower.lim.l, rsr2$ci.lower.lim.l)), max(c(rsr1$ci.upper.lim.u, rsr2$ci.upper.lim.u)))
#       if (log=="y" | log=="xy"){
#         ylim.mod[1] <- max(c(cut.at, ylim.mod[1]))
#       }
#     }
#   }
#   
#   par(mar = c(3,3,3,8))
#   
#   # rsr1 lower limit curve
#   loli1 <- rsr1$lower.lim
#   if (log=="y" | log=="xy"){
#     loli1 <- sapply(loli1, cut.at.l, l=cut.at)
#   }
#   plot(cova1, loli1, xlim=xlim, ylim=ylim.mod, type="l", lwd=lwd, col=rgb(col.low1[1], col.low1[2], col.low1[3]), 
#        xlab=xlab, ylab=ylab, log=log)
#   
#   # rsr1 upper limit curve
#   points(cova1, rsr1$upper.lim, type="l", lwd=lwd, col=rgb(col.upp1[1], col.upp1[2], col.upp1[3]))
#   
#   # rsr2 lower
#   loli2 <- rsr2$lower.lim
#   if (log=="y" | log=="xy"){
#     loli2 <- sapply(loli2, cut.at.l, l=cut.at)
#   }
#   points(cova2, loli2, type="l", lwd=lwd, col=rgb(col.low2[1], col.low2[2], col.low2[3]))
#   
#   # rsr2 upper
#   points(cova2, rsr2$upper.lim, type="l", lwd=lwd, col=rgb(col.upp2[1], col.upp2[2], col.upp2[3]))
#   
#   if (!is.null(grid.col)){
#     grid(col=grid.col)
#   }
#   
#   # Plotting confidence intervals
#   if (draw.cis){
#     # rsr1 confidence intervals
#     cilloli1 <- rsr1$ci.lower.lim.l
#     ciuloli1 <- rsr1$ci.lower.lim.u
#     if (log=="y" | log=="xy"){
#       cilloli1 <- sapply(cilloli1, cut.at.l, l=cut.at)
#       ciuloli1 <- sapply(ciuloli1, cut.at.l, l=cut.at)
#     }
#     collot1 <- rgb(col.low1[1], col.low1[2], col.low1[3], 1-transparency)
#     colupt1 <- rgb(col.upp1[1], col.upp1[2], col.upp1[3], 1-transparency)
#     polygon(c(cova1, rev(cova1)), c(cilloli1, rev(ciuloli1)), col=collot1, border=collot1)
#     polygon(c(cova1, rev(cova1)), c(rsr1$ci.upper.lim.l, rev(rsr1$ci.upper.lim.u)), col=colupt1, border=colupt1)
#     
#     # rsr2 confidence intervals
#     cilloli2 <- rsr2$ci.lower.lim.l
#     ciuloli2 <- rsr2$ci.lower.lim.u
#     if (log=="y" | log=="xy"){
#       cilloli2 <- sapply(cilloli2, cut.at.l, l=cut.at)
#       ciuloli2 <- sapply(ciuloli2, cut.at.l, l=cut.at)
#     }
#     collot2 <- rgb(col.low2[1], col.low2[2], col.low2[3], 1-transparency)
#     colupt2 <- rgb(col.upp2[1], col.upp2[2], col.upp2[3], 1-transparency)
#     polygon(c(cova2, rev(cova2)), c(cilloli2, rev(ciuloli2)), col=collot2, border=collot2)
#     polygon(c(cova2, rev(cova2)), c(rsr2$ci.upper.lim.l, rev(rsr2$ci.upper.lim.u)), col=colupt2, border=colupt2)
#   }
#   
#   legend("topright",
#          inset = c(-0.2, 0),
#          legend = c("Upper Limit (sd = 5)", "Lower Limit (sd = 5)", "Upper Limit (rs2)", "Lower Limit (rs2)"),
#          col = c(rgb(col.upp1[1], col.upp1[2], col.upp1[3]), rgb(col.low1[1], col.low1[2], col.low1[3]),
#                  rgb(col.upp2[1], col.upp2[2], col.upp2[3]), rgb(col.low2[1], col.low2[2], col.low2[3])),
#          lwd = lwd, 
#          title = "Limits",
#          xpd = TRUE,
#          cex = 0.8,
#          bty = "n") 
# }










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
    y <- ifelse(x < a | x > c, 0,
                ifelse(x <= b, (x - a) / ((b - a) * (c - a)),
                       (c - x) / ((c - b) * (c - a))))
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
    args <- list(...)
    if (distribution == "truncated_gaussian") {
        sigma <- args$sigma
        if (is.null(sigma)) {
            sigma <- 5
        }
        return(function(x, mean) {
            dnorm(x, mean = mean, sd = sigma) / dnorm(mean, mean = mean, sd = sigma)
        })
    } else if (distribution == "gaussian") {
        sigma <- args$sigma
        if (is.null(sigma)) {
            sigma <- 5
        }
        return(function(x, mean) {
            dnorm(x, mean = mean, sd = sigma) / dnorm(mean, mean = mean, sd = sigma)
        })
    } else if (distribution == "triangular") {
        a <- args$a
        b <- args$b
        c <- args$c
        return(function(x) {
            dtriang(x, a = a, b = b, c = c) / dtriang(b, a = a, b = b, c = c)
        })
    } else if (distribution == "trapezoidal") {
        a <- args$a
        b <- args$b
        c <- args$c
        d <- args$d
        return(function(x) {
            dtrapezoid(x, a = a, b = b, c = c, d = d) / dtrapezoid((b + c) / 2, a = a, b = b, c = c, d = d)
        })
    } else {
        stop("Unsupported distribution type")
    }
}


#' calculate_weight_threshold
#' @description
#' Calculate the weight threshold for a given distribution and parameters. Used to determine if there are enough points in the distribution.
#' 
#' @param distribution [character] The type of distribution to use for the weight function.
#' @param params [list] A list of parameters for the distribution.
#' @param n [int] Number of sample points used to calculate threshold, default is 40.
#' 
#' @return [numeric] Sum of weights
#' 
#' @example calculate_weight_threshold("gaussian", list(standard_deviation = 5))
#'          calculate_weight_threshold("triangular", list(vertex1 = 0.5))
#' 
#' @export

calculate_weight_threshold <- function(distribution, params, n = 40) {
    
    uniform_sample <- seq(0, 1, length.out = n)
    
    
    w_function <- switch(distribution,
                         "gaussian" = makeWeightFunction("gaussian", sigma = params$standard_deviation),
                         "truncated_gaussian" = makeWeightFunction("truncated_gaussian", sigma = params$standard_deviation),
                         "triangular" = {
                             vertex1 <- if (is.null(params$vertex1)) 0.5 else params$vertex1
                             makeWeightFunction("triangular", a = 0, b = vertex1, c = 1)
                         },
                         "trapezoidal" = {
                             vertex1 <- if (is.null(params$vertex1)) 0.3 else params$vertex1
                             vertex2 <- if (is.null(params$vertex2)) 0.6 else params$vertex2
                             makeWeightFunction(distribution = "trapezoidal", a = 0, b = vertex1, c = vertex2, d = 1)
                         }
    )
    
    
    weights <- if (distribution %in% c("gaussian", "truncated_gaussian")) {
        w_function(uniform_sample, mean = 0.5)
    } else {
        w_function(uniform_sample)
    }
    return(sum(weights))
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
#' @param plot.weight [logical] If TRUE, the weight function is plotted
#' 
#' @export

w_sliding.reflim <- function(x,covariate,distribution = "truncated_gaussian", standard_deviation = 5, 
                             start_point = NULL, vertex1 = NULL, vertex2 = NULL, end_point = NULL, 
                             window.size=NULL,step.width=NULL,lognormal=NULL,perc.trunc=2.5,n.min.window=200,
                             n.min=100,apply.rounding=FALSE, plot.weight=TRUE, weight_threshold = NULL, verbose = TRUE, MLE = FALSE) {
    # print(paste("sd = ", standard_deviation))
    
    is.nona <- !is.na(x) & !is.na(covariate)
    xx <- x[is.nona]
    covcomp <- covariate[is.nona]
    
    ord.cov <- order(covcomp)
    xx <- xx[ord.cov]
    covcomp <- covcomp[ord.cov]
    
    if(!is.numeric(xx)){stop("(reflim) x must be numeric.")}
    if(min(xx) < 0){stop("(reflim) only positive values allowed.")} # <=
    n <- length(xx)
    if(n < 39){stop(paste0("(iboxplot) n = ", n, ". The length of x should be 200 or more. The absolute minimum for reference limit estimation is 39."))}
    if(n < n.min){  # Determine enough points
        print(noquote(paste("n =", n, "where a minimum of", n.min, "is required. You may try to reduce n.min at the loss of accuracy.")))
        return(c(mean = NA, sd = NA, lower.lim = NA, upper.lim = NA))
    }
    
    cov.unique <- covcomp[!duplicated(covcomp)]
    n.steps <- length(cov.unique)
    
    if (verbose)
    print(paste("n.steps =", n.steps))
    if(n.steps==1){stop("The covariate is constant.")}
    
    if (!is.null(window.size) & !is.null(step.width)){
        n.steps <- ceiling(max(c(1,(covcomp[length(covcomp)]-covcomp[1]-window.size)/step.width)))
        if (verbose)
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
    
    if (is.null(weight_threshold))
    weight_threshold <- calculate_weight_threshold(distribution = distribution,
                                                   params = list(standard_deviation = standard_deviation,
                                                                 vertex1 = vertex1,
                                                                 vertex2 = vertex2),
                                                   n = 40)
    
    
    if (distribution == "gaussian") {
        # if (verbose)
        # print("Using gaussian")
        w_function <- makeWeightFunction("gaussian", sigma = standard_deviation)
        for (i in seq(min(covcomp), max(covcomp), length.out = n.steps)) {  # Generate an equally spaced sequence from the minimum to the maximum value of covcomp.
            www <- w_function(covcomp, mean = i)
            www_sum <- sum(www)
            if (www_sum < weight_threshold) {
                warning("Weight sum is too low. Skipping this step.")
                next
            }
            
            if (!MLE) {
                # print("not MLE method")
                res.reflim <- w_reflim(xx, www, n.min = n.min, apply.rounding = apply.rounding, lognormal = lognormal, plot.all = FALSE)
            
                lower.lim[i] <- res.reflim$limits[1]
                upper.lim[i] <- res.reflim$limits[2]
                ci.lower.lim.l[i] <- res.reflim$confidence.int[1]
                ci.lower.lim.u[i] <- res.reflim$confidence.int[2]
                ci.upper.lim.l[i] <- res.reflim$confidence.int[3]
                ci.upper.lim.u[i] <- res.reflim$confidence.int[4]
                sum.www[i] <- sum(www)
            }
            
            
            if (MLE) {
                # print("in MLE method")
                res.reflim <- w_reflimLOD.MLE(xx, weights = www, lod = min(xx[xx > 0]), n.lod = sum(xx == min(xx[xx > 0])), verbose = verbose)
                
                if (is.na(res.reflim$lower.limit)) {
                    warning("MLE failed for this step (pnorm.lod=0 or 1). Skipping.")
                    next
                }
                
                lower.lim[i] <- round(res.reflim$lower.limit, 4)
                upper.lim[i] <- round(res.reflim$upper.limit, 4)
                ci.lower.lim.l[i] <- res.reflim$lower.limit
                ci.lower.lim.u[i] <- res.reflim$lower.limit
                ci.upper.lim.l[i] <- res.reflim$upper.limit
                ci.upper.lim.u[i] <- res.reflim$upper.limit
                sum.www[i] <- sum(www)
            }
            
            distribution.type[i] <- ifelse(names(res.reflim)[1] == "mean", "normal", "lognormal")
            
            covariate.left[i] <- min(covcomp)
            covariate.right[i] <- max(covcomp)
            covariate.mean[i] <- covcomp[which.max(www)]    # Use the mean parameter to record the point where the weights are maximal
            covariate.median[i] <- median(covcomp)
            covariate.n[i] <- length(covcomp)  # Count of all covariates
            
            if (plot.weight)
            plot(covcomp, www, type = "l", col = "blue", lwd = 2, main = paste("Gaussian Weight Function at i =", i))   # Plot the weight function
            points(covcomp, www, col = "red")
            www_sum <- sum(www)
            text(x = mean(covcomp), y = mean(www),
                 labels = paste("sum of www=", round(www_sum,2)))
        }
    } else {
        if (!is.null(window.size) & !is.null(step.width)) {
            if (verbose)
            print("window.size & step.width not null")
            window.left <- covcomp[1]
            window.right <- window.left + window.size
            plot_index <- 1
            for (i in 1:n.steps) {
                is.in.interval <- covcomp >= window.left & covcomp <= window.right
                if (sum(is.in.interval) >= n.min) { # enough points in the interval
                    # print("enough points in the intercal")
                    # print(paste("window.left =", window.left, "window.right =", window.right))
                    
                    interval_cov <- covcomp[is.in.interval]
                    # print(interval_cov)
                    
                    xxx <- xx[is.in.interval]
                    
                    if (distribution == "truncated_gaussian") {
                        # if (verbose) print("Using truncated gaussian")
                        w_function <- makeWeightFunction(distribution, sigma = standard_deviation)
                        # www <- w_function(interval_cov, mean = median(interval_cov))
                        www <- w_function(interval_cov, mean = (min(interval_cov) + max(interval_cov)) / 2)
                    } else if (distribution == "triangular") {
                        # if (verbose) print("Using triangular")
 
                        vertex1 <- if (is.null(vertex1)) 0.5 else vertex1
                        
                        start_point.value <- min(interval_cov)
                        end_point.value <- max(interval_cov)
                        vertex1.value <- (end_point.value - start_point.value) * vertex1 + start_point.value  # vertex1 is the vertex of the triangle

                        w_function <- makeWeightFunction(distribution, a = start_point.value, b = vertex1.value, c = end_point.value)
                        www <- w_function(interval_cov)
                    } else if (distribution == "trapezoidal") {
                        # if (verbose) print("Using trapezoidal")

                        vertex1 <- if (is.null(vertex1)) 0.3 else vertex1
                        vertex2 <- if (is.null(vertex2)) 0.6 else vertex2
                        
                        start_point.value <- min(interval_cov)
                        end_point.value <- max(interval_cov)
                        vertex1.value <- (end_point.value - start_point.value) * vertex1 + start_point.value  # vertex1 is the left horizontal point
                        vertex2.value <- (end_point.value - start_point.value) * vertex2 + start_point.value  # vertex2 is the right horizontal point

                        w_function <- makeWeightFunction(distribution = distribution, a = start_point.value, b = vertex1.value, c = vertex2.value, d = end_point.value)
                        www <- w_function(interval_cov)
                    }
                    
                    www_sum <- sum(www)
                    sum.www[i] <- www_sum
                    # print(www_sum)
                    
                    if (www_sum < weight_threshold) {
                        warning("Weight sum is too low. Skipping this step.")
                        next
                    }
                    
                    if (plot.weight)
                    plot(interval_cov, www, type = "l", col = "blue", lwd = 2, main = paste("www VS interval_cov", plot_index))
                    points(interval_cov, www, col = "red")
                    # www_sum <- sum(www)
                    text(x = mean(interval_cov), y = mean(www), 
                         labels = paste("Sum of www =", round(www_sum, 2)),
                         col = "darkgreen", cex = 1.5, font = 2)
                    sum.www[i] <- www_sum
                    plot_index <- plot_index + 1
                    
                    
                    if (!MLE) {
                        res.reflim <- w_reflim(
                            xxx,
                            www,
                            n.min = n.min,
                            apply.rounding = apply.rounding,
                            lognormal = lognormal,
                            plot.all = FALSE
                        )
                        
                        
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
                    } else {
                        res.reflim <- MLE(xxx, www, verbose = verbose)
                        
                        lower.lim[i] <- round(res.reflim$lower.limit, 4)
                        upper.lim[i] <- round(res.reflim$upper.limit, 4)
                        ci.lower.lim.l[i] <- res.reflim$lower.limit
                        ci.lower.lim.u[i] <- res.reflim$lower.limit
                        ci.upper.lim.l[i] <- res.reflim$upper.limit
                        ci.upper.lim.u[i] <- res.reflim$upper.limit
                        # print(paste("lower.lim[ind] =", lower.lim[i], "upper.lim[ind] =", upper.lim[i]))
                    }
                    
                    
                        
                    covals <- covcomp[is.in.interval]
                    covariate.left[i] <- window.left
                    covariate.right[i] <- window.right
                    covariate.mean[i] <- mean(covals)
                    covariate.median[i] <- median(covals)
                    covariate.n[i] <- sum(is.in.interval)
                    
                } else {
                    if (verbose)
                    print("not enough points in the interval")
                    covariate.left[i] <- window.left
                    covariate.right[i] <- window.right
                    covariate.n[i] <- sum(is.in.interval)
                }
                window.left <- window.left + step.width
                window.right <- window.right + step.width
            }
        } else {
            if (verbose)
            print("window.size or step.width is null")
            ind <- 1
            indl <- 1
            indr <- 2
            plot_index <- 1
            while(indr <= length(cov.unique)) {
                is.in.interval <- covcomp >= cov.unique[indl] & covcomp < cov.unique[indr]
                
                if (sum(is.in.interval) >= n.min.window) {

                    interval_cov <- covcomp[is.in.interval]
                    
                    xxx <- xx[is.in.interval]

                    if (distribution == "truncated_gaussian") {
                        # if (verbose) print("Using truncated gaussian")
                        w_function <- makeWeightFunction(distribution, sigma = standard_deviation)
                        www <- w_function(interval_cov, mean = (min(interval_cov) + max(interval_cov)) / 2)
                    } else if (distribution == "triangular") {
                        # if (verbose) print("Using triangular")
                        vertex1 <- if (is.null(vertex1)) 0.5 else vertex1
                        
                        start_point.value <- min(interval_cov)
                        end_point.value <- max(interval_cov)
                        vertex1.value <- (end_point.value - start_point.value) * vertex1 + start_point.value

                        w_function <- makeWeightFunction(distribution, a = start_point.value, b = vertex1.value, c = end_point.value)
                        www <- w_function(interval_cov)
                    } else if (distribution == "trapezoidal") {
                        # if (verbose) print("Using trapezoidal")
                        vertex1 <- if (is.null(vertex1)) 0.3 else vertex1
                        vertex2 <- if (is.null(vertex2)) 0.6 else vertex2
                        
                        start_point.value <- min(interval_cov)
                        end_point.value <- max(interval_cov)
                        vertex1.value <- (end_point.value - start_point.value) * vertex1 + start_point.value
                        vertex2.value <- (end_point.value - start_point.value) * vertex2 + start_point.value
                        
                        w_function <- makeWeightFunction(distribution = distribution, a = start_point.value, b = vertex1.value, c = vertex2.value, d = end_point.value)
                        www <- w_function(interval_cov)
                    }
                    
                    www_sum <- sum(www)
                    sum.www[ind] <- www_sum
                    
                    if (www_sum < weight_threshold) {
                        indr <- indr + 1
                        warning("Weight sum is too low. Expanding the window.")
                        next
                    }
                    
                    if (plot.weight)
                    plot(interval_cov, www, type = "l", col = "blue", lwd = 2, main = paste("www VS interval_cov", plot_index))
                    points(interval_cov, www, col = "red")
                    www_sum <- sum(www)
                    text(x = mean(interval_cov), y = mean(www), 
                         labels = paste("Sum of www =", round(www_sum, 2)),
                         col = "darkgreen", cex = 1.5, font = 2)
                    sum.www[ind] <- www_sum
                    plot_index <- plot_index + 1
                    
                    if (!MLE) {
                        res.reflim <- w_reflim(
                            xxx,
                            www,
                            n.min = n.min,
                            apply.rounding = apply.rounding,
                            lognormal = lognormal,
                            plot.all = FALSE
                        )
                        
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
                    } else {
                        res.reflim <- MLE(xxx, www, verbose = verbose)
                        
                        lower.lim[ind] <- round(res.reflim$lower.limit, 4)
                        upper.lim[ind] <- round(res.reflim$upper.limit, 4)
                        ci.lower.lim.l[ind] <- res.reflim$lower.limit
                        ci.lower.lim.u[ind] <- res.reflim$lower.limit
                        ci.upper.lim.l[ind] <- res.reflim$upper.limit
                        ci.upper.lim.u[ind] <- res.reflim$upper.limit
                        # print(paste("lower.lim[ind] =", lower.lim[ind], "upper.lim[ind] =", upper.lim[ind]))
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
            }
        }
    }
    
    res <- data.frame(lower.lim,upper.lim,ci.lower.lim.l,ci.lower.lim.u,ci.upper.lim.l,ci.upper.lim.u,distribution.type,covariate.left,covariate.right,covariate.mean,covariate.median,covariate.n,sum.www)
    # Remove rows containing NA
    res <- res[!is.na(covariate.n) & !is.na(lower.lim),]
    # res <- res[!is.na(covariate.n),]
    return(res)
}




#' w_sliding.reflim.plot
#' 
#' @description 
#' This function is similar to the w_sliding.reflim function, but only records information about the weights of each point in each loop

w_sliding.reflim.plot <- function(x,covariate,distribution = "truncated_gaussian", 
                                  standard_deviation = 5, vertex1 = NULL, vertex2 = NULL, 
                                  window.size=NULL,step.width=NULL,lognormal=NULL,
                                  perc.trunc=2.5,n.min.window=200,n.min=100,apply.rounding=FALSE, weight_threshold = NULL, verbose = TRUE) {
    is.nona <- !is.na(x) & !is.na(covariate)
    xx <- x[is.nona]
    covcomp <- covariate[is.nona]
    
    ord.cov <- order(covcomp)
    xx <- xx[ord.cov]
    covcomp <- covcomp[ord.cov]
    
    if(!is.numeric(xx)){stop("(reflim) x must be numeric.")}
    if(min(xx) < 0){stop("(reflim) only positive values allowed.")} # <=
    n <- length(xx)
    if(n < 39){stop(paste0("(iboxplot) n = ", n, ". The length of x should be 200 or more. The absolute minimum for reference limit estimation is 39."))}
    if(n < n.min){  # Determine enough points
        print(noquote(paste("n =", n, "where a minimum of", n.min, "is required. You may try to reduce n.min at the loss of accuracy.")))
        return(c(mean = NA, sd = NA, lower.lim = NA, upper.lim = NA))
    }
    
    cov.unique <- covcomp[!duplicated(covcomp)]
    n.steps <- length(cov.unique)
    # print(paste("n.steps =", n.steps))
    if(n.steps==1){stop("The covariate is constant.")}
    
    if (!is.null(window.size) & !is.null(step.width)){
        n.steps <- ceiling(max(c(1,(covcomp[length(covcomp)]-covcomp[1]-window.size)/step.width)))
        # print(paste("get new n.steps =", n.steps))
    }
    
    x.interval <- list()
    t.interval <- list()
    w.interval <- list()
    
    sum.www <- rep(NA, n.steps)
    
    loop <- 0
    
    if (is.null(weight_threshold))
    weight_threshold <- calculate_weight_threshold(distribution = distribution,
                                                   params = list(standard_deviation = standard_deviation,
                                                                 vertex1 = vertex1,
                                                                 vertex2 = vertex2),
                                                   n = 40)
    
    if (distribution == "gaussian") {
        # print("Using gaussian")
        w_function <- makeWeightFunction("gaussian", sigma = standard_deviation)
        step_index <- 1
        for (i in seq(min(covcomp), max(covcomp), length.out = n.steps)) {  # Generate an equally spaced sequence from the minimum to the maximum value of covcomp.
            www <- w_function(covcomp, mean = i)
            
            www_sum <- sum(www)
            if (www_sum < weight_threshold) {
                warning("Weight sum is too low. Skipping this step.")
                # print("Weight sum is too low. Skipping this step.")
                next
            }
            
            # res.reflim <- w_reflim(xx, www, n.min = n.min, apply.rounding = apply.rounding, lognormal = lognormal, plot.all = FALSE)
            loop <- loop + 1
            
            x.interval[[step_index]] <- xx
            t.interval[[step_index]] <- covcomp
            w.interval[[step_index]] <- www
            
            step_index <- step_index + 1
        }
    } else {
        if (!is.null(window.size) & !is.null(step.width)) {
            # print("window.size & step.width not null")
            window.left <- covcomp[1]
            window.right <- window.left + window.size
            
            covariate.left <- numeric(n.steps)
            covariate.right <- numeric(n.steps)
            covariate.n <- numeric(n.steps)
            for (i in 1:n.steps) {
                
                is.in.interval <- covcomp >= window.left & covcomp <= window.right
                if (sum(is.in.interval) >= n.min) {
                    
                    interval_cov <- covcomp[is.in.interval]
                    t.interval[[i]] <- interval_cov
                    
                    xxx <- xx[is.in.interval]
                    x.interval[[i]] <- xxx 
                    
                    
                    if (distribution == "truncated_gaussian") {
                        w_function <- makeWeightFunction(distribution, sigma = standard_deviation)
                        www <- w_function(interval_cov, mean = (min(interval_cov) + max(interval_cov)) / 2)
                    } else if (distribution == "triangular") {
                        vertex1 <- if (is.null(vertex1)) 0.5 else vertex1
                        
                        start_point.value <- min(interval_cov)
                        end_point.value <- max(interval_cov)
                        vertex1.value <- (end_point.value - start_point.value) * vertex1 + start_point.value
                        
                        w_function <- makeWeightFunction(distribution, a = start_point.value, b = vertex1.value, c = end_point.value)
                        www <- w_function(interval_cov)
                    } else if (distribution == "trapezoidal") {
                        vertex1 <- if (is.null(vertex1)) 0.3 else vertex1
                        vertex2 <- if (is.null(vertex2)) 0.6 else vertex2
                        
                        start_point.value <- min(interval_cov)
                        end_point.value <- max(interval_cov)
                        vertex1.value <- (end_point.value - start_point.value) * vertex1 + start_point.value
                        vertex2.value <- (end_point.value - start_point.value) * vertex2 + start_point.value
                        
                        w_function <- makeWeightFunction(distribution = distribution, a = start_point.value, b = vertex1.value, c = vertex2.value, d = end_point.value)
                        www <- w_function(interval_cov)
                    }
                    w.interval[[i]] <- www
                    
                    www_sum <- sum(www)
                    
                    sum.www[i] <- www_sum
                    
                    if (www_sum < weight_threshold) {
                        warning("Weight sum is too low. Skipping this step.")
                        # print("Weight sum is too low. Skipping this step.")
                        next
                    }
                    
                    # res.reflim <- w_reflim(xxx, www, n.min = n.min, apply.rounding = apply.rounding, lognormal = lognormal, plot.all = FALSE)
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
            # print("window.size or step.width is null")
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
                    
                    if (distribution == "truncated_gaussian") {
                        w_function <- makeWeightFunction(distribution = distribution, sigma = standard_deviation)
                        www <- w_function(interval_cov, mean = (min(interval_cov) + max(interval_cov)) / 2)
                    } else if (distribution == "triangular") {
                        vertex1 <- if (is.null(vertex1)) 0.5 else vertex1
                        
                        start_point.value <- min(interval_cov)
                        end_point.value <- max(interval_cov)
                        vertex1.value <- (end_point.value - start_point.value) * vertex1 + start_point.value
                        
                        w_function <- makeWeightFunction(distribution, a = start_point.value, b = vertex1.value, c = end_point.value)
                        www <- w_function(interval_cov)
                    } else if (distribution == "trapezoidal") {
                        vertex1 <- if (is.null(vertex1)) 0.3 else vertex1
                        vertex2 <- if (is.null(vertex2)) 0.6 else vertex2
                        
                        start_point.value <- min(interval_cov)
                        end_point.value <- max(interval_cov)
                        vertex1.value <- (end_point.value - start_point.value) * vertex1 + start_point.value
                        vertex2.value <- (end_point.value - start_point.value) * vertex2 + start_point.value
                        
                        w_function <- makeWeightFunction(distribution = distribution, a = start_point.value, b = vertex1.value, c = vertex2.value, d = end_point.value)
                        
                        www <- w_function(interval_cov)
                    }
                    w.interval[[ind]] <- www 
                    
                    www_sum <- sum(www)
                    
                    sum.www[ind] <- www_sum
                    
                    if (www_sum < weight_threshold) {
                        indr <- indr + 1
                        warning("Weight sum is too low. Expanding the window.")
                        next
                    }
                    
                    # res.reflim <- w_reflim(xxx, www, n.min = n.min, apply.rounding = apply.rounding, lognormal = lognormal, plot.all = FALSE)
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
    
    
    if (verbose) 
    print(paste("Number of loops = ", loop))
    res <- data.frame()
    
    last_was_separetor <- TRUE
    
    # for (i in 1:loop) { # caliper: long time waiting
    #     print(paste("i = ", i))
    #     temp_df <- data.frame(
    #         x = x.interval[[i]],
    #         t = t.interval[[i]],
    #         w = w.interval[[i]]
    #     )
    # 
    #     res <- rbind(res, temp_df)
    # 
    #     if (nrow(temp_df) > 0) {
    #         res <- rbind(res, temp_df)
    # 
    #         if (!last_was_separetor) {
    #             separator <- data.frame(x = "---", t = "---", w = "---")
    #             res <- rbind(res, separator)
    #         }
    #         last_was_separetor <- FALSE
    #     } else {
    #         last_was_separetor <- TRUE
    #     }
    # 
    # }
    
    #optimized
    all_dfs <- vector("list", loop * 2)  # Leave space for data and separators
    df_count <- 0

    for (i in 1:loop) {
        if (!is.null(x.interval[[i]]) && !is.null(t.interval[[i]]) && !is.null(w.interval[[i]])) {
            df_count <- df_count + 1
            all_dfs[[df_count]] <- data.frame(
                x = x.interval[[i]],
                t = t.interval[[i]],
                w = w.interval[[i]]
            )

            # Add separators only when needed
            if (i < loop && !is.null(x.interval[[i+1]])) {
                df_count <- df_count + 1
                all_dfs[[df_count]] <- data.frame(
                    x = "---",
                    t = "---",
                    w = "---"
                )
            }
        }
    }

    # Merge all data frames at once
    res <- do.call(rbind, all_dfs[1:df_count])
    return(res)
}


#' w_reflim
#' 
#' @description 
#' Using the qq chart method, calculate the upper and lower limits
#' 
#' @param targets [numeric] upper and lower limits of the target
#' 
#' @importFrom reflimR conf_int95
#' 
#' @export

w_reflim <- function (x, x_weight, lognormal = NULL, targets = NULL, perc.trunc = 2.5,
             n.min = 200, apply.rounding = TRUE, plot.it = FALSE, plot.all = FALSE, 
             print.n = TRUE, main = "reference limits", xlab = "x") {
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
    

    res.lognorm <- w_lognorm(x_clean, ww_clean, plot.it = FALSE)
    res.trunc <- w_iboxplot(x_clean, ww_clean, lognormal = lognormal, perc.trunc = perc.trunc,
                            apply.rounding = apply.rounding, plot.it = FALSE)
    
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
    if (!is.na(res.qq[3]) && !is.na(res.qq[4]) && res.qq[3] >= res.qq[4]) { # fixed: upper limit must > lower limit
        warning("Lower limit >= upper limit. Adjusting limits.")
        res.qq[4] <- res.qq[3] + 1
    }
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
        ip <- interpretation(res.lim[1:2], targets) # Check that the given limit and target values are within the permissible uncertainty range
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
#' Assessment of symmetry
#' 
#' @importFrom Hmisc wtd.quantile
#' 
#' @export

w_bowley <- function(x, x_weight) {
    w_quantiles <- wtd.quantile(x, x_weight, probs = c(0.25, 0.5, 0.75))
    if (abs(w_quantiles[3] - w_quantiles[2]) < .Machine$double.eps) {
        return(0)
    }
    return((w_quantiles[3] + w_quantiles[1] - 2 * w_quantiles[2]) / (w_quantiles[3] -
                                                                         w_quantiles[2]))
}


w_IQR <- function(x, x_weight) {
    w_quantiles <- wtd.quantile(x, x_weight, probs = c(0.25, 0.5, 0.75))
    iqr <- w_quantiles[3] - w_quantiles[1]
    return(iqr)
}

#' w_lognorm
#' 
#' @description 
#' Calculate whether lognorm or norm should be used
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
  
    # delta can determine whether the logarithmic transformation significantly reduces the skewness of the data
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
    
        slope <- (df.quant[2, 1] - df.quant[1, 1]) / (df.quant[2, 2] - df.quant[1, 2])
        
        intercept <- df.quant[1, 1] - slope * df.quant[1, 2]
        df.x <- cbind(df.x, transformed = intercept + slope * df.x[, 2])
    
        # Plotting probability density functions
        d1 <- density(df.x[, 1])
        d2 <- density(df.x[, 3])
        ymax <- max(max(d1$y), max(d2$y)) * 1.4
        plot(d1$x, d1$y, main = main, xlab = xlab, ylab = "", 
            type = "l", lwd = 2, yaxt = "n", xlim = c(0.9 * min(df.x[,1]), max(max(d1$x), max(d2$x))), ylim = c(0, 
                                                                                                                ymax))
        lines(d2$x, d2$y, col = "blue", lwd = 2)
        base.scale <- c(1, 1.5, 2, 3, 5, 7)
        num.scale <- c(base.scale * 10^-1,base.scale * 10^0,base.scale * 10^1,base.scale * 10^2,
                       base.scale *10^3,base.scale * 10^4)
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
            tx <- ifelse(lognormal, "lognormal distribution", "normal distribution")
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
#' The box plot method is used for iterative truncation to obtain the central 95% of potentially non-significant results.
#' 
#' @importFrom reflimR adjust_digits
#' @importFrom Hmisc wtd.quantile
#' 
#' @export

w_iboxplot <- function(x, x_weight, lognormal = NULL, perc.trunc = 2.5,
                       apply.rounding = TRUE, plot.it = FALSE, main = "w_iBoxplot", xlab = "x") {
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
    
    if (is.null(perc.trunc) || perc.trunc <= 0 || perc.trunc > 25) {
        stop("perc.trunc must not be NULL, negative, zero, or greater than 25.")
    }
    
    q.trunc <- perc.trunc/100
    # w_truncate_x <- function(x, x_weight, i) {
    #     qf <- ifelse(i == 1, qf <- qnorm(q.trunc) / qnorm(0.25),
    #                 qf <- qnorm(q.trunc) / qnorm(0.25 * (1 - 2 * q.trunc)
    #                                             + q.trunc))
    #     
    #     if (all(x_weight == 0)) {
    #         warning("All weights are zero. Use equal weights.")
    #         x_weight <- rep(1/length(x), length(x))
    #     }
    #     # print("x:")
    #     # print(x)
    #     # print("x_weight:")
    #     # print(x_weight)
    #     Q <- wtd.quantile(x, x_weight, c(0.25, 0.5, 0.75))
    #     var1 <- Q[2] - Q[1]
    #     var2 <- Q[3] - Q[2]
    #     var <- min(var1, var2)
    #     lim <- c(Q[2] - qf * var, Q[2] + qf * var)
    #     indices <- which(x >= lim[1] & x <= lim[2])
    #     return(list(x = x[indices], x_weight = x_weight[indices]))
    # }
    w_truncate_x <- function(x, x_weight, qf) {
        Q <- wtd.quantile(x, x_weight, c(0.25, 0.5, 0.75))
        var1 <- Q[2] - Q[1]
        var2 <- Q[3] - Q[2]
        var <- min(var1, var2)
        lim <- c(Q[2] - qf * var, Q[2] + qf * var)
        idx <- which(x >= lim[1] & x <= lim[2])
        return(list(x = x[idx], x_weight = x_weight[idx]))
    }
    
    print.progress <- function(x, x_weight, lognormal = FALSE) {
        if (lognormal) {
            x <- exp(x)
        }
        return(c(length(x), min(x), max(x), length(x_weight)))
    }
    
    n.steps <- 5
    for (i in 1:n.steps) {
        target.quantile <- 1 - q.trunc * i / n.steps
        alpha <- (2 * q.trunc) * (i - 1) / n.steps
        qf <- qnorm(target.quantile)/qnorm(0.75 - 0.25 * alpha)
        
        result <- w_truncate_x(x_clean, ww_clean, qf)
        x_clean <- result$x
        ww_clean <- result$x_weight
        progress <- rbind(progress, print.progress(x_clean, ww_clean, lognormal = lognormal))
    }

    n0 <- 1
    n1 <- 0
    # i <- 0
    qf <- qnorm(1 - q.trunc)/qnorm(0.75 - 0.25 * q.trunc)
    while (n0 > n1) {
        i <- i + 1
        n0 <- length(x_clean)
        result <- w_truncate_x(x_clean, ww_clean, qf)
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
    prop <- round(length(x_clean) * 100 / 0.95 / n, 1)
    if (prop > 100) {
        prop <- 100
    }

    if (plot.it) {

        # Select the range of data to be plotted
        # x1 <- x[x < median(x) + 8 * w_IQR(x, x_weight)]
        x1 <- x[x < median(x) + 8 * IQR(x)]
        # Calculate weighted density estimates
        d0 <- density(x1)
        # Determine the scope of the mapping data
        d1 <- data.frame(d0$x, d0$y)
        d <- subset(d1, d1[, 1] >= lim[1] & d1[, 1] <= lim[2])
        # Choosing the right histogram breakpoints based on the amount of data
        if (n < 200) {
            breaks <- "Sturges"
        } else {
        delta <- max(x1) - min(x1)
        breaks <- seq(from = min(x1) - 0.1 * delta, to = max(x1) + 0.1 * delta,
                        by = (lim[2] - lim[1]) / 10)
        }
        # Plotting weighted histograms
        hist(x1, freq = FALSE, breaks = breaks, yaxt = "n", ylim = c(0, max(d[, 2]) * 1.5),
            main = main, xlab = xlab, ylab = "", col = "white", border = "grey")
        # Plotting weighted density curves
        lines(d[, 1], d[, 2], col = "blue", lwd = 2)
        lines(rep(lim[1], 2), c(0, d[1, 2]), col = "blue", lwd = 2) # left border
        lines(rep(lim[2], 2), c(0, d[nrow(d), 2]), col = "blue", lwd = 2)  # right border
        # Adding text labels
        text(lim[1], 0, round(lim[1], digits), pos = 3)
        text(lim[2], 0, round(lim[2], digits), pos = 3)
        # Plotting a weighted box plot
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
#' qq plots are used to verify normality and to compare any two distributions
#' 
#' @importFrom Hmisc wtd.quantile
#' @importFrom reflimR adjust_digits
#' 
#' @export

w_truncated_qqplot <- function(x, x_weight, lognormal = NULL, perc.trunc = 2.5, n.min = 200,
                               apply.rounding = TRUE, plot.it = FALSE, main = "w_Q-Q plot",
                               xlab = "theoretical quantiles", ylab = "sample quantiles") {
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
    p1 <- seq(from = perc.trunc / 100, to = 1 - perc.trunc / 100, length.out = n.quantiles)
    p2 <- seq(from = 0, to = 1, length.out = n.quantiles)
    x.ax <- qnorm(p1) # theoretical quantiles
    y.ax <- wtd.quantile(x_clean, ww_clean, p2) #  sample quantiles
    central.part <- floor(0.05 * n.quantiles) : ceiling(0.95 * n.quantiles)
    reg <- lm(y.ax[central.part] ~ x.ax[central.part])
    a <- reg$coefficients[2]  # Regression Steigung slope
    b <- reg$coefficients[1]  #  Achsenabschnitt intercept
  
    result <- c(b, a, b - 1.96 * a, b + 1.96 * a)
    result <- setNames(result, c("mean", "sd", "lower.lim", "upper.lim"))
  
    if (lognormal) {
        names(result)[1:2] <- paste0(names(result)[1:2], "log") # meanlog  sdlog
        result[1:2] <- round(result[1:2], 3)
        result[3:4] <- exp(result[3:4])
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
            y.pos <- c(50, 100, 150, 200, 300, 400, 500, 1000, 1500) / (10^digits)
            axis(2, at = log(y.pos), labels = y.pos)
        }
        abline(v = 0)
        abline(v = c(-1.96, 1.96), lty = 2)
        abline(h = c(ll, ul),
               col = "green",
               lwd = 2)
        abline(reg$coefficients, lwd = 2, col = "blue")
        points(c(-1.96, 1.96), c(ll, ul), pch = 19, col = "green")
        text(-2, ll, formatC(result[3], digits, format = "f"), pos = 1)
        text(2, ul, formatC(result[4], digits, format = "f"), pos = 3)
      
    }
    return(list(result = result, lognormal = lognormal))
}