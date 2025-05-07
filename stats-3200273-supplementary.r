
library(truncnorm)



#################################################################################################
### Computes the standard Box-Cox transformation.                                             ###
###                                                                                           ###
### Arguments                                                                                 ###
### x          Data to be transformed                                                         ###
### lambda     The parameter of the Box-Cox transformation                                    ###
###                                                                                           ###
### Value      The Box-Cox transformed data                                                   ###
#################################################################################################
box.cox.trans <- function(x,lambda=1){
  if (lambda==0){
    return(log(x))
  }else{
    return((x^lambda - 1)/lambda)
  }
}

### Vectorised version of the Box-Cox transformation
box.cox.trans.v <- Vectorize(box.cox.trans)


#################################################################################################
### Computes the standard inverse Box-Cox transformation.                                     ###
###                                                                                           ###
### Arguments                                                                                 ###
### x          Data to be transformed                                                         ###
### lambda     The parameter of the (inverse) Box-Cox transformation                          ###
###                                                                                           ###
### Value      The inverse Box-Cox transformed data                                           ###
#################################################################################################
box.cox.inv.trans <- function(x,lambda=1){
  if (lambda==0){
    return(exp(x))
  }else{
    return((x*lambda + 1)^(1/lambda))
  }
}



### Vectorised version of the inverse Box-Cox transformation
box.cox.inv.trans.v <- Vectorize(box.cox.inv.trans)



#################################################################################################
### Derivative of the Box-Cox transformation.                                                 ###
###                                                                                           ###
### Arguments                                                                                 ###
### x          Value where the derivative should be computed                                  ###
### lambda     The parameter of the Box-Cox transformation                                    ###
###                                                                                           ###
### Value      Derivative of the Box-Cox transformation at x                                  ###
#################################################################################################
d.box.cox.trans <- function(x,lambda=1){
  if (lambda==0){
    return(1/x)
  }else{
    return(x^(lambda - 1))
  }
}



#################################################################################################
### Derivative of the inverse Box-Cox transformation.                                         ###
###                                                                                           ###
### Arguments                                                                                 ###
### x          Value where the derivative should be computed                                  ###
### lambda     The parameter of the (inverse) Box-Cox transformation                          ###
###                                                                                           ###
### Value      Derivative of the inverse Box-Cox transformation at x                          ###
#################################################################################################
d.box.cox.inv.trans <- function(x,lambda=1){
  if (lambda==0){
    return(exp(x))
  }else{
    return((x*lambda + 1)^((1/lambda) - 1))
  }
}


#################################################################################################
### Density of an inverse Box-Cox and then Box-Cox transformed (truncated) Normal distribution###
###                                                                                           ###
### Arguments                                                                                 ###
### x          Value where the density should be computed                                     ###
### mu         Expectation of the Normal distribution                                         ###
### sigma      Standard deviation of the Normal distribution                                  ###
### lambda     The parameter of the Box-Cox transformation                                    ###
### lambda.inv The parameter of the inverse Box-Cox transformation                            ###
###                                                                                           ###
### Value      Density of the transformed (truncated) Normal distribution at x                ###
#################################################################################################
dens.bcinv.bc <- function(x,mu=5,sigma=1,lambda.inv=1,lambda=1){
  return(dnorm(box.cox.inv.trans(box.cox.trans(x,lambda=lambda.inv),lambda=lambda),mean=mu,sd=sigma)*d.box.cox.trans(box.cox.inv.trans(x,lambda=lambda),lambda=lambda.inv)*d.box.cox.inv.trans(x,lambda=lambda))
}


### Vectorised version of the density of a Box-Cox and then inverse Box-Cox transformed (truncated) Normal distribution 
dens.bcinv.bc.v <- Vectorize(dens.bcinv.bc)



#################################################################################################
### Density of an inverse Box-Cox transformed (truncated) Normal distribution                 ###
###                                                                                           ###
### Arguments                                                                                 ###
### x          Value where the density should be computed                                     ###
### mu         Expectation of the Normal distribution                                         ###
### sigma      Standard deviation of the Normal distribution                                  ###
### lambda     The parameter of the Box-Cox transformation                                    ###
###                                                                                           ###
### Value      Density of the transformed (truncated) Normal distribution at x                ###
#################################################################################################
dens.box.cox.inv <- function(x,mu=5,sigma=1,lambda=1){
  return(dnorm(box.cox.trans(x,lambda=lambda),mean=mu,sd=sigma)*d.box.cox.trans(x,lambda=lambda))
}





#################################################################################################
### Removes outliers from a vector of measured values with values below LOD. It is assumed    ###
### that the values from the healthy subjects follow a normal distribution. It is also        ###
### assumed that the median of the values from the healthy subjects is above LOD.             ###
###                                                                                           ###
### Arguments                                                                                 ###
### measured.values  The measured values above LOD.                                           ###
### lod              LOD                                                                      ###
### n.lod            The number of values below LOD                                           ###
### right.quantile   The (iterated) estimation of the spread is based on the median and       ###
###                  the quantile specified by right.quantile. The default is the third       ###
###                  quartile.                                                                ###
###                                                                                           ###
### Value            A list with the following parameters:                                    ###
### selected.values  The subset of values from measured.values that are assumed to be below   ###
###                  the 97.5%-quantile of the healthy population. If the assumption that the ###
###                  median of the values from the healthy subjects is above LOD is not       ###
###                  fulfilled, NA is returned.                                               ###
### upper.truncation Estimate for the 97.5%-quantile of the healthy population. If the        ###
###                  assumption that the median of the values from the healthy subjects is    ###
###                  above LOD is not fulfilled, NA is returned.                              ###                                            ###               ###
### median           Estimate for the median of the healthy population.                       ###
### lod              LOD as it was the specified in the argument of this function.            ###
### n.lod            The number of values below LOD as it was the specified in the argument   ###
###                  of this function.                                                        ###
#################################################################################################
modTrunc <- function(measured.values,lod,n.lod,right.quantile=0.75){

  n.min.values <- 2*n.lod+1
  quantile.factor <- qnorm(0.975)/qnorm(right.quantile)

  x <- c(rep(lod-1,n.lod),measured.values)
  if (length(x)<n.min.values){
    return(list(selected.values=NA,upper.truncation=NA,median=NA,lod=lod,n.lod=n.lod))
  }
  
  x.qmr <- quantile(x,probs=c(0.5,right.quantile))
  upper.truncation <- x.qmr[1] + (x.qmr[2]-x.qmr[1])*quantile.factor
  
  x.length.old <- length(x)
  x <- x[x<=upper.truncation]
  if (length(x)<n.min.values){
    return(list(selected.values=NA,upper.truncation=NA,median=NA,lod=lod,n.lod=n.lod))
  }
  if (length(x)==x.length.old){
    return(list(selected.values=x[-(1:n.lod)],upper.truncation=upper.truncation,median=x.qmr[1],lod=lod,n.lod=n.lod))
  }
  
  median.mod <- 0.5/0.975
  right.quantile.mod <- right.quantile/0.975
  
  while(length(x)<x.length.old){
    x.length.old <- length(x)
	x.qmr <- quantile(x,probs=c(median.mod,right.quantile.mod))
    upper.truncation <- x.qmr[1] + (x.qmr[2]-x.qmr[1])*quantile.factor
    x <- x[x<=upper.truncation]
	if (length(x)<n.min.values){
      return(list(selected.values=NA,upper.truncation=NA,median=NA,lod=lod,n.lod=n.lod))
	}
  }  
  
  return(list(selected.values=x[-(1:n.lod)],upper.truncation=upper.truncation,median=x.qmr[1],lod=lod,n.lod=n.lod))
  
}


#################################################################################################
### Removes outliers from a vector of measured values with values below LOD and estimates the ###
### reference interval based on a maximum likelihood approach. It is assumed that the values  ###
### from the healthy subjects follow a  normal distribution after the application of          ###
### pre-specified Box-Cox transformation. The default is lambda=0, i.e. it is assumed that    ###
### the values from the healthy subjects followa lognormal distribution. It is also assumed   ###
### that the median of the values from the  healthy subjects is above LOD.                    ###
###                                                                                           ###
### Arguments                                                                                 ###
### measured.values  The measured values above LOD.                                           ###
### lod              LOD                                                                      ###
### n.lod            The number of values below LOD                                           ###
### lambda           The lambda value for the Box-Cox transformation leading to a normal      ###
###                  distribution of the values from the healthy subjects.                    ###
### right.quantile   The (iterated) estimation of the spread is based on the median and       ###
###                  the quantile specified by right.quantile. The default is the third       ###
###                  quartile.                                                                ###
###                                                                                           ###
### Value            A list with the following parameters:                                    ###
### lower.limit      The estimate for the lower limit of the reference interval. If the       ###
###                  assumption that the median of the values from the healthy subjects is    ###
###                  above LOD is not fulfilled, NA is returned.                              ###
### upper.limit      The estimate for the upper limit of the reference interval. If the       ###
###                  assumption that the median of the values from the healthy subjects is    ###
###                  above LOD is not fulfilled, NA is returned.                              ###
### percentile1      The estimate for the 1% quantile of the values from the healthy subjects.###
###                  If the assumption that the median of the values from the healthy         ###
###                  subjects is above LOD is not fulfilled, NA is returned.                  ###
### percentile99     The estimate for the 99% quantile of the values from the healthy         ###
###                  subjects. If the assumption that the median of the values from the       ###
###                  healthy subjects is above LOD is not fulfilled, NA is returned.          ###
### mu.log           The estimate for the expected value of the normal distribution of the    ###
###                  Box-Cox transformed values from the healthy subjects. If the assumption  ###
###                  that the median of the values from the healthy subjects is above LOD is  ###
###                  not fulfilled, NA is returned.                                           ###
### sigma.log        The estimate for the standard deviation of the normal distribution of    ###
###                  the Box-Cox transformed values from the healthy subjects. If the         ###
###                  assumption that the median of the values from the healthy subjects is    ###
###                  above LOD is not fulfilled, NA is returned.                              ###
### upper.truncation Estimate for the 97.5%-quantile of the healthy population. If the        ###
###                  assumption that the median of the values from the healthy subjects is    ###
###                  above LOD is not fulfilled, NA is returned.                              ###
### selected.values  The subset of values from measured.values that are assumed to be below   ###
###                  the 97.5%-quantile of the healthy population. If the assumption that the ###
###                  median of the values from the healthy subjects is above LOD is not       ###
###                  fulfilled, NA is returned.                                               ###
### lod              LOD as it was the specified in the argument of this function.            ###
### n.lod            The number of values below LOD as it was specified in the argument of    ###
###                  this function.                                                           ###
### minus.log.likelihood  Negative log-likelihood 
#################################################################################################
reflimLOD.MLE <- function(measured.values,lod,n.lod,lambda=0,right.quantile=0.75){
  transformed.measured.values <- box.cox.trans(measured.values,lambda=lambda)
  transformed.lod <- box.cox.trans(lod,lambda=lambda)
  
  normal.result <- modTrunc(transformed.measured.values,transformed.lod,n.lod,right.quantile=right.quantile)
  
  if (is.na(normal.result$upper.truncation)){
    return(list(lower.limit=NA,upper.limit=NA,mu.log=NA,sigma.log=NA,upper.truncation=NA,selected.values=NA,lod=lod,n.lod=n.lod))
  }
  
   
  
  obj.fun <- function(pars){
    pnorm.lod <- ptruncnorm(transformed.lod,b=normal.result$upper.truncation,mean=pars[1],sd=pars[2])
	
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


#################################################################################################
### Same as reflimLOD.MLE but replacing the MLE approach by the quantile-based approach.      ###
#################################################################################################
reflimLOD.Quant <- function(measured.values,lod,n.lod,lambda=0,right.quantile=0.75){
  transformed.measured.values <- box.cox.trans(measured.values,lambda=lambda)
  transformed.lod <- box.cox.trans(lod,lambda=lambda)
  
  normal.result <- modTrunc(transformed.measured.values,transformed.lod,n.lod,right.quantile=right.quantile)
  
  if (is.na(normal.result$upper.truncation)){
    return(list(lower.limit=NA,upper.limit=NA,mu.log=NA,sigma.log=NA,upper.truncation=NA,selected.values=NA,lod=lod,n.lod=n.lod))
  }
  
  sigma <- (normal.result$upper.truncation - transformed.lod)/(qnorm(0.975)-qnorm(0.975*n.lod/(length(normal.result$selected.values)+n.lod)))
  mu <- normal.result$upper.truncation - sigma*qnorm(0.975)
  
  lims <- box.cox.inv.trans(qnorm(c(0.025,0.975),mean=mu,sd=sigma),lambda=lambda)
  lims1perc <- box.cox.inv.trans(qnorm(c(0.01,0.99),mean=mu,sd=sigma),lambda=lambda)
  
  return(list(lower.limit=lims[1],upper.limit=lims[2],percentile1=lims1perc[1],percentile99=lims1perc[2],mu.log=unname(mu),sigma.log=unname(sigma),upper.truncation=unname(box.cox.inv.trans(normal.result$upper.truncation,lambda=lambda)),selected.values=box.cox.inv.trans(normal.result$selected.values,lambda=lambda),lod=lod,n.lod=n.lod,lambda=lambda))
}







#################################################################################################
### Similar to reflimLOD.MLE, but computes bootstrap confidence intervals for the             ###
### reference interval limits and 1% and 99% quantile.                                        ###
###                                                                                           ###
### Arguments                                                                                 ###
### measured.values  The measured values above LOD.                                           ###
### lod              LOD                                                                      ###
### n.lod            The number of values below LOD                                           ###
### lambda           The lambda value for the Box-Cox transformation leading to a normal      ###
###                  distribution of the values from the healthy subjects.                    ###
### right.quantile   The (iterated) estimation of the spread is based on the median and       ###
###                  the quantile specified by right.quantile. The default is the third       ###
###                  quartile.                                                                ###
### conf.level       Confidence level for the confidence interval.                            ###
### n.bootstrap      Number of bootstrap samples for the computation of confidence interval.  ###
###                                                                                           ###
### Value               A list with the following parameters:                                 ###
### lower.limit.ci      Confidence interval for the lower limit of the reference interval.    ###
### upper.limit.ci      Confidence interval for the upper limit of the reference interval.    ###
### percentile1.ci      Confidence interval for the 1% quantile of the values from the healthy###
###                     subjects.                                                             ###
### percentile99.ci     Confidence interval for the 1% quantile of the values from the healthy###
###                     subjects.                                                             ###
### mu.log.ci           Confidence interval for the expected value of the normal distribution ###
###                     of the Box-Cox transformed values from the healthy subjects.          ###
### sigma.log.ci        Confidence interval for the standard deviation of the normal          ###
###                     distribution of the Box-Cox transformed values from the healthy       ###
###                     subjects.                                                             ###
### upper.truncation.ci Confidence interval for the upper truncation.                         ###
#################################################################################################
ci.reflimLOD.MLE <- function(measured.values,lod,n.lod,lambda=0,right.quantile=0.75,conf.level=0.95,n.bootstrap=1000){
  lower.limits <- rep(NA,n.bootstrap)
  upper.limits <- rep(NA,n.bootstrap)
  percentile1s <- rep(NA,n.bootstrap)
  percentile99s <- rep(NA,n.bootstrap)
  mu.logs <- rep(NA,n.bootstrap)
  sigma.logs <- rep(NA,n.bootstrap)
  upper.truncations <- rep(NA,n.bootstrap)
  
  x <- c(rep(lod/2,n.lod),measured.values)
  for (i in 1:n.bootstrap){
    xi <- sample(x,length(x),replace=T)
	mvi <- subset(xi,xi>=lod)
	n.lodi <- sum(xi<lod)
	
	resi <- reflimLOD.MLE(mvi,lod,n.lodi,lambda=lambda,right.quantile=right.quantile)
	
	if (!is.na(resi$lower.limit)){
	  lower.limits[i] <- resi$lower.limit
      upper.limits[i] <- resi$upper.limit
	  percentile1s[i] <- resi$percentile1
      percentile99s[i] <- resi$percentile99
      mu.logs[i] <- resi$mu.log
      sigma.logs[i] <- resi$sigma.log
      upper.truncations[i] <- resi$upper.truncation
	}
  }
  
  conf.lims <- c((1-conf.level)/2,1-(1-conf.level)/2)
	
  lower.limit.ci <- quantile(lower.limits,conf.lims,na.rm=T) 
  upper.limit.ci <- quantile(upper.limits,conf.lims,na.rm=T)      
  percentile1.ci <- quantile(percentile1s,conf.lims,na.rm=T)
  percentile99.ci <- quantile(percentile99s,conf.lims,na.rm=T)
  mu.log.ci <- quantile(mu.logs,conf.lims,na.rm=T)
  sigma.log.ci <- quantile(sigma.logs,conf.lims,na.rm=T)
  upper.truncation.ci <- quantile(upper.truncations,conf.lims,na.rm=T)

  return(list(lower.limit.ci=lower.limit.ci,upper.limit.ci=upper.limit.ci,percentile1.ci=percentile1.ci,percentile99.ci=percentile99.ci,mu.log.ci=mu.log.ci,sigma.log.ci=sigma.log.ci,upper.truncation.ci=upper.truncation.ci))
}




#################################################################################################
### Computes the coefficient of determination for the regression line for the QQ-plot         ###
###                                                                                           ###
### Arguments                                                                                 ###
### res.lodiboxplot  Result object returned by the function reflimLOD.MLE                     ###
###                                                                                           ###
### Value            A list with the following parameters:                                    ###
### r.squared        Coefficient of determination for the regression line for the QQ-plot     ###
### adj.r.squared    Adjusted coefficient of determination                                    ###
### upper.truncation.ci Confidence interval for the upper truncation.                         ###
#################################################################################################
compute.r.squared <- function(res.lodiboxplot){
  
  truncated.measured.values <- res.lodiboxplot$selected.values
  n.lod <- res.lodiboxplot$n.lod
  lambda <- res.lodiboxplot$lambda
  transformed.measured.values <- box.cox.trans(truncated.measured.values,lambda=lambda)
    
  n.total <- (length(truncated.measured.values) + n.lod)/0.975
  
  quants.norm <- qnorm((n.lod+(1:length(truncated.measured.values)))/(n.total+1))
  
  quants.dat <- transformed.measured.values[order(transformed.measured.values)]
  
  regline <- lm(quants.dat~quants.norm)
  
  sulm <-summary(regline)
  
  return(list(r.squared=sulm$r.squared,adj.r.squared=sulm$adj.r.squared))
}


#################################################################################################
### Generates a QQ-plot                                                                       ###
###                                                                                           ###
### Arguments                                                                                 ###
### res.lodiboxplot  Result object returned by the function reflimLOD.MLE                     ###
###                  distribution of the values from the healthy subjects.                    ###
### ...              Standard graphics parameters                                             ###
#################################################################################################
lod.qqplot <- function(res.lodiboxplot,pch=16,line.col="red",lwd=2,col.grid=NA,xlab="Theoretical Quantiles",ylab="Transformed Sample Quantiles"){
  
  truncated.measured.values <- res.lodiboxplot$selected.values
  n.lod <- res.lodiboxplot$n.lod
  lambda <- res.lodiboxplot$lambda
  
  transformed.measured.values <- box.cox.trans(truncated.measured.values,lambda=lambda)
  
  n.total <- (length(truncated.measured.values) + n.lod)/0.975
  
  quants.norm <- qnorm((n.lod+(1:length(truncated.measured.values)))/(n.total+1))
  
  quants.dat <- transformed.measured.values[order(transformed.measured.values)]
  plot(quants.norm,quants.dat,pch=pch,xlab=xlab,ylab=ylab)
  if (!is.na(col.grid)){
    grid(col=col.grid)
  }
  if (!is.na(line.col)){
    regline <- lm(quants.dat~quants.norm)
	abline(regline,col=line.col,lwd=lwd)
  }
}


#################################################################################################
### Computes and plots the R-squared values of the QQ-plots for different lambdas.            ###
###                                                                                           ###
### Arguments                                                                                 ###
### measured.values  The measured values above LOD.                                           ###
### lod              LOD                                                                      ###
### n.lod            The number of values below LOD                                           ###
### lambdas          The lambda values for which R-squared should be computed.                ###
### adjusted         Indicates whether the adjusted R-squared values should be used.          ###
### right.quantile.  As in reflimLOD.MLE.                                                     ###
### lwd,ylim,pch     Standard graphics parameters.                                            ###
###                                                                                           ###
### Value            A data frame with two columns: lambda and R-squared values               ###
###                  of this function.                                                        ###
#################################################################################################
plot.r.squared <- function(measured.values,lod,n.lod,lambdas=seq(from=0,to=1,by=0.1),adjusted=F,right.quantile=0.75,lwd=2,ylim=NULL,pch=16,col.grid=NULL){
  rsqs <- rep(-1,length(lambdas))
  strylab <- ""
  if (adjusted){
    strylab <- "Adjusted "	
  }
  for (i in 1:length(lambdas)){
    lbxpi <- reflimLOD.MLE(measured.values,lod,n.lod,lambda=lambdas[i],right.quantile=right.quantile)
	rsqs2 <- compute.r.squared(lbxpi)
	if (adjusted){
	  rsqs[i] <- rsqs2$adj.r.squared
	}else{
	  rsqs[i] <- rsqs2$r.squared
	}
  }
  
  plot(lambdas,rsqs,ylim=ylim,xlab=expression(lambda),ylab=paste0(strylab,"R-squared"),pch=pch)
  if (!is.null(col.grid)){
    grid(col=col.grid)
  }
  points(lambdas,rsqs,type="l",lwd=lwd)
  
  r.squared <- rsqs
  res <- data.frame(lambdas,r.squared) 
  colnames(res)[1] <- "lambda"
  
  return(res)
}



#################################################################################################
### Computes and plots the reference intervals for different lambdas.                         ###
###                                                                                           ###
### Arguments                                                                                 ###
### measured.values  The measured values above LOD.                                           ###
### lod              LOD                                                                      ###
### n.lod            The number of values below LOD                                           ###
### lambdas          The lambda values for which R-squared should be computed.                ###
### adjusted         Indicates whether the adjusted R-squared values should be used.          ###
### right.quantile.  As in reflimLOD.MLE.                                                     ###
### lwd,ylim,pch     Standard graphics parameters.                                            ###
###                                                                                           ###
### Value            A data frame with three columns: lambda, lower and upper reference limit ###
#################################################################################################
plot.reflims <- function(measured.values,lod,n.lod,lambdas=seq(from=0,to=1,by=0.1),right.quantile=0.75,lwd=2,ylim=NULL,ylab="",pch=c(15,16),col=c(2,4),col.grid=NULL){
  lower.limit <- rep(-1,length(lambdas))
  upper.limit <- rep(-1,length(lambdas))
  for (i in 1:length(lambdas)){
    lbxpi <- reflimLOD.MLE(measured.values,lod,n.lod,lambda=lambdas[i],right.quantile=right.quantile)
	lower.limit[i] <- lbxpi$lower.limit
	upper.limit[i] <- lbxpi$upper.limit
  }
  
  ylims <- ylim
  if (is.null(ylim)){
    ylims <- c(min(lower.limit),max(upper.limit))
  }
  plot(lambdas,lower.limit,ylim=ylims,xlab=expression(lambda),ylab=ylab,pch=pch[1],col=col[1])
  if (!is.null(col.grid)){
    grid(col=col.grid)
  }
  points(lambdas,lower.limit,type="l",lwd=lwd,col=col[1])
  points(lambdas,upper.limit,pch=pch[2],col=col[2])
  points(lambdas,upper.limit,type="l",lwd=lwd,col=col[2])
  
  res <- data.frame(lambdas,lower.limit,upper.limit) 
  colnames(res)[1] <- "lambda"
  
  return(res)
}




#################################################################################################
### Plots a histogram of the selected measured values and the estimated density curve.        ###
###                                                                                           ###
### Arguments                                                                                 ###
### res.lodiboxplot  Result object returned by the function reflimLOD.MLE                     ###
### xlab,...         Standard graphics parameters for the histogram and the density curve.    ###
#################################################################################################
lod.hist <- function(res.lodiboxplot,xlab="",ylab="Frequency",main="",lwd=2,col.curve=2,breaks=10,col.grid=NULL){
  tmv <- res.lodiboxplot$selected.values
  lambda <- res.lodiboxplot$lambda
  mybreaks <- breaks
  if (is.null(breaks)){
    mybreaks <- 10
  }
  if (length(mybreaks)==1){
    mybreaks <- seq(from=min(tmv),to=max(tmv),length.out=mybreaks+1)
  }else{
    mybreaks <- seq(from=min(c(res.lodiboxplot$lod,min(breaks))),to=max(c(tmv,max(breaks))),length.out=length.breaks)
  }
  
  area <- (length(tmv)+res.lodiboxplot$n.lod)*(mybreaks[2]-mybreaks[1])
  hist(tmv,breaks=mybreaks,xlab=xlab,ylab=ylab,main=main)
  if (!is.null(col.grid)){
    grid(col=col.grid)
  }
  curve(area*dens.box.cox.inv(x,mu=res.lodiboxplot$mu.log,sigma=res.lodiboxplot$sigma.log,lambda=lambda),add=T,lwd=lwd,col=col.curve)
  
}
   

#################################################################################################
### Computes the MLE for mu and sigma of a normal distribution truncated to the interval      ###
### [a,b].                                                                                    ###
###                                                                                           ###
### Arguments                                                                                 ###
### x   The sample from the truncated normal distribution                                     ###
### a   Lower truncation                                                                      ###
### b   Upper truncation                                                                      ###
###                                                                                           ###
### Value                                                                                     ###
### A list with mu and sigma as the corresponding estimates                                   ###
#################################################################################################
fit.trunc.norm <- function(x,a=-Inf,b=Inf){
  if (sum(x<a)>0 | sum(x>b)>0){
    return(NULL)
  }else{
    obj.fun <- function(pars){
      return(-sum(log(dtruncnorm(x,a=a,b=b,mean=pars[1],sd=pars[2]))))
    }
	
	pars.initial <- c(median(x),sd(x))
    optim.result <- optim(pars.initial,obj.fun)
	return(list(mu=optim.result$par[1],sigma=optim.result$par[2]))
  }
}



#################################################################################################
### Generates an artificial data set from a standard normal distribution for the healthy      ###
### population and a values from a pathological population with expected value mu and         ###
### standard deviation 1.                                                                     ###
###                                                                                           ###
### Arguments                                                                                 ###
### n                  Sample size                                                            ###
### prop.lod.healthy   (Expected) Proportion of values below LOD                              ###
### prop.path          Proportion of values from the pathological population                  ###
### mu.path            Expected value of the normal distribution from the path. population    ###
###                                                                                           ###
### Value                                                                                     ###
### A list with the measured values above LOD, the number of values below LOD and the         ###
### based on the specified desired proportion of values below LOD in the healthy population   ###
#################################################################################################
lod.artificial.sample <- function(n,prop.lod.healthy=0,prop.path=0,mu.path=3){

  n.path <- round(n*prop.path)
  n.healthy <- n - n.path
  lod <- qnorm(prop.lod.healthy)
  
  x.all <- c(rnorm(n.healthy),rnorm(n.path,mean=mu.path))
  
  n.lod <- sum(x.all<lod)
  
  x <- x.all[x.all>=lod]
  
  return(list(measured.values=x,n.lod=n.lod,lod=lod))
  
}


#########################################################################################
#########################################################################################
#################################   Example   ###########################################
#########################################################################################
#########################################################################################


# ### Generate a synthetic data set
# sim.ex <- lod.artificial.sample(1000,prop.lod.healthy=0.25,prop.path=0.2,mu.path=5)
# ### Transform to a log-normal distribution
# sim.ex$measured.values <- exp(sim.ex$measured.values)
# sim.ex$lod <- exp(sim.ex$lod)
# 
# 
# ### Example for reflimLOD.MLE
# result.MLE <- reflimLOD.MLE(sim.ex$measured.values,sim.ex$lod,sim.ex$n.lod,lambda=0)
# print(result.MLE$lower.limit)
# print(result.MLE$upper.limit)
# 
# ### Example for reflimLOD.Quant
# result.quant <- reflimLOD.Quant(sim.ex$measured.values,sim.ex$lod,sim.ex$n.lod,lambda=0)
# print(result.quant$lower.limit)
# print(result.quant$upper.limit)
# 
# 
# 
# 
# ### Plot a histogram of the values after truncation including the fitted density.
# lod.hist(result.MLE,xlab="TNT male",col.grid=1)
# 
# ### QQ-plot (for the transformed trunated values. Here no transformatiion is required.)
# lod.qqplot(result.MLE,col.grid=1)
# 
# ### Compute 95% confidence intervals for the reference limits.
# cis <- ci.reflimLOD.MLE(sim.ex$measured.values,sim.ex$lod,sim.ex$n.lod,lambda=0)  
# print(cis$lower.limit.ci)
# print(cis$upper.limit.ci)
# 
# 
# 
# ### Plot R-squared values for different lambdas
# plot.r.squared(sim.ex$measured.values,sim.ex$lod,sim.ex$n.lod,col.grid=1)
# 
# ### Plot the reference intervals for different lambdas
# plot.reflims(sim.ex$measured.values,sim.ex$lod,sim.ex$n.lod,col.grid=1)




