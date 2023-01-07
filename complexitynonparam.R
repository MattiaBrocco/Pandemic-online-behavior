require(sm)
require(gam)
require(fpp2)
require(lmtest)
require(DIMORA)
require(splines)
require(ggplot2)
require(Metrics)
require(forecast)
require(lubridate)
require(MLmetrics)

# SOURCE FOR BIAS FORMULA
# http://rafalab.dfci.harvard.edu/pages/754/section-05.pdf

##LOESS-------
loess.bias.var <- function(dX){
  bias.v <- c()
  var.v <- c()
  mse.v <- c()
  mape.v <- c()
  
  res.v <- c()
  span.i <- c()
  deg.i <- c()
  for (i in seq(0.1, 3, length.out = 300))
  {
    loessMod <- loess(dX~seq(1, length(dX)), span = i, degree = 2)
    
    res <- resid(loessMod)
    bias2 <- sum((dX-mean(loessMod$fitted))^2)/length(dX)
    var <- mean((loessMod$fitted-mean(loessMod$fitted))^2)#/length(dX)
    mse.iter <- rmse(dX, loessMod$fitted) #sum((loessMod$fitted-dX)^2)/length(dX)
    
    p.mape <- abs((dX-loessMod$fitted)/dX)
    p.mape[is.na(p.mape)] <- 0 # predicted and actual are zero
    # actual = 0, pred != 0
    p.mape[is.infinite(p.mape)] <- mean(p.mape[!is.infinite(p.mape)])
    mape.iter <- sum(p.mape)/length(p.mape)
    
    bias.v <- append(bias.v, bias2)
    var.v <- append(var.v, var)
    mse.v <- append(mse.v, mse.iter)
    mape.v <- append(mape.v, mape.iter)
    res.v <- append(res.v, sum(res))
    
    span.i <- append(span.i, i)
  }
  
  out <- cbind(span.i, bias.v, var.v, mse.v, mape.v, res.v)
  out <- data.frame(out)
  names(out) <- c("Span", "Bias", "Variance",
                  "RMSE", "MAPE", "SSR")
  
  best.values <- which(out$RMSE == min(out$RMSE))
  
  y.lim.1 <- min(out[,-c(1, 4, 5)])*0.95
  y.lim.2 <- 1.05*max(out[,-c(1, 4, 5)])
  
  plot(seq(1, dim(out)[1]), out$Variance,
       type = "l", ylim = c(y.lim.1, y.lim.2))
  lines(seq(1, dim(out)[1]), out$Bias, col = 2)
  legend(dim(out)[1]*0.7, y.lim.2*0.25, legend=c("Variance", "Bias"),
         col = c("black", "red"), lty=1:1, cex=0.6)
  abline(v = best.values, lty = 2)
  
  return(out)
}


##LOCAL REGRESSION----
locreg.bias.var <- function(data){
  bias.v <- c()
  var.v <- c()
  error.v <- c()
  mape.v <- c()
  h.i <- c()
  iter.array <- seq.int(1, 1000, 3)
  for (i in iter.array)
    {
    locreg <- sm.regression(data$Time, data$Close,
                            h = i, display = "none")
    
    bias2 <- sum((data$Close-mean(locreg$model.y))^2)/length(data$Time)
    var <- mean((locreg$model.y-mean(locreg$model.y))^2)#/length(data$Time)
    
    interp <- approx(locreg$model.y, method = "linear",
                     rule = 2, n = length(data$Close))
    mse.iter <- rmse(data$Close, interp$y)
    # sum((interp$y-data$Close)^2)/length(data$Close)
    
    p.mape <- abs((data$Close-interp$y)/data$Close)
    p.mape[is.na(p.mape)] <- 0 # predicted and actual are zero
    # actual = 0, pred != 0
    p.mape[is.infinite(p.mape)] <- mean(p.mape[!is.infinite(p.mape)])
    mape.iter <- sum(p.mape)/length(p.mape)
    
    
    bias.v <- append(bias.v, bias2)
    var.v <- append(var.v, var)
    error.v <- append(error.v, mse.iter)
    mape.v <- append(mape.v, mape.iter)
    
    h.i <- append(h.i, i)
  }
  out <- cbind(h.i, bias.v, var.v, error.v, mape.v)
  out <- data.frame(out)
  names(out) <- c("h.param", "Bias", "Variance", "RMSE", "MAPE")
  
  best.values <- which(out$RMSE == min(out$RMSE))
  
  y.lim.1 <- min(out[,-c(1, 4, 5)])*0.95
  y.lim.2 <- 1.05*max(out[,-c(1, 4, 5)])
  
  plot(iter.array, out$Variance,
       type = "l", ylim = c(y.lim.1, y.lim.2))
  lines(iter.array, out$Bias, col = 2)
  legend(dim(out)[1]*0.7, y.lim.2*0.25, legend=c("Variance", "Bias"),
         col = c("black", "red"), lty=1:1, cex=0.6)
  abline(v = best.values, lty = 2)
  
  return(out)
}


##SMOOTHING SPLINE----
sspline.bias.var <- function(data){
  bias.v <- c()
  var.v <- c()
  error.v <- c()
  mape.v <- c()
  lambda.i <- c()
  iter.array <- seq(1e-5, 10, length.out = 300)
  for (i in iter.array)
  {
    ss <- smooth.spline(data$Time, data$Close, lambda = i, cv = 10)
    
    bias2 <- sum((data$Close-mean(ss$y))^2)/length(data$Time)
    var <- mean((ss$y-mean(ss$y))^2)#/length(data$Time)
    
    mse.iter <- rmse(data$Close, ss$y) # sum((ss$y-data$Close)^2)/length(data$Close)
    
    p.mape <- abs((data$Close-ss$y)/data$Close)
    p.mape[is.na(p.mape)] <- 0 # predicted and actual are zero
    # actual = 0, pred != 0
    p.mape[is.infinite(p.mape)] <- mean(p.mape[!is.infinite(p.mape)])
    mape.iter <- sum(p.mape)/length(p.mape)
    # MAPE(ss$y, data$Close)
    
    bias.v <- append(bias.v, bias2)
    var.v <- append(var.v, var)
    error.v <- append(error.v, mse.iter)
    mape.v <- append(mape.v, mape.iter)
    
    lambda.i <- append(lambda.i, i)
  }
  out <- cbind(lambda.i, bias.v, var.v, error.v, mape.v)
  out <- data.frame(out)
  names(out) <- c("lambda", "Bias", "Variance", "RMSE", "MAPE")
  
  best.values <- which(out$RMSE == min(out$RMSE))
  
  y.lim.1 <- min(out[,-c(1, 4, 5)])*0.95
  y.lim.2 <- 1.05*max(out[,-c(1, 4, 5)])
  
  plot(iter.array, out$Variance,
       type = "l", ylim = c(y.lim.1, y.lim.2))
  lines(iter.array, out$Bias, col = 2)
  legend(dim(out)[1]*0.7, y.lim.2*0.25, legend=c("Variance", "Bias"),
         col = c("black", "red"), lty=1:1, cex=0.6)
  abline(v = best.values, lty = 2)
  
  return(out)
}
