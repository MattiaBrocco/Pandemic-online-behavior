require(sm)
require(gam)
require(fpp2)
require(lmtest)
require(DIMORA)
require(splines)
require(ggplot2)
require(forecast)
require(lubridate)

##LOESS-------
loess.bias.var <- function(dX){
  bias.v <- c()
  var.v <- c()
  error.v <- c()
  res.v <- c()
  span.i <- c()
  deg.i <- c()
  for (i in seq(0.1, 3, length.out = 300))
  {
    loessMod <- loess(dX~seq(1, length(dX)), span = i, degree = 2)
    
    res <- resid(loessMod)
    bias2 <- sum((mean(loessMod$fitted)-dX)^2)/length(dX)
    var <- sum((mean(loessMod$fitted)-loessMod$fitted)^2)/length(dX)
    
    bias.v <- append(bias.v, bias2)
    var.v <- append(var.v, var)
    error.v <- append(error.v, bias2+var)
    res.v <- append(res.v, sum(res))
    
    span.i <- append(span.i, i)
  }
  
  out <- cbind(span.i, bias.v, var.v, error.v, res.v)
  out <- data.frame(out)
  names(out) <- c("Span", "Bias",
                  "Variance", "Total.Error", "SSR")
  
  best.values <- which(out$Total.Error == min(out$Total.Error))
  
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