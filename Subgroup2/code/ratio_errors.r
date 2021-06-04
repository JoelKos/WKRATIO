###############
#   WKRATIO   #
###############

library(bootstrap)

#' Bootstrap and jackknife for the estimation of the ratio sum(y_i)/sum(x_i), its standard error, and 95 % C.I.
#' @param xdata = dataframe with columns in order x, y
#' @param n.boot = number of bootstrap samples. Default number is 10,000.
#' @return $ratio = sum(y_i)/sum(x_i) minus jackknife estimated bias; $se = jackknife estimate for standard error; $quantiles = 95% C.I. from
#' bootstrap. 
ratio_calc <- function(xdata, n.boot=10000) {
  
  theta <- function(x,xdata){ sum(xdata[x,2])/sum(xdata[x,1]) }
  
  results.boot <- bootstrap(1:length(xdata[,1]),n.boot,theta,xdata)
  results.jack <- jackknife(1:length(xdata[,1]),theta,xdata)
  
  jack.res <- mean(results.jack$jack.values)
  
  out <- list( sum(xdata[,2])/sum(xdata[,1])-(results.jack$jack.bias), results.jack$jack.se, quantile(results.boot$thetastar,c(0.025, 0.975)))
  names(out) <- c("ratio","se","quantiles")
  return(out)
}

# Example with simulated data

test.xdata <- data.frame(x=seq(50,100,1), y = 5*seq(50,100,1)+rnorm(51,0,50))
plot(test.xdata) # Plot of simulated data

ratio_calc(test.xdata) # with 10,000 bootstrap samples
ratio_calc(test.xdata,n.boot=100) # with 100 bootstrap samples
ratio_calc(test.xdata,n.boot=1e5) # with 100,000 bootstrap samples
