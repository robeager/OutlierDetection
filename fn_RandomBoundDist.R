#' Creates a random normal distribution within the specified bounds
#' 
#' WARNING: This function does not preserve the standard deviation
#' @param n The number of values to be generated
#' @param mean The mean of the distribution
#' @param sd The standard deviation of the distribution
#' @param lower The lower limit of the distribution
#' @param upper The upper limit of the distribution
rtnorm <- function(n, mean = 0, sd = 1, lower = -1, upper = 1){
  mean = ifelse(test = (is.na(mean)|| (mean < lower) || (mean > upper)),
                yes = mean(c(lower, upper)),
                no = mean)
  data <- rnorm(n, mean = mean, sd = sd) # data
  
  if (!is.na(lower) && !is.na(upper)){ # adjust data to specified range
    drange <- range(data)            # data range
    irange <- range(lower, upper)    # input range
    data <- (data - drange[1]) / (drange[2] - drange[1]) # normalize data (make it 0 to 1)
    data <- (data * (irange[2] - irange[1])) + irange[1] # adjust to specified range
  }
  return(data)
}