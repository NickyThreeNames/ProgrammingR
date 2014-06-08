corr <- function(directory = "specdata", threshold = 0, id=1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  ## (local) function for values of single monitor
  comp0 <- function(id0) {
    d <- getmonitor(id0, directory)
    nobs <- nrow(d[complete.cases(d),])
    # now calculate correllation (sulfate, nitrate)
    correl <- cor(d$sulfate, d$nitrate, use = "pairwise.complete.obs")
    return(c(nobs, correl))
  }
  
  data = t(data.frame(list(lapply(id, comp0))))
  nobs= data[,1]
  correl = data[,2]
  return(as.vector(correl[nobs > threshold]))
}
##(a) lapply returns a list of 2-element vectors, so the fucntion t() is needed to transpose it.

##(b) didn't know about local functions, Tried it and it worked.

##(c) this one is easy when you have heard about functions on vectors (google for map / reduce)
