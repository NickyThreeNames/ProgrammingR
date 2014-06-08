complete <- function(directory = "specdata" , id = 1:332) {
  
  comp0 <- function(id0) {
    d <- getmonitor(id0, directory)
    return(nrow(d[complete.cases(d),]))
  }
  
  return(data.frame("id" = id,"nobs" = array(lapply(id, comp0)))) 
}