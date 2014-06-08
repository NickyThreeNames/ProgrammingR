getmonitor <- function(id, directory = "specdata", summarize = FALSE) {
  
  ## 'id' is a vector of length 1 indicating the monitor ID
  ## number. The user can specify 'id' as either an integer, a
  ## character, or a numeric.
  
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'summarize' is a logical indicating whether a summary of
  ## the data should be printed to the console; the default is
  ## FALSE
  
  ## returns a data frame containing that monitor's data)
  
  file_id <- file.path(directory, sprintf("%03d.csv", as.integer(id)))
  data <- read.csv(file_id)
  if(summarize == TRUE){
    print(summary(data))
  }
  return(data)
}
##(a) You need to use print; just "summary(data)" as in the interpreter doesn't work;

##(b) always use the OS independent functions to glue directory and filename together. So I googled and found file.path;

##(c) also googled for the formatting and found sprintf. I knew that one already from C.

##(d) always use return -- even if it is not needed officially. it makes it easy to see where the function ends.