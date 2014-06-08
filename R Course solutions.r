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

complete <- function(directory = "specdata" , id = 1:332) {

    comp0 <- function(id0) {
        d <- getmonitor(id0, directory)
        return(nrow(d[complete.cases(d),]))
    }

    return(data.frame("id" = id,"nobs" = array(lapply(id, comp0)))) 
}
## This is just a small variation on the code for corr. Note that I reused getmonitor() in both functions. 