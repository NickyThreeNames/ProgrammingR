best <- function(state, outcome){
  raw <-(read.csv("./outcome-of-care-measures.csv", colClasses = "character"))
  
  outcome_selected <- switch(outcome,
                             "heart attack" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
                             "heart failure" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                             "pneumonia" = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia",
                             stop( "invalid outcome"))
  
  
  state_data <- raw[raw$State == state, c("Hospital.Name", outcome_selected)]
  
  if (nrow(state_data)==0){
    stop("invalid state")
  } else {
    
    state_data[,2] <- as.numeric(state_data[,2])
    state_data <- na.omit(state_data)
    min_rate <- min(state_data[,2])
    min_hospital <- state_data[which(state_data[,2] == min_rate), "Hospital.Name"]
  }
  return(min_hospital)
}          