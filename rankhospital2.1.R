rankhospital <- function(state, outcome, num="best"){
  raw <-(read.csv("./outcome-of-care-measures.csv", colClasses = "character"))
  
  outcome_selected <- switch(outcome,
                             "heart attack" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
                             "heart failure" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                             "pneumonia" = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia",
                             stop( "invalid outcome"))
  state_data <- raw[raw$State == state, c(outcome_selected, "Hospital.Name")]
  
  if (nrow(state_data)==0){
    stop("invalid state")
  } else {
    state_data[,2] <- as.numeric(state_data[,2])
    ordered_state_data <- order(state_data[outcome_selected], state_data$Hospital.Name, na.last=NA)
    
    if (num == "best") {
      as.character(state_data$Hospital.Name[ordered_state_data[1]])
    } else if (num == "worst") {
      as.character(state_data$Hospital.Name[ordered_state_data[length(ordered_state_data)]])
    } else if (is.numeric(num)) {
      as.character(state_data$Hospital.Name[ordered_state_data[num]])
    } else {
      stop("invalid num")
    }
  }   
    
    
}