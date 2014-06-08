##R Programming assignment 3

##rankhospital <- rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
 ## minmort <-  <- min(as.numeric(as.character(dati[,17])))
  
  ##makes sure to kick out the raw <-(read.csv("outcome-of-care-measures.csv", colClasses = "character"))
 ## v
##}


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
	 state_data[,1] <- as.character(state_data[,1])
    ordered_state <- state_data[order(outcome_selected, "Hospital.Name")]
    }
  return(ordered_state[1,1])
	}          

### preserve version with if/else 

bestb <- function(state, outcome){
  raw <-(read.csv("./outcome-of-care-measures.csv", colClasses = "character"))
 
  outcome_selected <- if (outcome == "heart attack"){
						"Column for heart attack"
						} else if (outcome == "heart failure") {
						"column for heart failure"
						} else if (outcome == "pneumonia"){
						"column fro heart failure"
						} else { stop("invalid outcome")}
	state_data <- raw[raw$State == state, c("Hospital Name", outcome_selected)]
		if (nrow(state_data)==0) {
		stop("invalid state")
		} else {
		
     state_data[,2] <- as.numeric(state_data[,2])
	 state_ordered <- order(state_data[column], as.character(state_data$Hospital.Name))
		return (as.character(state_ordered$Hospital.Name[1]))
    }
	}  	