
best <- function(state, outcome) {
  data <- read.csv("outcome-of-care-measures.csv", colClasses = 'character', na.string = "Not Available")
  needed <- data.frame(data[,c(2,7,11,17,23)])
  data.split <- split(needed, needed$State)
  states <- data.split[[state]]
  if(outcome == "heart attack") {
    ranks <- states[order(as.numeric(states$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),states$Hospital.Name, na.last=NA),]
    if (is.null(ranks[1,1]) == TRUE){
      stop('invalid state')
    } else{
      return(ranks[1,1])
    }
  } else if(outcome == "pneumonia") {
    ranks <- states[order(as.numeric(states$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),states$Hospital.Name, na.last=NA),]
    if (is.null(ranks[1,1]) == TRUE){
      stop('invalid state')
    } else{
      return(ranks[1,1])
    }
  } else if(outcome == "heart failure") {
    ranks <- states[order(as.numeric(states[,4]),states$Hospital.Name, na.last=NA),]
    if (is.null(ranks[1,1]) == TRUE){
      stop('invalid state')
    } else{
      return(ranks[1,1])
    }
  } else {
    stop('invalid outcome')
  }
  
}






