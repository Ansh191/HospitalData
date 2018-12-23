
rankhospital <- function(state,outcome,num = 'best'){
  decreasing <- F
  data <- read.csv("outcome-of-care-measures.csv", colClasses = 'character', na.string = "Not Available")
  needed <- data.frame(data[,c(2,7,11,17,23)])
  data.split <- split(needed, needed$State)
  states <- data.split[[state]]
  if (num == 'best'){
    num <- 1
  } 
  if (num == 'worst'){
    num <- 1
    decreasing <- T
  }
  if(outcome == "heart attack") {
    ranks <- states[order(as.numeric(states[,3]), states$Hospital.Name, na.last=NA, decreasing = decreasing),]
    if (is.null(ranks[num,1]) == TRUE){
      stop('invalid state')
    } else{
      return(ranks[num,1])
    }
  } else if(outcome == "pneumonia") {
    ranks <- states[order(as.numeric(states[,5]), states$Hospital.Name,na.last=NA, decreasing = decreasing),]
    if (is.null(ranks[num,1]) == TRUE){
      stop('invalid state')
    } else{
      head(ranks)
      return(ranks[num,1])
      
    }
    
  } else if(outcome == "heart failure") {
    ranks <- states[order(as.numeric(states[,4]), states$Hospital.Name,na.last=NA, decreasing = decreasing),]
    if (is.null(ranks[num,1]) == TRUE){
      stop('invalid state')
    } else{
      return(ranks[num,1])
    }
    
  } else {
    stop('invalid outcome')
  }
}