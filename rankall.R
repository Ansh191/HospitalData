rankall <- function(outcome,num = "best"){
  decreasing <- F
  all_states <- c(1)
  data <- read.csv("outcome-of-care-measures.csv", colClasses = 'character', na.string = "Not Available")
  needed <- data.frame(data[,c(2,7,11,17,23)])
  data.split <- split(needed, needed$State)
  
  state_all <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN","MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VI", "VA", "WA", "WV", "WI", "WY", "GU")
  for (i in seq_along(state_all)){
    nam <- paste("name",i,sep = "")
    state <- state_all[i]
    states <- data.split[[state]]
    rate_heart_attack <- states$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
    rate_pneumonia <- states$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
    rate_heart_failure <- states$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
    if (num == 'best'){
      num <- 1
    } 
    if (num == 'worst'){
      num <- 1
      decreasing <- T
    }
    if(outcome == "heart attack") {
      ranks <- states[order(as.numeric(rate_heart_attack),states$Hospital.Name, na.last=NA, decreasing = decreasing),]
      assign(nam, ranks[num,1])
      
    } else if(outcome == "pneumonia") {
      ranks <- states[order(as.numeric(rate_pneumonia),states$Hospital.Name, na.last=NA, decreasing = decreasing),]
      assign(nam, ranks[num,1])
      
    
    } else if(outcome == "heart failure") {
      #new_states <- data.frame(states[,c(1,4)])
      ranks <- states[order(as.numeric(rate_heart_failure),states$Hospital.Name, na.last=NA, decreasing = decreasing),]
      #ranks <- states[sort(new_states,na.last = NA,decreasing = decreasing, method = "shell")]
      assign(nam, c(ranks[num,1]))
      
    
    } else {
      stop('invalid outcome')
    }
  }
  result = data.frame(c(name1,name2,  name3,  name4,  name5,  name6,  name7,  name8,  name9,  name10, name11, name12, name13,
                       name14, name15, name16, name17, name18, name19, name20, name21, name22,name23, name24, name25,name26,
                       name27, name28, name29, name30, name31, name32, name33, name34, name35, name36, name37, name38, name39,
                       name40, name41, name42, name43, name44, name45, name46, name47, name48, name49, name50, name51, name52,
                       name53, name54),state_all)
  colnames(result)<-c("hospital","state")
  result
}
