rankall <- function(outcome, num = "best") {
 
  data <- read.csv("outcome-of-care-measures.csv")
  states <- levels(data$State)

  result <- matrix(nrow = 0, ncol = 2)
  for(state in states){
    hospital <- rankhospital(state, outcome, num)
    result <- rbind(result, c(hospital, state))
   
  }

  finalRank <- data.frame(result)
  colnames(finalRank) <- c( 'hospital', 'state')
  finalRank
}