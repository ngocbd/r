
best <- function(state, outcome) {
  
  df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  cols <- c(11, 17, 23)

  if(state %in% df$State) {
   
      df <- df[df$State == state, ]
      df <- df[order(df[ ,2]), ]
     
      if(outcome == "heart attack") {
        best <- df[which.min(df[, cols[1]]), "Hospital.Name"]
       
      } else if(outcome == "heart failure") {
        best <- df[which.min(df[, cols[2]]), "Hospital.Name"]
      
      } else if(outcome == "pneumonia"){
        best <- df[which.min(df[, cols[3]]), "Hospital.Name"]
      
      }
      else
      {
        stop("invalid outcome")
      }

       return (as.character(best))
      }
        
   
    
   else {
    stop("invalid state")
  }
}