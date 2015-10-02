rankhospital <- function(state, outcome, num = "best") {
    options(warn=-1)
    data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
    
    if (!(state %in% data$State))
        stop("invalid state")
    if (!(outcome %in% c("heart attack", "heart failure", "pneumonia")))
        stop("invalid outcome")
    
    cols <- if (outcome == "heart attack") c(2, 11)
            else if (outcome == "heart failure") c(2, 17)
            else c(2, 23)
    hospital <- data[data$State == state, cols]
    hospital[, 2] <- as.numeric(hospital[, 2])
    good <- is.na(hospital[, 2]) == F
    hospital <- hospital[good, ]
    hospital.sorted <- hospital[order(hospital[, 2], hospital[, 1]), ]
    if (num == "best") num <- 1
    else if (num == "worst") num <- nrow(hospital.sorted)
    hospital.sorted[num, 1]
}