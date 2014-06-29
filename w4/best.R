best <- function(state, outcome) {
    #read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character",
                     na.strings = "Not Available")
    data[,11] <- as.numeric(data[,11])
    data[,17] <- as.numeric(data[,17])
    data[,23] <- as.numeric(data[,23])

    #check validity of parameters
    if(!(state %in% data[,7])) {
        stop("invalid state")
    }
    if(!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) {
        stop("invalid outcome")
    }

    #return hospital name in that state with lowest 30-day death rate
    hospitals <- split(data, data$State)[[state]]

    if(outcome == "heart attack") {
        hospitals <- hospitals[
          hospitals$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack ==
            min(hospitals[, 11], na.rm = T), 2]
        hospitals <- sort(hospitals)
        return(hospitals[1])
    } else if(outcome == "heart failure") {
        hospitals <- hospitals[
          hospitals$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure ==
            min(hospitals[, 17], na.rm = T), 2]
        hospitals <- sort(hospitals)
        return(hospitals[1])
    } else if(outcome == "pneumonia") {
        hospitals <- hospitals[
          hospitals$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia ==
            min(hospitals[, 23], na.rm = T), 2]
        hospitals <- sort(hospitals)
        return(hospitals[1])
    }
}
