rankhospital <- function(state, outcome, num = "best") {
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

    #return hospital name in that state with the given rank 30-day death rate
    hospitals <- split(data, data$State)[[state]]

    if(outcome == "heart attack") {
        hospitals <- hospitals[order(
          hospitals$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,
          hospitals$Hospital.Name, na.last = NA), ]
        if(num == "best") {
            return(hospitals[1, 2])
        } else if(num == "worst") {
            return(hospitals[nrow(hospitals), 2])
        } else if(is.numeric(num)) {
            if(num > nrow(hospitals)) {
               return(NA)
            } else {
                return(hospitals[num, 2])
            }
        } else {
            stop("invalid num")
        }
    } else if(outcome == "heart failure") {
        hospitals <- hospitals[order(
          hospitals$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,
          hospitals$Hospital.Name, na.last = NA), ]
        if(num == "best") {
            return(hospitals[1, 2])
        } else if(num == "worst") {
            return(hospitals[nrow(hospitals), 2])
        } else if(is.numeric(num)) {
            if(num > nrow(hospitals)) {
               return(NA)
            } else {
                return(hospitals[num, 2])
            }
        } else {
            stop("invalid num")
        }
    } else if(outcome == "pneumonia") {
        hospitals <- hospitals[order(
          hospitals$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,
          hospitals$Hospital.Name, na.last = NA), ]
        if(num == "best") {
            return(hospitals[1, 2])
        } else if(num == "worst") {
            return(hospitals[nrow(hospitals), 2])
        } else if(is.numeric(num)) {
            if(num > nrow(hospitals)) {
               return(NA)
            } else {
                return(hospitals[num, 2])
            }
        } else {
            stop("invalid num")
        }
    }
}
