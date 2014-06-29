rankall <- function(outcome, num = "best") {
    #read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character",
                     na.strings = "Not Available")
    data[,11] <- as.numeric(data[,11])
    data[,17] <- as.numeric(data[,17])
    data[,23] <- as.numeric(data[,23])
    data[,7] <- as.factor(data[,7])

    #check validity of parameters
    if(!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) {
        stop("invalid outcome")
    }

    #return a data frame with the hospital names and the (abbreviated) state name
    hospitals <- split(data, data$State)
    df <- data.frame(hospital = character(), state = character())
    lev <- levels(data[,7])

    for(i in 1:length(lev)) {
        hospState <- hospitals[[lev[i]]]
        state <- lev[i]
        if(outcome == "heart attack") {
            hospState <- hospState[order(
              hospState$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,
              hospState$Hospital.Name, na.last = NA), ]
            if(num == "best") {
                rank <- hospState[1, 2]
            } else if(num == "worst") {
                rank <- hospState[nrow(hospState), 2]
            } else if(is.numeric(num)) {
                if(num > nrow(hospState)) {
                    rank <- NA
                } else {
                    rank <- hospState[num, 2]
                }
            } else {
                stop("invalid num")
            }
        } else if(outcome == "heart failure") {
            hospState <- hospState[order(
              hospState$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,
              hospState$Hospital.Name, na.last = NA), ]
            if(num == "best") {
                rank <- hospState[1, 2]
            } else if(num == "worst") {
                rank <- hospState[nrow(hospState), 2]
            } else if(is.numeric(num)) {
                if(num > nrow(hospState)) {
                    rank <- NA
                } else {
                    rank <- hospState[num, 2]
                }
            } else {
                stop("invalid num")
            }
        } else if(outcome == "pneumonia") {
            hospState <- hospState[order(
              hospState$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,
              hospState$Hospital.Name, na.last = NA), ]
            if(num == "best") {
                rank <- hospState[1, 2]
            } else if(num == "worst") {
                rank <- hospState[nrow(hospState), 2]
            } else if(is.numeric(num)) {
                if(num > nrow(hospState)) {
                    rank <- NA
                } else {
                    rank <- hospState[num, 2]
                }
            } else {
                stop("invalid num")
            }
        }
        df <- rbind(df, data.frame(hospital = rank, state = state))
    }
    return(df)
}
