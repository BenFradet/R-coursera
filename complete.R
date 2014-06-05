complete <- function(directory, id = 1:332) {
    res <- data.frame(id = numeric(), nobs = numeric())
    for(i in id) {
        filename <- ""
        if(i < 10) {
            filename <- paste(directory, "/00", i, ".csv", sep = "")
        } else if(i < 100) {
            filename <- paste(directory, "/0", i, ".csv", sep = "")
        } else {
            filename <- paste(directory, "/", i, ".csv", sep = "")
        }
        tmpDF <- read.csv(filename)
        res <- rbind(res, 
                     data.frame(id = i, 
                                nobs = nrow(tmpDF[complete.cases(tmpDF),])))
    }
    return(res)
}
