pollutantmean <- function(directory, pollutant, id = 1:332) {
    res <- data.frame()
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
        res <- rbind(res, tmpDF[pollutant]) 
    }
    return(mean(res[,1], na.rm = T))
}
