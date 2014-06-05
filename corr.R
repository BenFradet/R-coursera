source("complete.R")

corr <- function(directory, threshold = 0) {
    res <- c()
    for(i in 1:332) {
        if(complete(directory, i)$nobs[1] > threshold) {
            filename <- ""
            if(i < 10) {
                filename <- paste(directory, "/00", i, ".csv", sep = "")
            } else if(i < 100) {
                filename <- paste(directory, "/0", i, ".csv", sep = "")
            } else {
                filename <- paste(directory, "/", i, ".csv", sep = "")
            }
            df <- read.csv(filename)
            res <- append(res, 
                          cor(df$sulfate, df$nitrate, use = "complete.obs"))
        }
    }
    return(res)
}
