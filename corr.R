corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        
        ## Return a numeric vector of correlations
        corr <- numeric()
        cmp <- complete(directory)
        
        cmpOverThreshIds <- cmp[cmp$nob > threshold,] 
        if (length(cmpOverThreshIds) > 0) {
                for(id in cmpOverThreshIds$id) {
                        monitorData <- read.csv(paste0(directory, "/", 
                                                       formatC(id, width=3, flag="0"), ".csv"))
                        cmpCases <- complete.cases(monitorData)
                        nitrate <- monitorData[cmpCases,]$nitrate
                        sulfate <- monitorData[cmpCases,]$sulfate
                        monCor <- cor(nitrate, sulfate)
                        corr <- append(corr, monCor)
                }
        }
        corr
}