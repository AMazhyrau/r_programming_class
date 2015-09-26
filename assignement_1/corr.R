corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!
  source("readDataFromDir.R")
  file_path <- paste(getwd(), directory, sep = "/")
  data_set <- readDataFromDir(file_path, pattern = "*.csv")
  
  complete_cases <- lapply(data_set, function(x) x[complete.cases(x),])
  correlations <- numeric(0)
  for (i in seq_along(complete_cases)) {
    if (nrow(complete_cases[[i]]) >= threshold) {
      correlations <- c(correlations, cor(complete_cases[[i]]$sulfate, 
                                          complete_cases[[i]]$nitrate))
    }
  }
  correlations
}
