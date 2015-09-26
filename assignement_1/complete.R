complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  ##   file_path <- paste(getwd(), directory, sep = "/")
  ##   
  ##   file_list <- paste(file_path, 
  ##                      list.files(file_path, pattern = "*.csv"), 
  ##                      sep = "/")
  ##   
  ##   data_set <- lapply(file_list[id], function(x) read.csv(x, header = TRUE))
  
  source("readDataFromDir.R")
  file_path <- paste(getwd(), directory, sep = "/")
  data_set <- readDataFromDir(file_path, pattern = "*.csv")
  
  number_of_complete_cases <- unlist(lapply(data_set[id], 
                                     function(x) sum(!is.na(x$sulfate) 
                                                     & !is.na(x$nitrate))))
  
  data.frame(id = id, nobs = number_of_complete_cases)
}
