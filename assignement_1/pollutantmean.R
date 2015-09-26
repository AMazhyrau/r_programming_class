pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round the result!
  
  #file_path <- paste(getwd(), directory, sep = "/")
  #file_list <- paste(file_path, list.files(file_path, pattern = "*.csv"), sep = "/")
  
  #data_set <- lapply(file_list[id], function(x) read.csv(x, header = TRUE))
  
  source("readDataFromDir.R")
  file_path <- paste(getwd(), directory, sep = "/")
  data_set <- readDataFromDir(file_path, pattern = "*.csv")
  
  pollutant_data <- unlist(lapply(data_set[id], "[", c(pollutant)))
  
  mean(pollutant_data, na.rm = TRUE)
}
