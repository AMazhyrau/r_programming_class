readDataFromDir <- function(file_path, pattern = "*.csv") {
  file_list <- paste(file_path, 
                     list.files(file_path, pattern), 
                     sep = "/")
  
  lapply(file_list, function(x) read.csv(x, header = TRUE))
}
