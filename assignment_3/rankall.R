# Function 'rankall' that takes two arguments: an outcome name (outcome) 
# and a hospital ranking(num). The function reads the outcome-of-care-measures.csv
# file and returns a 2-column data frame containing the hospital in 
# each state that has the ranking specified in num.
# The function return a value for every state (some may be NA). 
# The first column in the data frame is named hospital, which contains
# the hospital name, and the second column is named state, which contains 
# the 2-character abbreviation for the state name. Hospitals that 
# do not have data on a particular outcome should be excluded from 
# the set of hospitals when deciding the rankings.
rankall <- function(outcome, num = "best") {
  source("rankhospital.R")
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", 
                   colClasses = "character",
                   na.strings = "Not Available")
  
  ## Check that outcome are valid
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  if (!outcome %in% outcomes) stop("invalid outcome")
  
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  state <- levels(factor(data[, "State"]))
  hospital <- vector(mode = "character") 
  
  for (i in seq(state)) {
    hospital[i] <- rankhospital(state[i], outcome, num)
  }
  data.frame(hospital, state)
}
