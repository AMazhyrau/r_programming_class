## Function for finding the best hospital in state
## The function reads the outcome-of-care-measures.csv file
## and returns a character vector with the name of the hospital
## that has the best (i.e. lowest) 30-day mortality for 
## the specified outcome in that state.
## The hospital name is the name provided in the Hospital.Name variable.
## The outcomes can be one of “heart attack”, “heart failure”,
## or “pneumonia”. Hospitals that do not have data on a particular
## outcome should be excluded from the set of hospitals when 
## deciding the rankings.

## take two arguments: the 2-character abbreviated name of a state
## and an outcome name.
best <- function(state, outcome) {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", 
                    colClasses = "character",
                    na.strings = "Not Available")
  
  ## Check that state and outcome are valid
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  if (!outcome %in% outcomes) stop("invalid outcome")
  
  states <- unique(data[,7])
  if (!state %in% states) stop("invalid state")
  
  ## create subset for current state
  data_by_state <- subset(data, State == state)
  
  ## Convert the outcome to collumn number
  ## I think it's better to use 'match' function
  ## instead if-else statement
  target_columns_from_table <- 
    c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
      "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
      "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  
  column <- target_columns_from_table[match(outcome, outcomes)]
  
  ## Remove rows with Na values for target collumn and state
  
  data_by_state <- 
    data_by_state[complete.cases(data_by_state[,column]),]
  
  ## Find and Return hospital name in that state with lowest 30-day 
  ## death rate
  column_values <- as.double(data_by_state[, column])
  rows <- which(column_values == min(column_values))
  best_hospitals <- data_by_state[rows, "Hospital.Name"]
  
  if (length(best_hospitals) > 1) {
    hospitals_sorted <- sort(best_hospitals)
    hospitals_sorted[1]
  } else {
    best_hospitals
  }
}

