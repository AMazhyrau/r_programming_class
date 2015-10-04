# 'rankhospital' function takes three arguments:
# the 2-character abbreviated name of a state (state), an outcome
# (outcome), and the ranking of a hospital in that state for that 
# outcome (num). The function reads the outcome-of-care-measures.csv
# file and returns a character vector with the name of the hospital 
# that has the ranking specified by the num argument. The num argument
# can take values “best”, “worst”, or an integer indicating the ranking
# (smaller numbers are better). If the number given by num is larger 
# than the number of hospitals in that state, then the function should 
# return NA. Hospitals that do not have data on a particular outcome
# should be excluded from the set of hospitals when deciding the rankings.
rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", 
                   colClasses = "character",
                   na.strings = "Not Available")
  
  ## Check that state and outcome are valid
  states <- unique(data[,7])
  if (!state %in% states) stop("invalid state")
  ## Check that state and outcome are valid
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  if (!outcome %in% outcomes) stop("invalid outcome")
  
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
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  desired_data <- 
    data_by_state[order(as.double(data_by_state[[column]]), 
                        data_by_state[["Hospital.Name"]], 
                        decreasing = FALSE, na.last = NA),]
  
  ## check the num value
  ## will automatically return NA if num > nrow
  ## or it's equal other text value
  if (num == "best") {
    num = 1
  } else if (num == "worst") {
    num = nrow(desired_data)
  } 

  desired_data[num, "Hospital.Name"]
}
