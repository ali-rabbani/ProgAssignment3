
# detects wrong name of the state and returns a msg (with error?)
teststate <- function(x) {
    if(is.na(match(x, unique(outcome$State)))){
        stop("invalid state")
        
    }
    else{print(x)}
}



## if lowest values are more than 1 this will return the first alphabetical hospital
outcome[order(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, 
              outcome$Hospital.Name), ][1, 2]