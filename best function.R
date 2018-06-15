best <- function(state, outcome){
    
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        ## HOW COULD I DEFINE THE COLCLASSES OF ONLY THESE 3, 4 COLUMNS WITHOUT
        ## BOTHERING ABOUT THE OTHERS
        suppressWarnings(for(i in c(11, 17, 23)){
            data[ , i] <- as.numeric(data[, i])
        }
        )
      
        ##checking if there are invalid names
        
        if(is.na(match(state, unique(data$State)))){
            stop("invalid state")
            
        }
        
        if(is.na(match(outcome, c("heart attack", "heart failure", "pneumonia")))){
            stop("invalid outcome")
            
        }
        
        ##Subset by state
        data1 <- data[data$State == state, ]
        ## Finding the value
        
        if(outcome =="heart attack"){
            data1[order(data1$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, 
                          data1$Hospital.Name), ][1, 2]
        }
        else if(outcome == "heart failure"){
            data1[order(data1$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, 
                       data1$Hospital.Name), ][1, 2]
        }
        else if(outcome == "pneumonia"){
            data1[order(data1$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, 
                       data1$Hospital.Name), ][1, 2]
        }
        ## IS THERE AN EASIER SMARTER WAY OF DOING THIS??
        ## IS THERE A WAY MY ARGUMENT WAS AUTOMATICALLY TRANSLATED INTO ITS LONG COLNAME?
}