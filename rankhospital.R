rankhospital <- function(state, outcome, num = "best"){
    
    temp <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = F)
    data <- temp[ , c(2, 7, 11, 17, 23)]
    colnames(data) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
 
    suppressWarnings(for(i in c(3, 4, 5)){
        data[ , i] <- as.numeric(data[, i])})
    
    if(is.na(match(state, unique(data$state)))){
        stop("invalid state")}
    
    if(is.na(match(outcome, c("heart attack", "heart failure", "pneumonia")))){
        stop("invalid outcome")}
    
    data1 <- data[data$state == state, ]
    

    if(num == "best"){
        data1[order(data1[[outcome]], data1$"hospital"), ][1, 1]}
    else if(num == "worst"){
        data1[order(-data1[[outcome]], data1$"hospital"), ][1, 1]}
    else {data1[order(data1[[outcome]], data1$"hospital"), ][num, 1]}
    }
    
