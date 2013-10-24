rankhospital <- function(state, outcome, num = "best") {
	
	outcome1 <- read.csv("outcome-of-care-measures.csv", colClasses = "character")## Read outcome data

	disease <- c("heart attack","heart failure","pneumonia")

	us_states <-  levels(factor(outcome1$State))
	
	## Check that state and outcome are valid
	
	if(is.na(match(state,us_states)) == TRUE || is.na(match(outcome,disease)== TRUE))
	
	{	if(is.na(match(state,us_states)) == TRUE){stop("invalid state")}
		else{stop("invalid outcome")}
	}

	else
	{	
	outcome1[ , 11] <- suppressWarnings(as.numeric(outcome1[, which(names(outcome1)== "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack")]))
	outcome1[ , 17] <- suppressWarnings(as.numeric(outcome1[, which(names(outcome1)== "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure")]))
	outcome1[ , 23] <- suppressWarnings(as.numeric(outcome1[, which(names(outcome1)== "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")]))

	hn <- outcome1$Hospital.Name
	s <- outcome1$State
	a <- outcome1$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
	f <- outcome1$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
	p <- outcome1$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia

	data <- data.frame(s,a,f,p,hn)
	df <- subset(data,s == state)
	
	if(outcome == disease[1]){source("sorting.r")
	return(sorting(df$a,df$hn,num))}

	if(outcome == disease[2]){
	source("sorting.r")
	return(sorting(df$f,df$hn,num))}

	if(outcome == disease[3]){source("sorting.r")
	return(sorting(df$p,df$hn,num))}

	
	}
		

## Return hospital name in that state with lowest 30-day death
## rate
}


 


