best <- function(state, outcome) {

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

	df <- data.frame(s,a,f,p,hn)

	if(outcome == disease[1]){ans <- subset(df,s == state)
	ans <- subset(ans,a == min(ans$a,na.rm =TRUE))}

	if(outcome == disease[2]){ans <- subset(df,s == state)
	ans <- subset(ans,f == min(ans$f,na.rm =TRUE))}

	if(outcome == disease[3]){ans <- subset(df,s == state)
	ans <- subset(ans,p == min(ans$p,na.rm =TRUE))}



	}


	hospital_names <- as.character(ans$hn)
	hospital_names <- sort(hospital_names)
	return(hospital_names[1])
	

## Return hospital name in that state with lowest 30-day death
## rate
}


 


