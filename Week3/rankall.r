rankall  <- function(outcome, num = "best") {
	
	outcome1 <- read.csv("outcome-of-care-measures.csv", colClasses = "character")## Read outcome data

	disease <- c("heart attack","heart failure","pneumonia")

	us_states <-  levels(factor(outcome1$State))
	
	## Check that state and outcome are valid
	
	if(is.na(match(outcome,disease)== TRUE))
	
	{	stop("invalid outcome")
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

	state <- numeric()
	hospital <- numeric()

	for(i in 1:length(us_states))
	{
	df <- subset(data,s == us_states[i])
	
	if(outcome == disease[1]){source("sorting.r")
	hospital <- c(hospital,sorting(df$a,df$hn,num))}

	if(outcome == disease[2]){source("sorting.r")
	hospital <- c(hospital,sorting(df$f,df$hn,num))}

	if(outcome == disease[3]){source("sorting.r")
	hospital <- c(hospital,sorting(df$p,df$hn,num))}

	state <- c(state,us_states[i])
	}

	x <- data.frame(hospital,state)
	row.names(x)<- x$state
	return(x)
		
}
## Return hospital name in that state with lowest 30-day death
## rate
}


 


