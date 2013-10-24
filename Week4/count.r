count <- function(cause = NULL) {
	
	death <- c("asphyxiation", "blunt force", "other","shooting","stabbing", "unknown")	

## Check that "cause" is non-NULL; else throw error
	if (is.null(cause) == TRUE)
		{
			stop("Invalid cause ! ")
		}
	## Check that specific "cause" is allowed; else throw error

	
	else if (length(grep(cause,death,ignore.case=TRUE))== 0)
		{
			stop("Invalid cause ! ")
		}

	else{
## Read "homicides.txt" data file

	homicides <- readLines("homicides.txt")	

## Extract causes of death
	cause <- paste("Cause:",cause)
	num <- length(grep(cause, homicides, ignore.case=TRUE))

## Return integer containing count of homicides for that cause
	
	return (num)
}

}

