agecount <- function(age = NULL) {
## Check that "age" is non-NULL; else throw error

	if(is.null(age)==TRUE)
		{ stop("Invalid")	}
	else{
	
	
## Read "homicides.txt" data file

	homicides <- readLines("homicides.txt")

## Extract ages of victims; ignore records where no age is
## given
	
	pattern <- paste(as.character(age),"years")
	num <- length(grep(pattern, homicides, ignore.case=TRUE))	

## Return integer containing count of homicides for that age
	
	return (num)
	
}}