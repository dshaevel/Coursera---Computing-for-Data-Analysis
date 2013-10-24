complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases
	
	
	nobs <- numeric()

	for (i in 1:length(id)) {
	if(id[i] > 99)
		{filename <- paste(as.character(id[i]),".csv",sep = "")}
	else if (id[i] > 9 && id[i] < 100)
		{filename <- paste("0",as.character(id[i]),".csv",sep = "")}
	else
		{filename <- paste("00",as.character(id[i]),".csv",sep = "")}
	
	file_n <- paste(directory,filename, sep="/")	
	my_t <- read.csv(file_n)	
	c <- complete.cases(my_t)
	c1 <- as.data.frame(table(c))
	
	nobs = c(nobs,c1[2,2])
	}
	data.frame(id,nobs)
}