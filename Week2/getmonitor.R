getmonitor <- function(id, directory, summarize = FALSE) {
        ## 'id' is a vector of length 1 indicating the monitor ID
        ## number. The user can specify 'id' as either an integer, a
        ## character, or a numeric.
        
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'summarize' is a logical indicating whether a summary of
        ## the data should be printed to the console; the default is
        ## FALSEdata <- getmonitor(1, "specdata")
        
        ## Your code here
	
	
	id_n <- as.numeric(id)
	if(id_n > 99)
		{filename <- paste(as.character(id_n),".csv",sep = "")}
	else if (id_n > 9 && id_n < 100)
		{filename <- paste("0",as.character(id_n),".csv",sep = "")}
	else
		{filename <- paste("00",as.character(id_n),".csv",sep = "")}
	
		
	file_n <- paste(directory,filename, sep="/")
	if(summarize == TRUE){(print(summary(read.csv(file_n))))
					return(read.csv(file_n))}
	else {return(read.csv(file_n))}
	
	
}