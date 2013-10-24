corr1 <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0

        ## Return a numeric vector of correlations
	corel <- numeric()
	x <- list.files(path = "specdata", pattern = ".csv", full.names = TRUE)
	source("complete.r")
	n <- complete(directory)
		
	
	if(threshold > max(n$nobs))
		{return(as.numeric(NULL))}
	else{
	for (i in 1:332)
	{
		df <- read.csv(x[i])
		df <- df[complete.cases(df), ]
		if(nrow(df) > threshold)
			{corel <- c(corel,cor(df$sulfate,df$nitrate))}
	}
	return(corel)
}
}