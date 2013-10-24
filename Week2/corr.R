corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0

        ## Return a numeric vector of correlations
	cc <- complete(directory)
	cc <- cc[complete.cases(cc), ]
	corel <- numeric()	
	ans <- numeric()
	
	for(k in 1:nrow(cc))
		{if (cc$nobs[k] > threshold)
			{ans = c(ans,cc$id[k])}
		}
	
	if(threshold > max(cc$nobs)){return(as.integer(NULL))}
	else{
	for (i in 1:length(ans)) {		
		if(ans[i] > 99)
			{filename <- paste(as.character(ans[i]),".csv",sep = "")}
		else if (ans[i] > 9 && ans[i] < 100)
			{filename <- paste("0",as.character(ans[i]),".csv",sep = "")}
		else
			{filename <- paste("00",as.character(ans[i]),".csv",sep = "")}	

		file_n <- paste(directory,filename, sep="/")		
		my_t <- read.csv(file_n)	
				
		corel <- c(corel,cor(my_t$sulfate,my_t$nitrate,use ="complete.obs"))
	}

	return(corel)
}
}
