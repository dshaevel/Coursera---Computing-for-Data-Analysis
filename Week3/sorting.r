sorting <- function(disease,hospital,rank_num){
		
	df1 <- data.frame(disease,hospital)
	df <- subset(df1, is.na(df1$disease)==FALSE)

	ranksort <- order(df$disease,df$hospital)	
	sorted_table <- df[ranksort, ]

	rank <- 1:nrow(df)
	disease_rate <- sorted_table$disease
	hospital_name <- sorted_table$hospital
	df <- data.frame(disease_rate,hospital_name,rank)

	if(rank_num == "best"){rank_num <- 1}
	if(rank_num == "worst"){rank_num <- max(df$rank)}
	if(rank_num > max(df$rank)){return(NA)}
	
	answer <- subset(df, rank == rank_num)
	
	return(as.character(answer$hospital_name))


}