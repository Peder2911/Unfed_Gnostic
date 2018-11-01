#' Expands Durations Into Separate Dates. 
#'
#' Takes a data set with date-start and date-end, expands rows with duration
#' (eg. different start and end) into separate rows
#' allocates events randomly throughout constitutive dates.
#' Called in getTimeline
#'
#' @param df data frame to transform 
#' @keywords durations dates ged 
#' @export
#' @importFrom magrittr "%>%"
#' @examples
#' expanded <- dateExpand(gedData) 

dateExpand<-function(df,startVar,endVar,eventVar){

	df$uid <- seq(1,nrow(df))
	df$diff <- df[[endVar]] - df[[startVar]]

	# expand data set where there is diff, diff no. of times
	# It repeats diff no. of times per row #
	df <- transform(df[rep(1:nrow(df),df$diff+1),]) # why transform?

	# Counts number of consecutive occurences of same row name (repeated)
	df$consec <- sequence(rle(df[['uid']])$lengths)-1 
	
	# Date is start date plus repetition (consec)
	df$date <- df[[startVar]] + df$consec 

	# This generates a random distribution vector for each "zero-day" row 
	# with length = duration of event.
	# The distribution has the total number of deaths during the event period as its sum.
	# TODO add death substats (eg. deaths_a_distrib) etc.
	
	distrib <- apply(df[which(df$consec == 0),],1,function(x){
		n <- as.numeric(gsub('[^0-9]','',x['diff']))+1
		sum <- as.numeric(x[eventVar])
		vec<-UnfedGnostic::randVect(n,sum)
		})%>%
		unlist()
	df$eventsDistrib <- distrib

	df
}

