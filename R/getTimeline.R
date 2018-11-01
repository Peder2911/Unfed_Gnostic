#' Makes Event Data Into Timeline
#'
#' Turns a dataset containing 1
#' @param df A data frame with columns date,dyad_name,dyad_new_id,deaths,and country.
#' @param start Start date
#' @param end End date 
#' @keywords ged timeline dates 
#' @export
#' @importFrom magrittr "%>%"
#' @examples
#' getTimeline(ged,'1999-01-01','2000-01-01') 

getTimeline<-function(df,startVar,endVar,eventVar,start,end,groupingVar = NULL){

	dfExp <- UnfedGnostic::dateExpand(df,startVar,endVar,eventVar)

	gvName <- eval(groupingVar)

	if(is.null(groupingVar)){
		dfSum <- dfExp %>% dplyr::group_by(date)%>%
			dplyr::summarise(events = sum(eventsDistrib))}
	else{
		dfExp <- dfExp %>% dplyr::rename(gv = groupingVar)
		dfSum <- dfExp %>% dplyr::group_by(date,gv)%>%
			dplyr::summarise(events = sum(eventsDistrib))%>%
			dplyr::rename(gvName = gv)}

	timeRange <- data.frame(date= seq(start,end,by='days'))

	dfOut <- merge(dfSum,timeRange,by='date',all.y=TRUE)%>%
		dplyr::arrange(date)

	dfOut <- tidyr::separate(dfOut,'date',into=c('year','month','day'),remove = FALSE)
	dfOut$week <- lubridate::week(dfOut$date)

	dfOut$events <- ifelse(is.na(dfOut$events),0,dfOut$events)
	dfOut
}


