#' Explodes a timeline of events-per-day into individual events 
#'
#' Explodes a timeline summary into individual events 
#' where each data point accounts for a single occurrence of the event count.
#' Useful for vizualising with geom_freqpoly and other stat_bin based geoms
#'
#' @param tl timeline
#' @keywords dates 
#' @export
#' @importFrom magrittr "%>%"
#' @examples
#' ged <- ged%>%
#'    getTimeline('1999-01-01','2000-01-01')
#'    explodeTimeline('deaths')

explodeTimeline <- function(tl,countVar,dropCountVar = TRUE){

	tl <- tl[tl[[countVar]] > 0,]
	print(nrow(tl))

	types <- sapply(tl,class)

	explodeRow <- function(row,countVar){
		row <- as.list(row)
		nCount <- row[[countVar]]%>%
			as.numeric()
		nCount <- nCount -1
		product <- tibble::as.tibble(row)

		if(nCount > 0){
			for(n in 1:nCount){
				product <- rbind(product,row)
			}
			product
		}
	       	else {
			product
		}
	}

	tl_out <- apply(tl,1,explodeRow,countVar)%>%
		dplyr::bind_rows()%>%
		UnfedGnostic::copyTypes(tl)%>%
		dplyr::filter(countVar > 0)

	if(dropCountVar){
		tl_out <- tl_out[,!names(tl_out) == countVar]
	}
}

