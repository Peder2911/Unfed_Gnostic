#' Copy Data Types 
#'
#' Copy types from data frame y, to data frame x
#' @param x "To" data frame
#' @param y "From" data frame
#' @keywords types metaprogramming convenience
#' @export
#' @examples
#' mtchar <- setTypes(mtcars,rep('character',11))
#' mtchar2 <- copyTypes(mtcars,mtchar)

copyTypes <- function(x,y){
	types <- sapply(y,class)
	setTypes(x,types)
}

