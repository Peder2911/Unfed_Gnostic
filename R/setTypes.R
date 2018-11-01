#' Set Types By Vector 
#' 
#' Set the data types of a data frame using a character vector.
#' The vector can be created by sapply(data,class)
#' @param df A data frame
#' @param types A character vector of length ncol(df)
#' @keywords types metaprogramming 
#' @export
#' @importFrom magrittr "%>%"
#' @examples
#' setTypes(mtcars,rep('character',11)) 

setTypes <- function(df,types){
  expressions <- sapply(types,function(x){
    paste('as.',x,sep='')%>%
      parse(text = .)
  })

  i = 1
  for(e in expressions){
    df[[i]] <- eval(e)(df[[i]])
    i <- i + 1
  }
  df
}

