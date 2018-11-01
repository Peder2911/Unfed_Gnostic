#' Random Vector With Given Sum 
#' 
#' This function returns a vector of integers, with length N, and a sum of M
#'
#' @param N length of vector
#' @param M sum of vector 
#' @param sd standard deviation 
#' @param pos.only Only positive values? 
#' @keywords random 
#' @export
#' @examples
#' v <- rand_vect(10,100) 

randVect <- function(N, M, sd = 1, pos.only = TRUE) {
  vec <- rnorm(N, M/N, sd)
  if (abs(sum(vec)) < 0.01) vec <- vec + 1
  vec <- round(vec / sum(vec) * M)
  deviation <- M - sum(vec)
  for (. in seq_len(abs(deviation))) {
    vec[i] <- vec[i <- sample(N, 1)] + sign(deviation)
  }
  if (pos.only) while (any(vec < 0)) {
    negs <- vec < 0
    pos  <- vec > 0
    vec[negs][i] <- vec[negs][i <- sample(sum(negs), 1)] + 1
    vec[pos][i]  <- vec[pos ][i <- sample(sum(pos ), 1)] - 1
  }
  vec
}
