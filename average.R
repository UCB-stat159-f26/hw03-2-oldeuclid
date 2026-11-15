#' @title Average
#' @description Computes the average (i.e. mean)
#' @param x input vector
#' @param na.rm whether to remove missing values
#' @return computed mean
#' @export
#' @examples
#' vec <- 1:5
#' average(vec)
#'
#' vec_na <- c(1, 2, NA, 4, 5)
#' average(vec_na)
#' average(vec_na, na.rm = TRUE)
average <- function(x, na.rm = FALSE) {
  # decide whether to remove missing values
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  # compute average
  avg <- sum(x) / length(x)
  return(avg)
}
