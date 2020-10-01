#' Problem 1 - Multiples of 3 and 5
#'
#'
#' @details
#' If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
#'
#' Find the sum of all the multiples of 3 or 5 below 1000.
#'

p1 <- function(n, ...) {
 dots <- as.integer(c(...))
 sets <- purrr::map(dots, function(i) {
   seq(i, n-1L, i)
 })
 sum(sapply(sets, length))
}


