#' Problem 1 - Multiples of 3 and 5
#'
#' This is a solution to \href{https://projecteuler.net/problem=1}{Euler problem 1}.
#'
#' If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
#' Find the sum of all the multiples of 3 or 5 below 1000.
#'
#' @return a double vector
#'
#' @export
#' @examples
#' p1()
#'
p1 <- function() {
  sumFactors(1000, 3, 5)
}

#' @param n a natural number below which (non-inclusive) to sum all multiples of given numbers
#' @param ... natural numbers below `n` that will serve as multiples to sum
#'
#' @details
#' # sumFactors
#'
#' This function generalizes the answer to Project Euler problem #1. It is a simple way of
#' implementing the \href{https://www.wikiwand.com/en/Inclusion%E2%80%93exclusion_principle}{inclusion-exclusion principle}
#' in R. Given any arbitrary length vector of natural numbers (`...`) and a limit (`n`), the sum of all multiples of the
#' provided numbers can be obtained.
#'
#' @describeIn p1 generic function for Project Euler problem #1
#' @export
#' @examples
#' sumFactors(1000, 3, 5)
sumFactors <- function(n, ...) {
 lapply(c(...), function(i) {
   seq(i, n - 1L, i)
 }) %>%
    unlist %>%
    unique %>%
    sum
}




