% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/problem1.R
\name{p1}
\alias{p1}
\alias{sumFactors}
\title{Problem 1 - Multiples of 3 and 5}
\usage{
p1()

sumFactors(n, ...)
}
\arguments{
\item{n}{a natural number below which (non-inclusive) to sum all multiples of given numbers}

\item{...}{natural numbers below \code{n} that will serve as multiples to sum}
}
\value{
a double vector
}
\description{
This is a solution to \href{https://projecteuler.net/problem=1}{Euler problem 1}.
}
\details{
If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
Find the sum of all the multiples of 3 or 5 below 1000.
}
\section{Functions}{
\itemize{
\item \code{sumFactors}: generic function for Project Euler problem #1
}}

\section{sumFactors}{
This function generalizes the answer to Project Euler problem #1. It is a simple way of
implementing the \href{https://www.wikiwand.com/en/Inclusion\%E2\%80\%93exclusion_principle}{inclusion-exclusion principle}
in R. Given any arbitrary length vector of natural numbers (\code{...}) and a limit (\code{n}), the sum of all multiples of the
provided numbers can be obtained.
}

\examples{
p1()

sumFactors(1000, 3, 5)
}
