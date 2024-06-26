% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/solve.nonogram.R
\name{solve.nonogram}
\alias{solve.nonogram}
\title{solve.nonogram}
\usage{
\method{solve}{nonogram}(
  nonogram,
  algorithm = "perm_elim",
  row_permutations = NULL,
  column_permutations = NULL,
  row_patterns = NULL,
  column_patterns = NULL,
  verbose = TRUE,
  max_iter = 100,
  parallel = TRUE
)
}
\arguments{
\item{nonogram}{An object of class nonogram.}

\item{algorithm}{Algorithm used to solve the nonogram. Currently only \code{"perm_elim"}
(default) and \code{"force"} algorithms are implemented.}

\item{row_permutations}{A matrix with the same number of columns as there are
rows in the nonogram, and whose rows represent every possible permutation
of 0s and 1s for the number of rows in the nonogram. Default = \code{NULL}.}

\item{column_permutations}{A matrix with the same number of columns as there are
columns in the nonogram, and whose rows represent every possible permutation
of 0s and 1s for the number of columns in the nonogram. Default = \code{NULL}.}

\item{row_patterns}{A list whose elements are the run list encodings of each
column in \code{row_permutations}. Default = \code{NULL}.}

\item{column_patterns}{A list whose elements are the run list encodings of each
column in \code{column_permutations}. Default = \code{NULL}.}

\item{verbose}{If \code{TRUE} (the default), prints progress messages to the console.}

\item{max_iter}{Maximum number of iterations allowed by the solver (default is 100).}

\item{parallel}{If \code{TRUE} (default), the perm_elim algorithm will find initial
row and column solutions in parallel using all available cores.}
}
\value{
A solved nonogram containing the found solution.
}
\description{
Given a valid nonogram object, will attempt to find a solution
that satisfies the row and column patterns.
}
\details{
When \code{algorithm = "perm_elim"}, the permutation elimination algorithm is used.
The permutation elimination algorithm follows the following process:
\enumerate{
\item{For a nonogram with m rows and n columns, if m = n then generate
a single matrix with n columns whose rows represent every possible
permutation of 0s and 1s for a vector of length n. If m != n then
generate two matrices, one containing all possible row permutations
and one containing all possible column permutations.}
\item{Convert each permutation from the full permutation matrix/matrices
into its run length-encoded form. E.g. 000110111101 would become
241 in run length encoding.}
\item{For each row and column of the nonogram, find every permutation in
the full permutation matrix appropriate for that dimension, whose rows
have the same run length encoding as the nonogram's row/column in question.
Set these permutations as the initial solution set for that row/column.
Repeat this process until every row and column has an initial solution set.}
\item{For each row and column of the nonogram, identify any elements across
the solution set that are all 0 or all 1 for all possible permutations.
E.g. if all permutations for column j have a 0 at element k, then eliminate
any solutions in row k that don't have a 0 at element j. Perform this
action for every row and column. A single iteration has passed once
this has been applied to every row and column once.}
\item{Repeat step 4 until only a single solution remains for every row and
column. Return the nonogram with the solution.}
}\preformatted{If the `row_permutations` and `column_permutations` arguments are `NULL`, the 
full permutation sets will first be generated by calling `make_full_perm_set()`
using the appropriate dimension from the nonogram object. If solving multiple
nonograms with the same dimensions, it will be faster to computer the full
permutation sets first, and pass these to the `row_permutations` and
`column_permutations` arguments. If `row_permutations` is not `NULL` but
`column_permutations` is, provided the nonogram is square (same number of rows
and columns), the `row_permutations` argument will also be used for the columns.

Similarly, if the `row_patterns` and `column_patterns` arguments are `NULL`, the 
run length encodings will be calculated from the fullpermutation sets. If solving 
multiple nonograms with the same dimensions, it will be faster to computer the
run length encodings first, and pass these to the `row_patterns` and
`column_patterns` arguments. If `row_patterns` is not `NULL` but
`column_patterns` is, provided the nonogram is square (same number of rows
and columns), the `row_patterns` argument will also be used for the columns.

If `verbose = TRUE` then progress text will be printed to the console. Once
the algorithm begins "Eliminating non-viable permutations", the number of
possible remaining solutions for each column and row will be printed for each
iteration. Only once all the values are 1, has a final solution been converged
upon.

The permutation elimination algorithm will fail to converge on a solution
if the nonogram has a regular checkerboard pattern.

When `algorithm = "force"`, the brute force algorithm is used. The brute force
algorithm follows the following process:  
}

\enumerate{
\item{For an m  n nonogram with s filled squares (the sum of all the patterns
for one dimension), calculate the proportion of filled squares, p, given by
p = s / (m x n).}
\item{Randomly generate an m x n proposal matrix of p 1s and 1 - p 0s.}
\item{Generate run length encodings of all rows and columns of this proposal matrix}
\item{Compare the run length encodings of the proposal matrix to the nonogram
to be solved. If they are identical, stop and return the proposal matrix as
the solution. If not, repeat from (2).}
}\preformatted{As the number of possible solutions is 2^(n x m), the brute force
algorithm is rarely useful beyond very small nonograms. Technically the number
of solutions is smaller than 2^(n x m) when we constrain the proposal
matrix to have the same proportion of 1s as the nonogram to be solved, but
still the number of possible solutions becomes quickly intractable with even
moderately-sized nonograms.
}
}
\examples{
# Without pre-defining full permutation matrices
f <- examples[[31]]
f <- solve(f)

# With pre-defined full permutation matrices
full_perms <- make_full_perm_set(15)
f <- solve(f, row_permutations = full_perms, column_permutations = full_perms)

}
