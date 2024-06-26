
<!-- README.md is generated from README.Rmd. Please edit that file -->

# nonogram

<!-- badges: start -->

[![](https://img.shields.io/badge/devel%20version-0.0.0.9000-blue.svg)](https://github.com/hrj21/nonogram)
[![](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
<!-- badges: end -->

R package for solving black and white nonogram puzzles.

## Installation

You can install the nonogram package from [GitHub](https://github.com/)
with:

``` r
# install.packages("devtools")
devtools::install_github("hrj21/nonogram")
```

## Example of defining, solving, and plotting a simple 3x3 nonogram

The package defines an S3 class `nonogram` used for representing
nonogram puzzles. The `nonogram()` constructor function allows you to
define a nonogram by hand. The `rows` and `columns` arguments should be
lists of numeric vectors representing the patterns in each row and
column (starting with the top row and leftmost column).

``` r
library(nonogram)

non_3x3 <- nonogram(
  rows    = list(c(1, 1), 1, 3),
  columns = list(c(1, 1), 2, c(1, 1))
)

non_3x3
```

There are currently two algorithms implemented for solving nonograms: a
brute force algorithm, and a permutation elimination algorithm. The
brute force algorithm calculates the proportion of filled squares in the
nonogram, and keeps proposing random matrices of 0s and 1s until the row
and column patterns of a proposal match those of the nonogram in
question.

To solve our nono\_3x3 nonogram using the brute force algorithm, we
simply call the `solve()` method, specifying `algorithm = "force"`. You
will need to set the `max_iter` argument higher than the default (10).

``` r
non_3x3 <- solve(non_3x3, algorithm = "force", max_iter = Inf)
# ?solve.nonogram
```

You can plot the solved nongram by passing it to the `plot()` method.

You can also embed plots, for example:

``` r
plot(non_3x3)
# ?plot.nonogram
```

## Example of solving bigger nonograms

As the number of possible solutions is 2^(n x m), the brute force
algorithm is rarely useful beyond very small nonograms. Technically the
number of solutions is smaller than 2^(n x m) when we constrain the
proposal matrix to have the same proportion of 1s as the nonogram to be
solved, but still the number of possible solutions becomes quickly
intractable with even moderately-sized nonograms.

The other algorithm is the permutation elimination algorithm, which
proceeds as follows:

1.  For a nonogram with m rows and n columns, if m = n then generate a
    single matrix with n columns whose rows represent every possible
    permutation of 0s and 1s for a vector of length n. If m != n then
    generate two matrices, one containing all possible row permutations
    and one containing all possible column permutations.
2.  Convert each permutation from the full permutation matrix/matrices
    into its run length-encoded form. E.g. 000110111101 would become 241
    in run length encoding.
3.  For each row and column of the nonogram, find every permutation in
    the full permutation matrix appropriate for that dimension, whose
    rows have the same run length encoding as the nonogram’s row/column
    in question. Set these permutations as the initial solution set for
    that row/column. Repeat this process until every row and column has
    an initial solution set.
4.  For each row and column of the nonogram, identify any elements
    across the solution set that are all 0 or all 1 for all possible
    permutations. E.g. if all permutations for column j have a 0 at
    element k, then eliminate any solutions in row k that don’t have a 0
    at element j. Perform this action for every row and column. A single
    iteration has passed once this has been applied to every row and
    column once.
5.  Repeat step 4 until only a single solution remains for every row and
    column. Return the nonogram with the solution.

Unless you have a specific nonogram you wish to solve or create,
defining nonograms by hand can become a little tedious. Thankfully, the
package comes with the `examples` dataset, which is a list of many
predefined nonograms of various sizes. Let’s pick one to solve.

``` r
data(examples)

ex36 <- examples[[36]]
ex36
plot(ex36, with_solution = FALSE)
```

Let’s solve our 15 x 15 nonogram using the permutation elimination
algorithm (the default). In fact, all the arguments in the call to
`solve` below are the defaults, except the nonogram itself.

``` r
ex36 <- solve(ex36, algorithm = "perm_elim", verbose = TRUE, parallel = TRUE)
plot(ex36)
```

The main issue with this algorithm is that all possible permutations of
0s and 1s for the row and column lengths must be calculated up front.
This quickly consumes a lot of memory and processing power and so this
algorithm doesn’t scale to puzzles larger than 30 x 30.

Even if solving smaller puzzles, if you are solving multiple puzzles of
the same size, it is more efficient to calculate these full permutation
matrices once, rather than separately for each puzzle. Let’s say we wish
to solve puzzles that are 15 x 15, we can calculate the matrix of all
possible permutations of 0 and 1 of length 15 using the function
`make_full_perm_set()`.

``` r
perms15 <- make_full_perm_set(15)
perms15[1:10, ] # just the first 10 rows
dim(perms15)
```

``` r
ex36 <- ex36 |> 
  solve(row_permutations = perms15, column_permutations = perms15) |> 
  plot()
```

## Conclusion

This package does not implement the best or most efficient nonogram
solving algorithms, but you are very welcome to create a pull request to
improve the existing package and implement better algorithms (such as
those discussed at <https://webpbn.com/survey/>). This also isn’t the
only R package for solving nonograms. Please also check out
<https://github.com/coolbutuseless/nonogram>.
