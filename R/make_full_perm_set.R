#' make_full_perm_set
#'
#' @description Given a length n, `make_full_perm_set` finds all permutations of
#'     length n whose elements can be 0 or 1.
#'     
#' @param length The length of each permutation.
#'
#' @return Returns a matrix with n columns, where each row corresponds to a
#'    different permutation of 0s and 1s.
#' @export
#'
#' @examples
#' # All possible permutations of length 3
#' make_full_perm_set(3)
make_full_perm_set <- function(length) {
  gtools::permutations(2, length, c(0, 1), repeats = TRUE)
}
