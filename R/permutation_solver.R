#' permutation_solver
#'
#' @description Given a row or column pattern, a set of candidate patterns,
#'    and the set of all possible permutations of the row/column's length,
#'    `permutation_solver` identifies the set of permutations that satisfy the
#'    pattern TODO: edit this so it takes just the true pattern and the permutations, and include the
#'    rle step inside it!
#' @param pattern 
#' @param permutation_patterns 
#'
#' @return
#' @export
#'
#' @examples
permutation_solver <- function(pattern, permutation_patterns, full_perms) {
  matches <- unlist(lapply(permutation_patterns, function(x) {
    if (length(x) == length(pattern)) {
      all(x == pattern)
    } else
      FALSE
  }))
  
  full_perms[matches, , drop = FALSE]
}
