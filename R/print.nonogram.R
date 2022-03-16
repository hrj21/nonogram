#' print.nonogram
#'
#' @description Simply prints the dimensions of the nonogram and whether it has 
#'     been solved. If the nonogram has been solved, the solution matrix is also 
#'     printed.
#' @param nonogram An object of class `nonogram`.
#'
#' @export
#'
#' @examples
#' f <- nonogram(
#'     rows = list(c(1), c(1), c(2, 1), c(2, 1), c(3, 2), c(4, 3), c(5, 4),  c(1),  c(8),  c(6)),
#'     columns = list(c(1), c(2, 1),  c(3, 2),  c(5, 2),  c(7, 2),  c(3),  c(5, 2),  c(3, 2),  c(2, 1),  c(1))
#'    )
#'    
#'print(f)
#'
#'f <- solve(f)
#'
#'print(f)
print.nonogram <- function(nonogram) {
  cat("Nonogram with dimensions: ", nonogram$nrows, " x ", nonogram$ncolumns, sep = "")
  cat("\nSolved: ", nonogram$solved, "\n", sep = "")
  if(nonogram$solved) { 
    print(matrix(unlist(nonogram$column_solution), ncol = nonogram$ncolumns))
  } 
}