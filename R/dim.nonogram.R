#' dim.nonogram
#'
#' @description Returns the dimensions of a nonogram object.
#' @param nonogram An object of class nonogram.
#'
#' @return Vector of length 2.
#' @export
#'
#' @examples
#' f <- examples[[31]]
#' dim(f)
dim.nonogram <- function(nonogram) {
  nonogram$dims
}
