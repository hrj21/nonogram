#' nonogram
#'
#' @description Constructor function to create an instance of the S3 class nonogram
#' @param rows A list whose elements are numeric vectors indicating the pattern 
#'     within each row (e.g. `c(2, 1)` indicates a row with a run of length 2 
#'     followed by a run of length 1). The first element in the list corresponds
#'     to the top row of the nonogram.
#' @param columns A list whose elements are numeric vectors indicating the pattern 
#'     within each column (e.g. `c(2, 1)` indicates a column with a run of length 2 
#'     followed by a run of length 1). The first element in the list corresponds
#'     to the leftmost column of the nonogram.
#'
#' @return A nonogram object containing the found solution.
#' @export
#'
#' @examples
#' nonogram(
#'     rows    = list(c(1), c(1), c(2, 1), c(2, 1), c(3, 2), c(4, 3), c(5, 4),  c(1),  c(8),  c(6)),
#'     columns = list(c(1), c(2, 1),  c(3, 2),  c(5, 2),  c(7, 2),  c(3),  c(5, 2),  c(3, 2),  c(2, 1),  c(1))
#'    )
nonogram <- function(rows, columns) {
  if(!is.list(rows)) 
    stop("rows must be a list.")
  if(!is.list(columns)) 
    stop("columns must be a list.")
  if(!is.numeric(unlist(rows))) 
    stop("rows must be a list of numeric vectors.")
  if(!is.numeric(unlist(columns))) 
    stop("columns must be a list of numeric vectors.")
  if(sum(unlist(rows)) != sum(unlist(columns))) 
    stop("Rows and columns indicate different numbers of filled squares.")
  if(any(lapply(rows, function(x) sum(unlist(x)) + length(x) - 1) > length(columns))) 
    stop("Over full rows detected. Check the rows are correct")
  if(any(lapply(columns, function(x) sum(unlist(x)) + length(x) - 1) > length(rows))) 
    stop("Over full columns detected. Check the rows are correct")
  
  structure(
    list(
      "rows"            = rows, 
      "columns"         = columns,
      "nrows"           = length(rows),
      "ncolumns"        = length(columns),
      "dims"            = c(length(rows), length(columns)),
      "solved"          = FALSE,
      "row_solution"    = NULL,
      "column_solution" = NULL
    ), 
    class = "nonogram"
  )
}