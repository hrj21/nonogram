#' nonogram
#'
#' @description Constructor function to create an instance of the S3 class nonogram.
#' @param rows A list whose elements are numeric vectors indicating the pattern 
#'     within each row. 
#' @param columns A list whose elements are numeric vectors indicating the pattern 
#'     within each column. 
#'
#' @details Each element of the `rows` and `columns` arguments should be a numeric
#'     vector giving the pattern of runs for that row or column. For example, 
#'     `c(2, 1)` indicates a row or column with a run of length 2 followed by a 
#'     run of length 1, reading from left to right for a row and reading from top 
#'     to bottom for a column. The first element of the list given to `rows` will 
#'     correspond to the top row of the nonogram.The first element of the list 
#'     given to `columns` will correspond to the leftmost column of the nonogram.
#'
#' @return An object of S3 class nonogram, with the following elements:
#' \itemize{
#'  \item{`rows`: }          {The list of row patterns used to construct the nonogram.}
#'  \item{`columns`: }       {The list of column patterns used to construct the nonogram.}
#'  \item{`nrows`: }         {The number of rows.}
#'  \item{`ncolumns`: }      {The number of columns.}
#'  \item{`dims`: }          {A vector of length 2 indicating the number of rows an columns.}
#'  \item{`solved`: }        {Logical flag indicating whether the object contains a solution.}
#'  \item{`row_solution`: }  {List of row solutions.}
#'  \item{`column_solution`: }{List of column solutions.}
#' }
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