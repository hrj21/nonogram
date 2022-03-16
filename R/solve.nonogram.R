#' solve.nonogram
#'
#' @description Solves nonograms blah blah blah
#' @param nonogram 
#' @param row_permutations 
#' @param column_permutations 
#' @param verbose 
#' @param max_iter 
#'
#' @return
#' @export
#'
#' @examples
solve.nonogram <- function(nonogram, row_permutations = NULL, column_permutations = NULL, verbose = TRUE, max_iter = 100) {
  if(is.null(row_permutations) | is.null(column_permutations)) {
    if(verbose) cat(format(Sys.time(), usetz = TRUE), ": Generating initial possible permutations\n")
    if(nonogram$nrows == nonogram$ncolumns) {
      row_permutations <- column_permutations <- make_full_perm_set(nonogram$ncolumns)
    } else {
      column_permutations <- make_full_perm_set(nonogram$ncolumns)
      row_permutations <- make_full_perm_set(nonogram$nrows)
    }
  }
  
  if(verbose) cat(format(Sys.time(), usetz = TRUE), ": Matching all possible solutions for each row and column\n")
  row_solutions_original <- row_solutions <- lapply(nonogram$rows, permutation_solver, column_permutations)
  column_solutions_original <- column_solutions <- lapply(nonogram$columns, permutation_solver, row_permutations)

  if(verbose) cat(format(Sys.time(), usetz = TRUE), ": Creating empty lists of search vectors\n")
  rows_known <- rep(list(rep(NA, nonogram$ncolumns)), nonogram$nrows)
  columns_known <- rep(list(rep(NA, nonogram$nrows)), nonogram$ncolumns)
  
  if(verbose) cat(format(Sys.time(), usetz = TRUE), ": Eliminating non-viable permutations\n")
  
  iter <- 0
  
  while(
    !(all(sapply(row_solutions, function(x) dim(x)[1]) == 1) &
      all(sapply(column_solutions, function(x) dim(x)[1]) == 1))
  ) {
    
    iter <- iter + 1
    
    if(iter > max_iter) {                        
      stop("Maximum iterations reached.")
    }
    
    eq0 <- lapply(column_solutions, function(x) { # all equal to 0?
      apply(x, 2, function(a) all(a == 0))
    }) 
    
    eq1 <- lapply(column_solutions, function(x) {  # all equal to 1?
      apply(x, 2, function(a) all(a == 1))
    })
    
    for(j in 1:nonogram$ncolumns) {            
      for(i in 1:nonogram$nrows) {
        if(eq0[[j]][i]) {
          rows_known[[i]][j] <- 0
        } else if(eq1[[j]][i]) {
          rows_known[[i]][j] <- 1
        }
      }
    }
    
    eq0 <- lapply(row_solutions, function(x) {
      apply(x, 2, function(a) all(a == 0))
    }) 
    
    eq1 <- lapply(row_solutions, function(x) {
      apply(x, 2, function(a) all(a == 1))
    })
    
    for(j in 1:nonogram$ncolumns) {
      for(i in 1:nonogram$nrows) {
        if(eq0[[j]][i]) {
          columns_known[[i]][j] <- 0
        } else if(eq1[[j]][i]) {
          columns_known[[i]][j] <- 1
        }
      }
    }
    
    column_solutions <- mapply(
      function(x, y) x[apply(x, 1, function(z) return(all(z == y, na.rm = TRUE))), , drop = FALSE],
      column_solutions, columns_known,
      SIMPLIFY = FALSE)
    
    row_solutions <- mapply(
      function(x, y) x[apply(x, 1, function(z) return(all(z == y, na.rm = TRUE))), , drop = FALSE],
      row_solutions, rows_known,
      SIMPLIFY = FALSE
    )
    
    column_solutions <- lapply(1:nonogram$ncolumns, function(x){
      if(dim(column_solutions[[x]])[1] == 0) {
        column_solutions[[x]] <- column_solutions_original[[x]]
      } else column_solutions[[x]] <- column_solutions[[x]]
    })
    
    row_solutions <- lapply(1:nonogram$nrows, function(x){
      if(dim(row_solutions[[x]])[1] == 0) {
        row_solutions[[x]] <- row_solutions_original[[x]]
      } else row_solutions[[x]] <- row_solutions[[x]]
    })
    
    if(verbose) {
      if(verbose) cat(format(Sys.time(), usetz = TRUE), ": Iter", iter, ":\n")
      cat("Columns: ", sapply(column_solutions, function(x) dim(x)[1]), "\n")
      cat("Rows:    ", sapply(row_solutions, function(x) dim(x)[1]), "\n")
    }
  }
  
  output_nonogram <- nonogram
  output_nonogram$row_solution <- row_solutions
  output_nonogram$column_solution <- column_solutions
  output_nonogram$solved <- TRUE
  output_nonogram
}
