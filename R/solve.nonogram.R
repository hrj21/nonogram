#' solve.nonogram
#'
#' @description Given a valid nonogram object, will attempt to find a solution 
#'     that satisfies the row and column patterns.
#' @param nonogram An object of class nonogram.
#' @param algorithm Algorithm used to solve the nonogram. Currently only `"perm_elim"`
#'     (default) and `"force"` algorithms are implemented.
#' @param row_permutations A matrix with the same number of columns as there are 
#'     rows in the nonogram, and whose rows represent every possible permutation
#'     of 0s and 1s for the number of rows in the nonogram. Default = `NULL`.
#' @param column_permutations A matrix with the same number of columns as there are 
#'     columns in the nonogram, and whose rows represent every possible permutation
#'     of 0s and 1s for the number of columns in the nonogram. Default = `NULL`.
#' @param row_patterns A list whose elements are the run list encodings of each
#'     column in `row_permutations`. Default = `NULL`.
#' @param column_patterns A list whose elements are the run list encodings of each
#'     column in `column_permutations`. Default = `NULL`.
#' @param verbose If `TRUE` (the default), prints progress messages to the console.
#' @param max_iter Maximum number of iterations allowed by the solver (default is 100).
#' @param parallel If `TRUE` (default), the perm_elim algorithm will find initial
#'     row and column solutions in parallel using all available cores.
#'
#' @return A solved nonogram containing the found solution.
#' 
#' @details When `algorithm = "perm_elim"`, the permutation elimination algorithm is used.
#'     The permutation elimination algorithm follows the following process:
#'    \enumerate{
#'        \item{For a nonogram with m rows and n columns, if m = n then generate 
#'            a single matrix with n columns whose rows represent every possible 
#'            permutation of 0s and 1s for a vector of length n. If m != n then 
#'            generate two matrices, one containing all possible row permutations
#'            and one containing all possible column permutations.}
#'        \item{Convert each permutation from the full permutation matrix/matrices
#'            into its run length-encoded form. E.g. 000110111101 would become
#'            241 in run length encoding.}
#'        \item{For each row and column of the nonogram, find every permutation in
#'            the full permutation matrix appropriate for that dimension, whose rows 
#'            have the same run length encoding as the nonogram's row/column in question.
#'            Set these permutations as the initial solution set for that row/column.
#'            Repeat this process until every row and column has an initial solution set.}
#'        \item{For each row and column of the nonogram, identify any elements across
#'            the solution set that are all 0 or all 1 for all possible permutations.
#'            E.g. if all permutations for column j have a 0 at element k, then eliminate 
#'            any solutions in row k that don't have a 0 at element j. Perform this 
#'            action for every row and column. A single iteration has passed once
#'            this has been applied to every row and column once.}
#'        \item{Repeat step 4 until only a single solution remains for every row and
#'            column. Return the nonogram with the solution.}
#'    }
#' 
#'     If the `row_permutations` and `column_permutations` arguments are `NULL`, the 
#'     full permutation sets will first be generated by calling `make_full_perm_set()`
#'     using the appropriate dimension from the nonogram object. If solving multiple
#'     nonograms with the same dimensions, it will be faster to computer the full
#'     permutation sets first, and pass these to the `row_permutations` and
#'     `column_permutations` arguments. If `row_permutations` is not `NULL` but
#'     `column_permutations` is, provided the nonogram is square (same number of rows
#'     and columns), the `row_permutations` argument will also be used for the columns.
#'     
#'     Similarly, if the `row_patterns` and `column_patterns` arguments are `NULL`, the 
#'     run length encodings will be calculated from the fullpermutation sets. If solving 
#'     multiple nonograms with the same dimensions, it will be faster to computer the
#'     run length encodings first, and pass these to the `row_patterns` and
#'     `column_patterns` arguments. If `row_patterns` is not `NULL` but
#'     `column_patterns` is, provided the nonogram is square (same number of rows
#'     and columns), the `row_patterns` argument will also be used for the columns.
#'     
#'     If `verbose = TRUE` then progress text will be printed to the console. Once
#'     the algorithm begins "Eliminating non-viable permutations", the number of
#'     possible remaining solutions for each column and row will be printed for each
#'     iteration. Only once all the values are 1, has a final solution been converged
#'     upon.
#'     
#'     The permutation elimination algorithm will fail to converge on a solution
#'     if the nonogram has a regular checkerboard pattern.
#'     
#'     When `algorithm = "force"`, the brute force algorithm is used. The brute force
#'     algorithm follows the following process:  
#'     
#'    \enumerate{
#'        \item{For an m  n nonogram with s filled squares (the sum of all the patterns 
#'            for one dimension), calculate the proportion of filled squares, p, given by 
#'            p = s / (m x n).}
#'        \item{Randomly generate an m x n proposal matrix of p 1s and 1 - p 0s.}
#'        \item{Generate run length encodings of all rows and columns of this proposal matrix}
#'        \item{Compare the run length encodings of the proposal matrix to the nonogram
#'            to be solved. If they are identical, stop and return the proposal matrix as 
#'            the solution. If not, repeat from (2).}     
#'    }
#'     
#'     As the number of possible solutions is 2^(n x m), the brute force
#'     algorithm is rarely useful beyond very small nonograms. Technically the number
#'     of solutions is smaller than 2^(n x m) when we constrain the proposal
#'     matrix to have the same proportion of 1s as the nonogram to be solved, but
#'     still the number of possible solutions becomes quickly intractable with even
#'     moderately-sized nonograms.
#'     
#' @export
#'
#' @examples
#' # Without pre-defining full permutation matrices
#' f <- examples[[31]]
#' f <- solve(f)
#' 
#' # With pre-defined full permutation matrices
#' full_perms <- make_full_perm_set(15)
#' f <- solve(f, row_permutations = full_perms, column_permutations = full_perms)
#' 
solve.nonogram <- function(nonogram, algorithm = "perm_elim", row_permutations = NULL, 
                           column_permutations = NULL, row_patterns = NULL, column_patterns = NULL,
                           verbose = TRUE, max_iter = 100, parallel = TRUE) {
  
  handlers("progress")
  
  if(algorithm == "perm_elim") {
    
    if(parallel) plan(multisession) else plan(sequential)
    handlers(global = verbose)
    
    if(is.null(row_permutations)) {
      if(verbose) cat(format(Sys.time(), usetz = TRUE), ": Generating initial possible row permutations\n")
      row_permutations <- make_full_perm_set(nonogram$nrows)
    } 
    if(is.null(column_permutations)){
      if(nonogram$nrows == nonogram$ncolumns) {
        column_permutations <- row_permutations
      } else {
        if(verbose) cat(format(Sys.time(), usetz = TRUE), ": Generating initial possible column permutations\n")
        column_permutations <- make_full_perm_set(nonogram$ncolumns)
      }
    } 
    if(is.null(row_patterns)) {
      if(verbose) cat(format(Sys.time(), usetz = TRUE), ": Generating initial possible row patterns\n")
      row_patterns <- future_apply(row_permutations, 1, function(x){
        pat <- rle(x)$lengths[rle(x)$values==1]
        if(length(pat) == 0) {
          0
        } else pat
      })
    }
    if(is.null(column_patterns)) {
      if(nonogram$nrows == nonogram$ncolumns) {
        column_patterns <- row_patterns
      } else {
        if(verbose) cat(format(Sys.time(), usetz = TRUE), ": Generating initial possible column patterns\n")
        column_patterns <- future_apply(column_permutations, 1, function(x){
          pat <- rle(x)$lengths[rle(x)$values==1]
          if(length(pat) == 0) {
            0
            } else pat
        })
      }
    }
      
    if(ncol(row_permutations) != nonogram$nrows) {
      stop("The row_permutations argument has the incorrect number of columns for the nonogram.")
    } 
    if(ncol(column_permutations) != nonogram$ncolumns) {
      stop("The column_permutations argument has the incorrect number of columns for the nonogram.")
    }
    
    if(verbose) cat(format(Sys.time(), usetz = TRUE), ": Matching all possible starting solutions for each row\n")
    
    perm_solve_with_progress <- function(pattern) {
      p <- progressor(along = pattern)
      y <- future_lapply(pattern, function(pattern, ...) {
        p()
        permutation_solver(
          pattern,
          permutation_patterns = column_permutations, 
          full_patterns = column_patterns,
          verbose = verbose
        )
      })
    }
    
    row_solutions_original <- row_solutions <- 
      perm_solve_with_progress(nonogram$rows)
    
    if(verbose) cat(format(Sys.time(), usetz = TRUE), ": Matching all possible starting solutions for each column\n")
    
    column_solutions_original <- column_solutions <-
      perm_solve_with_progress(nonogram$columns)
    
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
  }
  if(algorithm == "force") {
    
    n <- sum(unlist(nonogram$rows))
    size <- prod(dim(nonogram))
    prob <- n / size
    
    proposal_mat <- matrix(rbinom(size, 1, prob), ncol = dim(nonogram)[2])
    row_solutions <- apply(proposal_mat, 1, function(x) rle(x)$lengths[rle(x)$values==1])
    column_solutions <- apply(proposal_mat, 2, function(x) rle(x)$lengths[rle(x)$values==1])
    row_rle <- apply(proposal_mat, 1, function(x) rle(x)$lengths[rle(x)$values==1])
    column_rle <- apply(proposal_mat, 2, function(x) rle(x)$lengths[rle(x)$values==1])
    
    iter <- 0
    
    suppressWarnings({
    while(!(all(unlist(nonogram$rows) == unlist(row_rle)) & 
            all(unlist(nonogram$columns) == unlist(column_rle)))) {
      
      iter <- iter + 1
      
      if(iter > max_iter) {                        
        stop("Maximum iterations reached.")
      }
      
      if(verbose & iter %% 100 == 0) cat(format(Sys.time(), usetz = TRUE), ": Iteration ", iter, "\n")
      
      proposal_mat <- matrix(rbinom(size, 1, prob), ncol = dim(nonogram)[2])
      row_solutions <- apply(proposal_mat, 1, matrix, nrow = 1, simplify = FALSE)
      column_solutions <- apply(proposal_mat, 2, matrix, nrow = 1, simplify = FALSE)
      row_rle <- apply(proposal_mat, 1, function(x) rle(x)$lengths[rle(x)$values==1])
      column_rle <- apply(proposal_mat, 2, function(x) rle(x)$lengths[rle(x)$values==1])
    }
    })
  }
  
  nonogram$row_solution <- row_solutions
  nonogram$column_solution <- column_solutions
  nonogram$solved <- TRUE
  nonogram
}
