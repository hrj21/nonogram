make_full_perm_set <- function(length) {
  gtools::permutations(2, length, c(0, 1), repeats = TRUE)
}

permutation_solver <- function(pattern, permutation_patterns) {
  matches <- unlist(lapply(permutation_patterns, function(x) {
    if (length(x) == length(pattern)) {
      all(x == pattern)
    } else
      FALSE
  }))
  
  perms[matches, , drop = FALSE]
}

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

print.nonogram <- function(nonogram) {
  cat("Nonogram with dimensions: ", nonogram$nrows, " x ", nonogram$ncolumns, sep = "")
  cat("\nSolved: ", nonogram$solved, "\n", sep = "")
  if(nonogram$solved) { 
    print(matrix(unlist(nonogram$column_solution), ncol = nonogram$ncolumns))
  } 
}

dim.nonogram <- function(x) {
  c(x$nrows, x$ncolumns)
}

solve.nonogram <- function(nonogram, row_permutations = NULL, column_permutations = NULL, verbose = TRUE, max_iter = 100) {
  if(is.null(row_permutations) | is.null(column_permutations)) {
    cat(format(Sys.time(), usetz = TRUE), ": Generating initial possible permutations\n")
    if(nonogram$nrows == nonogram$ncolumns) {
      row_permutations <- column_permutations <- make_full_perm_set(nonogram$ncolumns)
    } else {
      column_permutations <- make_full_perm_set(nonogram$ncolumns)
      row_permutations <- make_full_perm_set(nonogram$nrows)
    }
  }
  
  if(verbose) cat(format(Sys.time(), usetz = TRUE), ": Run length encoding all initial possible permuations\n")
  row_patterns <- apply(row_permutations, 1, function(x){
    pat <- rle(x)$lengths[rle(x)$values==1]
    if(length(pat) == 0) {
      0
    } else pat
  })
  
  column_patterns <- apply(column_permutations, 1, function(x){
    pat <- rle(x)$lengths[rle(x)$values==1]
    if(length(pat) == 0) {
      0
    } else pat
  })
  
  if(verbose) cat(format(Sys.time(), usetz = TRUE), ": Matching all possible solutions for each row and column\n")
  row_solutions_original <- row_solutions <- lapply(nonogram$rows, permutation_solver, column_patterns)
  column_solutions_original <- column_solutions <- lapply(nonogram$columns, permutation_solver, row_patterns)
  
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
    
    ## columns
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
    
    ## rows
    eq0 <- lapply(row_solutions, function(x) { # all equal to 0?
      apply(x, 2, function(a) all(a == 0))
    }) 
    
    eq1 <- lapply(row_solutions, function(x) {  # all equal to 0?
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
    
    # Update solutions --------------------------------------------------------
    column_solutions <- mapply(
      function(x, y) x[apply(x, 1, function(z) return(all(z == y, na.rm = TRUE))), , drop = FALSE],
      column_solutions, columns_known,
      SIMPLIFY = FALSE)
    
    row_solutions <- mapply(
      function(x, y) x[apply(x, 1, function(z) return(all(z == y, na.rm = TRUE))), , drop = FALSE],
      row_solutions, rows_known,
      SIMPLIFY = FALSE
    )
    
    ## Replace empty elements with original permutations
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


# Extract solution to df --------------------------------------------------
as.data.frame.nonogram <- function(nonogram, with_solution = TRUE) {
  if(!nonogram$solved & with_solution) stop("This nonogram has not been solved.")
  
  if(with_solution) {
    data.frame(
      value = factor(unlist(nonogram$column_solution)),
      column = factor(rep(1:nonogram$ncolumns, each = nonogram$nrows)),
      row = factor(rep(1:nonogram$nrows, nonogram$ncolumns))
    )
  } else {
    data.frame(
      value = factor(0),
      column = factor(rep(1:nonogram$ncolumns, each = nonogram$nrows)),
      row = factor(rep(1:nonogram$nrows, nonogram$ncolumns))
    )
  }
}

# Plot --------------------------------------------------------------------
plot.nonogram <- function(nonogram, with_solution = TRUE) {
  if(!nonogram$solved & with_solution) stop("This nonogram has not been solved.")
  
  solution_df <- as.data.frame(nonogram, with_solution = with_solution)
  
  p <- ggplot(solution_df, aes(column, row, fill = value)) +
    geom_tile() +
    scale_fill_manual(values = c("white", "black")) +
    scale_x_discrete(expand = expansion(0)) +
    scale_y_discrete(expand = expansion(0), limits = rev, position = "right") +
    geom_vline(xintercept = seq(1, length(solution_df[, 1]), 1) + 0.5, size = 0.1, col = "grey") +
    geom_hline(yintercept = seq(1, length(solution_df[, 1]), 1) + 0.5, size = 0.1, col = "grey") +
    theme_bw() +
    theme(
      legend.position = "none", 
      axis.title = element_blank(), 
      plot.background = element_rect(fill = "transparent", color = NA)
    )
  
  column_grob <- lapply(nonogram$columns, function(x) {c(rep("", max(lengths(nonogram$columns)) - length(x)), x)}) |> 
    unlist() |> 
    matrix(byrow = TRUE, nrow = nonogram$ncolumns) |> 
    t() |> 
    gridExtra::tableGrob(theme = gridExtra::ttheme_minimal())
  
  column_grob$widths <- unit(rep(1/ncol(column_grob), ncol(column_grob)), "npc")
  column_grob$heights <- unit(rep(1/nrow(column_grob), nrow(column_grob)), "npc")
  
  row_grob <- lapply(nonogram$rows, function(x) {c(rep("", max(lengths(nonogram$rows)) - length(x)), x)}) |> 
    unlist() |> 
    matrix(byrow = TRUE, nrow = nonogram$nrows) |> 
    gridExtra::tableGrob(theme = gridExtra::ttheme_minimal())
  
  row_grob$widths <- unit(rep(1/ncol(row_grob), ncol(row_grob)) + 0.1, "npc")
  row_grob$heights <- unit(rep(1/nrow(row_grob), nrow(row_grob)), "npc")
  
  layout <- "
    #BBBBB
    ACCCCC
    ACCCCC
    ACCCCC
    ACCCCC
  "
  wrap_elements(row_grob) + wrap_elements(column_grob) + p + plot_layout(design = layout)
}


