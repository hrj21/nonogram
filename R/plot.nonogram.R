#' plot.nonogram
#'
#' @description Method for plotting solved and unsolved nonograms using the ggplot2 and patchwork packages.
#' @param nonogram An object of class nonogram.
#' @param with_solution with_solution `TRUE` (default) or `FALSE`. Whether the plot should 
#'     contain the nonogram solution.
#'
#' @return An object of classes `patchwork`, `gg`, and `ggplot`.
#' 
#' @details If `with_solution` = `FALSE`, the function will plot an empty nonogram   
#'    with the row and column patterns indicated. If `with_solution` = `TRUE` and 
#'    the nonogram does not have a solution, the function will throw an error.
#'     
#' @export
#'
#' @examples
#' 'f <- examples[[33]] |> solve()
#' plot(f)
#' plot(f, with_solution = FALSE)
#' 
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
