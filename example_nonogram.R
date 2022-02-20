# Define nonogram ---------------------------------------------------------
columns <- list(
  c(2, 2),
  c(3, 2),
  c(5, 2),
  c(5),
  c(5),
  c(2, 1),
  c(2, 1, 1),
  c(3, 3),
  c(2, 1, 1),
  c(4, 3)
)

rows <- list(
  c(4, 4),
  c(4, 5),
  c(3, 1, 1, 1),
  c(3, 1),
  c(3),
  c(1, 1),
  c(1),
  c(1, 3),
  c(3, 1, 1),
  c(3, 5)
)

# Generate possible solutions per row/column ------------------------------
column_solutions <- lapply(columns, permutation_solver, patterns)
row_solutions <- lapply(rows, permutation_solver, patterns)

column_solutions_original <- column_solutions
row_solutions_original <- row_solutions

# Create empty lists of search vectors ------------------------------------
columns_known <- rep(list(rep(NA, 10)), 10)
rows_known <- rep(list(rep(NA, 10)), 10)

# Solve for guaranteed squares --------------------------------------------
while(
  !(all(sapply(row_solutions, function(x) dim(x)[1]) == 1) &
  all(sapply(column_solutions, function(x) dim(x)[1]) == 1))
) {
## columns
eq0 <- lapply(column_solutions, function(x) { # all equal to 0?
  apply(x, 2, function(a) all(a == 0))
  }) 

eq1 <- lapply(column_solutions, function(x) {  # all equal to 1?
  apply(x, 2, function(a) all(a == 1))
})

for(j in 1:10) {
  for(i in 1:10) {
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

for(j in 1:10) {
  for(i in 1:10) {
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
column_solutions <- lapply(1:10, function(x){
  if(dim(column_solutions[[x]])[1] == 0) {
    column_solutions[[x]] <- column_solutions_original[[x]]
  } else column_solutions[[x]] <- column_solutions[[x]]
})

row_solutions <- lapply(1:10, function(x){
  if(dim(row_solutions[[x]])[1] == 0) {
    row_solutions[[x]] <- row_solutions_original[[x]]
  } else row_solutions[[x]] <- row_solutions[[x]]
})

print(sapply(column_solutions, function(x) dim(x)[1]))
print(sapply(row_solutions, function(x) dim(x)[1]))
}

# Plot solution -----------------------------------------------------------
## rearrange into dataframe
solution_df <- data.frame(
  value = factor(unlist(column_solutions)),
  column = factor(rep(1:10, each = 10)),
  row = factor(rep(1:10, 10))
)

## plot
library(ggplot2)

ggplot(solution_df, aes(column, row, fill = value)) +
  geom_tile() +
  scale_fill_manual(values = c("white", "black")) +
  scale_x_discrete(expand = expansion(0)) +
  scale_y_discrete(expand = expansion(0), limits = rev) +
  geom_vline(xintercept = seq(1, length(solution_df[, 1]), 1) + 0.5, color = "black", size = 0.1) +
  geom_hline(yintercept = seq(1, length(solution_df[, 1]), 1) + 0.5, color = "black", size = 0.1) +
  theme_bw()
  
