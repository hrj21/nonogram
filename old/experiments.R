
##

tmp <- rep(list(0:1), 5)
tab <- do.call(expand.grid, tmp)

which(colSums(t(tab) == c(0,1,1,0,0)) == ncol(tab))

s <- c(2, 3, 1)
start_seq <- rep(0, 10)
pos <- 1

for(i in s) {
  start_seq[pos:(pos+i-1)] <- 1
  pos <- pos+i + 1
}


shift <- function(x, lag) {
  n <- length(x)
  xnew <- rep(0, n)
  if (lag < 0) {
    xnew[1:(n-abs(lag))] <- x[(abs(lag)+1):n]
  } else if (lag > 0) {
    xnew[(lag+1):n] <- x[1:(n-lag)]
  } else {
    xnew <- x
  }
  xnew
}

pat <- start_seq
pat_as_num <- as.numeric(paste(start_seq, collapse = ""))
n_shifts <- nchar(pat_as_num) - nchar(sub("0*$", "", pat_as_num))
end_seq <- shift(pat, n_shifts)
pat_list <- list(start_seq)

while(!all(tail(pat_list, 1)[[1]] == end_seq)) {
  val <- tail(pat_list, 1)[[1]]
  pat_list <- append(pat_list, list(shift(val, 1)))
}

matrix(unlist(pat_list), nrow = length(pat_list), byrow = TRUE)


