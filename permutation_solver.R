# Define pattern of interest ----------------------------------------------
pattern <- c(2, 3, 1) # works
pattern <- c(2)       # works
pattern <- c(1, 1, 1) # works
pattern <- c(0)       # works
pattern <- c(4, 5)    # works
pattern <- c(3, 3, 1) # works

# Generate all permutations of length 10 ----------------------------------
perms <- gtools::permutations(2, 10, c(0, 1), repeats = TRUE) # p = n^r

# Convert each permutation into a pattern ---------------------------------
patterns <- apply(perms, 1, function(x){
  pat <- rle(x)$lengths[rle(x)$values==1]
  if(length(pat) == 0) {
    0
  } else pat
})

# Identify permutations that match the pattern of interest -----------------
matches <- unlist(lapply(patterns, function(x) {
  if (length(x) == length(pattern)) {
    all(x == pattern)
  } else
    FALSE
}))


# Test --------------------------------------------------------------------
valid <- perms[matches, , drop = FALSE]

all(
  sapply(1:nrow(valid), function(i) {
    all(rle(valid[i,])$lengths[rle(valid[i,])$values==1] == pattern)
    })
)

# Solve for guaranteed squares --------------------------------------------
valid
apply(valid, 2, function(a) length(unique(a))==1)
unique(valid[, apply(valid, 2, function(a) length(unique(a))==1)])

## general approach:
# calculate valid for every pattern in the nonogram
# find the pattern with the highest number of known values
# remove permutations from other axis that do not satisfy these known values
# repeat until all have only a single solution
# convert these into coordinates and values that cal be plotted