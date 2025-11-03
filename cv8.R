rm(list=ls())
setwd('V:/MPA/MPAPRG/exercise_08')


# Task 1
FindSorted <- function(permutation){
  for (i in 1:(length(permutation)-1)){
    if (permutation[i] + 1 != permutation[i + 1]){
      return(i+1)
    }
  }
}

# print(FindSorted(c(0, 1, 2, 3, 6, 7, 4, 5, 8)))

# Task 2
IndicateAscending <- function(permutation){
  indication <- rep(0, times=length(permutation))
  indication[1] <- 1
  indication[length(indication)] <- 1
  
  for (i in 1:(length(permutation)-1)){
    if (permutation[i] + 1 == permutation[i + 1]){
      indication[i] <- 1
      indication[i + 1] <- 1
    }
  }
  
  return(indication)
}

print(IndicateAscending(c(0, 4, 5, 3, 2, 1, 6, 7, 8)))

# Task 3
BreakpointSort <- function(permutation){
  new_perm <- c(0, permutation, length(permutation)+1)
  distance <- 0
  
  while (!(all(new_perm == c(0:(length(new_perm)-1))))){
    distance <-  distance + 1
    unsorted_idx <- FindSorted(new_perm)
    indication <- IndicateAscending(new_perm)
    if (all(indication == rep(1, length(new_perm)))){
      new_perm[unsorted_idx:(length(new_perm)-1)] <- rev(new_perm[unsorted_idx:(length(new_perm)-1)])
    }else{
      smallest_descend <- min(new_perm[!as.logical(indication)])
      s_d_idx <- which(new_perm == smallest_descend)
      new_perm[unsorted_idx:s_d_idx] <- rev(new_perm[unsorted_idx:s_d_idx])
    }
  }
  return(new_perm)
  # return(distance)
}

print(BreakpointSort(c(5, 1, 4, 3, 7, 8, 9, 2, 6)))


