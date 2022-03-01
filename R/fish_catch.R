#' Fish Catch Function
#' 
#' This function takes a vector of fish names and returns the most common fish, rarest fish, and total number of fish caught.
#' 
#' @param fish, a vector of fish species (names)
#' @return list with the following items
#' \describe{
#' \item{most_common}{Name of the most frequently caught species}
#' \item{rarest}{Name of the least frequently caught species}
#' \item{total}{Total number of fish caught}
#' }
#' @author Sydney Rilum

fish_catch = function(fish) {
  
  # make sure data is in class factor
  fish = as.factor(fish)
  
  # determine the most common species
  most_common = names(which.max(summary(fish)))
  
  # determine the least common species
  rarest = names(which.min(summary(fish)))
  
  # calculate total number of fish
  total = length(fish)
  
  # output from function
  return(c(most_common = most_common, 
           rarest = rarest, 
           total = total))
}