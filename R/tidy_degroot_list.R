#' Wrapper function for tidyDegroot that calls tidyDegroot on a list of degroot
#' matrices
#' 
#' @param pred: a list of matrices, generally produced by ergmmDegroot
#' @param id: a vector of ID values that will be added as a column to the
#' resulting data.frame
#' 
#' @return a data.frame (in tibble form) with 4 columns: the 3 columns produced
#' by tidyDegroot, plus an additional column for the number of draws, which 
#' denotes the index from the original list
tidyDegrootList <- function(pred, id) {
  require(dplyr)
  
  # Verify that all the elements of pred are matrices with the same dimensions
  stopifnot(all(vapply(pred, is.matrix, FUN.VALUE = T, USE.NAMES = F)))
  stopifnot(length(unique(lapply(pred, dim))) == 1)
  
  lapply(pred, tidyDegroot, id = id) %>%
    bind_rows(.id = "draws") %>%
    return
}
