#' Converts predictions from degroot into a tidy dataset
#' 
#' @param pred: a matrix of predictions
#' @param id: a vector whose length is nrow(pred) that gives ID values for pred.
#' IF it is missing, it will be set to 1:nrow(pred)
#' 
#' @note This function is in beta, please use with caution!  It also involves
#' loading several other dependencies, which isn't ideal.
#' 
#' @return A tibble (i.e., a data.frame) with 3 columns: an id column (id),
#' a column for the predicted value from the simulation, and a value for the 
#' number of iterations, and a column for the number of draws
tidyDegroot <- function(pred, id) {

  require(dplyr)
  require(tidyr)
  require(readr)
  
  # Verify that pred is a matrix
  stopifnot(is.matrix(pred))

  # Create an ID vector if one doesn't exist, and then test that it appropriate
  if (missing(id))
    id <- seq_len(nrow(pred))
  
  stopifnot(is.vector(id))
  stopifnot(length(id) == nrow(pred))
  
  pred %>%
    as.data.frame %>%
    mutate(id = id) %>%
    gather(key = "iter", value = "pred.value", -id) %>%
    mutate(iteration = as.integer(parse_number(iter))) %>%
    select(-iter) %>%
    tbl_df %>%
    return
}
