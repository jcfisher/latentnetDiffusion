rowNormalize <- function(X) {
  # A function to normalize matrices such that the values in each row sum to 1
  # Warning: No checks on this function. Use with caution!
  #
  # Args:
  #   X: a numeric matrix
  #
  # Returns:
  #   A row-normalized matrix
  rs.X <- Matrix::rowSums(X)
  zeroes <- (rs.X == 0)
  X[!zeroes, ] <- X[!zeroes, ] / rs.X[!zeroes]
  if (any(zeroes)) {
    X[zeroes, ] <- 0
  }
  return(X)
}