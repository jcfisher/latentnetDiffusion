#' Row-normalizes a matrix with arbitrary weighting for the diagonal
#' 
#' @param net: a statnet network object or a square matrix
#' @param a: a numeric value indicating how much weight to give to self vs.
#' other.  1 indicates full self-weight (diagonal), 0 indicates full other-
#' weight (off diagonal).  Can be a vector to give each person different
#' weights.
#' 
#' @note Calculates the construct matrix from Friedkin and Johnson (2011:40).
#' That matrix is defined as \eqn{AC + I - A}, where:
#' \deqn{C_{ij} = \frac{W_{ij}}{\sum_{k: i \ne k} W_{ik}}}
#' \eqn{C_{ii} = 0}, \eqn{A_{ii} = a_i}, and \eqn{A_{ij} = 0} for all
#' \eqn{i \ne j}.
#' 
#' Among indegree isolates, \eqn{\sum_{k: i \ne k} W_{ik} = 0}.  In those cases,
#' \eqn{C_{ij} \coloneqq 0}, and \eqn{a_{i} \coloneqq 1}. 
#' 
#' @references Friedkin, Noah E. and Eugene c. Johnson. 2011. 
#' \emph{Social Influence Network Theory: A Sociological Examination of Small
#' Group Dynamics}. Cambridge University Press: New York.
#' 
#' @return a square matrix whose rows sum to 1 or 0

makeConstructMatrix <- function(net, a) {
  
  # Check net, and convert it to an adjacency matrix if necessary
  if (network::is.network(net)) {
    mat <- net[, ]  # as.matrix throws errors sometimes
  } else if (is.matrix(net)) {
    if (nrow(net) != ncol(net)) 
      stop("number of rows of net must equal number of columns")
    mat <- net
  } else {
    stop("net must be a statnet network or a square matrix object")
  }
  
  # Check that a is appropriate length, number
  if (!(length(a) %in% c(1, nrow(mat))))
    stop("a must be either length 1, or must have the same number of elements as net has rows")
  if (!all(vapply(a, `>=`, 0, FUN.VALUE = T, USE.NAMES = F)))
    stop("all values of a must be greater than or equal to 0")
  if (!all(vapply(a, `<=`, 1, FUN.VALUE = T, USE.NAMES = F)))
    stop("all values of a must be less than or equal to 1")
  
  diag(mat) <- 0
  C <- latentnetDiffusion::rowNormalize(mat)
  
  # See http://stackoverflow.com/questions/42207139/comparing-floats-with-more-than-1-integer
  rounded.rowsums <- round(rowSums(C), floor(-log10(.Machine$double.eps)))
  
  if (!all(rounded.rowsums %in% c(0, 1)))
    warning("C matrix values do not sum to 0 or 1")
  
  # Fix isolated dyads -- definitionally (my defn.) assigned to weight the last
  # observation at 1
  isos <- which(rounded.rowsums == 0)
  if (length(a) == 1) {
    a <- rep(a, times = nrow(C))
  }
  a[isos] <- 1
  
  # Create diagonal matrix A and identity matrix I
  I <- diag(nrow(C))
  A <- diag(a)
  
  # Convert to a weight matrix and return
  return(A %*% C + I - A)
}