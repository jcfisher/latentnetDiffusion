#' Calculates the simple weighted avergaging model
#' 
#' @param W: the weight matrix to use
#' @param Y: the starting positions to use
#' @param iterations: the number of iterations to take
#' @param all.iter: T/F of whether all the iterations are returned in a matrix
#' @param row.normalize: T/F indicator of whether the matrix should be row
#' normalized before using
#' @param self.weight: numeric value(s) in [0, 1], indicating how much each
#' person weights him-/herself vs. others.  0 indicates full self-weight, and 
#' 1 indicates full other-weight.  Only used if row.normalize is TRUE.
#' 
#' @details Uses the DeGroot weighted averaging model, which just returns
#' (W ^ iterations) %*% Y.  If row.normalize is true, set the diagonal equal to
#' 1 and divide each row by it's sum.  Otherwise, throw a warning if each row
#' doesn't sum to 1.
#' 
#' @note Do NOT use DTMC for this -- DTMC requires that the Y variable sum to 1.
#' 
#' @return A vector with the weighted average taken (iter) times
degroot <- function(W, Y, iterations, all.iter = F, row.normalize = T,
                    self.weight = 1) {
  require(expm)
  require(statnet)
  
  if (!(is.network(W) || is.matrix(W)))
    stop("W must be a list or a matrix.")
  
  if (!(is.matrix(Y)))
    stop("Y must be a matrix")
  
  if (is.network(W))
    W <- as.matrix(W)
  
  # Check row normalization
  if (row.normalize) {
    W <- latentnetDiffusion::makeConstructMatrix(W, self.weight)
  } else {
    if (any(!(round(rowSums(C),
                    floor(-log10(.Machine$double.eps))) %in% c(0, 1))))
      warning("Input matrix not row normalized -- will probably give incorrect answer.")
  }
  
  if (all.iter) {
    steps <- Reduce(`%*%`, rep(list(W), iterations), init = Y, right = T,
                  accumulate = T)
    out <- do.call(cBind, rev(steps))
  } else {
    out <- (W %^% iterations)  %*% Y
  }
  
  return(out)
}