#' Run the degroot function on a series transition matrices
#' 
#' @param mat.list: a list of matrices
#' @param Y: the initial values of the diffusion process
#' @param all.iter: T/F indicator of whether every iteration of the diffusion
#' process should be saved
#' @param row.normalize: T/F, should the matrices be row normalized before
#' proceeding?
#' @param self.weight: numeric value(s) in [0, 1], indicating how much each
#' person weights him-/herself vs. others.  0 indicates full self-weight, and 
#' 1 indicates full other-weight.  Only used if row.normalize is TRUE.
#'
#' @return If all.iter is TRUE, a matrix of Y X length(mat.list), with the 
#' outcome of the diffusion process at each iteration.  If all.iter is FALSE,
#' the outcome of the last iteration 

degrootList <- function(mat.list, Y, all.iter = T, row.normalize = T,
                        self.weight = 1) {
  require(Matrix)
  
  if (!is.list(mat.list))
    stop("mat.list must be a list")
  
  if (!all(sapply(mat.list, class) %in% c("network", "matrix")))
    stop("mat.list must be a list of networks or matrices")
  
  # Convert any networks to adjacency matrices
  nets <- which(sapply(mat.list, is.network))
  mat.list[nets] <- lapply(mat.list[nets], as.matrix)
  
  # Convert to sparse matrices
  # Note: this causes *so* many problems that I'm getting rid of it.  I'm not
  # sure what this was supposed to gain by inclusion, but possibly I could add
  # it back in later.
  # mat.list <- lapply(mat.list, Matrix)
  # Y <- Matrix(Y)
  
  if (row.normalize) {
    mat.list <- lapply(mat.list, makeConstructMatrix, a = self.weight)
  }
  
  # Run the iterative multiplication
  # See: http://stackoverflow.com/questions/28548853/how-do-i-use-reduce-to-multiply-several-matrices-in-reverse-order/28548995#28548995
  # W[[k]] %*% W[[k - 1]] %*% ... %*% W[[1]] %*% Y
  steps <- Reduce(`%*%`, rev(mat.list), init = Y, right = T,
                  accumulate = all.iter)
  if (all.iter) {
    out <- do.call(cBind, rev(steps))
  } else {
    out <- steps
  }
  
  return(out)
}