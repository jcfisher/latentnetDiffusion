#' Calculates latent space diffusion using the degroot function
#' 
#' @param ergmm: the object to get the posterior from
#' @param draws: the number of the draws from the posterior to take
#' @param simulate: logical indictor of whether to use the predicted
#' probabilities or the posterior predictive distribution
#' @param iterations: the number of iterations to run the simulation
#' @param ...: additional parameters passed to degroot (if simulate is FALSE)
#' or degrootList (if simulate is TRUE)
#' 
#' @details This function calculates latent space diffusion, as described in
#' Fisher (2015), for a given number of draws from the posterior distribution
#' of a latent space model.  If simulate is FALSE (the default), the function
#' uses predicted probabilities of a tie to simulate diffusion.  Otherwise, if
#' simulated is TRUE, the function simulates (iterations) networks drawn from
#' the first (draws) draws from the posterior predictive distribution, and then 
#' runs the diffusion process over those simulated networks.
#' 
#' More specifically, when simulate is FALSE, the function gets the predicted
#' probability from the first (draws) draws from the posterior distribution.
#' Holding each of those draws constant, it calculates the degroot function
#' (iterations) times on each draw, returning a list of the output of the 
#' degroot function on each draw
#' 
#' When simulate is TRUE, the function simulates (iterations) networks from each
#' of the first (draws) draws from the posterior, creating the posterior
#' predictive distribution.  Then, for each of the draws, the function runs
#' degrootList, meaning that it calculates the weighted averaging over the first
#' network, followed by the second network, and so on until (iterations) is
#' reached.  A list is returned.
#' 
#' @return A (draws) length list of the simulation output for each draw from the
#' posterior.
#' 
ergmmDegroot <- function(ergmm, Y, draws = 5, simulate = FALSE, iterations = 5,
                             ...) {
  require(statnet)
  if (class(ergmm) != "ergmm")
    stop("ergmm must be a latentnet ergmm object.")
  
  if (!is.numeric(draws) || draws %% 1 != 0 || draws < 1 || length(draws) > 1)
    stop("draws must be a whole number greater than 0.")
  
  if (!is.logical(simulate))
    stop("simulate must be a logical variable")
    
  if (simulate) {
    # Draw (iterations) networks from the first (draws) draws from the posterior
    pred.nets <- lapply(
        1:draws,
        function(i) {
          sim <- with(ergmm, simulate(model, par = sample[[i]], prior = prior, 
                                      nsim = iterations))
          # BUG: simulate does not create a named network.series object, so we
          # recast it as a list.
          class(sim) <- NULL
          return(sim)
          }
        )
    
    # Loop over the draws, and get the diffusion results
    out <- lapply(pred.nets, degrootList, Y = Y, ...)
                     
  } else {
    
    # Save the predicted probabilities from the first (draws) from the posterior
    predicted <- lapply(seq(draws), function(i) predict(ergmm, type = i))
    
    # Loop over those draws and get diffusion results
    out <- lapply(predicted, degroot, Y = Y, iterations = iterations, ...)
  }
  return(out)
}