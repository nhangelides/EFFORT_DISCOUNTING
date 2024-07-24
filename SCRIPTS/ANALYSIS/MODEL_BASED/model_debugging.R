# model_debugging.R

library(optimx)

# Function to generate random starting points
random.starts <- function(n, model_type = "multiplicative_cost") {
  k <- rexp(n, rate=5)
  lambda <- rexp(n, rate=0.1)
  beta <- rexp(n, rate=5)
  alpha <- runif(n, -1, 1)
  gamma <- runif(n, 0.5, 2.5)  # Avoid generating extremely small or large values for gamma
  delta <- runif(n, -1, 1)  # Added delta parameter
  return(matrix(c(k, lambda, beta, alpha, gamma, delta), ncol=6))
}

# Modified multiplicative cost model with interaction term on k
multiplicative_cost_with_interaction_on_k <- function(bias, reward, k, delta, cost, gamma, autonomy) {
  bias + reward - (k + delta * autonomy) * cost^gamma
}

# Diagnostic function for parameter estimation
estimate.params.diagnostics <- function(d, model_type = "multiplicative_cost") {
  starts <- random.starts(20, model_type)
  print("Initial starting points:")
  print(head(starts))
  
  # Define the negative log likelihood function with added diagnostics
  lik <- function(x) {
    cat("Parameters:", x, "\n")
    nll <- get.negloglik(d, model_type)(x)
    cat("Negative Log Likelihood:", nll, "\n")
    return(nll)
  }
  
  n_param <- ncol(starts)
  
  # Define the parameter bounds
  lower_bounds <- c(k=0, lambda=0, beta=-Inf, alpha=-1, gamma=0, delta=-1)
  upper_bounds <- c(k=Inf, lambda=Inf, beta=Inf, alpha=1, gamma=Inf, delta=1)
  
  pars <- multiStart(starts, lik, action='optimize',
                     lower=lower_bounds, upper=upper_bounds,
                     control=list(trace=6))  # Increase trace for more verbose output
  
  print(pars)
  ix <- which.min(pars$fvalue)
  cat("Optimization finished. Minimum found at index", ix, "\n")
  
  # Return both parameters and nLL value
  list(params = pars$par[ix,], nll = min(pars$fvalue))
}

# Debugging function to investigate negative log-likelihood for different values of k
debug_nLL_for_k <- function(d, params, k_vals) {
  # Extract parameters
  lambda <- params[1]
  beta <- params[2]
  alpha <- params[3]
  gamma <- params[4]
  delta <- params[5]
  
  # Extract data columns
  autonomy <- scale(d$autonomy)
  h_effort <- d$Reward
  efforts <- d$Cost
  
  # Container for nLL values
  nLL_vals <- numeric(length(k_vals))
  
  # Compute nLL for each value of k
  for (i in seq_along(k_vals)) {
    k <- k_vals[i]
    vals.l_effort <- multiplicative_cost_with_interaction_on_k(0, 1, k, delta, 0.1, gamma, autonomy)
    vals.h_effort <- multiplicative_cost_with_interaction_on_k(beta, h_effort, k, delta, efforts, gamma, autonomy)
    
    probs.l_effort <- pmin(1, pmax(0, (1 + exp(-lambda * (vals.l_effort - vals.h_effort)))^-1))
    probs.h_effort <- 1 - probs.l_effort
    
    # Compute the negative log-likelihood for this set of parameters
    nLL_vals[i] <- -sum(d$Choice * log(probs.h_effort) + (1 - d$Choice) * log(probs.l_effort))
  }
  
  return(list(k_vals = k_vals, nLL_vals = nLL_vals))
}

# Function to plot nLL vs k
plot_nLL_vs_k <- function(debug_results) {
  plot(debug_results$k_vals, debug_results$nLL_vals, type="l", 
       xlab="k", ylab="Negative Log Likelihood",
       main="Negative Log Likelihood vs. k")
}

# Main debugging workflow
run_model_debugging <- function(data, model_type = "multiplicative_cost") {
  cat("Starting model debugging for", model_type, "model\n")
  
  # Run diagnostic parameter estimation
  model_results <- estimate.params.diagnostics(data, model_type)
  print("Estimated parameters:")
  print(model_results$params)
  print("Minimum negative log-likelihood:")
  print(model_results$nll)
  
  # Debug nLL for different k values
  k_vals <- seq(0, 10, by=0.1)
  debug_results <- debug_nLL_for_k(data, model_results$params, k_vals)
  
  # Plot results
  plot_nLL_vs_k(debug_results)
  
  cat("Debugging completed. Check the plot for nLL vs k.\n")
}

# Example usage (commented out):
# source("data_loader.R")  # Ensure this loads your data into 'data' variable
# run_model_debugging(data, "multiplicative_cost")