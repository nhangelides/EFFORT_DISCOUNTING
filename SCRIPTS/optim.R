# optimization.R

library(DEoptim)

source("models.R")
source("utils.R")

get.negloglik <- function(d, model_type = "linear") {
  l_effort <- 1 # Value of low effort choice
  h_effort <- d$Reward # Value of high effort choice
  
  res <- d$response # Define as subject response and change to 1 or 0
  efforts <- d$Cost # Define as level of effort on each trial
  condition <- d$Condition # Assuming the dataset has a 'condition' column
  feedback <- d$feedback # Assuming the dataset has a 'feedback' column
  
  function(x, debug = F) {
    log_message(paste("Starting optimization with parameters k=", x[1], "lambda=", x[2], "beta=", x[3], "gamma=", x[4], "p=", x[5], "alpha=", x[6]))
    
    k       <- x[1]
    lambda  <- x[2]
    beta    <- x[3]
    gamma   <- x[4]
    p       <- x[5]
    alpha   <- x[6] # New parameter for interaction effect
    
    if (model_type == "linear") {
      vals.l_effort <- linear(0, 1, k, .1, alpha, condition, feedback)
      vals.h_effort <- linear(beta, h_effort, k, efforts, alpha, condition, feedback)
    } else if (model_type == "parabolic") {
      vals.l_effort <- parabolic(0, 1, k, .1, alpha, condition, feedback)
      vals.h_effort <- parabolic(beta, h_effort, k, efforts, alpha, condition, feedback)
    } else if (model_type == "flex_quadratic") {
      vals.l_effort <- flex_quadratic(0, 1, k, .1, gamma, alpha, condition, feedback)
      vals.h_effort <- flex_quadratic(beta, h_effort, k, efforts, gamma, alpha, condition, feedback)
    } else if (model_type == "hyperbolic") {
      vals.l_effort <- hyperbolic(0, 1, k, .1, alpha, condition, feedback)
      vals.h_effort <- hyperbolic(beta, h_effort, k, efforts, alpha, condition, feedback)
    } else if (model_type == "sigmoidal") {
      vals.l_effort <- sigmoidal(0, 1, k, efforts, p, alpha, condition, feedback)
      vals.h_effort <- sigmoidal(beta, h_effort, k, efforts, p, alpha, condition, feedback)
    } else {
      stop("Unknown model type")
    }
    
    probs.l_effort  <- (1 + exp(-lambda * (vals.l_effort - vals.h_effort)))^{-1}
    probs.h_effort  <- 1 - probs.l_effort
    
    # Negative log likelihood
    -sum(log(1e-10 + probs.h_effort * res + probs.l_effort * (1-res)))
  }
}

# Increase the number of random starts
random.starts <- function(n, model_type = "linear") {
  k <- rexp(n, rate=5)
  lambda <- rexp(n, rate=0.1)
  beta <- rexp(n, rate=5)
  gamma <- runif(n, 0.5, 2.5)  # Generating gamma values; adjust the range as required
  p <- rexp(n, rate=0.3)
  alpha <- runif(n, -2, 1)  # Adjusted bounds for alpha
  return(matrix(c(k, lambda, beta, gamma, p, alpha), ncol=6))
}

estimate.params <- function(d, model_type = "linear") {
  starts <- random.starts(100, model_type)  # Increase random starts to 100
  
  print(head(starts))  # This will print the first few rows of the starting points matrix
  
  # Pass model_type to get.negloglik
  log_message("Starting optimization...")
  lik <- get.negloglik(d, model_type)
  
  # Wrapper function to make lik compatible with DEoptim
  lik_wrapper <- function(x) {
    lik(x)
  }
  
  n_param <- dim(starts)[2]   # number of parameters
  
  # Define the parameter bounds for all models, ensuring gamma cannot be negative and alpha is between -1 and 1
  lower_bounds <- c(k=0, lambda=0, beta=-10, gamma=0, p = 0, alpha=-2)
  upper_bounds <- c(k=10, lambda=10, beta=10, gamma=5, p=20, alpha=1)
  
  # Use DEoptim for global optimization (you can use this for models that don't converge with spg)
  result <- DEoptim(lik_wrapper, lower=lower_bounds, upper=upper_bounds, control=list(NP=100, itermax=200, trace=TRUE))
  
  param_vector <- result$bestmem
  names(param_vector) <- c("k", "lambda", "beta", "gamma", "p", "alpha")
  
  min_nll <- result$bestval
  
  log_message(paste("Optimization finished. Minimum nLL value:", min_nll))
  
  # Return both parameters and nLL value
  list(params = param_vector, nll = min_nll)  
}
