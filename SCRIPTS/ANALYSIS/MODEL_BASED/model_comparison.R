# model_comparison.R

calculate_bic <- function(nll, n_params, n_observations) {
  return(n_observations * log(nll) + n_params * log(n_observations))
}

compare_models <- function(results_list) {
  results_df <- data.frame(
    Model = character(),
    Parameter = character(),
    Value = numeric(),
    nLL = numeric(),
    BIC = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (model_name in names(results_list)) {
    model_results <- results_list[[model_name]]
    params <- model_results$params
    nll <- model_results$nll
    
    n <- nrow(data)  # Assuming 'data' is available in the global environment
    le <- length(params)
    bic <- calculate_bic(nll, le, n)
    
    temp_df <- data.frame(
      Model = rep(model_name, le),
      Parameter = names(params),
      Value = as.numeric(params),
      nLL = rep(nll, le),
      BIC = rep(bic, le),
      stringsAsFactors = FALSE
    )
    
    results_df <- rbind(results_df, temp_df)
  }
  
  return(results_df)
}