# In main.R

source("data_loader.R")
source("models.R")
source("optimization.R")
source("model_comparison.R")
source("utils.R")
source("model_free_analysis.R")  # Add this line

log_message("Script started.")

# Load and preprocess data
base_path <- "/Users/nickangelides/Dropbox/PENN/SOC_EEfRT/DATA/"
data_list <- load_and_preprocess_data(base_path)
all_data <- data_list$all_data
subset_data <- data_list$subset_data
payouts <- data_list$payouts


# Perform model-free analysis
model_free_results <- perform_model_free_analysis(all_data)

# Print or save results as needed
print(model_free_results$logistic_regression)
print(model_free_results$within_subject$t_test)
print(model_free_results$order_effects$pairwise)
print(model_free_results$feedback_effects$pairwise)

# Save plots
ggsave("cost_by_condition.png", model_free_results$plots$bar_cost_cond)
ggsave("reward_by_condition.png", model_free_results$plots$bar_reward_cond)

# Define models to compare
models <- c("linear", "parabolic", "flex_quadratic", "hyperbolic", "sigmoidal")

# Run parameter estimation for each model
all_results <- list()
for (model in models) {
  log_message(paste("Processing model:", model))
  model_results <- estimate.params(data, model)
  all_results[[model]] <- model_results
  log_message(paste("Finished model:", model))
}

# Compare models
results_df <- compare_models(all_results)

# Print results
print(results_df)

log_message("Script finished.")