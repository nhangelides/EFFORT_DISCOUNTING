# model_free_analysis.R

library(dplyr)
library(ggplot2)
library(emmeans)
library(tidyr)

perform_model_free_analysis <- function(df) {
  results <- list()
  
  # Basic EDA
  results$condition_response_table <- table(df$Condition, df$response)
  
  # Logistic regression
  model <- glm(response ~ Reward*Cost*Condition + feedback, family = "binomial", data = df)
  results$logistic_regression <- summary(model)
  
  # Within-subject effects
  results$within_subject <- analyze_within_subject(df)
  
  # Order effects
  results$order_effects <- analyze_order_effects(df)
  
  # Feedback effects
  results$feedback_effects <- analyze_feedback_effects(df)
  
  # Average response by condition, cost, and reward
  results$avg_response <- calculate_avg_response(df)
  
  # Visualizations
  results$plots <- create_plots(df)
  
  return(results)
}

analyze_within_subject <- function(df) {
  subjects_both_conditions <- df %>%
    group_by(subject) %>%
    filter(length(unique(Condition)) > 1) %>%
    ungroup()
  
  subject_means <- subjects_both_conditions %>%
    group_by(subject, Condition) %>%
    summarize(mean_response = mean(response, na.rm = TRUE))
  
  difference_scores <- subject_means %>%
    spread(Condition, mean_response) %>%
    mutate(diff_score = `1` - `0`)
  
  t_test_result <- t.test(difference_scores$`1`, difference_scores$`0`, paired = TRUE)
  
  return(list(difference_scores = difference_scores, t_test = t_test_result))
}

analyze_order_effects <- function(df) {
  order_summary <- df %>%
    group_by(cond_order, Condition) %>%
    summarize(mean_response = mean(response, na.rm = TRUE),
              sd_response = sd(response, na.rm = TRUE))
  
  lm_result <- lm(response ~ Condition * cond_order, data = df)
  marginal_means <- emmeans(lm_result, ~ Condition * cond_order)
  pairwise_comparisons <- pairs(marginal_means, adjust = "tukey")
  
  return(list(summary = order_summary, lm = summary(lm_result), 
              marginal_means = marginal_means, pairwise = pairwise_comparisons))
}

analyze_feedback_effects <- function(df) {
  feedback_summary <- df %>%
    group_by(feedback, Condition) %>%
    summarize(mean_response = mean(response, na.rm = TRUE),
              sd_response = sd(response, na.rm = TRUE))
  
  lm_result <- lm(response ~ Condition * feedback, data = df)
  marginal_means <- emmeans(lm_result, ~ Condition * feedback)
  pairwise_comparisons <- pairs(marginal_means, adjust = "tukey")
  
  return(list(summary = feedback_summary, lm = summary(lm_result), 
              marginal_means = marginal_means, pairwise = pairwise_comparisons))
}

calculate_avg_response <- function(df) {
  avg_response <- df %>%
    group_by(Reward, Cost, Condition) %>%
    summarize(avg_response = mean(response, na.rm = TRUE))
  
  return(avg_response)
}

create_plots <- function(df) {
  avg_response <- calculate_avg_response(df)
  
  bar_cost_cond <- ggplot(avg_response, aes(x = as.factor(Cost), y = avg_response, fill = Condition)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Probability vs. Cost by Condition",
         x = "Cost",
         y = "Probability",
         fill = "Condition") +
    coord_cartesian(ylim = c(0.5, 1)) +
    theme_minimal()
  
  bar_reward_cond <- ggplot(avg_response, aes(x = as.factor(Reward), y = avg_response, fill = Condition)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Probability vs. Reward by Condition",
         x = "Reward",
         y = "Probability",
         fill = "Condition") +
    coord_cartesian(ylim = c(0.5, 1)) +
    theme_minimal()
  
  return(list(bar_cost_cond = bar_cost_cond, bar_reward_cond = bar_reward_cond))
}