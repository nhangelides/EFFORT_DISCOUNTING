# models.R

linear <- function(beta, reward, k, effort, alpha, condition, feedback) {
  interaction_effect <- condition * feedback
  modulated_reward <- reward + alpha * interaction_effect
  beta + modulated_reward - k * effort
}

parabolic <- function(beta, reward, k, effort, alpha, condition, feedback) {
  interaction_effect <- condition * feedback
  modulated_reward <- reward + alpha * interaction_effect
  beta + modulated_reward^{.8} - k*(effort^{2})
}

flex_quadratic <- function(beta, reward, k, effort, gamma, alpha, condition, feedback) {
  interaction_effect <- condition * feedback
  modulated_reward <- reward + alpha * interaction_effect
  beta + modulated_reward^{.8} - k*(effort^{gamma})
}

hyperbolic <- function(beta, reward, k, effort, alpha, condition, feedback) {
  interaction_effect <- condition * feedback
  modulated_reward <- reward + alpha * interaction_effect
  beta + (modulated_reward / (1 + k * effort))
}

sigmoidal <- function(beta, reward, k, effort, p, alpha, condition, feedback) {
  interaction_effect <- condition * feedback
  modulated_reward <- reward + alpha * interaction_effect
  beta + modulated_reward^{.8} * (1-((1/(1+exp(-k*(effort-p)))-1/(1+exp(k*p)) * (1+(1/(exp(k*p)))))))
}