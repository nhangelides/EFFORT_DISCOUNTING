# utils.R

log_message <- function(message) {
  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), ":", message, "\n")
}