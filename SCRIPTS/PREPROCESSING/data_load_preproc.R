# data_loader.R
# load discounting behavioral file
# this loads individual subject files
# then creates a single longform file (all subjects in one dataset)
# analysis is done as omnibus (population-wise)

library(stringr)

load_and_preprocess_data <- function(base_path) {
  log_message("Starting data loading and preprocessing...")
  
  # Set paths
  fullpath <- paste0(base_path, "ALL_SUBS_RUNS/")
  outpath <- paste0(base_path, "LONGFORM/")
  
  # Get file names
  file.names <- dir(fullpath, pattern = "EEfRT_1*")
  sids <- unique(as.integer(substr(gsub("EEfRT_", "", gsub(".csv", "", file.names)), 1, 3)))
  
  # Find file names for all individual files
  temp.comp1 <- list.files(fullpath, pattern = "*_run1")
  temp.comp4 <- list.files(fullpath, pattern = "*_run4")
  temp.coop2 <- list.files(fullpath, pattern = "*_run2")
  temp.coop3 <- list.files(fullpath, pattern = "*_run3")
  
  coop <- c(temp.coop2, temp.coop3)
  comp <- c(temp.comp1, temp.comp4)
  
  # Read competitive data
  name.list.comp0 <- lapply(paste0(fullpath, comp[1:2]), read.csv, header = TRUE, fill = TRUE, sep = ',')
  named.list.comp <- lapply(paste0(fullpath, comp[3:length(comp)]), read.csv, header = TRUE, fill = TRUE, sep = ',', colClasses = c(rep(NA, 32), "NULL"))
  both.comps <- c(name.list.comp0, named.list.comp)
  
  # Read cooperative data
  named.list.coop <- lapply(paste0(fullpath, coop), read.csv, header = TRUE, fill = TRUE, sep = ',', colClasses = c(rep(NA, 32), "NULL"))
  
  # Create longform datasets
  longdf.coop <- do.call(rbind, named.list.coop)
  longdf.comp <- do.call(rbind, both.comps)
  df.all <- rbind(longdf.coop, longdf.comp)
  
  # Process payouts
  process_payouts <- function(df, condition) {
    subjects <- unique(df$subject)
    payouts <- numeric(length(subjects))
    for (i in seq_along(subjects)) {
      cur_subject <- df[df$subject == subjects[i], ]
      if (condition == "COMP") {
        payouts[i] <- max(0, (cur_subject$MoneyCumu[44] - cur_subject$OtherMoneyCumu[44]) / 10)
      } else {
        payouts[i] <- cur_subject$MoneyCumu[44] / 10
      }
    }
    return(data.frame(SID = subjects, Payout = payouts))
  }
  
  p_comp <- process_payouts(longdf.comp, "COMP")
  p_coop <- process_payouts(longdf.coop, "COOP")
  
  p <- merge(p_comp, p_coop, by = "SID", suffixes = c("_COMP", "_COOP"))
  p$TOTAL <- p$Payout_COMP + p$Payout_COOP
  
  # Add warmth and competence data
  w_c <- read.csv(paste0(base_path, "warmth_competence.csv"), stringsAsFactors = FALSE)
  df.all <- merge(df.all, w_c, by.x = "Target", by.y = "Target", all.x = TRUE)
  
  # Write output files
  write.csv(longdf.comp, file = paste0(outpath, 'COMPETITIVE_ALL.csv'), row.names = FALSE)
  write.csv(longdf.coop, file = paste0(outpath, 'COOPERATIVE_ALL.csv'), row.names = FALSE)
  write.csv(df.all, file = paste0(outpath, "LF_ALL.csv"), row.names = FALSE)
  
  # Create subset
  even_subjects <- unique(df.all$subject[df.all$subject %% 2 == 0])
  odd_subjects <- unique(df.all$subject[df.all$subject %% 2 == 1])
  selected_subjects <- c(sample(even_subjects, 10), sample(odd_subjects, 10))
  subset_df <- df.all[df.all$subject %in% selected_subjects, ]
  write.csv(subset_df, file = paste0(outpath, "LF_subset.csv"), row.names = FALSE)
  
  log_message("Data loading and preprocessing completed.")
  
  return(list(
    all_data = df.all,
    subset_data = subset_df,
    payouts = p
  ))
}

# Helper function to log messages
log_message <- function(message) {
  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), ":", message, "\n")
}