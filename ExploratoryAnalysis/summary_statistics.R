library(dplyr)
library(readr)

load("ProcessedData/processed_AnalysisData.Rdata")

if (!exists("processed_data")) {
  stop("Expected object `processed_data` was not found in ProcessedData/processed_AnalysisData.Rdata.")
}

required_columns <- c("fishNum", "species", "FishTrack", "pingNumber", "TS_mean")
missing_columns <- setdiff(required_columns, names(processed_data))

if (length(missing_columns) > 0) {
  stop(paste("Missing required columns:", paste(missing_columns, collapse = ", ")))
}

track_data <- processed_data %>%
  mutate(
    fishNum = as.character(fishNum),
    species = as.character(species),
    FishTrack = as.character(FishTrack)
  )

safe_min <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  min(x, na.rm = TRUE)
}

safe_max <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  max(x, na.rm = TRUE)
}

fish_ping_counts <- track_data %>%
  group_by(fishNum, species) %>%
  summarize(
    n_pings = n_distinct(pingNumber),
    n_distinct_tracks = n_distinct(FishTrack),
    n_targets = n(),
    ping_number_min = safe_min(pingNumber),
    ping_number_max = safe_max(pingNumber),
    ts_mean_avg = mean(TS_mean, na.rm = TRUE),
    ts_mean_sd = sd(TS_mean, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(species, fishNum)

species_summary_statistics <- track_data %>%
  group_by(species) %>%
  summarize(
    n_fish = n_distinct(fishNum),
    n_pings = n_distinct(pingNumber),
    n_distinct_tracks = n_distinct(FishTrack),
    n_targets = n(),
    ts_mean_avg = mean(TS_mean, na.rm = TRUE),
    ts_mean_sd = sd(TS_mean, na.rm = TRUE),
    ts_mean_min = safe_min(TS_mean),
    ts_mean_median = median(TS_mean, na.rm = TRUE),
    ts_mean_max = safe_max(TS_mean),
    .groups = "drop"
  ) %>%
  arrange(species)

species_fish_counts <- species_summary_statistics %>%
  select(species, n_fish, n_targets, n_pings, n_distinct_tracks)

overall_summary <- tibble(
  n_species = n_distinct(track_data$species),
  n_fish = n_distinct(track_data$fishNum),
  n_targets = nrow(track_data),
  n_pings = n_distinct(track_data$pingNumber),
  n_distinct_tracks = n_distinct(track_data$FishTrack),
  ts_mean_overall_mean = mean(track_data$TS_mean, na.rm = TRUE),
  ts_mean_overall_sd = sd(track_data$TS_mean, na.rm = TRUE),
  ts_mean_overall_min = safe_min(track_data$TS_mean),
  ts_mean_overall_median = median(track_data$TS_mean, na.rm = TRUE),
  ts_mean_overall_max = safe_max(track_data$TS_mean)
)

write_csv(species_fish_counts, "ExploratoryAnalysis/species_fish_counts.csv")
write_csv(fish_ping_counts, "ExploratoryAnalysis/fish_ping_counts.csv")
write_csv(species_summary_statistics, "ExploratoryAnalysis/species_summary_statistics.csv")
write_csv(overall_summary, "ExploratoryAnalysis/overall_summary_statistics.csv")

cat("Wrote ExploratoryAnalysis/species_fish_counts.csv\n")
cat("Wrote ExploratoryAnalysis/fish_ping_counts.csv\n")
cat("Wrote ExploratoryAnalysis/species_summary_statistics.csv\n")
cat("Wrote ExploratoryAnalysis/overall_summary_statistics.csv\n")
