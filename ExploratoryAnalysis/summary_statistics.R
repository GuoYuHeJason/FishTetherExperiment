library(dplyr)
library(readr)

rdata_path <- "ProcessedData/processed_AnalysisData.Rdata"

if (!file.exists(rdata_path)) {
  stop(paste("Input file does not exist:", rdata_path))
}

load(rdata_path)

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

safe_write_csv <- function(data, path) {
  tryCatch(
    {
      write_csv(data, path)
      cat(paste("Wrote", path, "\n"))
    },
    error = function(e) {
      stop(paste("Failed to write CSV:", path, "|", conditionMessage(e)))
    }
  )
}

fish_level_summary <- track_data %>%
  group_by(fishNum, species) %>%
  summarize(
    n_pings = n_distinct(pingNumber),
    n_tracks = n_distinct(FishTrack),
    n_targets = n(),
    ping_number_min = safe_min(pingNumber),
    ping_number_max = safe_max(pingNumber),
    ts_mean_avg = mean(TS_mean, na.rm = TRUE),
    ts_mean_sd = sd(TS_mean, na.rm = TRUE),
    ts_mean_min = safe_min(TS_mean),
    ts_mean_median = median(TS_mean, na.rm = TRUE),
    ts_mean_max = safe_max(TS_mean),
    .groups = "drop"
  ) %>%
  arrange(species, fishNum)

species_summary <- track_data %>%
  group_by(species) %>%
  summarize(
    n_fish = n_distinct(fishNum),
    n_pings = n_distinct(pingNumber),
    n_tracks = n_distinct(FishTrack),
    n_targets = n(),
    ts_mean_avg = mean(TS_mean, na.rm = TRUE),
    ts_mean_sd = sd(TS_mean, na.rm = TRUE),
    ts_mean_min = safe_min(TS_mean),
    ts_mean_median = median(TS_mean, na.rm = TRUE),
    ts_mean_max = safe_max(TS_mean),
    .groups = "drop"
  ) %>%
  arrange(species)

species_fish_counts <- species_summary %>%
  select(species, n_fish, n_targets, n_pings, n_tracks)

overall_summary <- tibble(
  n_species = n_distinct(track_data$species),
  n_fish = n_distinct(track_data$fishNum),
  n_targets = nrow(track_data),
  n_pings = n_distinct(track_data$pingNumber),
  n_tracks = n_distinct(track_data$FishTrack),
  ts_mean_overall_mean = mean(track_data$TS_mean, na.rm = TRUE),
  ts_mean_overall_sd = sd(track_data$TS_mean, na.rm = TRUE),
  ts_mean_overall_min = safe_min(track_data$TS_mean),
  ts_mean_overall_median = median(track_data$TS_mean, na.rm = TRUE),
  ts_mean_overall_max = safe_max(track_data$TS_mean)
)

safe_write_csv(species_fish_counts, "ExploratoryAnalysis/species_fish_counts.csv")
safe_write_csv(
  fish_level_summary %>%
    select(fishNum, species, n_pings, n_tracks, n_targets),
  "ExploratoryAnalysis/fish_ping_counts.csv"
)
safe_write_csv(fish_level_summary, "ExploratoryAnalysis/fish_level_summary.csv")
safe_write_csv(species_summary, "ExploratoryAnalysis/species_summary_statistics.csv")
safe_write_csv(overall_summary, "ExploratoryAnalysis/overall_summary_statistics.csv")

cat("Summary statistics generation completed successfully.\n")
