# Script: combined_TS_compensation_plots.R
# Purpose: Generate one TS compensation figure per species (LT, LWF, SMB)
#          combining all individuals within each species.

library(dplyr)
library(ggplot2)

# Source custom functions (adjust path if needed)
source("Analysis_Scripts/custom_functions.R", echo = FALSE)

# ==================== Configuration ====================
output_dir <- "ExportedFigures"
max_total_rows_per_species <- 100000   # Limit total points per species
required_join_keys <- c("fishNum", "FishTrack", "Frequency")

# Create output directory if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# ==================== Define fish IDs per species ====================
species_list <- list(
  LT  = paste0("LT",  sprintf("%03d", 1:21)),
  LWF = paste0("LWF", sprintf("%03d", 1:15)),
  SMB = paste0("SMB", sprintf("%03d", 1:13))
)

# ==================== Helper function to safely read a fish's data ====================
read_fish_data_safe <- function(fish_id) {
  # Reads compensated and uncompensated data, joins, computes ts_difference.
  # Returns NULL if any error or missing required columns.
  
  comp <- tryCatch(
    read_comp_freq_response(fish_id),
    error = function(e) {
      message("  Skipping ", fish_id, ": cannot read compensated file (", e$message, ")")
      return(NULL)
    }
  )
  
  uncomp <- tryCatch(
    read_uncomp_freq_response(fish_id),
    error = function(e) {
      message("  Skipping ", fish_id, ": cannot read uncompensated file (", e$message, ")")
      return(NULL)
    }
  )
  
  if (is.null(comp) || is.null(uncomp)) return(NULL)
  
  # Check required columns
  missing_comp <- setdiff(required_join_keys, names(comp))
  missing_uncomp <- setdiff(required_join_keys, names(uncomp))
  
  if (length(missing_comp) > 0 || length(missing_uncomp) > 0) {
    warning("  Missing required keys for ", fish_id,
            "; comp missing: [", paste(missing_comp, collapse=", "), "]",
            "; uncomp missing: [", paste(missing_uncomp, collapse=", "), "] – skipping")
    return(NULL)
  }
  
  # Join and compute difference
  joined <- inner_join(comp, uncomp, by = required_join_keys) %>%
    mutate(ts_difference = TS - uncompTS,
           fish_id = fish_id)   # Keep track of individual (optional)
  
  return(joined)
}

# ==================== Process each species ====================
for (species in names(species_list)) {
  message("\nProcessing species: ", species)
  fish_ids <- species_list[[species]]
  
  # List to hold data frames from each fish
  species_data_list <- list()
  
  for (fish_id in fish_ids) {
    fish_data <- read_fish_data_safe(fish_id)
    if (!is.null(fish_data)) {
      species_data_list[[length(species_data_list) + 1]] <- fish_data
    }
  }
  
  if (length(species_data_list) == 0) {
    message("  No valid data for species ", species, " – skipping plot")
    next
  }
  
  # Combine all fish data for this species
  combined_data <- bind_rows(species_data_list)
  message("  Total rows before sampling: ", nrow(combined_data))
  
  # Randomly subsample if too many rows (to keep plot manageable)
  if (nrow(combined_data) > max_total_rows_per_species) {
    combined_data <- combined_data %>%
      slice_sample(n = max_total_rows_per_species)
    message("  Downsampled to ", max_total_rows_per_species, " rows")
  }
  
  # Create plot
  # Using all points from all individuals of the species, with high transparency
  p <- ggplot(combined_data, aes(x = Frequency, y = ts_difference)) +
    geom_point(alpha = 0.02, size = 0.8) +
    labs(
      title = paste("TS compensation across frequency -", species),
      x = "Frequency",
      y = "TS difference (compensated - uncompensated, dB)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      panel.grid.minor = element_blank()
    )
  
  # Save figure
  filename <- file.path(output_dir, paste0("TS_compensation_across_frequency_", species, ".png"))
  ggsave(filename, plot = p, width = 8, height = 5, dpi = 300)
  message("  Saved plot: ", filename)
}

message("\nAll species plots completed.")