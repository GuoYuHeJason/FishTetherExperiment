library(dplyr)
library(ggplot2)

source("Analysis_Scripts/custom_functions.R", echo = FALSE)

set.seed(123)

fish_ids <- c("LT016", "LT015", "LWF007", "LWF010", "SMB005", "SMB006")
output_dir <- "ExportedFigures"
max_sample_size <- 10000
required_join_keys <- c("fishNum", "FishTrack", "Frequency")

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

for (fish_id in fish_ids) {
  comp <- read_comp_freq_response(fish_id)
  uncomp <- read_uncomp_freq_response(fish_id)

  if (!all(required_join_keys %in% names(comp)) || !all(required_join_keys %in% names(uncomp))) {
    stop(paste("Missing required join keys for fish", fish_id))
  }

  tsdiff <- inner_join(comp, uncomp, by = required_join_keys) %>%
    mutate(TSdifference = TS - uncompTS)

  if (nrow(tsdiff) > max_sample_size) {
    tsdiff <- tsdiff[sample(nrow(tsdiff), max_sample_size), ]
  }

  p <- ggplot(tsdiff, aes(Frequency, TSdifference)) +
    geom_point(alpha = 0.01) +
    labs(
      title = paste("TS compensation across frequency -", fish_id),
      x = "Frequency (kHz)",
      y = "TS difference (compensated - uncompensated, dB)"
    )

  ggsave(
    filename = file.path(output_dir, paste0("TS_compensation_across_frequency_", fish_id, ".png")),
    plot = p,
    width = 8,
    height = 5,
    dpi = 300
  )
}
