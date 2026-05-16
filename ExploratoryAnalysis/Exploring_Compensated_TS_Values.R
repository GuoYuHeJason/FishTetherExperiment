library(dplyr)
library(ggplot2)

source("Analysis_Scripts/custom_functions.R", echo = FALSE)
# uses custom functions that read from individual fish files, can't be generalized.
set.seed(123)

fish_ids <- c("LT016", "LT015", "LWF007", "LWF010", "SMB005", "SMB006")
output_dir <- "ExportedFigures"
max_rows_per_fish <- 10000
required_join_keys <- c("fishNum", "FishTrack", "Frequency")

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

for (fish_id in fish_ids) {
  comp <- read_comp_freq_response(fish_id)
  uncomp <- read_uncomp_freq_response(fish_id)

  missing_comp_keys <- setdiff(required_join_keys, names(comp))
  missing_uncomp_keys <- setdiff(required_join_keys, names(uncomp))

  if (length(missing_comp_keys) > 0 || length(missing_uncomp_keys) > 0) {
    stop(
      paste0(
        "Missing required join keys for fish ", fish_id,
        "; comp missing: [", paste(missing_comp_keys, collapse = ", "), "]",
        "; uncomp missing: [", paste(missing_uncomp_keys, collapse = ", "), "]"
      )
    )
  }

  ts_difference_data <- inner_join(comp, uncomp, by = required_join_keys) %>%
    mutate(ts_difference = TS - uncompTS)

  if (nrow(ts_difference_data) > max_rows_per_fish) {
    ts_difference_data <- ts_difference_data[sample.int(nrow(ts_difference_data), max_rows_per_fish), ]
  }

  p <- ggplot(ts_difference_data, aes(x = Frequency, y = ts_difference)) +
    geom_point(alpha = 0.01) +
    labs(
      title = paste("TS compensation across frequency -", fish_id),
      x = "Frequency",
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
