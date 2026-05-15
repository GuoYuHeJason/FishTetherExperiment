args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) stop("Usage: Rscript subset_rdata.R /path/to/input.RData [n_rows] [out_prefix]")

infile <- args[1]
n_rows <- if (length(args) >= 2) as.integer(args[2]) else 100
out_prefix <- if (length(args) >= 3) args[3] else sub("\\.RData$", "", basename(infile))

loaded_names <- load(infile)
# find first data.frame or matrix
found_name <- NULL
for (nm in loaded_names) {
  obj <- get(nm)
  if (is.data.frame(obj) || is.matrix(obj)) { found_name <- nm; break }
}
if (is.null(found_name)) stop("No data.frame or matrix found in the RData file.")

df <- get(found_name)
subset_df <- head(as.data.frame(df), n_rows)

rdata_out <- paste0(out_prefix, "_subset.RData")
csv_out <- paste0(out_prefix, "_subset.csv")

# save subset as RData (object named as subset_df)
save(subset_df, file = rdata_out)
write.csv(subset_df, file = csv_out, row.names = FALSE)

cat("Wrote:", rdata_out, "\n")
cat("Wrote:", csv_out, "\n")