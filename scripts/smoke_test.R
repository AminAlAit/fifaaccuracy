cat("=== FIFA Accuracy Docker Smoke Test ===\n")

# 1. Core package loading
cat("\n[1] Loading packages...\n")
suppressPackageStartupMessages({
  library(tidyverse)
  library(readr)
  library(dplyr)
  library(ggplot2)
  library(hrbrthemes)
  library(gganimate)
  library(gifski)
  library(wordcloud)
  library(rworldmap)
  library(gghighlight)
  library(ggridges)
  library(countrycode)
  library(janitor)
  library(reshape2)
})
cat("  OK: core packages loaded\n")

# Optional packages
if (requireNamespace("ggflags", quietly = TRUE)) {
  library(ggflags)
  cat("  OK: ggflags loaded\n")
} else {
  cat("  SKIP: ggflags not installed (optional)\n")
}

# 2. Data file discovery
cat("\n[2] Locating data files...\n")
find_data_file <- function(filename) {
  candidates <- c(
    file.path("data", filename),
    file.path("..", "data", filename),
    file.path(".", filename),
    file.path("~", "workspace", filename)
  )
  for (cand in candidates) {
    if (file.exists(cand)) return(cand)
  }
  stop(paste("Data file not found:", filename))
}

fifa_path    <- find_data_file("fifatable.csv")
stats_path   <- find_data_file("tm_stats.csv")
trophies_path <- find_data_file("tm_trophies.csv")
cat("  OK: fifatable.csv  ->", fifa_path, "\n")
cat("  OK: tm_stats.csv   ->", stats_path, "\n")
cat("  OK: tm_trophies.csv ->", trophies_path, "\n")

# 3. Data loading and integrity checks
cat("\n[3] Loading fifatable.csv...\n")
table <- read_csv(fifa_path, col_types = cols(...1 = col_skip()), show_col_types = FALSE)

n_rows    <- nrow(table)
n_players <- n_distinct(table$SoFifaID)
min_fifa  <- min(table$FifaIndex, na.rm = TRUE)
max_fifa  <- max(table$FifaIndex, na.rm = TRUE)

cat(sprintf("  Rows:       %d (expected 43,199)\n", n_rows))
cat(sprintf("  Players:    %d (expected 5,391)\n", n_players))
cat(sprintf("  FIFA range: %d to %d (expected 7 to 21)\n", min_fifa, max_fifa))

stopifnot(
  "Row count mismatch"    = n_rows    == 43199,
  "Player count mismatch" = n_players == 5391,
  "FIFA range wrong"      = min_fifa == 7 && max_fifa == 21
)
cat("  PASS: all integrity checks\n")

# 4. Load supporting datasets
cat("\n[4] Loading tm_stats.csv and tm_trophies.csv...\n")
stats    <- read_csv(stats_path,    show_col_types = FALSE)
trophies <- read_csv(trophies_path, show_col_types = FALSE)
cat(sprintf("  tm_stats.csv:    %d rows, %d columns\n", nrow(stats),    ncol(stats)))
cat(sprintf("  tm_trophies.csv: %d rows, %d columns\n", nrow(trophies), ncol(trophies)))

# 5. Quick ggplot2 smoke (renders to temp file, no display needed)
cat("\n[5] Testing ggplot2 render...\n")
p <- ggplot(table %>% filter(!is.na(overall)) %>% sample_n(500),
            aes(x = overall, y = max_potential)) +
  geom_point(alpha = 0.4, color = "#1a73e8") +
  labs(title = "Overall vs Potential", x = "Overall", y = "Potential")
tmp <- tempfile(fileext = ".png")
ggsave(tmp, plot = p, width = 6, height = 4, dpi = 72)
cat("  OK: plot saved to", tmp, "\n")

cat("\n=== All checks passed ===\n")
