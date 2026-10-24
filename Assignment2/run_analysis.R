source("analysis.R")
source("display.R")

# Download the portfolio data
stocks <- load_stock_data()

# Calculate statistics for each stock
results <- list()

for (symbol in names(stocks)) {
  results[[symbol]] <- calculate_statistics(
    stocks[[symbol]],
    symbol
  )
}

# Display and save tables
summary_table <- display_stock_data(stocks, results)

# Save charts
for (symbol in names(results)) {
  plot_stock(results[[symbol]], symbol)
}

plot_statistics(summary_table)

# Save the R objects and session information
saveRDS(
  list(stocks = stocks, results = results),
  "output/analysis_results.rds"
)

writeLines(
  capture.output(sessionInfo()),
  "output/session_info.txt"
)

cat("\nPortfolio statistics:\n")
print(summary_table, row.names = FALSE)

cat("\nFinished! Results are saved in the output folder.\n")