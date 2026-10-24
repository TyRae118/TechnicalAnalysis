# Display imported data and export the results
display_stock_data <- function(stocks, results) {
  dir.create("output", showWarnings = FALSE)
  
  for (symbol in names(stocks)) {
    cat("\n==========", symbol, "==========\n")
    
    cat("\nFirst six rows:\n")
    print(head(stocks[[symbol]]), row.names = FALSE)
    
    cat("\nLast six rows:\n")
    print(tail(stocks[[symbol]]), row.names = FALSE)
    
    cat("\nStatistics:\n")
    print(results[[symbol]]$summary, row.names = FALSE)
    
    write.csv(
      stocks[[symbol]],
      paste0("output/", symbol, "_data.csv"),
      row.names = FALSE
    )
    
    write.csv(
      results[[symbol]]$data,
      paste0("output/", symbol, "_moving_average.csv"),
      row.names = FALSE
    )
  }
  
  summary_table <- do.call(
    rbind,
    lapply(results, function(result) result$summary)
  )
  
  rownames(summary_table) <- NULL
  
  write.csv(
    summary_table,
    "output/portfolio_statistics.csv",
    row.names = FALSE
  )
  
  return(summary_table)
}


# Save a closing price and moving average chart
plot_stock <- function(result, symbol) {
  stock_data <- result$data
  
  png(
    paste0("output/", symbol, "_price_SMA.png"),
    width = 1200,
    height = 700
  )
  on.exit(dev.off())
  
  plot(
    stock_data$Date,
    stock_data$Close,
    type = "l",
    col = "blue",
    lwd = 2,
    main = paste(symbol, "Closing Price and 20-Day Moving Average"),
    xlab = "Date",
    ylab = "Closing price (USD)"
  )
  
  lines(
    stock_data$Date,
    stock_data$SMA,
    col = "orange",
    lwd = 2
  )
  
  legend(
    "topleft",
    legend = c("Closing price", "20-day moving average"),
    col = c("blue", "orange"),
    lty = 1,
    lwd = 2,
    bty = "n"
  )
}


# Save a comparison of the portfolio statistics
plot_statistics <- function(summary_table) {
  png("output/portfolio_comparison.png", width = 1200, height = 700)
  on.exit(dev.off())
  
  par(mfrow = c(1, 2))
  
  barplot(
    rbind(summary_table$Mean, summary_table$Median),
    beside = TRUE,
    names.arg = summary_table$Symbol,
    col = c("steelblue", "lightgreen"),
    main = "Mean and Median",
    ylab = "Closing price (USD)"
  )
  
  legend(
    "topleft",
    legend = c("Mean", "Median"),
    fill = c("steelblue", "lightgreen"),
    bty = "n"
  )
  
  barplot(
    summary_table$Standard_deviation,
    names.arg = summary_table$Symbol,
    col = "orange",
    main = "Closing Price Standard Deviation",
    ylab = "Sample standard deviation (USD)"
  )
}