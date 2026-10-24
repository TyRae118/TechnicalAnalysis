# Read portfolio symbols and download each stock into a data frame
load_stock_data <- function(file = "portfolio.txt",
                            from = "2025-01-01",
                            to = "2026-01-01") {
  
  symbols <- unique(toupper(trimws(readLines(file, warn = FALSE))))
  symbols <- symbols[nzchar(symbols)]
  
  if (length(symbols) == 0) {
    stop("No stock symbols found in portfolio.txt.")
  }
  
  stocks <- list()
  
  for (symbol in symbols) {
    cat("Downloading", symbol, "\n")
    
    stock <- quantmod::getSymbols(
      symbol,
      src = "yahoo",
      from = from,
      to = to,
      auto.assign = FALSE
    )
    
    if (NROW(stock) == 0) {
      stop(paste("No data returned for", symbol))
    }
    
    stocks[[symbol]] <- data.frame(
      Date = as.Date(zoo::index(stock)),
      Open = as.numeric(quantmod::Op(stock)),
      High = as.numeric(quantmod::Hi(stock)),
      Low = as.numeric(quantmod::Lo(stock)),
      Close = as.numeric(quantmod::Cl(stock)),
      Volume = as.numeric(quantmod::Vo(stock)),
      Adjusted = as.numeric(quantmod::Ad(stock))
    )
  }
  
  return(stocks)
}


# Find the most frequent value, including tied modes
calculate_mode <- function(values) {
  values <- values[is.finite(values)]
  unique_values <- unique(values)
  counts <- tabulate(match(values, unique_values))
  
  if (length(counts) == 0 || max(counts) == 1) {
    return("No repeated closing price")
  }
  
  modes <- sort(unique_values[counts == max(counts)])
  
  return(paste(formatC(modes, format = "f", digits = 2),
               collapse = ", "))
}


# Calculate statistics using daily closing prices in USD
calculate_statistics <- function(stock_data, symbol, window = 20) {
  stock_data <- stock_data[order(stock_data$Date), ]
  
  valid <- is.finite(stock_data$Close)
  removed <- sum(!valid)
  stock_data <- stock_data[valid, ]
  
  prices <- stock_data$Close
  
  if (length(prices) < window) {
    stop(paste("Not enough closing prices for", symbol))
  }
  
  stock_data$SMA <- as.numeric(TTR::SMA(prices, n = window))
  
  summary <- data.frame(
    Symbol = symbol,
    Observations = length(prices),
    Missing_rows_removed = removed,
    Mean = mean(prices),
    Mode = calculate_mode(round(prices, 2)),
    Median = median(prices),
    Standard_deviation = sd(prices),
    Latest_SMA = tail(stock_data$SMA, 1)
  )
  
  return(list(data = stock_data, summary = summary))
}