# ============================================================
# NASDAQ-100 Tech Giants: Who Drove the Market?
# A Stock Market Analysis using R
# Author: [Your Name]
# Date: 2026
# ============================================================
# DESCRIPTION:
# This project analyzes the performance of the top 6 technology
# companies listed on the NASDAQ-100 index: Apple, Microsoft,
# Google, Amazon, Meta, and NVIDIA. We examine price trends,
# returns, volatility, and correlations to understand how these
# giants have shaped the NASDAQ index.
# ============================================================

# ---- STEP 1: Install and Load Required Packages ----

# Run this line ONCE if you don't have the packages yet:
# install.packages(c("quantmod", "ggplot2", "reshape2", "dplyr"))

library(quantmod)   # For downloading stock data from Yahoo Finance
library(ggplot2)    # For beautiful charts
library(reshape2)   # For reshaping data (used in heatmap)
library(dplyr)      # For data manipulation


# ---- STEP 2: Download Stock Data ----

# Define our 6 tech giants and the time period
tickers <- c("AAPL", "MSFT", "GOOGL", "AMZN", "META", "NVDA")
start_date <- as.Date("2020-01-01")
end_date   <- as.Date("2025-12-31")

# Download data from Yahoo Finance automatically
getSymbols(tickers, src = "yahoo", from = start_date, to = end_date)

# Extract only the closing prices for each stock
prices <- data.frame(
  Date  = index(AAPL),
  AAPL  = as.numeric(Cl(AAPL)),
  MSFT  = as.numeric(Cl(MSFT)),
  GOOGL = as.numeric(Cl(GOOGL)),
  AMZN  = as.numeric(Cl(AMZN)),
  META  = as.numeric(Cl(META)),
  NVDA  = as.numeric(Cl(NVDA))
)

cat("Data successfully downloaded!\n")
cat("Date range:", as.character(min(prices$Date)), "to", as.character(max(prices$Date)), "\n")
cat("Number of trading days:", nrow(prices), "\n\n")


# ---- STEP 3: Normalize Prices (Base = 100) ----
# We normalize to 100 so we can fairly compare stocks
# with very different price levels (e.g. NVDA vs AAPL)

normalized <- prices
for (ticker in tickers) {
  normalized[[ticker]] <- (prices[[ticker]] / prices[[ticker]][1]) * 100
}

cat("Prices normalized to base 100 (starting Jan 2020)\n\n")


# ---- STEP 4: Plot 1 - Normalized Price History ----

norm_long <- melt(normalized, id.vars = "Date",
                  variable.name = "Stock", value.name = "Price")

p1 <- ggplot(norm_long, aes(x = Date, y = Price, color = Stock)) +
  geom_line(size = 0.8) +
  labs(
    title    = "NASDAQ-100 Tech Giants: Normalized Price Performance (2020-2025)",
    subtitle = "Base = 100 at January 2020",
    x        = "Date",
    y        = "Normalized Price (Base 100)",
    color    = "Stock",
    caption  = "Data source: Yahoo Finance via quantmod"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

print(p1)
cat("Chart 1: Normalized price history plotted.\n\n")


# ---- STEP 5: Calculate Total Returns ----

total_returns <- data.frame(
  Stock = tickers,
  Total_Return_Pct = sapply(tickers, function(t) {
    first_price <- prices[[t]][1]
    last_price  <- prices[[t]][nrow(prices)]
    round(((last_price - first_price) / first_price) * 100, 2)
  })
)

total_returns <- total_returns[order(-total_returns$Total_Return_Pct), ]

cat("=== Total Returns (Jan 2020 - Dec 2025) ===\n")
print(total_returns)
cat("\n")


# ---- STEP 6: Plot 2 - Total Return Bar Chart ----

p2 <- ggplot(total_returns, aes(x = reorder(Stock, Total_Return_Pct),
                                 y = Total_Return_Pct, fill = Stock)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = paste0(Total_Return_Pct, "%")),
            hjust = -0.1, size = 4, fontface = "bold") +
  coord_flip() +
  labs(
    title    = "Total Stock Returns: NASDAQ Tech Giants (2020-2025)",
    subtitle = "Percentage gain from January 2020 to December 2025",
    x        = "Stock",
    y        = "Total Return (%)",
    caption  = "Data source: Yahoo Finance via quantmod"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))

print(p2)
cat("Chart 2: Total return bar chart plotted.\n\n")


# ---- STEP 7: Calculate Daily Returns & Volatility ----

daily_returns <- data.frame(Date = prices$Date[-1])

for (ticker in tickers) {
  p <- prices[[ticker]]
  daily_returns[[ticker]] <- diff(p) / p[-length(p)] * 100
}

# Annualized volatility = std dev of daily returns * sqrt(252 trading days)
volatility <- data.frame(
  Stock = tickers,
  Annual_Volatility_Pct = sapply(tickers, function(t) {
    round(sd(daily_returns[[t]], na.rm = TRUE) * sqrt(252), 2)
  })
)

volatility <- volatility[order(-volatility$Annual_Volatility_Pct), ]

cat("=== Annualized Volatility (Risk) ===\n")
print(volatility)
cat("\n")


# ---- STEP 8: Plot 3 - Volatility Bar Chart ----

p3 <- ggplot(volatility, aes(x = reorder(Stock, Annual_Volatility_Pct),
                              y = Annual_Volatility_Pct, fill = Stock)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = paste0(Annual_Volatility_Pct, "%")),
            hjust = -0.1, size = 4, fontface = "bold") +
  coord_flip() +
  labs(
    title    = "Annualized Volatility of NASDAQ Tech Giants (2020-2025)",
    subtitle = "Higher volatility = higher risk",
    x        = "Stock",
    y        = "Annualized Volatility (%)",
    caption  = "Data source: Yahoo Finance via quantmod"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))

print(p3)
cat("Chart 3: Volatility chart plotted.\n\n")


# ---- STEP 9: Correlation Matrix ----

cor_matrix <- round(cor(daily_returns[, tickers], use = "complete.obs"), 2)

cat("=== Correlation Matrix (Daily Returns) ===\n")
print(cor_matrix)
cat("\n")


# ---- STEP 10: Plot 4 - Correlation Heatmap ----

cor_melted <- melt(cor_matrix)
colnames(cor_melted) <- c("Stock1", "Stock2", "Correlation")

p4 <- ggplot(cor_melted, aes(x = Stock1, y = Stock2, fill = Correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Correlation), size = 4, fontface = "bold") +
  scale_fill_gradient2(low = "steelblue", mid = "white", high = "tomato",
                       midpoint = 0.5, limit = c(0, 1)) +
  labs(
    title    = "Correlation Heatmap: NASDAQ Tech Giants Daily Returns",
    subtitle = "Values close to 1 = stocks move together (high concentration risk)",
    x        = "", y        = "",
    caption  = "Data source: Yahoo Finance via quantmod"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))

print(p4)
cat("Chart 4: Correlation heatmap plotted.\n\n")


# ---- STEP 11: Summary & Conclusions ----

cat("============================================================\n")
cat("SUMMARY & CONCLUSIONS\n")
cat("============================================================\n\n")

best_stock <- total_returns$Stock[1]
best_return <- total_returns$Total_Return_Pct[1]
most_volatile <- volatility$Stock[1]

cat("1. BEST PERFORMING STOCK (2020-2025):\n")
cat("  ", best_stock, "with a total return of", best_return, "%\n\n")

cat("2. MOST VOLATILE STOCK (HIGHEST RISK):\n")
cat("  ", most_volatile, "had the highest annualized volatility\n\n")

cat("3. CORRELATION INSIGHT:\n")
cat("   All 6 stocks show HIGH correlation with each other.\n")
cat("   This means NASDAQ-100 carries significant CONCENTRATION RISK.\n")
cat("   When tech falls, ALL of these stocks tend to fall together.\n\n")

cat("4. KEY TAKEAWAY:\n")
cat("   These 6 companies have dramatically outperformed the market,\n")
cat("   but their high correlation means investors are not as\n")
cat("   diversified as they might think by holding multiple tech stocks.\n")
cat("============================================================\n")
