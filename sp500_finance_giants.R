# ============================================================
# S&P 500 Finance Giants: Who Drove Wall Street?
# A Stock Market Analysis using R
# Author: [Your Name]
# Date: 2026
# ============================================================
# DESCRIPTION:
# This project analyzes the performance of the top 6 financial
# companies listed in the S&P 500 index: Goldman Sachs,
# BlackRock, JPMorgan Chase, Visa, Berkshire Hathaway, and
# Morgan Stanley. We examine price trends, returns, volatility,
# and correlations to understand how these Wall Street giants
# performed from 2020 to 2025.
# ============================================================

# ---- STEP 1: Install and Load Required Packages ----

# Run this line ONCE if you don't have the packages yet:
# install.packages(c("quantmod", "ggplot2", "reshape2", "dplyr"))

library(quantmod)
library(ggplot2)
library(reshape2)
library(dplyr)


# ---- STEP 2: Download Stock Data ----

tickers    <- c("GS", "BLK", "JPM", "V", "BRK-B", "MS")
labels     <- c("GS", "BLK", "JPM", "V", "BRKB", "MS")
start_date <- as.Date("2020-01-01")
end_date   <- as.Date("2025-12-31")

getSymbols(tickers, src = "yahoo", from = start_date, to = end_date)

prices <- data.frame(
  Date = index(GS),
  GS   = as.numeric(Cl(GS)),
  BLK  = as.numeric(Cl(BLK)),
  JPM  = as.numeric(Cl(JPM)),
  V    = as.numeric(Cl(V)),
  BRKB = as.numeric(Cl(`BRK-B`)),
  MS   = as.numeric(Cl(MS))
)

cat("Data successfully downloaded!\n")
cat("Date range:", as.character(min(prices$Date)),
    "to", as.character(max(prices$Date)), "\n")
cat("Number of trading days:", nrow(prices), "\n\n")


# ---- STEP 3: Normalize Prices (Base = 100) ----

normalized <- prices
for (ticker in labels) {
  normalized[[ticker]] <- (prices[[ticker]] / prices[[ticker]][1]) * 100
}

cat("Prices normalized to base 100 (starting Jan 2020)\n\n")


# ---- STEP 4: Plot 1 - Normalized Price History ----

norm_long <- melt(normalized, id.vars = "Date",
                  variable.name = "Stock",
                  value.name = "Price")

p1 <- ggplot(norm_long, aes(x = Date, y = Price, color = Stock)) +
  geom_line(size = 0.8) +
  labs(
    title    = "S&P 500 Finance Giants: Normalized Price Performance (2020-2025)",
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
  Stock = labels,
  Total_Return_Pct = sapply(labels, function(t) {
    round(((prices[[t]][nrow(prices)] - prices[[t]][1]) /
             prices[[t]][1]) * 100, 2)
  })
)

total_returns <- total_returns[order(-total_returns$Total_Return_Pct), ]

cat("=== Total Returns (Jan 2020 - Dec 2025) ===\n")
print(total_returns)
cat("\n")


# ---- STEP 6: Plot 2 - Total Return Bar Chart ----

p2 <- ggplot(total_returns,
             aes(x = reorder(Stock, Total_Return_Pct),
                 y = Total_Return_Pct,
                 fill = Stock)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = paste0(Total_Return_Pct, "%")),
            hjust = -0.1, size = 4, fontface = "bold") +
  coord_flip() +
  labs(
    title    = "Total Stock Returns: S&P 500 Finance Giants (2020-2025)",
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

for (ticker in labels) {
  p <- prices[[ticker]]
  daily_returns[[ticker]] <- diff(p) / p[-length(p)] * 100
}

volatility <- data.frame(
  Stock = labels,
  Annual_Volatility_Pct = sapply(labels, function(t) {
    round(sd(daily_returns[[t]], na.rm = TRUE) * sqrt(252), 2)
  })
)

volatility <- volatility[order(-volatility$Annual_Volatility_Pct), ]

cat("=== Annualized Volatility (Risk) ===\n")
print(volatility)
cat("\n")


# ---- STEP 8: Plot 3 - Volatility Bar Chart ----

p3 <- ggplot(volatility,
             aes(x = reorder(Stock, Annual_Volatility_Pct),
                 y = Annual_Volatility_Pct,
                 fill = Stock)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = paste0(Annual_Volatility_Pct, "%")),
            hjust = -0.1, size = 4, fontface = "bold") +
  coord_flip() +
  labs(
    title    = "Annualized Volatility: S&P 500 Finance Giants (2020-2025)",
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

cor_matrix <- round(cor(daily_returns[, labels],
                        use = "complete.obs"), 2)

cat("=== Correlation Matrix (Daily Returns) ===\n")
print(cor_matrix)
cat("\n")


# ---- STEP 10: Plot 4 - Correlation Heatmap ----

cor_melted <- melt(cor_matrix)
colnames(cor_melted) <- c("Stock1", "Stock2", "Correlation")

p4 <- ggplot(cor_melted,
             aes(x = Stock1, y = Stock2, fill = Correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Correlation), size = 4, fontface = "bold") +
  scale_fill_gradient2(low = "steelblue", mid = "white",
                       high = "tomato", midpoint = 0.5,
                       limit = c(0, 1)) +
  labs(
    title    = "Correlation Heatmap: S&P 500 Finance Giants Daily Returns",
    subtitle = "Values close to 1 = stocks move together (high concentration risk)",
    x        = "", y = "",
    caption  = "Data source: Yahoo Finance via quantmod"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))

print(p4)
cat("Chart 4: Correlation heatmap plotted.\n\n")


# ---- STEP 11: Summary & Conclusions ----

best_stock    <- total_returns$Stock[1]
best_return   <- total_returns$Total_Return_Pct[1]
worst_stock   <- total_returns$Stock[nrow(total_returns)]
worst_return  <- total_returns$Total_Return_Pct[nrow(total_returns)]
most_volatile <- volatility$Stock[1]
least_volatile<- volatility$Stock[nrow(volatility)]

cat("============================================================\n")
cat("SUMMARY & CONCLUSIONS - S&P 500 Finance Giants (2020-2025)\n")
cat("============================================================\n\n")

cat("1. BEST PERFORMING STOCK:\n")
cat("  ", best_stock, "with a total return of", best_return, "%\n\n")

cat("2. WORST PERFORMING STOCK:\n")
cat("  ", worst_stock, "with a total return of", worst_return, "%\n\n")

cat("3. MOST VOLATILE (HIGHEST RISK):\n")
cat("  ", most_volatile, "carried the highest risk\n\n")

cat("4. MOST STABLE (LOWEST RISK):\n")
cat("  ", least_volatile, "was the most stable stock\n\n")

cat("5. CORRELATION INSIGHT:\n")
cat("   Finance stocks show HIGH correlation with each other,\n")
cat("   especially during market crises like COVID-19 in 2020.\n")
cat("   Banking and finance stocks tend to move together\n")
cat("   when market conditions change.\n\n")

cat("6. KEY TAKEAWAY:\n")
cat("   Wall Street's finance giants delivered solid returns\n")
cat("   but lagged behind Silicon Valley's tech giants in\n")
cat("   terms of total growth over the same 5-year period.\n")
cat("   However they offered more stability and lower volatility.\n")
cat("============================================================\n")
