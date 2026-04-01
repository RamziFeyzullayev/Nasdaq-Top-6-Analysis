# ============================================================
# Silicon Valley vs Wall Street:
# NASDAQ Tech Giants vs S&P 500 Finance Giants (2020-2025)
# A Comparative Stock Market Analysis using R
# Author: [Your Name]
# Date: 2026
# ============================================================
# DESCRIPTION:
# This project compares the top 6 NASDAQ tech giants
# (Apple, Microsoft, Google, Amazon, Meta, NVIDIA) against
# the top 6 S&P 500 finance giants (Goldman Sachs, BlackRock,
# JPMorgan, Visa, Berkshire Hathaway, Morgan Stanley).
# The central question: Who won — Silicon Valley or Wall Street?
# ============================================================

# ---- STEP 1: Install and Load Required Packages ----

# install.packages(c("quantmod", "ggplot2", "reshape2", "dplyr"))

library(quantmod)
library(ggplot2)
library(reshape2)
library(dplyr)


# ---- STEP 2: Download All 12 Stocks ----

tech_tickers    <- c("AAPL", "MSFT", "GOOGL", "AMZN", "META", "NVDA")
finance_tickers <- c("GS", "BLK", "JPM", "V", "BRK-B", "MS")
finance_labels  <- c("GS", "BLK", "JPM", "V", "BRKB", "MS")
all_tickers     <- c(tech_tickers, finance_tickers)

start_date <- as.Date("2020-01-01")
end_date   <- as.Date("2025-12-31")

getSymbols(all_tickers, src = "yahoo", from = start_date, to = end_date)

prices <- data.frame(
  Date  = index(AAPL),
  AAPL  = as.numeric(Cl(AAPL)),
  MSFT  = as.numeric(Cl(MSFT)),
  GOOGL = as.numeric(Cl(GOOGL)),
  AMZN  = as.numeric(Cl(AMZN)),
  META  = as.numeric(Cl(META)),
  NVDA  = as.numeric(Cl(NVDA)),
  GS    = as.numeric(Cl(GS)),
  BLK   = as.numeric(Cl(BLK)),
  JPM   = as.numeric(Cl(JPM)),
  V     = as.numeric(Cl(V)),
  BRKB  = as.numeric(Cl(`BRK-B`)),
  MS    = as.numeric(Cl(MS))
)

all_labels <- c("AAPL", "MSFT", "GOOGL", "AMZN", "META", "NVDA",
                "GS", "BLK", "JPM", "V", "BRKB", "MS")

cat("All 12 stocks successfully downloaded!\n")
cat("Date range:", as.character(min(prices$Date)),
    "to", as.character(max(prices$Date)), "\n\n")


# ---- STEP 3: Normalize All Prices to Base 100 ----

normalized <- prices
for (t in all_labels) {
  normalized[[t]] <- (prices[[t]] / prices[[t]][1]) * 100
}


# ---- STEP 4: Plot 1 - All 12 Stocks Normalized (Tech vs Finance) ----

norm_long <- melt(normalized, id.vars = "Date",
                  variable.name = "Stock",
                  value.name = "Price")

norm_long$Sector <- ifelse(norm_long$Stock %in% c("AAPL","MSFT","GOOGL",
                                                    "AMZN","META","NVDA"),
                           "NASDAQ Tech", "S&P 500 Finance")

p1 <- ggplot(norm_long, aes(x = Date, y = Price,
                             color = Sector, group = Stock)) +
  geom_line(size = 0.7, alpha = 0.85) +
  scale_color_manual(values = c("NASDAQ Tech"     = "tomato",
                                "S&P 500 Finance" = "steelblue")) +
  labs(
    title    = "Silicon Valley vs Wall Street: Normalized Performance (2020-2025)",
    subtitle = "Red = NASDAQ Tech Giants | Blue = S&P 500 Finance Giants | Base = 100 at Jan 2020",
    x        = "Date",
    y        = "Normalized Price (Base 100)",
    color    = "Sector",
    caption  = "Data source: Yahoo Finance via quantmod"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

print(p1)
cat("Chart 1: All 12 stocks normalized performance plotted.\n\n")


# ---- STEP 5: Total Returns for All 12 Stocks ----

total_returns <- data.frame(
  Stock = all_labels,
  Total_Return_Pct = sapply(all_labels, function(t) {
    round(((prices[[t]][nrow(prices)] - prices[[t]][1]) /
             prices[[t]][1]) * 100, 2)
  }),
  Sector = c(rep("NASDAQ Tech", 6), rep("S&P 500 Finance", 6))
)

total_returns <- total_returns[order(-total_returns$Total_Return_Pct), ]

cat("=== Total Returns: All 12 Stocks ===\n")
print(total_returns)
cat("\n")


# ---- STEP 6: Plot 2 - Total Return Comparison Bar Chart ----

p2 <- ggplot(total_returns,
             aes(x = reorder(Stock, Total_Return_Pct),
                 y = Total_Return_Pct,
                 fill = Sector)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = paste0(Total_Return_Pct, "%")),
            hjust = -0.1, size = 3.5, fontface = "bold") +
  coord_flip() +
  scale_fill_manual(values = c("NASDAQ Tech"     = "tomato",
                               "S&P 500 Finance" = "steelblue")) +
  labs(
    title    = "Total Returns: NASDAQ Tech vs S&P 500 Finance (2020-2025)",
    subtitle = "Red = Tech Giants | Blue = Finance Giants",
    x        = "Stock",
    y        = "Total Return (%)",
    fill     = "Sector",
    caption  = "Data source: Yahoo Finance via quantmod"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))

print(p2)
cat("Chart 2: Total return comparison plotted.\n\n")


# ---- STEP 7: Average Return by Sector ----

sector_summary <- total_returns %>%
  group_by(Sector) %>%
  summarise(
    Avg_Return = round(mean(Total_Return_Pct), 2),
    Max_Return = max(Total_Return_Pct),
    Min_Return = min(Total_Return_Pct)
  )

cat("=== Average Return by Sector ===\n")
print(sector_summary)
cat("\n")


# ---- STEP 8: Volatility for All 12 Stocks ----

daily_returns <- data.frame(Date = prices$Date[-1])
for (t in all_labels) {
  p <- prices[[t]]
  daily_returns[[t]] <- diff(p) / p[-length(p)] * 100
}

volatility <- data.frame(
  Stock = all_labels,
  Volatility = sapply(all_labels, function(t) {
    round(sd(daily_returns[[t]], na.rm = TRUE) * sqrt(252), 2)
  }),
  Sector = c(rep("NASDAQ Tech", 6), rep("S&P 500 Finance", 6))
)

volatility <- volatility[order(-volatility$Volatility), ]

cat("=== Volatility Comparison: All 12 Stocks ===\n")
print(volatility)
cat("\n")


# ---- STEP 9: Plot 3 - Volatility Comparison ----

p3 <- ggplot(volatility,
             aes(x = reorder(Stock, Volatility),
                 y = Volatility,
                 fill = Sector)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = paste0(Volatility, "%")),
            hjust = -0.1, size = 3.5, fontface = "bold") +
  coord_flip() +
  scale_fill_manual(values = c("NASDAQ Tech"     = "tomato",
                               "S&P 500 Finance" = "steelblue")) +
  labs(
    title    = "Volatility Comparison: NASDAQ Tech vs S&P 500 Finance (2020-2025)",
    subtitle = "Higher bar = higher risk | Red = Tech | Blue = Finance",
    x        = "Stock",
    y        = "Annualized Volatility (%)",
    fill     = "Sector",
    caption  = "Data source: Yahoo Finance via quantmod"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))

print(p3)
cat("Chart 3: Volatility comparison plotted.\n\n")


# ---- STEP 10: Plot 4 - Risk vs Return Scatter (The Star Chart) ----

risk_return <- merge(
  total_returns[, c("Stock", "Total_Return_Pct", "Sector")],
  volatility[,   c("Stock", "Volatility")],
  by = "Stock"
)

p4 <- ggplot(risk_return,
             aes(x = Volatility,
                 y = Total_Return_Pct,
                 color = Sector,
                 label = Stock)) +
  geom_point(size = 5) +
  geom_text(vjust = -1, size = 3.5, fontface = "bold") +
  scale_color_manual(values = c("NASDAQ Tech"     = "tomato",
                                "S&P 500 Finance" = "steelblue")) +
  geom_hline(yintercept = mean(total_returns$Total_Return_Pct),
             linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = mean(volatility$Volatility),
             linetype = "dashed", color = "gray50") +
  annotate("text", x = min(volatility$Volatility) + 1,
           y = max(total_returns$Total_Return_Pct) * 0.95,
           label = "High Return\nLow Risk ★",
           color = "darkgreen", size = 3.5, fontface = "bold") +
  annotate("text", x = max(volatility$Volatility) - 1,
           y = min(total_returns$Total_Return_Pct) + 20,
           label = "Low Return\nHigh Risk ✗",
           color = "darkred", size = 3.5, fontface = "bold") +
  labs(
    title    = "Risk vs Return: Silicon Valley vs Wall Street (2020-2025)",
    subtitle = "Top-left = ideal (high return, low risk) | Dashed lines = averages",
    x        = "Risk: Annualized Volatility (%)",
    y        = "Reward: Total Return (%)",
    color    = "Sector",
    caption  = "Data source: Yahoo Finance via quantmod"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

print(p4)
cat("Chart 4: Risk vs Return scatter plot plotted.\n\n")


# ---- STEP 11: Final Summary & Conclusions ----

tech_avg    <- sector_summary$Avg_Return[sector_summary$Sector == "NASDAQ Tech"]
finance_avg <- sector_summary$Avg_Return[sector_summary$Sector == "S&P 500 Finance"]
winner      <- ifelse(tech_avg > finance_avg, "NASDAQ Tech Giants", "S&P 500 Finance Giants")

tech_vol    <- round(mean(volatility$Volatility[volatility$Sector == "NASDAQ Tech"]), 2)
finance_vol <- round(mean(volatility$Volatility[volatility$Sector == "S&P 500 Finance"]), 2)

best_overall <- total_returns$Stock[1]
best_return  <- total_returns$Total_Return_Pct[1]

cat("============================================================\n")
cat("FINAL CONCLUSIONS: Silicon Valley vs Wall Street (2020-2025)\n")
cat("============================================================\n\n")

cat("1. OVERALL WINNER BY AVERAGE RETURN:\n")
cat("  ", winner, "\n")
cat("   Tech Average Return   :", tech_avg, "%\n")
cat("   Finance Average Return:", finance_avg, "%\n\n")

cat("2. BEST SINGLE STOCK ACROSS ALL 12:\n")
cat("  ", best_overall, "with", best_return, "% total return\n\n")

cat("3. RISK COMPARISON:\n")
cat("   Average Tech Volatility   :", tech_vol, "%\n")
cat("   Average Finance Volatility:", finance_vol, "%\n\n")

cat("4. KEY INSIGHT:\n")
cat("   Tech stocks delivered significantly higher returns\n")
cat("   but came with considerably higher risk and volatility.\n")
cat("   Finance stocks were more stable but grew more slowly.\n\n")

cat("5. FINAL TAKEAWAY:\n")
cat("   An investor choosing pure tech would have earned more,\n")
cat("   but experienced dramatic swings along the way.\n")
cat("   A mixed portfolio of both sectors would have balanced\n")
cat("   growth and stability — the essence of diversification.\n")
cat("============================================================\n")
