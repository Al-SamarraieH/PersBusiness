# Load required libraries
library(dplyr)
library(ggplot2)
library(zoo)

# Load the data
df <- read.csv("BPB.csv", stringsAsFactors = FALSE)

# Simple publication count by year
annual_data <- df %>%
  filter(!is.na(PY)) %>%
  count(PY, name = "publications") %>%
  arrange(PY)

# Create complete year sequence
all_years <- data.frame(PY = min(annual_data$PY):max(annual_data$PY))
annual_trends <- annual_data %>%
  right_join(all_years, by = "PY") %>%
  mutate(publications = ifelse(is.na(publications), 0, publications)) %>%
  arrange(PY)

# Apply smoothing
annual_trends$smoothed_count <- rollmean(annual_trends$publications, 
                                        k = 3, fill = NA, align = "center")

# Calculate linear regression for the trend line
linear_model <- lm(publications ~ PY, data = annual_trends)
linear_growth <- coef(linear_model)["PY"]
linear_r2 <- summary(linear_model)$r.squared

# Create the cleaner plot without vertical lines
p <- ggplot(annual_trends, aes(x = PY)) +
  # Main smoothed trend line - represents actual publication patterns
  geom_line(aes(y = smoothed_count), color = "black", linewidth = 1.2, alpha = 0.9, na.rm = TRUE) +
  # Markers on smoothed line - highlights individual data points
  geom_point(aes(y = smoothed_count), color = "black", size = 2, shape = 19, alpha = 0.7) +
  # Regression trend line (RED - now the primary focus) - shows overall growth trajectory
  geom_smooth(aes(y = publications), method = "lm", formula = y ~ x, 
              color = "red", linetype = "solid", se = TRUE, 
              fill = "red", alpha = 0.2, linewidth = 1) +
  # Era background colors (no vertical lines needed) - contextualizes temporal periods
  annotate("rect", xmin = min(annual_trends$PY), xmax = 2012, ymin = -Inf, ymax = Inf,
           alpha = 0.15, fill = "#E8F5E8") + # Light green - Foundational Era
  annotate("rect", xmin = 2013, xmax = 2020, ymin = -Inf, ymax = Inf,
           alpha = 0.15, fill = "#FFE5D9") + # Light peach - Integration Era
  annotate("rect", xmin = 2021, xmax = max(annual_trends$PY), ymin = -Inf, ymax = Inf,
           alpha = 0.15, fill = "#E6F3FF") + # Light blue - Expansion Era
  labs(
    x = "Publication Year", 
    y = "Number of publications",
    caption = "Black line: Actual publications | Red line: Linear trend with confidence band"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "darkgray", linetype = "dashed", linewidth = 0.3),
    panel.grid.minor = element_line(color = "darkgray", linetype = "dashed", linewidth = 0.2),
    panel.border = element_blank(),
    axis.line.x = element_line(color = "black", linewidth = 0.5),
    axis.line.y = element_line(color = "black", linewidth = 0.5),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray40", size = 11),
    axis.text = element_text(size = 10, color = "black"),
    axis.title.x = element_text(size = 11, color = "black"),
    axis.title.y = element_text(size = 11, color = "black", face = "bold")
  ) +
  scale_x_continuous(breaks = seq(1950, 2025, by = 10))

print(p)
ggsave("Evolutionary_Timeline_Clean.png", width = 12, height = 6, dpi = 300, bg = "white")

# Print growth statistics - Quantitative evidence for field development
cat("=== GROWTH STATISTICS FOR REVIEWER RESPONSE ===\n")
cat(sprintf("Linear growth rate: %.2f publications per year\n", linear_growth))
cat(sprintf("R-squared: %.3f (explains %.1f%% of variance)\n", linear_r2, linear_r2 * 100))
cat(sprintf("Period: %d to %d (%d years)\n", min(annual_trends$PY), max(annual_trends$PY), nrow(annual_trends)))
cat(sprintf("Total publications: %d\n", sum(annual_trends$publications)))
cat(sprintf("Average publications per year: %.1f\n", mean(annual_trends$publications)))

# Additional era-specific statistics for reviewer response - Validates three-era framework
cat("\n=== ERA-SPECIFIC STATISTICS ===\n")
era1 <- annual_trends %>% filter(PY <= 2012)
era2 <- annual_trends %>% filter(PY >= 2013 & PY <= 2020)
era3 <- annual_trends %>% filter(PY >= 2021)

cat(sprintf("Foundational Era (1946-2012): %d publications over %d years (avg: %.1f/year)\n", 
            sum(era1$publications), nrow(era1), mean(era1$publications)))
cat(sprintf("Integration Era (2013-2020): %d publications over %d years (avg: %.1f/year)\n", 
            sum(era2$publications), nrow(era2), mean(era2$publications)))
cat(sprintf("Expansion Era (2021-2025): %d publications over %d years (avg: %.1f/year)\n", 
            sum(era3$publications), nrow(era3), mean(era3$publications)))

# Calculate growth factors - Demonstrates field acceleration patterns
growth_era2 <- mean(era2$publications) / mean(era1$publications)
growth_era3 <- mean(era3$publications) / mean(era2$publications)

cat(sprintf("\nGrowth factors:\n"))
cat(sprintf("Foundational → Integration: %.1f-fold increase\n", growth_era2))
cat(sprintf("Integration → Expansion: %.1f-fold increase\n", growth_era3))