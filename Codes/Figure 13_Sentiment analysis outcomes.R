# Load required libraries
library(ggplot2)
library(gridExtra)

# Positive terms with corrected frequencies
positive_terms <- data.frame(
  word = c("leadership", "engage", "traits", "support", "enhance",
           "motivation", "importance", "trust", "quality", "success",
           "willingness", "loyalty", "effectiveness", "ability", "brand",
           "reliability", "confidence", "achievement", "innovation", "proactive"),
  n = c(1150, 950, 880, 650, 620,
        440, 437, 437, 430, 437,
        437, 350, 300, 295, 292,
        285, 283, 280, 265, 261)
)

# Negative terms with corrected frequencies
negative_terms <- data.frame(
  word = c("difficult", "depression", "risk", "narcissism", "conflict",
           "anxiety", "stress", "faking", "complex", "demands",
           "aggressive", "bias", "crisis", "abusive", "pressure",
           "pay", "bullying", "demand", "failure", "counterproductive"),
  n = c(1000, 730, 700, 480, 300,
        250, 200, 150, 100, 100,
        80, 80, 65, 65, 60,
        57, 57, 53, 50, 47)
)

# Custom plot theme with dark dashed gray grid lines
custom_theme <- theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "darkgray", linetype = "dashed", linewidth = 0.5),
    panel.grid.minor = element_line(color = "darkgray", linetype = "dashed", linewidth = 0.3),
    axis.text = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 14, color = "black"),
    axis.line.x = element_line(color = "black", linewidth = 0.5),
    axis.line.y = element_line(color = "black", linewidth = 0.5),
    panel.border = element_blank(),
    plot.title = element_blank(),
    axis.title.y = element_blank()
  )

# Plot for positive words (exact RGB green bars, no labels)
plot_positive <- ggplot(positive_terms, aes(x = n, y = reorder(word, n))) +
  geom_bar(stat = "identity", fill = "#22B14C", width = 0.8) +  # RGB(34,177,76)
  labs(x = "Frequency") +
  custom_theme +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, 1200))

# Plot for negative words (red bars, no labels)
plot_negative <- ggplot(negative_terms, aes(x = n, y = reorder(word, n))) +
  geom_bar(stat = "identity", fill = "red", width = 0.8) +
  labs(x = "Frequency") +
  custom_theme +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, 1200))

# Arrange the two plots side by side
grid.arrange(plot_positive, plot_negative, ncol = 2)