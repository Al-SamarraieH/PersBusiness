# FINAL CODE FOR FIGURE C1 - SENTIMENT ANALYSIS BY DECADE
# This script analyzes the sentiment trajectory of personality research abstracts
# across decades from the 1940s to 2020s using the Syuzhet sentiment dictionary

library(tidyverse)
library(syuzhet)

# 1. LOAD AND PREPARE THE SCOPUS DATASET
# The dataset contains 4,048 personality-in-business publications (1946-2025)
full_data <- read.csv("BPB.csv")

# 2. CALCULATE SENTIMENT SCORES USING SYUZHET DICTIONARY
# Syuzhet provides sentiment scores ranging from -1 (negative) to +1 (positive)
# based on emotional valence of words in the abstracts
full_data$AB <- iconv(full_data$AB, to = "UTF-8", sub = " ")  # Ensure text encoding
full_data$sentiment_score <- get_sentiment(full_data$AB, method = "syuzhet")

# 3. AGGREGATE SENTIMENT SCORES BY DECADE
# Categorize scores into Positive (>0.1), Negative (<-0.1), and Neutral (-0.1 to 0.1)
# Thresholds account for academic hedging and methodological language
decade_sentiment <- full_data %>%
  mutate(
    decade = floor(PY / 10) * 10,  # Group years into decades (1940s, 1950s, etc.)
    sentiment_category = case_when(
      sentiment_score > 0.1 ~ "Positive",
      sentiment_score < -0.1 ~ "Negative", 
      TRUE ~ "Neutral"
    )
  ) %>%
  count(decade, sentiment_category) %>%        # Count publications per category
  group_by(decade) %>%
  mutate(proportion = n / sum(n))              # Calculate proportional distribution

# 4. CREATE STACKED BAR CHART WITH PROFESSIONAL STYLING
# Visualizes the evolving sentiment composition across decades
figure_c1 <- ggplot(decade_sentiment, aes(x = factor(decade), y = proportion, fill = sentiment_category)) +
  geom_col(position = "stack", alpha = 0.9) +  # Stacked bars show composition
  labs(
    title = "Figure C1. Sentiment Distribution of Personality Research Abstracts by Decade (1940s-2020s)",
    x = "Decade", 
    y = "Proportion", 
    fill = "Sentiment"
  ) +
  # Color scheme: Green (positive), Orange (neutral), Red (negative)
  scale_fill_manual(values = c(
    "Positive" = "#2E8B57",  # Sea Green - represents constructive/optimistic tone
    "Neutral" = "#FFA500",   # Orange   - represents descriptive/methodological tone  
    "Negative" = "#DC143C"   # Crimson  - represents critical/challenge-oriented tone
  )) +
  theme_minimal() +
  theme(
    # Ensure all text is black for publication clarity
    text = element_text(color = "black", family = "sans"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black", face = "bold"),
    plot.title = element_text(color = "black", face = "bold", size = 12, hjust = 0.5),
    legend.text = element_text(color = "black"),
    legend.title = element_text(color = "black", face = "bold"),
    
    # Professional grid styling - only major x and y axes
    panel.grid.major.x = element_line(color = "darkgray", linetype = "dashed", linewidth = 0.3),
    panel.grid.major.y = element_line(color = "darkgray", linetype = "dashed", linewidth = 0.3),
    panel.grid.minor = element_blank(),  # Remove minor grid for cleaner appearance
    
    # Clean frame-free design
    panel.border = element_blank(),
    
    # Ensure white background for publication standards
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    
    # Legend placement for optimal readability
    legend.position = "bottom"
  ) +
  scale_y_continuous(labels = scales::percent)  # Display proportions as percentages

# 5. EXPORT HIGH-RESOLUTION FIGURE FOR PUBLICATION
# 300 DPI ensures print-quality resolution in manuscripts
ggsave("Figure_C1_Sentiment_by_Decade.png", figure_c1, 
       width = 10, height = 6, dpi = 300, bg = "white")

# 6. EXECUTION SUMMARY FOR TRANSPARENCY
cat("✓ Figure C1 created successfully!\n")
cat("✓ Decades analyzed:", paste(sort(unique(decade_sentiment$decade)), collapse = ", "), "\n")
cat("✓ File saved: Figure_C1_Sentiment_by_Decade.png\n")
cat("✓ Method: Syuzhet sentiment dictionary applied to article abstracts\n")
cat("✓ Dataset: n =", nrow(full_data), "publications from Scopus (1946-2025)\n")

# Display the underlying data for verification
print("Sentiment distribution by decade (proportions):")
print(decade_sentiment)