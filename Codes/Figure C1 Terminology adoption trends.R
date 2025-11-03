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

# Create the plot with only x and y axis lines (no frame)
p <- ggplot(annual_trends, aes(x = PY)) +
  geom_line(aes(y = smoothed_count), color = "black", linewidth = 1.2, alpha = 0.9, na.rm = TRUE) + # Black line
  geom_point(aes(y = smoothed_count), color = "black", size = 2, shape = 19, alpha = 0.7) + # Black markers on the line
  geom_vline(xintercept = c(2012, 2020), 
             linetype = "dashed", color = "red", linewidth = 1.2, alpha = 0.8) + # Red vertical lines
  annotate("rect", xmin = min(annual_trends$PY), xmax = 2012, ymin = -Inf, ymax = Inf,
           alpha = 0.15, fill = "#E8F5E8") + # Very light green for Foundational Era
  annotate("rect", xmin = 2013, xmax = 2020, ymin = -Inf, ymax = Inf,
           alpha = 0.15, fill = "#FFE5D9") + # Light peach for Integration Era
  annotate("rect", xmin = 2021, xmax = max(annual_trends$PY), ymin = -Inf, ymax = Inf,
           alpha = 0.15, fill = "#E6F3FF") + # Light blue for Expansion Era
  labs(
    x = "Publication Year", 
    y = "Number of publications"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "darkgray", linetype = "dashed", linewidth = 0.3),
    panel.grid.minor = element_line(color = "darkgray", linetype = "dashed", linewidth = 0.2),
    # Remove panel border and add only axis lines
    panel.border = element_blank(),
    axis.line.x = element_line(color = "black", linewidth = 0.5),
    axis.line.y = element_line(color = "black", linewidth = 0.5),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray40", size = 11),
    axis.text = element_text(size = 10, color = "black"),
    axis.title.x = element_text(size = 11, color = "black"),
    axis.title.y = element_text(size = 11, color = "black", face = "bold") # Bold y-axis label
  ) +
  scale_x_continuous(breaks = seq(1950, 2025, by = 10))

print(p)
ggsave("Evolutionary_Timeline_Clean.png", width = 12, height = 6, dpi = 300, bg = "white")