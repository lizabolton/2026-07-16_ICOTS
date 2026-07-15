library(ggplot2)

# 1. Create the dataset based on the provided text
data_bars <- data.frame(
  Country = c("New Zealand", "Canada", "Australia", "United Kingdom"),
  Share = c(50, 21, 22, 23)
)

data_points <- data.frame(
  Country = c("New Zealand", "Canada"),
  Share = c(62, 52),
  Label = c("62% (University of Auckland)", "52% (University of Toronto)")
)

# Enforce ordering matching the original style (bottom to top presentation logic)
data_bars$Country <- factor(data_bars$Country, levels = c("New Zealand", "United Kingdom", "Canada", "Australia"))
data_points$Country <- factor(data_points$Country, levels = c("New Zealand", "United Kingdom", "Canada", "Australia"))

# 2. Build the plot
ggplot() +
  geom_bar(data = data_bars, aes(x = Country, y = Share), stat = "identity", fill = "#1A433A", width = 0.6) +

  # Text inside the bars
  geom_text(data = data_bars, aes(x = Country, y = Share, label = paste0(Share, "%")),
            hjust = 1.3, color = "white", fontface = "bold", size = 4.5) +

  # Special dots for single institutions (blue)
  geom_point(data = data_points, aes(x = Country, y = Share), color = "blue", size = 4.5) +

  # Labels alongside the blue dots
  geom_text(data = data_points, aes(x = Country, y = Share, label = Label),
            hjust = -0.07, color = "blue", fontface = "bold", size = 4.5) +

  # Flip coordinates to make bars horizontal
  coord_flip(ylim = c(0, 100)) +

  # Define X-axis format (which becomes the horizontal axis after flip)
  scale_y_continuous(breaks = c(0, 20, 40, 60, 80), labels = c("0%", "20%", "40%", "60%", "80%")) +

  # Titles & Labels
  labs(
    subtitle = "Measures from mid-2020s reporting cycles",
    x = NULL,
    y = "Percentage of international students that are from China"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = "none",
    legend.box = "horizontal",
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA)
  )

ggsave(
  filename = "img/china_perc.png",
  bg = "transparent",
  width = 10,
  height = 6,
  dpi = 300
)
