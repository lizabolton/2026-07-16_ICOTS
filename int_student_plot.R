library(ggplot2)
library(dplyr)

enrollment_data <- data.frame(
  Country = c("New Zealand", "United Kingdom", "Australia", "Canada"),
  Baseline = c(14, 25, 29, 25),
  Top_Min  = c(NA, 32, 43, NA),
  Top_Max  = c(NA, 51, 50, NA),
  Toronto  = c(NA, NA, NA, 29),
  Auckland = c(21, NA, NA, NA)
)

# Sort Country
enrollment_data <- enrollment_data %>%
  mutate(Country = factor(Country, levels = Country[order(Baseline)]))

ggplot(enrollment_data, aes(x = Country, y = Baseline)) +
  geom_bar(
    stat = "identity",
    width = 0.6,
    fill = "#1A433A"
  ) +
  # Add the highlighted range segments for UK and Australia
  geom_segment(
    aes(
      xend = Country,
      y = Top_Min,
      yend = Top_Max,
      color = "Top 3 Universities Range"
    ),
    linewidth = 4,
    lineend = "round",
    na.rm = TRUE
  ) +
  # Add distinct boundary points to frame the top university ranges
  geom_point(
    aes(y = Top_Min, color = "Top 3 Universities Range"),
    size = 4,
    na.rm = TRUE
  ) +
  geom_point(
    aes(y = Top_Max, color = "Top 3 Universities Range"),
    size = 4,
    na.rm = TRUE
  ) +
  geom_point(
    aes(y = Toronto, color = "Toronto"),
    size = 4,
    na.rm = TRUE
  ) +
  geom_point(
    aes(y = Auckland, color = "Auckland"),
    size = 4,
    na.rm = TRUE
  ) +
  # Text Labels: Baseline Values
  geom_text(
    aes(label = paste0(Baseline, "%")),
    color = "white",
    hjust = 1.3,
    fontface = "bold",
    size = 4
  ) +
  # Text Labels: Custom annotations for the top ranges to avoid overlapping
  geom_text(
    aes(y = Top_Max, label = paste0(Top_Min, "—", Top_Max, "% (top 3)")),
    hjust = -0.1,
    color = "#6D1A25",
    fontface = "bold",
    size = 4.5,
    na.rm = TRUE
  ) +
  geom_text(
    aes(y = Toronto, label = paste0(Toronto, "% (University of Toronto)")),
    hjust = -0.1,
    color = "blue",
    fontface = "bold",
    size = 4.5,
    na.rm = TRUE
  ) +
  geom_text(
    aes(y = Auckland, label = paste0(Auckland, "% (University of Auckland)")),
    hjust = -0.1,
    color = "blue",
    fontface = "bold",
    size = 4.5,
    na.rm = TRUE
  ) +
  coord_flip() +
  theme_minimal(base_size = 14) +
  scale_color_manual(
    values = c(
      "Top 3 Universities Range" = "#6D1A25",
      "Toronto" = "blue",
      "Auckland" = "blue"
    )
  ) +
  scale_y_continuous(
    limits = c(0, 65),
    labels = function(x) paste0(x, "%")
  ) +
  # Design and Labels
  labs(
    subtitle = "All measures are from the mid-2020s, though vary by country/institution reporting cycles",
    x = NULL,
    y = "Percentage of students that are international"
  ) +
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
  filename = "img/international students.png",
  bg = "transparent",
  width = 10,
  height = 6,
  dpi = 300
)
