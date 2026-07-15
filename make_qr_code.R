#########################
# Setup & Package Loading
#########################

# Use library() to load packages cleanly at the top of the script
library(qrcode)
library(magick)
library(ggplot2)
library(dplyr)
library(grid)

#########################################
# Generate and Process the QR Code Matrix
#########################################

# Generate the base QR code using error correction level "H"
qr_raw <- qr_code("link.lizabolton.nz", ecl = "H")

# Convert the logical matrix into an X-Y coordinate data frame
qr_matrix <- as.matrix(qr_raw)
qr_df <- as.data.frame(which(qr_matrix == TRUE, arr.ind = TRUE))
colnames(qr_df) <- c("Y", "X")

# Flip the Y axis to prevent the QR code from printing upside down
qr_df$Y <- max(qr_df$Y) - qr_df$Y + 1

##############################
# Load & Format the Logo Image
##############################

# Read the logo image (magick auto-detects WebP files with .jpg extensions)
logo_raw <- image_read("img/ICOTS12-Logo.jpg")

# Convert the image to a grid graphical object (grob) for ggplot compatibility
logo_grob <- rasterGrob(as.raster(logo_raw), interpolate = TRUE)

##########################################
# Calculate Grid Dimensions and Dead Zones
##########################################

# Find the exact center coordinates of the QR code grid
center_x <- max(qr_df$X) / 2
center_y <- max(qr_df$Y) / 2

# Define the central boundaries to erase (the "hole" for the logo)
center_range_x <- c(center_x - 4, center_x + 4)
center_range_y <- c(center_y - 4, center_y + 4)

# Set the boundary size of the actual logo image to sit inside the hole
logo_size <- 4.5
xmin <- center_x - logo_size
xmax <- center_x + logo_size
ymin <- center_y - logo_size
ymax <- center_y + logo_size

##########################
# Filter out Center Pixels
##########################

# Remove pixels that fall inside the designated central dead zone
qr_with_hole <- qr_df %>%
  filter(
    !(X >= center_range_x[1] &
        X <= center_range_x[2] &
        Y >= center_range_y[1] &
        Y <= center_range_y[2])
  )

##########################
# Generate and Render Plot
##########################

ggplot(qr_with_hole, aes(x = X, y = Y)) +
  # Draw the QR code grid pixels as black squares
  geom_tile(fill = "black", width = 0.95, height = 0.95) +

  # Draw a clean white background buffer behind the logo
  annotate(
    "rect",
    xmin = xmin - 0.5,
    xmax = xmax + 0.5,
    ymin = ymin - 0.5,
    ymax = ymax + 0.5,
    fill = "white"
  ) +

  # Overlay the logo
  annotation_custom(
    logo_grob,
    xmin = xmin,
    xmax = xmax,
    ymin = ymin,
    ymax = ymax
  ) +

  # Clean up for scannability
  theme_void() +
  coord_fixed() +
  theme(panel.background = element_rect(fill = "white", color = NA))

ggsave(
  filename = "img/qr_code.png",
  width = 6,
  height = 6,
  dpi = 300,
  bg = "transparent"
)
