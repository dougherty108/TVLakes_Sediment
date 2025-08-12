library(magick)

# Read images
img1 <- image_read("data/bonneyCamera/Bonney_2022_03_18_06_00_31.jpg")
img2 <- image_read("data/bonneyCamera/Bonney_2022_03_18_18_00_25.jpg")

# Combine side-by-side
combined <- image_append(c(img1, img2))

# Set output width to 6 inches at 300 dpi (so 6 * 300 = 1800 px wide)
combined_resized <- image_resize(combined, "3000x")  

# Save new image
image_write(combined_resized, "Figures/FigureSX_bonneyCamera.png", density = 500)
