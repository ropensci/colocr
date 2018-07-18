# load library
library(imager)

# read image
img <- load.image('data-raw/MAP1LC3B/Image0003.jpg.frames/Image0003_.jpg')

# change to gray scale
img.g <- grayscale(img)

# threshold image
img.t <- threshold(img.g, '90%')

# make pixset
px <- as.pixset(1 - img.t)

# modify pixset
px.m <- px %>%
  shrink(5) %>%
  fill(5) %>%
  clean(10)

# load colored images
img2 <- load.image('data-raw/MAP1LC3B/Image0003.jpg.frames/Image0003_C002.jpg')
img3 <- load.image('data-raw/MAP1LC3B/Image0003.jpg.frames/Image0003_C003.jpg')

# highlight rois
layout(t(1:2))
plot(img2)
highlight(px.m, col = 'blue')

plot(img3)
highlight(px.m, col = 'blue')

# calculate correlations
v2 <- grayscale(img2)[as.logical(px.m)]
v3 <- grayscale(img3)[as.logical(px.m)]
