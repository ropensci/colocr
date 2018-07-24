# load required libraries
library(imager)
library(colocr)

# load images
img <- load.image(system.file('extdata', 'Image0001_.jpg', package = 'colocr'))       # merge

# change the merge image to gray scale
img.g <- grayscale(img)

# choose parameters
px <- parameter_choose(img.g, threshold = 90)

# show pixset object structure
str(px)

labs.px <- labels_add(px, n = 5)
str(labs.px)

# show parameters for selecting ROIs
parameter_show(img, px, labels = labs.px)

# Calculate the correlation statistics
corr <- coloc_test(img, px, labels = labs.px,
                   type = 'all', num = TRUE)

corr$p  # PCC
corr$r  # MOC

# show the scatter and density of the pixel values
coloc_show(corr)
plot(corr$channel1 - mean(corr$channel1),
     corr$channel2 - mean(corr$channel2))

imname <- system.file('extdata/parrots.png',package='imager')
im <- load.image(imname) %>% grayscale
#Thresholding yields different discrete regions of high intensity
regions <- isoblur(im,10) %>% threshold("97%")
labels <- label(regions)
layout(t(1:2))
plot(regions,"Regions")
plot(labels,"Labels")

text(df$x, df$y, labels = as.character(df$value), col = 'yellow')

df <- as.data.frame(labels) %>%
  group_by(value) %>%
  summarise_all(median)
