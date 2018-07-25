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
