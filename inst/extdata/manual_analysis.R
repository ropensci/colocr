# load required libraries
library(imager)
library(colocr)

# load images
img <- load.image(system.file('extdata', 'Image0001_.jpg', package = 'colocr'))       # merge
img1 <- load.image(system.file('extdata', 'Image0001_C002.jpg', package = 'colocr'))  # red
img2 <- load.image(system.file('extdata', 'Image0001_C003.jpg', package = 'colocr'))  # green

# show images
#par(mfrow = c(1,3), mar = rep(1,4))
#plot(img, axes = FALSE, main = 'Merged')
#plot(img1, axes = FALSE, main = 'RKIP')
#plot(img2, axes = FALSE, main = 'LC3')

# change the merge image to gray scale
img.g <- grayscale(img)

# choose parameters
px <- parameter_choose(img.g, threshold = 90)

# show pixset object structure
str(px)

labs.df <- labels_add(px, n = 7)

# show parameters for selecting ROIs
par(mfrow=c(1,2), mar = rep(1,4))
parameter_show(img, img1, img2, px, labels = labs.df)

# Calculate the correlation statistics
corr <- coloc_test(img1, img2, px, labels = labs.df,
                   type = 'all', num = TRUE)

corr$p  # PCC
corr$r  # MOC

# show the scatter and density of the pixel values
coloc_show(corr)


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
