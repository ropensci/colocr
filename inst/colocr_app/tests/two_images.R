app <- ShinyDriver$new("../")
app$snapshotInit("two_images")

app$uploadFile(image1 = c(system.file('extdata', 'Image0003_.jpg', package = 'colocr'), 
                          system.file('extdata', 'Image0001_.jpg', package = 'colocr')))
app$setInputs(threshold = 90)
app$setInputs(roi_num = 3)
app$setInputs(name = "LC3")
app$setInputs(add = "click")
app$setInputs(add2 = "click")
app$snapshot()
