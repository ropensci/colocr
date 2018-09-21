app <- ShinyDriver$new("../")
app$snapshotInit("two_images")

app$uploadFile(image1 = c(system.file('extdata', 'Image0003_.jpg', package = 'colocr'), 
                          system.file('extdata', 'Image0001_.jpg', package = 'colocr')))
app$setInputs(threshold = 90, timeout_ = 10000)
app$setInputs(roi_num = 3, timeout_ = 10000)
app$setInputs(name = "LC3", timeout_ = 10000)
app$setInputs(add = "click", timeout_ = 10000)
app$setInputs(add2 = "click", timeout_ = 10000)
app$snapshot()
