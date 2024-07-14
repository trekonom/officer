test_that("extension_summary works", {
  doc <- read_pptx()

  expect_identical(
    extension_summary(doc),
    data.frame(
      name = character(0),
      uri = character(0),
      ns = character(0),
      prefix = character(0)
    )
  )

  expect_message(
    doc$presentation$extension_list$remove_extension("sldGuideLst")
  )

  expect_equal(
    doc$presentation$extension_list$remove_extension("sldGuideLst"),
    message(sprintf("No extension with name %s found.", "sldGuideLst"))
  )
})
