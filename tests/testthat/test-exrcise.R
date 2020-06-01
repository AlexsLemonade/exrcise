test_that("Exrcise transforms correctly", {
  in_test <- "reference.Rmd"
  blank_test <- "blanked.Rmd"
  rep_test <- "replaced.Rmd"
  comment_test <- "commented.Rmd"

  expect_equal(exrcise(in_test, paste0(in_test, ".ex"),
                       replace_flags = "solution",
                       comment = FALSE),
               readr::read_lines(blank_test))

  expect_equal(exrcise(in_test, paste0(in_test, ".ex"),
                       replace_flags = "solution",
                       comment = TRUE),
               readr::read_lines(comment_test))

  expect_equal(exrcise(in_test, paste0(in_test, ".ex"),
                       replace_flags = "solution",
                       replacement = "### YOUR CODE HERE",
                       comment = FALSE),
               readr::read_lines(rep_test))
})
