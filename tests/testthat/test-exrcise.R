test_that("Exrcise transforms correctly", {
  in_test <- "blanked.Rmd"
  blank_test <- "blanked.Rmd"
  rep_test <- "replaced.Rmd"
  expect_equal(exrcise(in_test, blank_test,
                       replace_flags = "solution"),
               readr::read_lines(blank_test))

  expect_equal(exrcise(in_test, rep_test,
                       replace_flags = "solution",
                       replacement = "### YOUR CODE HERE"),
               readr::read_lines(rep_test))
})
