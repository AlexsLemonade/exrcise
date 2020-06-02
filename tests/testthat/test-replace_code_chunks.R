test_that("filter_ends correctly filters end blocks", {
  starts = c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE)
  ends = c(TRUE, FALSE, TRUE, FALSE, FALSE, TRUE)
  true_ends = c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)
  expect_equal(filter_ends(starts, ends), true_ends)
  expect_equal(filter_ends(starts, true_ends), true_ends)
  expect_equal(filter_ends(rep(starts, 2), rep(ends, 2)), rep(true_ends, 2))
})
