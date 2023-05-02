test_that("calculates correct stats", {
  skip("no api key in github actions")

  cfb_undefeated_s1999_w4_stats <- get0("cfb_undefeated_s1999_w4_stats", envir = asNamespace("homefield"))

  output_stats <- homefield_stats(x = cfb_undefeated(season = 1999, week = 4))

  expect_equal(head(output_stats), cfb_undefeated_s1999_w4_stats, ignore_attr = TRUE)

})
