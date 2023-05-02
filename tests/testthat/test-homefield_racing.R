test_that("outputs gif file", {
  skip_on_ci()

  homefield_racing_data <- get0("cfb_undefeated_2022_stats", envir = asNamespace("homefield"))

  homefield_racing(x = homefield_racing_data,
                   stat_name = "land",
                   title = "2022 Season Week by Week - Undefeated CFB homefield Map",
                   subtitle = "Area in Square Miles",
                   caption = "Data Source: cfbd.com",
                   output_file = "homefield_racing_test.gif")

  expect_true(file.exists("homefield_racing_test.gif"))

  file.remove("homefield_racing_test.gif")

})
