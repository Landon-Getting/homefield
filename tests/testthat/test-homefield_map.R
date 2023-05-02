test_that("outputs png file", {
  skip("long runtime, no api key in github actions")

  homefield_data <- get0("homefield_data", envir = asNamespace("homefield"))

  homefield_map(x = homefield_data,
                output_file = "homefield_map_test.png",
                title = "homefield_map_test",
                credit = "Landon Getting")

  expect_true(file.exists("homefield_map_test.png"))

  file.remove("homefield_map_test.png")

})

