context("Test the fars_map_state in the fars package")

library(dplyr)
library(maps)
library(fars)

test_that("fars_map_state() works correctly", {
  expect_silent(fars_map_state(12, 2013))
  expect_silent(fars_map_state(16, 2014))
  expect_error(fars_map_state(17, 2018))
})
