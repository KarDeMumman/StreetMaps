context("Maps_api")
library(httr)
library(jsonlite)

# Testing that class is correct
test_that("Class is correct", {
  obj <- Maps_api(dataset = "se-7", year = "2019")
  expect_true(class(obj)[1] == "Maps_api")
})

test_that("Maps_api rejects wrong inputs as arguments", {
  expect_error(object <- Maps_api(Sweden))
  expect_error(object <- Maps_api(dataset = Finland))
  expect_error(object <- Maps_api(dataset = Finland, year = "2015"))
  expect_error(object <- Maps_api(dataset = se-7, year = "2010"))
  expect_error(object <- Maps_api(World,"2019"))
})

test_that("Maps_api accepts correct inputs as arguments",{
  expect_error(object <- Maps_api(dataset = "us-4", year = "2014"), NA)
  expect_error(object <- Maps_api(dataset = "world-2", year = "2010"), NA)
  expect_error(object <- Maps_api(dataset = "fi-8", year = "2011"), NA)
  expect_error(object <- Maps_api(dataset = "ch-8", year = "2010"), NA)
})
