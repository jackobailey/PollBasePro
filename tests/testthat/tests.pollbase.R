
# Tests: pollbase dataset


# 1. Housekeeping ---------------------------------------------------------

# Load packages

library(britpol)



# 2. Data tests -----------------------------------------------------------

# Test 1: Ensure that there are no duplicated IDs

test_that("There are no duplicated IDs", {
  expect_equal(
    length(table(pollbase$id)[table(pollbase$id) > 1]),
    0
  )
})


# Test 2: Ensure all values are between 0 and 1

test_that("All voting intention figures are between 0 and 1", {
  expect_gte(
    min(pollbase$con),
    0
  )
  expect_lte(
    max(pollbase$con),
    1
  )
  expect_gte(
    min(pollbase$lab),
    0
  )
  expect_lte(
    max(pollbase$lab),
    1
  )
  expect_gte(
    min(pollbase$lib),
    0
  )
  expect_lte(
    max(pollbase$lib),
    1
  )
})


# Test 3: Ensure correlations between parties are negative

test_that("Correlations between voting intention figures are negative", {
  expect_lt(
    cor.test(pollbase$con, pollbase$lab)$estimate,
    0
  )
  expect_lt(
    cor.test(pollbase$con, pollbase$lib)$estimate,
    0
  )
  expect_lt(
    cor.test(pollbase$lab, pollbase$lib)$estimate,
    0
  )
})


# Test 4: Ensure no start dates are after end dates

test_that("No start dates are after end dates", {
  expect_equal(
    sum(pollbase$start > pollbase$end),
    0
  )
})

