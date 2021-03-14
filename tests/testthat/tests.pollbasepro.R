
# Tests: pollbasepro dataset


# 1. Housekeeping ---------------------------------------------------------

# Load packages

library(BritPol)
library(dplyr)
library(purrr)
library(brms)
library(here)


# Load validation/correlation models

cor_all <- BritPol:::cor_mods[[1]]
cor_con <- BritPol:::cor_mods[[2]]
cor_lab <- BritPol:::cor_mods[[3]]
cor_lib <- BritPol:::cor_mods[[4]]



# 2. Data tests -----------------------------------------------------------

# Test 1: Ensure that there are no missing dates

test_that("There are no missing dates", {
  expect_equal(
    length(setdiff(seq.Date(min(pollbasepro$date), max(pollbasepro$date), "days"), pollbasepro$date)),
    0
  )
})


# Test 2: Ensure all voting intention estimates are between 0 and 1

test_that("All voting intention estimates are between 0 and 1", {
  expect_gte(
    min(pollbasepro$con_est),
    0
  )
  expect_lte(
    max(pollbasepro$con_est),
    1
  )
  expect_gte(
    min(pollbasepro$lab_est),
    0
  )
  expect_lte(
    max(pollbasepro$lab_est),
    1
  )
  expect_gte(
    min(pollbasepro$lib_est),
    0
  )
  expect_lte(
    max(pollbasepro$lib_est),
    1
  )
})


# Test 3: Ensure correlations between parties are negative

test_that("Correlations between voting intention figures are negative", {
  expect_lt(
    median(pluck(posterior_samples(cor_all, "rescor__libest__conest"), 1)),
    0
  )
  expect_lt(
    median(pluck(posterior_samples(cor_all, "rescor__libest__labest"), 1)),
    0
  )
  expect_lt(
    median(pluck(posterior_samples(cor_all, "rescor__conest__labest"), 1)),
    0
  )
})


# Test 4: Ensure validation correlations exceed 80%

test_that("Correlations between voting intention figures are negative", {
  expect_gt(
    median(pluck(posterior_samples(cor_con, "rescor"), 1)),
    0.8
  )
  expect_gt(
    median(pluck(posterior_samples(cor_lab, "rescor"), 1)),
    0.8
  )
  expect_gt(
    median(pluck(posterior_samples(cor_lib, "rescor"), 1)),
    0.8
  )
})


# Test 5: Check that date subset indicators exhibit appropriate frequencies

test_that("Check that date subset indicators exhibit appropriate frequencies", {
  expect_gt(
    sum(pollbasepro$week),
    sum(pollbasepro$month)
  )
  expect_gt(
    sum(pollbasepro$month),
    sum(pollbasepro$quarter)
  )
  expect_gt(
    sum(pollbasepro$quarter),
    sum(pollbasepro$year)
  )
})


# Test 6: Check that election estimates are consistent across parties

test_that("Check that date subset indicators exhibit appropriate frequencies", {
  expect_equal(
    pollbasepro$con_err[pollbasepro$con_err == 0],
    pollbasepro$lab_err[pollbasepro$con_err == 0]
  )
  expect_equal(
    pollbasepro$con_err[pollbasepro$con_err == 0],
    pollbasepro$lib_err[pollbasepro$con_err == 0]
  )
  expect_equal(
    pollbasepro$lab_err[pollbasepro$lab_err == 0],
    pollbasepro$lib_err[pollbasepro$lab_err == 0]
  )
})

