library(datree)
context("Basic node functionality")

test_that("o_node generates outcome nodes", {
  expect_equal(o_node(1)$value, 1)
  expect_equal(o_node(1, "test")$name, "test")
})

# Uncertainty node tests ----
children <- list(
  o_node(10, "A"),
  o_node(20, "B"),
  o_node(30, "C")
  )

full_prob <- c(.4, .3, .3)
true_value <- sum(c(10, 20, 30) * full_prob)

trunc_2prob <- c(.4, .3)
trunc_1prob <- c(.4)


test_that("u_node generates uncertainty nodes with appropriate value", {
  expect_equal(u_node(children, full_prob)$value, true_value)
  expect_equal(u_node(children, trunc_1prob)$value, true_value)
  expect_equal(u_node(children, trunc_2prob)$value, true_value)
})

bad_prob_too_many <- c(.4, .3, .2, .1)
bad_prob_too_large <- c(.4, .3, .4)

test_that("u_node generates throws appropriate errors", {
  expect_error(u_node(children, bad_prob_too_many), "Too many")
  expect_error(u_node(children, bad_prob_too_large), "greater than 1")
})
