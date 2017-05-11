library(datree)
context("Basic node functionality")

test_that("o_node generates outcome nodes", {
  expect_equal(o_node(1)$value, ~ 1)
  expect_equal(o_node(1, "test")$name, "test")
})

# Uncertainty node tests ----
children <- list(
  o_node(~ x, "A"),
  o_node(~ y, "B"),
  o_node(~ z, "C")
  )

full_prob <- list(~ a, ~ b, ~ 1 - a - b)

u <- u_node(children, full_prob)

test_that("u_node generates uncertainty nodes with appropriate value", {
  expect_equal(u$value, ~ sum(c(x, y, z) * c(a, b, 1 - a - b)))
})

x <- 10; y <- 20; z <- 30; a <- .2; b <- .6;
new_x <- 15; new_a <- .3
org_data <- list(x = x, y = y, z = z, a = a, b = b)
new_data <- list(x = new_x, y = y, z = z, a = new_a, b = b)
true_value <- sum(c(x, y, z) * c(a, b, 1 - a - b))
new_true_value <- sum(c(new_x, y, z) * c(new_a, b, 1 - new_a - b))

test_that("evaluate works for uncertainty nodes with variable values", {
  expect_equal(evaluate(u, org_data), true_value)
  expect_equal(evaluate(u, new_data), new_true_value)
})

# Decision node tests ----
children <- list(
  o_node(~ x, "Rain"),
  o_node(~ y, "Sunny")
  )

prob_1 <- list(~ a, ~ 1 - a)
prob_2 <- list(~ b, ~ 1 - b)

u1 <- u_node(children, prob_1, "Alt1")
u2 <- u_node(children, prob_2, "Alt2")

d <- d_node(list(u1, u2))

test_that("d_node generates decision nodes with appropriate attributes", {
  expect_equal(d$value, ~ max(sum(c(x, y) * c(a, 1 - a)),
                              sum(c(x, y) * c(b, 1 - b))))
  expect_equal(d$children, list(u1, u2))
})

x <- 10; y <- 20; a <- .2; b <- .6
true_value <- max(x * a + y * (1 - a), x * b + y * (1 - b))
org_data <- list(x = x, y = y, a = a, b = b)
new_x <- 30; new_b <- .1
new_true_value <- max(new_x * a + y * (1 - a), new_x * new_b + y * (1 - new_b))
new_data <- list(x = new_x, y = y, a = a, b = new_b)
test_that("evaluate works for decision nodes with variable values", {
  expect_equal(evaluate(d, org_data), true_value)
  expect_equal(evaluate(d, new_data), new_true_value)
})

test_that("decide works for decision nodes with variable values", {
  expect_equal(decide(d, org_data), tibble::tibble(
    alternative = c("Alt1", "Alt2"),
    evalue = c(x * a + y * (1 - a), x * b + y * (1 - b))
  ))
  expect_equal(decide(d, new_data), tibble::tibble(
    alternative = c("Alt1", "Alt2"),
    evalue = c(new_x * a + y * (1 - a), new_x * new_b + y * (1 - new_b))
  ))
})

