test_that("power_detect matches known analytic values", {
  # n = 1 collapses to p exactly
  expect_equal(power_detect(1, 0.3), 0.3)

  # n = 0 -> no trials -> zero detection, for any p
  expect_equal(power_detect(0, 0.5), 0)

  # p = 0 -> never detectable, for any n
  expect_equal(power_detect(5, 0), 0)

  # p = 1 -> guaranteed detection for n >= 1
  expect_equal(power_detect(3, 1), 1)

  # specific hand-computed points
  expect_equal(power_detect(2, 0.5), 0.75)          # 1 - 0.5^2
  expect_equal(power_detect(3, 0.1), 0.271)         # 1 - 0.9^3
  expect_equal(power_detect(10, 0.2), 0.8926258176) # default tolerance handles FP
})

test_that("power_detect stays within [0, 1] for valid inputs", {
  grid <- expand.grid(n = 0:20, p = seq(0, 1, by = 0.05))
  out <- mapply(power_detect, grid$n, grid$p)
  expect_true(all(out >= 0 & out <= 1))
})

test_that("power_detect is monotonic", {
  # increasing in n for fixed 0 < p < 1
  vals_n <- power_detect(1:10, 0.3)
  expect_true(all(diff(vals_n) > 0))

  # increasing in p for fixed n >= 1
  vals_p <- power_detect(4, seq(0.1, 0.9, by = 0.1))
  expect_true(all(diff(vals_p) > 0))
})

test_that("power_detect is vectorised over both args", {
  expect_equal(power_detect(c(1, 2), 0.5), c(0.5, 0.75))
  expect_equal(power_detect(2, c(0.1, 0.5)), c(0.19, 0.75))
})

test_that("power_detect propagates NA", {
  expect_true(is.na(power_detect(NA_real_, 0.5)))
  expect_true(is.na(power_detect(2, NA_real_)))
})

test_that("power_detect rejects invalid inputs", {
  expect_error(power_detect(2, 1.5))
  expect_error(power_detect(2, -0.1))
  expect_error(power_detect(-1, 0.5))
})
