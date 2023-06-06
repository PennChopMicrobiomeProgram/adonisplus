test_that("check_lhs has no error if LHS matches", {
  expect_equal(check_lhs(y ~ x + z, y ~ .), NULL)
})

test_that("check_lhs produces error if LHS does not match", {
  expect_error(
    check_lhs(my_dist ~ study_group * study_day, distmat ~ .),
    "LHS of formula must be")
})

test_that("check_lhs works for multi-part LHS", {
  expect_error(check_lhs(y + z ~ w + x, y ~ .))
})
