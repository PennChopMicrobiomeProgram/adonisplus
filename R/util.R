check_lhs <- function(formula, expected) {
  formula_lhs <- formula[[2]]
  expected_lhs <- expected[[2]]
  if (formula_lhs != expected_lhs) {
    stop(
      "LHS of formula must be exactly '", expected_lhs, "'. ",
      "Saw '", formula_lhs, "'."
    )
  }
}
