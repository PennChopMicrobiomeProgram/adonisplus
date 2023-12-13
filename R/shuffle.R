#' Shuffle a vector within each group
#'
#' @param x The vector to be shuffled.
#' @param g A vector of groups.
#' @return A version of x where elements have been
#'   shuffled within each group.
#' @export
shuffle_within_groups <- function(x, g) {
  stats::ave(x, g, FUN = sample)
}

#' Shuffle a vector, but swap whole groups
#' @param x The vector to be shuffled.
#' @param g A vector of groups.
#' @return A version of x where elements have been
#'   shuffled with each group.
#' @export
shuffle_between_groups <- function(x, g) {
  g <- as.factor(g)
  x <- as.factor(x)
  # What is the value of x for every unique value of g?
  x_vals_per_g <- tapply(x, g, unique, simplify = F)
  # Check that x has one unique value for each value of g. If not, raise an
  # error with an informative message.
  if (!all(sapply(x_vals_per_g, length) == 1)) {
    stop(
      "Multiple values detected within grouping factor.\n\n",
      "  Value of g: value(s) of x\n",
      "  -----------------------\n",
      paste0("  ", names(x_vals_per_g), ": ", x_vals_per_g, collapse = "\n")
    )
  }
  # Values of x for each value of g.
  x_per_g <- unlist(x_vals_per_g)
  # Shuffled values of x for each value of g.
  x_per_g_shuffled <- sample(x_per_g)
  # Names are also shuffled, but we want to associate shuffled values
  # with the original names.
  names(x_per_g_shuffled) <- names(x_per_g)
  # Now we need to get the new values of x for each value of g.  The unique
  # values of g are stored in the names, the shuffled values of x are stored
  # in the vector x_per_g_shuffled.
  x_per_g_shuffled[as.character(g)]
}
