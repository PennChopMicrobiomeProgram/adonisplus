#' @importFrom generics tidy
#' @export
generics::tidy

#' Tidy an \code{adonis} object
#'
#' @param x an object returned from \code{vegan::adonis()}.
#' @param ... Additional arguments are not used.
#' @return A \code{tibble} with the following columns:
#'   \item{term}{The name of the regression term.}
#'   \item{df}{Degrees of freedom used by the model.}
#'   \item{sumsq}{Sum of squares explained by this term.}
#'   \item{meansq}{
#'     Mean sum of squares. Equal to total sum of squares divided by degrees
#'     of freedom.}
#'   \item{statistic}{
#'     The value of a pseudo-F-statistic to use in the permutation test.}
#'   \item{r.squared}{
#'     R-squared statistic, or the percent of variation explained by the model.}
#'   \item{p.value}{P-value from the permutation test.}
#' @export
tidy.adonis <- function (x, ...) {
  ret <- tibble::as_tibble(x$aov.tab, rownames = "term")
  colnames(ret) <- c(
    "term", "df", "sumsq", "meansq", "statistic", "r.squared", "p.value")
  attr(ret, "heading") <- NULL
  ret
}
