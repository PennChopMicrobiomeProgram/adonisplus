#' Tidy PERMANOVA
#'
#' @param x an object returned from \code{vegan::adonis()}.
#' @return A \code{data.frame} with the following columns:
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
#' @importFrom broom tidy
#' @export
tidy.adonis <- function (x) {
  ret <- data.frame(
    term = rownames(x$aov.tab), x$aov.tab,
    stringsAsFactors = FALSE, row.names = NULL)
  colnames(ret) <- c(
    "term", "df", "sumsq", "meansq", "statistic", "r.squared", "p.value")
  ret
}


#' Permutational multivariate analysis of variance with restricted permutations
#'
#' @param d Distance matrix. Can be a matrix or an object of class \code{dist}
#' @param data Data to use in the test
#' @param sample_id_col Column of sample IDs, i.e. the identifiers that
#'   correspond to each item in the distance matrix
#' @param group1 First predictor variable, usually a factor
#' @param group2 Second predictor variable
#' @param nesting_var Variable that defines the nesting in the experiment,
#'   typically indicating a subject ID or cage ID
#' @param permutations Number of permutations
#' @param first_within Should the first predictor be shuffled within group?
#' @param second_within Should the second fixed effec be shuffled within group?
#' @return The results from \code{vegan::adonis()} in tidy format
#' @export
permanova_with_shuffle_2_groups <- function(d, data,
                                            sample_id_col,
                                            group1, group2,
                                            nesting_var, covariates = NA,
                                            permutations = 999, seed = 42,
                                            first_within=F, second_within=F) {
  group1 <- rlang::enquo(group1)
  group2 <- rlang::enquo(group2)
  nesting_var <- rlang::enquo(nesting_var)
  sample_id_col <- rlang::enquo(sample_id_col)

  set.seed(seed)
  data <- as.data.frame(data)
  dist_toTest <- usedist::dist_subset(d, as.character(dplyr::pull(data, !!sample_id_col)))
  form1 <- paste("dist_toTest", "~", rlang::quo_text(group1), " * ", rlang::quo_text(group2))

  if (!is.na(covariates)) {
    form1 <- paste(form1, " + ", covariates)
  }
  a_ixn_orj <- vegan::adonis(as.formula(form1), data=data, permutations=permutations)

  terms_perm <- c(rlang::quo_text(group1),
                  rlang::quo_text(group2),
                  paste0(rlang::quo_text(group1), ":", rlang::quo_text(group2)))

  tidy_output <- tidy.adonis(a_ixn_orj)
  f_ixn_all <- tidy_output[match(terms_perm, tidy_output$term), "statistic"]

  fs_permuted <- replicate(permutations, {
    s_permuted <- data

    if (first_within) {
      s_permuted <- s_permuted %>%
        dplyr::mutate(!!group1 := shuffle_within_groups(!!group1, !!nesting_var))
    } else {
      s_permuted <- s_permuted %>%
        dplyr::mutate(!!group1 := shuffle_between_groups(!!group1, !!nesting_var))
    }

    if (second_within) {
      s_permuted <- s_permuted %>%
        dplyr::mutate(!!group2 := shuffle_within_groups(!!group2, !!nesting_var))
    } else {
      s_permuted <- s_permuted %>%
        dplyr::mutate(!!group2 := shuffle_between_groups(!!group2, !!nesting_var))    }

    a_permuted <- vegan::adonis(as.formula(form1), s_permuted, permutations = 4)

    temp_output <- tidy.adonis(a_permuted)
    temp_output[match(terms_perm, temp_output$term), "statistic"]
  })
  p_ixn <- rowSums(cbind(f_ixn_all, fs_permuted) >= f_ixn_all, na.rm = T) / (dim(fs_permuted)[2] + 1)

  tidy_output[match(terms_perm, tidy_output$term), "p.value"] <- p_ixn
  tidy_output
}
