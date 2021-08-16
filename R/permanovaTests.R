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
#' @param data Data to use in the test
#' @param distmat Distance matrix. Can be a matrix or an object of class
#'   \code{dist}
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
adonis_nested <- function(data, distmat, sample_id_col, group1, group2,
                          nesting_var, covariates = NA, permutations = 999,
                          seed = 42, first_within = FALSE,
                          second_within = FALSE) {
  group1_name <- rlang::as_name(rlang::ensym(group1))
  group2_name <- rlang::as_name(rlang::ensym(group2))

  set.seed(seed)
  data <- as.data.frame(data)
  sample_ids <- as.character(dplyr::pull(data, {{ sample_id_col }}))
  distmat <- usedist::dist_subset(distmat, sample_ids)

  adonis_formula <- paste("distmat", "~", group1_name, " * ", group2_name)
  if (!is.na(covariates)) {
    adonis_formula <- paste(adonis_formula, " + ", covariates)
  }
  adonis_formula <- as.formula(adonis_formula)

  a_observed <- vegan::adonis(adonis_formula, data=data, permutations=permutations)
  res <- tidy.adonis(a_observed)

  terms_to_permute <- c(
    group1_name, group2_name, paste0(group1_name, ":", group2_name))
  term_idxs <- match(terms_to_permute, res$term)
  f_observed <- res[term_idxs, "statistic"]

  fs_permuted <- replicate(permutations, {
    trial_data <- data

    if (first_within) {
      trial_data <- trial_data %>%
        dplyr::mutate("{{ group1 }}" := shuffle_within_groups({{ group1 }}, {{ nesting_var }}))
    } else {
      trial_data <- trial_data %>%
        dplyr::mutate("{{ group1 }}" := shuffle_between_groups({{ group1 }}, {{ nesting_var }}))
    }

    if (second_within) {
      trial_data <- trial_data %>%
        dplyr::mutate("{{ group2 }}" := shuffle_within_groups({{ group2 }}, {{ nesting_var }}))
    } else {
      trial_data <- trial_data %>%
        dplyr::mutate("{{ group2 }}" := shuffle_between_groups({{ group2 }}, {{ nesting_var}}))
    }

    trial_a <- vegan::adonis(adonis_formula, trial_data, permutations = 4)
    trial_res <- tidy.adonis(trial_a)
    trial_res[term_idxs, "statistic"]
  })

  fs_greater <- sweep(cbind(f_observed, fs_permuted), 1, f_observed, `>=`)
  p_permuted <- apply(fs_greater, 1, function (x) sum(x) / length(x))
  res[term_idxs, "p.value"] <- p_permuted
  res
}
