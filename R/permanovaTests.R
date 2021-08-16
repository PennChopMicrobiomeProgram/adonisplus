#' @importFrom generics tidy
#' @export
generics::tidy

#' Tidy an \code{adonis} object
#'
#' @param x an object returned from \code{vegan::adonis()}.
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
tidy.adonis <- function (x) {
  ret <- tibble::as_tibble(x$aov.tab, rownames = "term")
  colnames(ret) <- c(
    "term", "df", "sumsq", "meansq", "statistic", "r.squared", "p.value")
  attr(ret, "heading") <- NULL
  ret
}


#' Permutational multivariate analysis of variance for repeated measures
#'
#' @param data Data to use in the test
#' @param distmat Distance matrix, either a matrix or an object of class
#'   \code{dist}
#' @param sample_id_var Variable that defines the sample IDs, i.e. the
#'   identifiers that correspond to each item in the distance matrix
#' @param group1 First predictor variable, usually a factor
#' @param group2 Second predictor variable
#' @param rep_meas_var Variable that indicates the repeated measures in the
#'   experiment, typically a subject ID or cage ID
#' @param permutations Number of permutations
#' @param group1_within Does the first predictor change within the unit of
#'   repeated measures? Typically, the first predictor is a study group, into
#'   which each subject is assigned. In this case, the study group is
#'   constant within each subject, and \code{first_within} should be
#'   \code{FALSE}.
#' @param group2_within Does the second predictor change within the unit of
#'   repeated measures? Typically, the second predictor is a study day, and
#'   each subject is sampled over time. In this case, the study day assumes
#'   several values within each subject, and \code{group2_within} should be
#'   \code{TRUE}.
#' @return The results from \code{vegan::adonis()} in tidy format
#' @export
adonis_repeated_measures <- function(data, distmat,
                                     group1 = study_group,
                                     group2 = study_day,
                                     sample_id_var = SampleID,
                                     rep_meas_var = SubjectID,
                                     covariates = NA, permutations = 999,
                                     seed = 42,
                                     group1_within = FALSE,
                                     group2_within = TRUE) {
  group1_name <- rlang::as_name(rlang::ensym(group1))
  group2_name <- rlang::as_name(rlang::ensym(group2))

  set.seed(seed)
  sample_ids <- as.character(dplyr::pull(data, {{ sample_id_var }}))
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
  f_observed <- res$statistic[term_idxs]

  fs_permuted <- replicate(permutations, {
    trial_data <- data

    if (group1_within) {
      trial_data <- trial_data %>%
        dplyr::mutate("{{ group1 }}" := shuffle_within_groups({{ group1 }}, {{ rep_meas_var }}))
    } else {
      trial_data <- trial_data %>%
        dplyr::mutate("{{ group1 }}" := shuffle_between_groups({{ group1 }}, {{ rep_meas_var }}))
    }

    if (group2_within) {
      trial_data <- trial_data %>%
        dplyr::mutate("{{ group2 }}" := shuffle_within_groups({{ group2 }}, {{ rep_meas_var }}))
    } else {
      trial_data <- trial_data %>%
        dplyr::mutate("{{ group2 }}" := shuffle_between_groups({{ group2 }}, {{ rep_meas_var}}))
    }

    trial_a <- vegan::adonis(adonis_formula, trial_data, permutations = 4)
    trial_res <- tidy.adonis(trial_a)
    trial_res$statistic[term_idxs]
  })

  fs_greater <- sweep(cbind(f_observed, fs_permuted), 1, f_observed, `>=`)
  p_permuted <- apply(fs_greater, 1, function (x) sum(x) / length(x))
  res$p.value[term_idxs] <- p_permuted
  res
}
