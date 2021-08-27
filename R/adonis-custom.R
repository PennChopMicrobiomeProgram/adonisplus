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
#' @param covariates Any additional covariates to include in the model,
#'   provided as a character vector. If \code{covariates} is not \code{NA}, the
#'   value is inserted directly into the model formula in front of the terms
#'   for \code{group1} and \code{group2}
#' @param permutations Number of permutations
#' @param seed Random seed, set just before the initial call to
#'   \code{vegan::adonis()}
#' @param group1_permute How to perform restricted permutations for
#'   \code{group1}, either \code{"between"} or \code{"within"}. Typically, the
#'   first predictor is a study group, into which each subject is assigned.
#'   In this case, the study group is constant within each subject, and
#'   \code{group1_permute} should be \code{"between"}, to permute the study
#'   groups between subjects.
#' @param group2_permute How to perform restricted permutations for
#'   \code{group2}, either \code{"between"} or \code{"within"}. Typically, the
#'   second predictor is a study day, and each subject is sampled over time.
#'   In this case, \code{group2_permute} should be \code{"within"}, to permute
#'   the study days within each subject.
#' @return The results from \code{vegan::adonis()} in tidy format
#' @export
adonis_repeated_measures <- function(data, distmat,
                                     group1 = study_group,
                                     group2 = study_day,
                                     sample_id_var = SampleID,
                                     rep_meas_var = subject_id,
                                     covariates = NA, permutations = 999,
                                     seed = 42,
                                     group1_permute = "between",
                                     group2_permute = "within") {
  group1_name <- rlang::as_name(rlang::ensym(group1))
  group2_name <- rlang::as_name(rlang::ensym(group2))

  group1_permute <- match.arg(group1_permute, c("between", "within"))
  group2_permute <- match.arg(group2_permute, c("between", "within"))
  permute_fcns <- list(
    between = shuffle_between_groups,
    within = shuffle_within_groups)
  group1_fcn <- permute_fcns[[group1_permute]]
  group2_fcn <- permute_fcns[[group2_permute]]

  # Need to check that this function works properly in the context of group_by()
  # This is why Ceylan had cast the table of samples to data.frame()

  sample_ids <- as.character(dplyr::pull(data, {{ sample_id_var }}))
  distmat <- usedist::dist_subset(distmat, sample_ids)

  if (is.na(covariates)) {
    adonis_formula <- paste0("distmat ~ ", group1_name, " * ", group2_name)
  } else {
    adonis_formula <- paste0(
      "distmat ~ ", covariates, " + ", group1_name, " * ", group2_name)
  }
  adonis_formula <- as.formula(adonis_formula)

  set.seed(seed)
  a_observed <- vegan::adonis(adonis_formula, data=data, permutations=permutations)
  res <- tidy.adonis(a_observed)

  terms_to_permute <- c(
    group1_name, group2_name, paste0(group1_name, ":", group2_name))
  term_idxs <- match(terms_to_permute, res$term)
  f_observed <- res$statistic[term_idxs]

  fs_permuted <- replicate(permutations, {
    trial_data <- data %>%
      dplyr::mutate("{{ group1 }}" := group1_fcn({{ group1 }}, {{ rep_meas_var }})) %>%
      dplyr::mutate("{{ group2 }}" := group2_fcn({{ group2 }}, {{ rep_meas_var }}))
    trial_a <- vegan::adonis(adonis_formula, trial_data, permutations = 4)
    trial_res <- tidy.adonis(trial_a)
    trial_res$statistic[term_idxs]
  })

  fs_greater <- sweep(cbind(f_observed, fs_permuted), 1, f_observed, `>=`)
  p_permuted <- apply(fs_greater, 1, function (x) sum(x) / length(x))
  res$p.value[term_idxs] <- p_permuted
  res
}

#' Permutational multivariate analysis of variance. Should mostly be used
#' internally for the permanova_posthoc function
#'
#' @param data Data to use in the test
#' @param distmat Distance matrix, either a matrix or an object of class
#'   \code{dist}
#' @param formula_string The right side of the formula to test of class \code{character}
#' @param sample_id_var Variable that defines the sample IDs, i.e. the
#'   identifiers that correspond to each item in the distance matrix
#' @param rep_meas_var Variable that indicates the repeated measures in the
#'   experiment, typically a subject ID or cage ID. Will be passed to the strata
#'   parameter in the \code{vegan::adonis()} call
#' @param permutations Number of permutations
#' @param seed Random seed, set just before the initial call to
#'   \code{vegan::adonis()}
#' @export
adonis_run <- function(data, distmat,
                       formula_string,
                       sample_id_var = SampleID,
                       rep_meas_var = NULL,
                       permutations = 999,
                       seed = 42) {

  sample_ids <- as.character(dplyr::pull(data, {{ sample_id_var }}))
  
  strata_ids <- c()
  if (!rlang::quo_is_null(rlang::enquo(rep_meas_var))) {
    strata_ids <- as.character(dplyr::pull(data, {{ rep_meas_var }}))
  }

  if (!grepl("~", formula_string)) {
    formula_string <- paste0("distmat ~ ", formula_string)
  }
  adonis_formula <- as.formula(formula_string)

  distmat <- usedist::dist_subset(distmat, sample_ids)

  set.seed(seed)
  if(is.null(strata_ids)) {
    tidy.adonis(adonis(adonis_formula, data=data, permutations=permutations))
  } else {
    tidy.adonis(adonis(adonis_formula, data=data, permutations=permutations, strata=strata_ids))
  }
}

#' Permutational multivariate analysis of variance for pseudo post-hoc tests
#' Subsets the data into pairs of groups and runs adonis
#'
#' @param data Data to use in the test
#' @param distmat Distance matrix, either a matrix or an object of class
#'   \code{dist}
#' @param formula_string The right side of the formula to test of class \code{character}
#' @param sample_id_var Variable that defines the sample IDs, i.e. the
#'   identifiers that correspond to each item in the distance matrix
#' @param rep_meas_var Variable that indicates the repeated measures in the
#'   experiment, typically a subject ID or cage ID. Will be passed to the strata
#'   parameter in the \code{vegan::adonis()} call
#' @param group_var The variable to subset into pair,typically the study group.
#'   Pairs of variable will be subset and tested again
#' @param p_cutoff The pseudo post hoc tests will be conducted only if the term's
#'   p value is below the cutoff.
#' @param permutations Number of permutations
#' @param seed Random seed, set just before the initial call to
#'   \code{vegan::adonis()}
#' @export
adonis_posthoc <- function(data, distmat,
                              formula_string,
                              sample_id_var = SampleID,
                              rep_meas_var = NULL,
                              group_var = study_group,
                              p_cutoff = 0.05,
                              permutations = 999,
                              seed = 42) {

  a_ixn <- adonis_run(data, distmat, formula_string, {{sample_id_var}}, {{rep_meas_var}}, permutations, seed) %>%
    dplyr::mutate(comparison = "all")

  combs <- combn(as.character(unique( dplyr::pull(data, {{ group_var }}) )), 2)
  num_tests <- ncol(combs)

  if (dplyr::filter(a_ixn, term == rlang::as_name(rlang::ensym(group_var)) )$p.value < p_cutoff) {
    for (i in 1:num_tests){
      s_temp <- dplyr::filter(data, {{group_var}} %in% combs[,i])
      a_ixn <- rbind(a_ixn,
                     adonis_run(s_temp, distmat, formula_string, {{sample_id_var}}, {{rep_meas_var}}, permutations, seed) %>%
                       dplyr::mutate(comparison = paste(combs[,i], collapse=' - '))
      )
    }
  }
  a_ixn
}
