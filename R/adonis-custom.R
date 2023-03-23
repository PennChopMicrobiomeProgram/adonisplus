#' Permutational multivariate analysis of variance, plus
#'
#' @param data Data to use in the test.
#' @param distmat Distance matrix, either a matrix or an object of class
#'   \code{dist} The distance matrix will automatically be filtered and
#'   re-arranged to match the rows of \code{data}.
#' @param formula Model formula. The LHS must be "distmat ~". The formula can
#'   either be a literal formula or a string that can be converted into a
#'   formula.
#' @param sample_id_var Variable in \code{data} that defines the sample IDs,
#'   i.e. the identifiers that correspond to each item in the distance matrix.
#' @param rep_meas_var Variable in \code{data} that indicates the repeated
#'   measures in the experiment, typically a subject ID or cage ID.
#' @param shuffle Named character vector that specifies how to carry out
#'   restricted permutations for variables in \code{data}. Names should
#'   correspond to variables in \code{data}. Values should be either
#'   \code{"between"} or \code{"within"}, meaning that values should be
#'   shuffled between or within levels of \code{rep_meas_var}. See Details for
#'   more info.
#' @param permutations Number of permutations.
#' @param seed Random seed, set just before the initial call to
#'   \code{vegan::adonis()}.
#' @return The results from \code{vegan::adonis()} in tidy format.
#' @details
#' A typical experimental design has subjects in a few groups sampled
#' repeatedly over a few time points. If the variable denoting the group is
#' \code{study_group} and the variable denoting the time point is
#' \code{time_point}, then the \code{shuffle} argument would be
#' \code{c(study_group = "between", time_point = "within")}. During the
#' permutation stage, the values of the study group will be shuffled between
#' subjects, preserving the value within each subject. Conversely, the values
#' of the time point will be shuffled only within each subject.
#' @export
adonisplus <- function(data, distmat, formula, sample_id_var = SampleID,
                       rep_meas_var = subject_id, shuffle = NULL,
                       permutations = 999, seed = 42) {
  sample_ids <- data %>%
    dplyr::pull({{ sample_id_var }}) %>%
    as.character()
  distmat <- usedist::dist_subset(distmat, sample_ids)
  formula <- stats::as.formula(formula)
  # TODO: determine if LHS is equal to "distmat ~" and stop if not

  assign("distmat", distmat, .GlobalEnv)

  set.seed(seed)
  a_observed <- vegan::adonis2(
    formula = formula, data = data, permutations = permutations)
  result <- tidy.anova.cca(a_observed)

  if (!is.null(shuffle)) {
    rep_meas_vals <- data %>%
      dplyr::pull({{ rep_meas_var }})

    # Here, vars is the variables that will undergo custom permutations
    vars <- names(shuffle)
    # TODO: handle errors from missing variable names
    # Matrix of variables by terms
    vars_by_terms <- attr(stats::terms(formula), "factors")
    nterms <- length(colnames(vars_by_terms))
    # TODO: handle errors due to variable names not matching
    # Keep only the variables that we will shuffle manually
    vars_idx <- match(vars, rownames(vars_by_terms))
    # Need drop = FALSE or R converts to a vector when nterms == 1
    vars_by_terms <- vars_by_terms[vars_idx, , drop = FALSE]
    term_idx <- which(colSums(vars_by_terms) > 0)

    f_observed <- result$statistic[term_idx]
    fs_permuted <- replicate(permutations, {
      trial_data <- data
      for (var in vars) {
        method <- shuffle[var]
        shuffle_functions <- list(
          between = shuffle_between_groups,
          within = shuffle_within_groups)
        # Use match.arg in case someone writes "bet" instead of "between"
        method <- match.arg(method, names(shuffle_functions))
        fcn <- shuffle_functions[[method]]
        old_vals <- trial_data[[var]]
        new_vals <- fcn(old_vals, rep_meas_vals)
        trial_data[[var]] <- new_vals
      }
      trial_a <- vegan::adonis2(formula, trial_data, permutations = 4)
      trial_result <- tidy.anova.cca(trial_a)
      trial_result$statistic[term_idx]
    })

    if (nterms > 1) {
      fs_greater <- sweep(cbind(f_observed, fs_permuted), 1, f_observed, `>=`)
      p_permuted <- apply(fs_greater, 1, function (x) sum(x) / length(x))
    } else {
      fs_greater <- c(f_observed, fs_permuted) >= f_observed
      p_permuted <- sum(fs_greater) / length(fs_greater)
    }
    result$p.value[term_idx] <- p_permuted
  }
  result
}

#' Post-hoc tests for permutational multivariate analysis of variance
#'
#' @param data Data to use in the test.
#' @param ... Additional arguments passed to \code{adonisplus()}.
#' @param which Variable on which to perform post-hoc comparisons.
#' @param alpha The threshold for rejecting the null hypothesis in the main
#'   comparison.
#' @return The results in tidy format.
#' @export
adonispost <- function(data, ..., which = study_group, alpha = 0.05) {
  var_name <- rlang::as_name(rlang::ensym(which))

  result_main <- adonisplus(data, ...) %>%
    dplyr::mutate(comparison = paste("All", var_name)) %>%
    dplyr::select(comparison, term, dplyr::everything()) %>%
    dplyr::filter(!(term %in% c("Residual", "Total")))

  var_levels <- data %>%
    dplyr::pull({{ which }}) %>%
    as.factor() %>%
    levels()
  pairs <- utils::combn(var_levels, 2, simplify = FALSE)

  make_pairwise_comparison <- function (pair) {
    pair_data <- data %>%
      dplyr::filter({{ which }} %in% pair)
    adonisplus(pair_data, ...) %>%
        dplyr::mutate(comparison = paste(pair, collapse = " - ")) %>%
        dplyr::select(comparison, term, dplyr::everything()) %>%
        dplyr::filter(!(term %in% c("Residual", "Total")))
  }
  result_posthoc <- lapply(pairs, make_pairwise_comparison) %>%
    dplyr::bind_rows()
  dplyr::bind_rows(result_main, result_posthoc)
}
