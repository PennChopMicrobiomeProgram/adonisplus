#' Tidy PERMANOVA
#'
#' @param x an object returned from [vegan::adonis()].
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


#' PERMANOVA with 2 group shuffle
#'
#' @param dist_matrix Distance matrix
#' @param s_toTest data frame that holds the labels
#' @param SampleID column name that holds the IDs
#' @param group_label1 first fixed effect
#' @param group_label2 second fixed effect
#' @param rep_mes_label random effect
#' @param perm number of permutations
#' @param first_within should the first fixed effec be shuffled within group?
#' @param second_within should the second fixed effec be shuffled within group?
#' @return permuted PERMANOVA results in tidy form
#' @export
permanova_with_shuffle_2_groups <- function(dist_matrix, s_toTest, SampleID, group_label1, group_label2, rep_mes_label, covariates, perm, first_within=F, second_within=F){
  group_label1 <- enquo(group_label1)
  group_label2 <- enquo(group_label2)
  rep_mes_label <- enquo(rep_mes_label)
  SampleID <- enquo(SampleID)

  set.seed(42)
  s_toTest <- as.data.frame(s_toTest)
  dist_toTest <- usedist::dist_subset(dist_matrix, as.character(pull(s_toTest, !!SampleID)))
  form1 <- paste("dist_toTest", "~", rlang::quo_text(group_label1), " * ", rlang::quo_text(group_label2))
  #print(rlang::quo_text(group_label1))

  if (!is.na(covariates)) {
    form1 <- paste(form1, " + ", covariates)
  }
  a_ixn_orj <- adonis(as.formula(form1), data=s_toTest, permutations=perm)

  terms_perm <- c(rlang::quo_text(group_label1),
                  rlang::quo_text(group_label2),
                  paste0(rlang::quo_text(group_label1), ":", rlang::quo_text(group_label2)))

  tidy_output <- tidy_permanova(a_ixn_orj)
  f_ixn_all <- tidy_output[match(terms_perm, tidy_output$Term),"F.Model"]

  fs_permuted <- replicate(perm, {
    s_permuted <- s_toTest

    if (first_within) {
      s_permuted <- s_permuted %>%
        mutate(!!group_label1 := shuffle_within_groups(!!group_label1, !!rep_mes_label))
    } else {
      s_permuted <- s_permuted %>%
        mutate(!!group_label1 := shuffle_between_groups(!!group_label1, !!rep_mes_label))
    }

    if (second_within) {
      s_permuted <- s_permuted %>%
        mutate(!!group_label2 := shuffle_within_groups(!!group_label2, !!rep_mes_label))
    } else {
      s_permuted <- s_permuted %>%
        mutate(!!group_label2 := shuffle_between_groups(!!group_label2, !!rep_mes_label))    }

    a_permuted <- adonis(as.formula(form1), s_permuted, permutations = 4)

    temp_output <- tidy_permanova(a_permuted)
    temp_output[match(terms_perm, temp_output$Term),"F.Model"]
    #c(a_permuted_g1$aov.tab[1, 4], a_permuted_g2$aov.tab[1, 4], a_permuted$aov.tab[3, 4])
  })
  p_ixn <- rowSums(cbind(f_ixn_all, fs_permuted) >= f_ixn_all, na.rm = T) / (dim(fs_permuted)[2] + 1)

  tidy_output[match(terms_perm, tidy_output$Term),"p.value"] <- p_ixn
  tidy_output
}



