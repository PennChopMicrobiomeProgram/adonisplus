adonis_res <- list(
  aov.tab = data.frame(
    Df = c(1, 9, 10),
    SumsOfSqs = c(0.141236772727273, 1.4724405, 1.61367727272727),
    MeanSqs = c(0.141236772727273, 0.1636045, NA),
    F.Model = c(0.863281711244329, NA, NA),
    R2 = c(0.0875247951460386, 0.912475204853961, 1),
    `Pr(>F)` = c(0.635, NA, NA),
    row.names = c("study_group", "Residuals", "Total")
  )
)
class(adonis_res) <- "adonis"

test_that("tidy.adonis produces expected result", {
  expected <- tibble::tibble(
    term = c("study_group", "Residuals", "Total"),
    df = c(1, 9, 10),
    sumsq = c(0.141236772727273, 1.4724405, 1.61367727272727),
    meansq = c(0.141236772727273, 0.1636045, NA),
    statistic = c(0.863281711244329, NA, NA),
    r.squared = c(0.0875247951460386, 0.912475204853961, 1),
    p.value = c(0.635, NA, NA)
  )
  expect_equal(tidy(adonis_res), expected)
})

adonis2_res <- data.frame(
  Df = c(1, 9, 10),
  SumOfSqs = c(0.141236772727273, 1.4724405, 1.61367727272727),
  R2 = c(0.0875247951460386, 0.912475204853961, 1),
  F = c(0.863281711244329, NA, NA),
  `Pr(>F)` = c(0.643, NA, NA),
  row.names = c("study_group", "Residual", "Total")
)
class(adonis2_res) <- c("anova.cca", "anova", "data.frame")

test_that("tidy.anova.cca produces expected result", {
  expected <- tibble::tibble(
    term = c("study_group", "Residual", "Total"),
    df = c(1, 9, 10),
    sumsq = c(0.141236772727273, 1.4724405, 1.61367727272727),
    r.squared = c(0.0875247951460386, 0.912475204853961, 1),
    statistic = c(0.863281711244329, NA, NA),
    p.value = c(0.643, NA, NA)
  )
  expect_equal(tidy(adonis2_res), expected)
})
