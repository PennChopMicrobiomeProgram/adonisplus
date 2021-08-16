example_data <- tibble::tibble(
  SampleID = c(
    "C1", "C2", "C3", "C4", "C5", "P1", "P2", "P3", "P4", "P5", "P6"),
  time_point = c(rep("C", 5), rep("P", 6)),
  SubjectID = paste0("S", c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 6)),
  study_group = rep(c("G1", "G2", "G1", "G2"), c(3, 2, 3, 3)))

example_dist <- structure(
  c(
    0.367, 0.495, 0.425, 0.432, 0.698, 0.812, 0.698, 0.605, 0.41, 0.522, 0.461,
    0.417, 0.411, 0.687, 0.809, 0.655, 0.618, 0.385, 0.47, 0.348, 0.233, 0.416,
    0.84, 0.472, 0.497, 0.34, 0.412, 0.393, 0.559, 0.781, 0.499, 0.484, 0.409,
    0.467, 0.474, 0.804, 0.598, 0.495, 0.257, 0.403, 0.82, 0.59, 0.586, 0.525,
    0.594, 0.877, 0.832, 0.794, 0.83, 0.498, 0.6, 0.556, 0.447, 0.483, 0.348),
  Labels = c("C1", "C2", "C3", "C4", "C5", "P1", "P2", "P3", "P4", "P5", "P6"),
  Size = 11L, call = quote(as.dist.default(m = cp_dist)), class = "dist",
  Diag = FALSE, Upper = FALSE)

test_that("adonis_repeated_measures produces expected result", {
  expected <- tibble::tibble(
    term = c(
      "study_group", "time_point", "study_group:time_point",
      "Residuals", "Total"),
    df = c(1, 1, 1, 7, 10),
    sumsq = c(
      0.141236772727273, 0.2546555, 0.161871833333333,
      1.05591316666667, 1.61367727272727),
    meansq = c(
      0.141236772727273, 0.2546555, 0.161871833333333,
      0.150844738095238, NA),
    statistic = c(
      0.936305598131642, 1.68819610956015, 1.07310228634646, NA, NA),
    r.squared = c(
      0.0875247951460386, 0.157810675222318, 0.100312395835974,
      0.654352133795669, 1),
    p.value = c(0.4, 0.1, 0.3, NA, NA))
  observed <- adonis_repeated_measures(
    example_data, example_dist, group2 = time_point, permutations = 9)
  expect_equal(observed, expected)
})