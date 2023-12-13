example_data <- tibble::tibble(
  SampleID = c(
    "C1", "C2", "C3", "C4", "C5", "P1", "P2", "P3", "P4", "P5", "P6"
  ),
  time_point = c(rep("C", 5), rep("P", 6)),
  SubjectID = paste0("S", c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 6)),
  study_group = rep(c("G1", "G2", "G1", "G2"), c(3, 2, 3, 3)),
  age = c(1.2, 1.1, 1.3, 1.0, 1.4, 1.2, 1.1, 1.3, 1.0, 1.4, 1.5)
)

example_dist <- structure(
  c(
    0.367, 0.495, 0.425, 0.432, 0.698, 0.812, 0.698, 0.605, 0.41, 0.522, 0.461,
    0.417, 0.411, 0.687, 0.809, 0.655, 0.618, 0.385, 0.47, 0.348, 0.233, 0.416,
    0.84, 0.472, 0.497, 0.34, 0.412, 0.393, 0.559, 0.781, 0.499, 0.484, 0.409,
    0.467, 0.474, 0.804, 0.598, 0.495, 0.257, 0.403, 0.82, 0.59, 0.586, 0.525,
    0.594, 0.877, 0.832, 0.794, 0.83, 0.498, 0.6, 0.556, 0.447, 0.483, 0.348
  ),
  Labels = c("C1", "C2", "C3", "C4", "C5", "P1", "P2", "P3", "P4", "P5", "P6"),
  Size = 11L, call = quote(as.dist.default(m = cp_dist)), class = "dist",
  Diag = FALSE, Upper = FALSE
)

test_that("adonispost works", {
  example_data_3groups <- example_data %>%
    dplyr::mutate(study_group = rep(c("G1", "G2", "G3", "G1", "G2", "G3"), c(2, 2, 1, 2, 2, 2)))
  observed <- adonispost(
    example_data_3groups, example_dist, distmat ~ study_group,
    which = study_group
  )
  expected <- tibble::tibble(
    comparison = c("All study_group", "G1 - G2", "G1 - G3", "G2 - G3"),
    term = c("study_group", "study_group", "study_group", "study_group"),
    df = c(2, 1, 1, 1),
    sumsq = c(
      0.396595356060606, 0.2534925, 0.172570726190476, 0.160944833333333
    ),
    r.squared = c(
      0.2457711729374, 0.18721007197831, 0.162938895956797, 0.26470972004068
    ),
    statistic = c(
      1.30343028067264, 1.38198123911777, 0.973279580007741, 1.80003549112144
    ),
    p.value = c(0.154, 0.173, 0.478, 0.092)
  )
  expect_equal(observed, expected)
})

test_that("adonisplus works for one group with unrestricted permutations", {
  observed <- adonisplus(example_data, example_dist, distmat ~ study_group)
  expected <- tibble::tibble(
    term = c("study_group", "Residual", "Total"),
    df = c(1, 9, 10),
    sumsq = c(0.1412367727273, 1.4724405000000, 1.6136772727273),
    r.squared = c(0.087524795146, 0.912475204854, 1),
    statistic = c(0.8632817112443, NA, NA),
    p.value = c(0.635, NA, NA)
  )
  expect_equal(observed, expected)
})

test_that("adonisplus fails if LHS is not 'distmat'", {
  expect_error(
    adonisplus(example_data, example_dist, example_dist ~ study_group),
    regexp = "LHS of formula"
  )
})

test_that("adonisplus works for one group with restricted permutations", {
  observed <- adonisplus(
    example_data, example_dist, distmat ~ study_group,
    rep_meas_var = SubjectID,
    shuffle = c(study_group = "between"),
    permutations = 9
  )
  expected <- tibble::tibble(
    term = c("study_group", "Residual", "Total"),
    df = c(1, 9, 10),
    sumsq = c(0.141236772727273, 1.4724405, 1.61367727272727),
    r.squared = c(0.0875247951460386, 0.912475204853961, 1),
    statistic = c(0.863281711244329, NA, NA),
    p.value = c(0.8, NA, NA)
  )
  expect_equal(observed, expected)
})

test_that("adonisplus works for two groups", {
  observed <- adonisplus(
    example_data, example_dist, distmat ~ study_group * time_point,
    rep_meas_var = SubjectID,
    shuffle = c(study_group = "between", time_point = "within"),
    permutations = 9
  )
  expected <- tibble::tibble(
    term = c(
      "study_group", "time_point", "study_group:time_point",
      "Residual", "Total"
    ),
    df = c(1, 1, 1, 7, 10),
    sumsq = c(
      0.141236772727273, 0.2546555, 0.161871833333333,
      1.05591316666667, 1.61367727272727
    ),
    r.squared = c(
      0.0875247951460386, 0.157810675222318, 0.100312395835974,
      0.654352133795669, 1
    ),
    statistic = c(
      0.936305598131642, 1.68819610956015, 1.07310228634646, NA, NA
    ),
    p.value = c(0.4, 0.1, 0.3, NA, NA)
  )
  expect_equal(observed, expected)
})

test_that("adonisplus works for two groups with covariate", {
  observed <- adonisplus(
    example_data, example_dist, distmat ~ age + study_group * time_point,
    rep_meas_var = SubjectID,
    shuffle = c(study_group = "between", time_point = "within"),
    permutations = 9
  )
  expected <- tibble::tibble(
    term = c(
      "age", "study_group", "time_point", "study_group:time_point",
      "Residual", "Total"
    ),
    df = c(1, 1, 1, 1, 6, 10),
    sumsq = c(
      0.147625953372434, 0.12677620170778, 0.270223650980392,
      0.148155646153846, 0.920895820512821, 1.61367727272727
    ),
    r.squared = c(
      0.0914841869978943, 0.0785635417009472, 0.167458298847878,
      0.0918124389912542, 0.570681533462026, 1
    ),
    statistic = c(
      0.961841394547052, 0.825997027354399, 1.76061381729312,
      0.965292552232516, NA, NA
    ),
    p.value = c(0.7, 0.5, 0.1, 0.5, NA, NA)
  )
  expect_equal(observed, expected)
})
