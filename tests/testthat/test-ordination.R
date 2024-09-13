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

expected_axes <- tibble::tibble(
  Axis.1 = c(
    0.0218046657139025, 0.0373054791505106, 0.13833146386329,
    0.0482957523422068, 0.0906680281431401, 0.0120057411305378,
    -0.680446513436419, 0.0915777190805797, 0.0658719664029247,
    0.0777538535042316, 0.0968318441050968
  ),
  Axis.2 = c(
    -0.303086502259418, -0.277649583508124, 0.0538409488164232,
    -0.0371164252609219, -0.0685894632025572, 0.275689960626523,
    0.020089905908166, 0.306076031692592, 0.162663666052037,
    -0.100614869652573, -0.0313036692121472
  ),
  Axis.3 = c(
    0.0373672324641792, 0.0446548183955713, -0.106676969942398,
    0.0726191880827978, -0.148599519606932, -0.30153337102521,
    0.0100963735032305, 0.236709673732038, 0.169041261896952,
    -0.0652297761452144, 0.0515510886449863
  )
)

expected_pctvar <- structure(
  c(32.4014704674688, 23.8948365133504, 13.9227882293832),
  names = c("Axis.1", "Axis.2", "Axis.3")
)

expected_axislabel <- structure(
  c("PCoA axis 1 (32%)", "PCoA axis 2 (24%)", "PCoA axis 3 (14%)"),
  names = c("Axis.1", "Axis.2", "Axis.3")
)

test_that("pcoaplus works with default arguments", {
  obs <- pcoaplus(example_data, example_dist)
  expect_equal(class(obs), c("pcoaplus", "tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(obs), nrow(example_data))
  expect_equal(obs$Axis.1, expected_axes$Axis.1)
  expect_equal(obs$Axis.2, expected_axes$Axis.2)
  expect_equal(attr(obs, "pctvar"), expected_pctvar[1:2])
  expect_equal(attr(obs, "axislabel"), expected_axislabel[1:2])
})

test_that("pcoaplus works with more than two axes", {
  obs <- pcoaplus(example_data, example_dist, num_axes = 3)
  expect_equal(obs$Axis.3, expected_axes$Axis.3)
  expect_equal(attr(obs, "pctvar"), expected_pctvar[1:3])
  expect_equal(attr(obs, "axislabel"), expected_axislabel[1:3])
})
