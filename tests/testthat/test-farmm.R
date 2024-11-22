test_that("farmm_samples data is available", {
  expect_equal(nrow(farmm_samples), 414)
  expect_equal(colnames(farmm_samples), c(
    "sample_id", "subject_id", "study_day", "diet", "antibiotics", "height",
    "weight", "age", "bacterial_16S_copies", "num_reads", "host_frac"
  ))
  expect_equal(levels(farmm_samples$diet), c("Omnivore", "Vegan", "EEN"))
  expect_equal(levels(farmm_samples$antibiotics), c("pre", "current", "post"))
})

test_that("farmm_bc distance matrix matches farmm_samples data frame", {
  expect_equal(dim(as.matrix(farmm_bc)), c(414, 414))
  expect_equal(colnames(as.matrix(farmm_bc)), farmm_samples$sample_id)
  expect_equal(rownames(as.matrix(farmm_bc)), farmm_samples$sample_id)
})
