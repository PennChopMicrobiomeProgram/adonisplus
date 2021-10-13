library(tidyverse)
library(usedist)

farmm_samples <- read_tsv(
  "data-raw/farmm_samples.tsv", show_col_types = FALSE) %>%
  filter(SampleType %in% "Feces") %>%
  filter(Keep) %>%
  filter(!(study_day %in% "PS")) %>%
  mutate(study_day = as.integer(study_day)) %>%
  mutate(study_group = fct_relevel(study_group, "Omnivore", "Vegan", "EEN")) %>%
  mutate(current_antibiotics = fct_recode(
    current_antibiotics, pre = "Pre Antibiotics",
    current = "Antibiotics Treatment", post = "Post Antibiotics")) %>%
  mutate(current_antibiotics = fct_relevel(
    current_antibiotics, "pre", "current", "post")) %>%
  mutate(host_frac = host / (host + non_host)) %>%
  mutate(new_sample_id = paste(
    "farmm", SubjectID, sprintf("%02d", study_day), sep = "."))

farmm_new_sample_ids <- farmm_samples$new_sample_id
names(farmm_new_sample_ids) <- farmm_samples$SampleID

farmm_bc <- read_tsv(
  "data-raw/farmm_bc.tsv", show_col_types = FALSE) %>%
  rename(sample_id = `...1`) %>%
  column_to_rownames("sample_id") %>%
  as.matrix() %>%
  `[`(farmm_samples$SampleID, farmm_samples$SampleID)

colnames(farmm_bc) <- farmm_new_sample_ids[colnames(farmm_bc)]
rownames(farmm_bc) <- farmm_new_sample_ids[rownames(farmm_bc)]

farmm_samples <- farmm_samples %>%
  select(
    sample_id = new_sample_id,
    subject_id = SubjectID,
    study_day,
    diet = study_group,
    antibiotics = current_antibiotics,
    height,
    weight,
    age = Age,
    bacterial_16S_copies = copy_num_per_gram_feces,
    num_reads = non_host,
    host_frac) %>%
  arrange(diet, subject_id, study_day)

farmm_bc <- farmm_bc %>%
  as.dist() %>%
  dist_subset(farmm_samples$sample_id)

usethis::use_data(farmm_samples, overwrite = TRUE)
usethis::use_data(farmm_bc, overwrite = TRUE)
