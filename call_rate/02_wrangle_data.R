# ---
# title: Wrangle hawkears output and annotations
# author: Elly Knight
# created: 2026-08-21
# inputs: hawkears detections & WildTrax annotations
# outputs: csv of wrangled detections
# notes:
# ---

# 1. Setup ----

## 1.1 Load packages ----
library(tidyverse) # data manipulation and visualization (version: 2.0.0)

## 1.2 Set root path ----
root <- "G:/Shared drives/ABMI_Acoustics/Classifiers/HawkEars - Temporal Localization/call_rate"

## 1.3 Load annotations ----
load(file.path(root, "data/WTReports.Rdata"))

# 2. Wrangle HE output ----

## 2.1 Get file lists ----
files.fixed <- list.files(
  file.path(root, "data", "hawkears_output", "test", "fixed"),
  full.names = TRUE,
  pattern = "*.txt"
)
files.variable <- list.files(
  file.path(root, "data", "hawkears_output", "test", "variable"),
  full.names = TRUE,
  pattern = "*.txt"
)

## 2.2 Read in files ----
he.fixed <- do.call(
  rbind,
  lapply(files.fixed, function(file) {
    dat <- read.table(
      file,
      header = FALSE,
      sep = "\t",
      stringsAsFactors = FALSE
    )

    dat$file <- tools::file_path_sans_ext(basename(file))
    dat
  })
) |>
  tidyr::separate(V3, into = c("sp", "score"), sep = ";")

colnames(he.fixed) <- c("start", "end", "sp", "score", "file")

he.variable <- do.call(
  rbind,
  lapply(files.variable, function(file) {
    dat <- read.table(
      file,
      header = FALSE,
      sep = "\t",
      stringsAsFactors = FALSE
    )

    dat$file <- tools::file_path_sans_ext(basename(file))
    dat
  })
) |>
  tidyr::separate(V3, into = c("sp", "score"), sep = ";")

colnames(he.variable) <- c("start", "end", "sp", "score", "file")

# 3. Put them together ----

## 3.1 Filter out recordings that we didn't test ----
rec.use <- rec |>
  dplyr::filter(
    source_file_name %in% list.files(file.path(root, "recordings_test"))
  )

dets <- main |>
  inner_join(
    rec.use |>
      dplyr::select(task_id, source_file_name),
    by = "task_id"
  ) |>
  rename(file = source_file_name, start = detection_time) |>
  mutate(end = start + tag_duration, file = str_replace(file, ".wav", "")) |>
  dplyr::select(file, start, end, tag_id, individual_order, rms_peak_dbfs)

## 3.2 Join with annotations ----

#filter out anything after task_duration (180 seconds)

fixed <- he.fixed |>
  dplyr::filter(start < 180) |>
  mutate(
    classifier_id = row_number(),
    file = str_remove_all(file, "_scores")
  ) |>
  full_join(
    dets |> mutate(manual_id = row_number()),
    by = join_by(
      file,
      start <= end,
      end >= start
    ),
    suffix = c(".classifier", ".manual"),
    relationship = "many-to-many"
  ) |>
  mutate(
    outcome = case_when(
      !is.na(classifier_id) & !is.na(manual_id) ~ "true positive",
      !is.na(classifier_id) & is.na(manual_id) ~ "false positive",
      is.na(classifier_id) & !is.na(manual_id) ~ "false negative"
    )
  )

variable <- he.variable |>
  dplyr::filter(start < 180) |>
  mutate(
    classifier_id = row_number(),
    file = str_remove_all(file, "_scores")
  ) |>
  full_join(
    dets |> mutate(manual_id = row_number()),
    by = join_by(
      file,
      start <= end,
      end >= start
    ),
    suffix = c(".classifier", ".manual"),
    relationship = "many-to-many"
  ) |>
  mutate(
    outcome = case_when(
      !is.na(classifier_id) & !is.na(manual_id) ~ "true positive",
      !is.na(classifier_id) & is.na(manual_id) ~ "false positive",
      is.na(classifier_id) & !is.na(manual_id) ~ "false negative"
    )
  )

## 3.4 Combine ----
out <- rbind(
  fixed |>
    mutate(type = "fixed"),
  variable |>
    mutate(type = "variable")
)

## 3.3 Save output ----
write.csv(
  out,
  file = file.path(root, "data", "WrangledDetections.csv"),
  row.names = FALSE
)
