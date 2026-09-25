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

## 2.1 Get files ----

he.variable <- read.csv(file.path(root, "data", "hawkears_output", "test", "temporallocalization_oven_variable_test.csv")) |> 
  rename(sp = name, file = recording, start = start_time, end =end_time) |> 
  mutate(length = NA)

files.fixed <- data.frame(path = list.files(file.path(root, "data", "hawkears_output", "test" ), pattern="*fixed*", full.names = TRUE)) |> 
  mutate(length = as.numeric(str_sub(path, -12, -10)))

he.fixed <- data.frame()
for(i in 1:nrow(files.variable)){
  he.fixed <- read.csv(files.fixed$path[i]) |> 
    rename(sp = name, file = recording,
           start = start_time, end = end_time) |> 
    mutate(length = files.fixed$length[i]) |> 
    rbind(he.fixed)
  
}

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

#let's put a minimum duration on as well
variable <- he.variable |>
  dplyr::filter(start < 180,
                end - start >= 1) |>
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
) |> 
  mutate(duration.classifier = end.classifier - start.classifier,
         duration.manual = end.manual - start.manual)

## 3.3 Save output ----
write.csv(
  out,
  file = file.path(root, "data", "WrangledDetections.csv"),
  row.names = FALSE
)
