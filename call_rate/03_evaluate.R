# ---
# title: Compare call rate estimates fixed vs variable window length
# author: Elly Knight
# created: 2026-08-21
# inputs: combined hawkears detections & WildTrax annotations
# outputs:
# notes:
# ---

# 1. Setup ----

## 1.1 Load packages ----
library(tidyverse) # data manipulation and visualization (version: 2.0.0)

## 1.2 Set root path ----
root <- "G:/Shared drives/ABMI_Acoustics/Classifiers/HawkEars - Temporal Localization/call_rate"

## 1.3 Load annotations ----
dat <- read.csv(file.path(root, "data", "WrangledDetections.csv"))

# 2. Data presents ----

## 2.1 Outcomes ----
table(dat$outcome, dat$type)

## 2.2. Summarize to minute ----
unverif <- dat |>
  group_by(file, type) |>
  summarize(calls_unverified = n()) |>
  ungroup()

verif <- dat |>
  dplyr::filter(outcome == "true positive") |>
  group_by(file, type) |>
  summarize(calls_verified = n()) |>
  ungroup()

annotated <- dat |>
  dplyr::filter(!is.na(start.manual)) |>
  group_by(file) |>
  summarize(calls_annotated = n()) |>
  ungroup()

recs <- annotated |>
  left_join(unverif, by = "file") |>
  left_join(verif, by = c("file", "type")) |>
  mutate(
    calls_unverified = ifelse(is.na(calls_unverified), 0, calls_unverified),
    calls_verified = ifelse(is.na(calls_verified), 0, calls_verified)
  ) |>
  dplyr::select(file, type, calls_annotated, calls_unverified, calls_verified)

## 2.3 Visalize ----

ggplot(recs) +
  geom_point(aes(x = calls_annotated, y = calls_unverified, color = type)) +
  geom_smooth(
    aes(x = calls_annotated, y = calls_unverified, color = type),
    method = "lm"
  ) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed")

ggplot(recs) +
  geom_point(aes(x = calls_annotated, y = calls_verified, color = type)) +
  geom_smooth(
    aes(x = calls_annotated, y = calls_verified, color = type),
    method = "lm"
  ) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed")

## 2.4 Correlation ----
fixed <- recs |>
  dplyr::filter(type == "fixed")
cor(fixed$calls_annotated, fixed$calls_unverified, method = "pearson")
cor(fixed$calls_annotated, fixed$calls_verified, method = "pearson")

variable <- recs |>
  dplyr::filter(type == "variable")
cor(variable$calls_annotated, variable$calls_unverified, method = "pearson")
cor(variable$calls_annotated, variable$calls_verified, method = "pearson")
