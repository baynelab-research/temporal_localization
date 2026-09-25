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
library(gridExtra)

## 1.2 Set root path ----
root <- "G:/Shared drives/ABMI_Acoustics/Classifiers/HawkEars - Temporal Localization/call_rate"

## 1.3 Load annotations ----
raw <- read.csv(file.path(root, "data", "WrangledDetections.csv"))

## 1.4 Get just the default 3s fixed detections ----
dat <- raw |> 
  dplyr::filter(length %in% c(NA, 3))

# 2. Data presents ----

## 2.1 Outcomes ----
table(dat$outcome, dat$type)

## 2.2. Summarize to minute ----
unverif <- dat |>
  group_by(file, type) |>
  summarize(calls_unverified = n(),
            duration_unverified = sum(duration.classifier)) |>
  ungroup()

verif <- dat |>
  dplyr::filter(outcome == "true positive") |>
  group_by(file, type) |>
  summarize(calls_verified = n(),
            duration_verified = sum(duration.classifier)) |>
  ungroup()

annotated_1 <- dat |>
  dplyr::filter(!is.na(start.manual)) |>
  group_by(file) |>
  summarize(calls_annotated = n(),
            duration_annotated = sum(duration.manual)) |>
  ungroup()

annotated <- annotated_1 |> 
  mutate(type="fixed") |> 
  rbind(annotated_1 |> 
          mutate(type="variable"))

recs <- annotated |>
  left_join(unverif, by = c("file", "type")) |>
  left_join(verif, by = c("file", "type")) |>
  mutate(
    calls_unverified = ifelse(is.na(calls_unverified), 0, calls_unverified),
    calls_verified = ifelse(is.na(calls_verified), 0, calls_verified),
    duration_unverified = ifelse(is.na(duration_unverified), 0, duration_unverified),
    duration_verified = ifelse(is.na(duration_verified), 0, duration_verified)
  ) |>
  dplyr::select(file, type, calls_annotated, calls_unverified, calls_verified, duration_annotated, duration_unverified, duration_verified)

## 2.3 Visalize ----

plot.calls.unverified <- ggplot(recs) +
  geom_point(aes(x = calls_annotated, y = calls_unverified, color = type)) +
  geom_smooth(
    aes(x = calls_annotated, y = calls_unverified, color = type),
    method = "lm"
  ) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed")

plot.calls.verified <- ggplot(recs) +
  geom_point(aes(x = calls_annotated, y = calls_verified, color = type)) +
  geom_smooth(
    aes(x = calls_annotated, y = calls_verified, color = type),
    method = "lm"
  ) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed")

ggsave(grid.arrange(plot.calls.unverified, plot.calls.verified, ncol=2),
      file=file.path(root, "figures", "Calls_AllData.jpeg"), width = 10, height = 4)

## 2.4 Correlation ----
fixed <- recs |>
  dplyr::filter(type == "fixed")
cor(fixed$calls_annotated, fixed$calls_unverified, method = "pearson")
cor(fixed$calls_annotated, fixed$calls_verified, method = "pearson")

variable <- recs |>
  dplyr::filter(type == "variable")
cor(variable$calls_annotated, variable$calls_unverified, method = "pearson")
cor(variable$calls_annotated, variable$calls_verified, method = "pearson")

## 2.5 Duration ----
#this was from when we didn't set a minimum duration
# ggplot(dat |> 
#          dplyr::filter(type=="variable")) + geom_histogram(aes(x=duration.classifier, fill=outcome))
# 
# ggsave(file.path(root, "figures", "VariableDuration.jpeg"), width = 6, height = 4)

plot.duration.unverified <- ggplot(recs) + 
  geom_point(aes(x = duration_annotated, y = duration_unverified, color = type)) +
  geom_smooth(
    aes(x = duration_annotated, y = duration_unverified, color = type),
    method = "lm"
  ) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed")

plot.duration.verified <- ggplot(recs) + 
  geom_point(aes(x = duration_annotated, y = duration_verified, color = type)) +
  geom_smooth(
    aes(x = duration_annotated, y = duration_verified, color = type),
    method = "lm"
  ) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed")

ggsave(grid.arrange(plot.duration.unverified, plot.duration.verified, ncol=2),
       file=file.path(root, "figures", "Duration_AllData.jpeg"), width = 10, height = 4)


# 3. Summmarize at varying thresholds ----

## 3.1 Summarize ----
thresh <- seq(0.1, 0.99, 0.01)

out.list <- list()
for(i in 1:length(thresh)){
  
  dat.i <- dplyr::filter(dat, score > thresh[i])
  
  unverif.i <- dat.i |>
    group_by(file, type) |>
    summarize(calls_unverified = n(),
              duration_unverified = sum(duration.classifier)) |>
    ungroup()
  
  verif.i <- dat.i |>
    dplyr::filter(outcome == "true positive") |>
    group_by(file, type) |>
    summarize(calls_verified = n(),
              duration_verified = sum(duration.classifier)) |>
    ungroup()
  
  recs.i <- annotated |>
    left_join(unverif.i, by = c("file", "type")) |>
    left_join(verif.i, by = c("file", "type")) |>
    mutate(
      calls_unverified = ifelse(is.na(calls_unverified), 0, calls_unverified),
      calls_verified = ifelse(is.na(calls_verified), 0, calls_verified),
      duration_unverified = ifelse(is.na(duration_unverified), 0, duration_unverified),
      duration_verified = ifelse(is.na(duration_verified), 0, duration_verified)
    ) |>
    dplyr::select(file, type, calls_annotated, calls_unverified, calls_verified, duration_annotated, duration_unverified, duration_verified) |> 
    mutate(thresh = thresh[i])
  
  out.list[[i]] <- recs.i
  
  cat(i, " ")
  
}

out <- do.call(rbind, out.list)

## 3.2 Get the correlations ----
out.cor <- out |> 
  group_by(type, thresh) |> 
  summarize(cor_calls_verified = cor(calls_annotated, calls_verified, method="pearson"),
            cor_calls_unverified = cor(calls_annotated, calls_unverified, method="pearson"),
            cor_duration_verified = cor(duration_annotated, duration_verified, method="pearson"),
            cor_duration_unverified = cor(duration_annotated, duration_unverified, method="pearson")) |> 
  ungroup() |> 
  pivot_longer(cor_calls_verified:cor_duration_unverified, names_to="metric", values_to="correlation") |> 
  mutate(metric = str_remove(metric, "cor_")) |> 
  separate(metric, into=c("metric", "verification"))

## 3.3 Plot ----
ggplot(out.cor |> 
         dplyr::filter(thresh > 0.2), aes(x=thresh, y=correlation)) +
  geom_line(aes(colour=type)) +
  facet_grid(metric ~ verification, scales="free")

ggsave(file = file.path(root, "figures", "Correlations.jpeg"), width = 10, height = 8)

#4. Varying window lengths ----

## 4.2 Summarize ----
todo <- expand.grid(thresh = seq(0.1, 0.99, 0.01),
                    length = unique(raw$length)) |> 
  dplyr::filter(!is.na(length))

out.list2 <- list()
for(i in 1:nrow(todo)){
  
  dat.i <- dplyr::filter(raw, score > todo$thresh[i],
                         length %in% c(NA, todo$length[i]))
  
  unverif.i <- dat.i |>
    group_by(file, type) |>
    summarize(calls_unverified = n(),
              duration_unverified = sum(duration.classifier)) |>
    ungroup()
  
  verif.i <- dat.i |>
    dplyr::filter(outcome == "true positive") |>
    group_by(file, type) |>
    summarize(calls_verified = n(),
              duration_verified = sum(duration.classifier)) |>
    ungroup()
  
  recs.i <- annotated |>
    left_join(unverif.i, by = c("file", "type")) |>
    left_join(verif.i, by = c("file", "type")) |>
    mutate(
      calls_unverified = ifelse(is.na(calls_unverified), 0, calls_unverified),
      calls_verified = ifelse(is.na(calls_verified), 0, calls_verified),
      duration_unverified = ifelse(is.na(duration_unverified), 0, duration_unverified),
      duration_verified = ifelse(is.na(duration_verified), 0, duration_verified)
    ) |>
    dplyr::select(file, type, calls_annotated, calls_unverified, calls_verified, duration_annotated, duration_unverified, duration_verified) |> 
    mutate(thresh = todo$thresh[i],
           length = todo$length[i])
  
  out.list2[[i]] <- recs.i
  
  cat(i, " ")
  
}

out2 <- do.call(rbind, out.list2) |> 
  mutate(length = ifelse(type=="variable", NA, length))

## 4.2 Get the correlations ----
out.cor2 <- out2 |> 
  group_by(type, thresh, length) |> 
  summarize(cor_calls_verified = cor(calls_annotated, calls_verified, method="pearson"),
            cor_calls_unverified = cor(calls_annotated, calls_unverified, method="pearson"),
            cor_duration_verified = cor(duration_annotated, duration_verified, method="pearson"),
            cor_duration_unverified = cor(duration_annotated, duration_unverified, method="pearson")) |> 
  ungroup() |> 
  pivot_longer(cor_calls_verified:cor_duration_unverified, names_to="metric", values_to="correlation") |> 
  mutate(metric = str_remove(metric, "cor_")) |> 
  separate(metric, into=c("metric", "verification")) |> 
  mutate(category = paste0(type, " - ", length)) |> 
  dplyr::filter(category!="fixed - NA")

## 4.3 Plot ----
ggplot(out.cor2 |> 
         dplyr::filter(thresh > 0.2), aes(x=thresh, y=correlation)) +
  geom_line(aes(colour=category)) +
  geom_line(data= out.cor2 |> 
              dplyr::filter(type=="variable", thresh > 0.2),
            aes(x=thresh, y=correlation),
            colour="black", linewidth = 1) +
  facet_grid(metric ~ verification, scales="free")

ggsave(file = file.path(root, "figures", "Correlations_WindowLength.jpeg"), width = 10, height = 8)
