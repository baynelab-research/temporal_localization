# ---
# title: Download recordings for call rate temporal localization
# author: Elly Knight
# created: 2026-08-14
# inputs: NA
# outputs: wildTrax annotations & folder of recordings
# notes: 
# ---

# 1. Setup ----

## 1.1 Load packages ----
library(tidyverse) # data manipulation and visualization (version: 2.0.0)
library(wildrtrax) # download wildtrax data

## 1.2 Authenticate Wildtrax ----
source("call_rate/login.R")
wt_auth()

# 2. Download WT reports ----

## 2.1 Get reports ----
reports <- wt_download_report(sensor_id = "ARU", project_id = 2780,
                              reports=c("main", "recording"))

## 2.2 Save reports ----
main <- reports$ABMI_SingleSpecies_OVEN_SoundRates_SeasonalSeries_AnyYears_Bayne_main_report.csv
rec <- reports$ABMI_SingleSpecies_OVEN_SoundRates_SeasonalSeries_AnyYears_Bayne_recording_report.csv
save(main, rec, file="call_rate/data/WTReports.Rdata")

# 3. Download recordings  ----

## 3.1 Wrangle the list ----
dl <- rec |> 
  inner_join(main |> 
               dplyr::filter(species_code=="OVEN") |> 
               dplyr::select(recording_id) |> 
               unique()) |> 
  mutate(file = file.path("call_rate", "data", "recordings",
                          source_file_name))

write.csv(dl, "call_rate/data/RecordingList.csv")

## 3.2 Download ----
for(i in 1:nrow(dl)){
  
  download.file(dl$recording_url,
                dl$file,
                mode="wb")

}

# End of script ----