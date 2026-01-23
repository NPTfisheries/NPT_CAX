# -----------------------
# Author(s): Mike Ackerman
# Purpose: Prepare latest SnakeRiverFishStatus natural-origin spawner abundance (NOSA) estimates for upload to Coordinated Assessments (CAX) database. 
#   The goal is to replace all past NOSA estimates uploaded by NPT with the latest and provide them in a standardized manner that makes clear
#   IPTDS-based estimates to populations versus expanded estimates (accounting for unmonitored habitat).#   
# 
# Created Date: January 23, 2026
#   Last Modified: 
#
# Notes:

# clear environment
rm(list = ls())

# load libraries

# read in population escapement estimates
pop_esc_df = list.files("C:/Git/SnakeRiverFishStatus/output/syntheses",
                        pattern = "(Chinook|Steelhead).*\\.xlsx$",
                        full.names = TRUE) %>%
  discard(~ grepl("~\\$", basename(.x))) %>%  # exclude temp/lock files, if an issue
  map_dfr(~ read_excel(.x, sheet = "Pop_Tot_Esc"))


nosa_df = pop_esc_df %>%
  # remove columns that won't be needed
  select(-incl_sites, -mean, -mode, -sd, -cv, -p_qrf_se) %>%
  # CRSFC-s & SCUMA: Use estimates from SC1 as it provides the longer time-series; SC3 didn't operated until spawn year 2022
  filter(!popid %in% c("CRSFC-s", "SCUMA")) %>%
  mutate(
    popid = case_when(
      popid == "CRLMA-s/CRSFC-s" ~ "CRSFC-s",
      popid == "SCLAW/SCUMA"     ~ "SCUMA",
      TRUE                       ~ popid
    ),
    # set p_qrf to 1
    p_qrf = if_else(popid %in% c("CRSFC-s", "SCUMA"), 1, p_qrf),
    # and use unexpanded estimates
    median_exp    = if_else(popid %in% c("CRSFC-s", "SCUMA"), median, median_exp),
    lower95ci_exp = if_else(popid %in% c("CRSFC-s", "SCUMA"), lower95ci, lower95ci_exp),
    upper95ci_exp = if_else(popid %in% c("CRSFC-s", "SCUMA"), upper95ci, upper95ci_exp)
  ) %>%
  # toss out Tucannon estimates & estimates that cover multiple pops
  filter(!str_detect(popid, "/"),
         !str_detect(popid, "SNTUC")) 

