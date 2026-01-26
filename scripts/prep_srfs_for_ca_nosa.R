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
library(tidyverse)
library(readxl)
library(PITcleanr)
library(sf)

# read in population escapement estimates
pop_esc_df = list.files("data/SRFS",
                        pattern = "(Chinook|Steelhead).*\\.xlsx$",
                        full.names = TRUE) %>%
  discard(~ grepl("~\\$", basename(.x))) %>%  # exclude temp/lock files, if an issue
  map_dfr(~ read_excel(.x, sheet = "Pop_Tot_Esc"))

# read in site escapement estimates
site_esc_df = list.files("data/SRFS",
                         pattern = "(Chinook|Steelhead).*\\.xlsx$",
                         full.names = TRUE) %>%
  discard(~ grepl("~\\$", basename(.x))) %>%  # exclude temp/lock files, if an issue
  map_dfr(~ read_excel(.x, sheet = "Site_Esc"))

# i'll need lat/lons for sites
site_ll = queryInterrogationMeta() %>%
  select(site_code = siteCode, latitude, longitude) %>%
  bind_rows(queryMRRMeta() %>%
              select(site_code = siteCode, latitude, longitude)) %>%
  filter(!is.na(latitude) | !is.na(longitude)) %>%
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = 4326
  ) %>%
  mutate(
    EscapementLong = st_coordinates(.)[, 1],
    EscapementLat  = st_coordinates(.)[, 2]
  )

# location of local NPT CAX database
path_to_db = "data/StreamNet API interface DES version 2024.1 - NPT.accdb"

# connect to database, load necessary table(s), and disconnect
if(!is.null(path_to_db)) {
  source("R/connectNPTCAdbase.R")
  con = connectNPTCAdbase(path_to_db)
  pop_df = DBI::dbReadTable(con, "Populations")
  DBI::dbDisconnect(con)
}

# clean up pop_df
sr_pop_df = pop_df %>%
  filter(str_detect(ESU_DPS, "Snake River"),
         str_detect(CommonName, "Chinook|Steelhead"),
         !is.na(TRT_POP_ID),
         !PopStatus == "Extirpated") %>%
  select(CommonName,
         TRT_POP_ID,
         Run,
         ESU_DPS,
         MajorPopGroup,
         PopID,
         RecoveryDomain) %>%
  arrange(CommonName, MajorPopGroup, TRT_POP_ID)

# prep SnakeRiverFishStatus results for CAX NOSA table
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
         !str_detect(popid, "SNTUC")) %>%
  # do we want to try to add estimates from RAPH and PAHH? if so, these would need to be QC'd
  # bind_rows(
  #   site_esc_df %>%
  #     filter(site %in% c("PAHH", "RAPH") & !(species == "Steelhead" & site == "RAPH")) %>%
  #     select(-site_operational) %>%
  #     rename(pop_sites = site) %>%
  #     mutate(mpg = case_when(
  #       species == "Chinook"   & pop_sites == "RAPH" ~ "South Fork Salmon River",
  #       species == "Chinook"   & pop_sites == "PAHH" ~ "Upper Salmon River",
  #       species == "Steelhead" & pop_sites == "PAHH" ~ "Salmon River"
  #     )) %>%
  #     mutate(popid = case_when(
  #       species == "Chinook"   & pop_sites == "RAPH" ~ "SRLSR",
  #       species == "Chinook"   & pop_sites == "PAHH" ~ "SRPAH",
  #       species == "Steelhead" & pop_sites == "PAHH" ~ "SRPAH-s"
  #     )) %>%
  #     mutate(p_qrf = case_when(
  #       species == "Chinook"   & pop_sites == "RAPH" ~ 0.26,
  #       species == "Chinook"   & pop_sites == "PAHH" ~ 0.32,
  #       species == "Steelhead" & pop_sites == "PAHH" ~ 0.99
  #     ))
  # ) %>%
  # recode SFSMA to SFMAI
  mutate(popid = if_else(popid == "SFSMA", "SFMAI", popid)) %>%
  # join population information from CAX population table
  mutate(species = recode(species, "Chinook" = "Chinook salmon")) %>%
  left_join(sr_pop_df,
            by = c("species" = "CommonName", "popid" = "TRT_POP_ID")) %>%
  # add a lat/lon based on the first site in pop_sites
  mutate(site_code = str_extract(pop_sites, "^[^,]+")) %>%
  left_join(site_ll %>%
              st_drop_geometry()) %>%
  select(-site_code)



