# -----------------------
# Author(s): Mike Ackerman
# Purpose: Prepare latest SnakeRiverFishStatus natural-origin spawner abundance (NOSA) estimates for upload to Coordinated Assessments (CAX) database. 
#   The goal is to replace all past NOSA estimates uploaded by NPT with the latest and provide them in a standardized manner that makes clear
#   IPTDS-based estimates to populations versus expanded estimates (accounting for unmonitored habitat).#   
# 
# Created Date: January 23, 2026
#   Last Modified: January 27, 2026
#
# Notes:

# clear environment
rm(list = ls())

# load libraries
library(tidyverse)
library(readxl)
library(PITcleanr)
library(sf)
library(writexl)

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

# get lat/lons for sites
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

# the threshhold on which to consider a pop fully monitored by IPTDS 
threshhold = 0.99

# prep SnakeRiverFishStatus results for CAX NOSA table
srfs_to_cax_df = pop_esc_df %>%
  # trim out unneeded columns
  select(-incl_sites, -mean, -mode, -sd, -cv, -p_qrf_se) %>%
  # CRSFC-s & SCUMA: use estimates from SC1 as it provides the longer time-series; SC3 didn't operated until spawn year 2022
  filter(!popid %in% c("CRSFC-s", "SCUMA")) %>%
  mutate(
    popid = case_when(
      popid == "CRLMA-s/CRSFC-s" ~ "CRSFC-s",
      popid == "SCLAW/SCUMA"     ~ "SCUMA",
      TRUE                      ~ popid
    ),
    # for CRSFC-s & SCUMA, set p_qrf to 1 and use un-expanded estimates
    p_qrf         = if_else(popid %in% c("CRSFC-s", "SCUMA"), 1, p_qrf),
    median_exp    = if_else(popid %in% c("CRSFC-s", "SCUMA"), median,    median_exp),
    lower95ci_exp = if_else(popid %in% c("CRSFC-s", "SCUMA"), lower95ci, lower95ci_exp),
    upper95ci_exp = if_else(popid %in% c("CRSFC-s", "SCUMA"), upper95ci, upper95ci_exp)
  ) %>%
  # toss out Tucannon estimates & estimates for multiple populations
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
  # recode SFSMA to SFMAI; i'd argue CAX is using wrong popid that doesn't match ICTRT
  mutate(popid = if_else(popid == "SFSMA", "SFMAI", popid)) %>%
  # attach population metadata from sr_pop_df
  mutate(species = recode(species, "Chinook" = "Chinook salmon")) %>%
  left_join(sr_pop_df, by = c("species" = "CommonName", "popid" = "TRT_POP_ID")) %>%
  # add lat/lon based on the first site in pop_sites
  mutate(site_code = str_extract(pop_sites, "^[^,]+")) %>%
  left_join(site_ll %>% st_drop_geometry(), by = "site_code") %>%
  select(-site_code) %>%
  # split IPTDS-based vs habitat-expanded records
  mutate(
    PopFit = if_else(p_qrf >= threshhold, "Same", "Portion"),
    MetaComments = case_when(
      p_qrf >= threshhold                       ~ "STADEM and DABOM",
      p_qrf <  threshhold & PopFit == "Portion" ~ "STADEM and DABOM",
      p_qrf <  threshhold & PopFit == "Same"    ~ "STADEM, DABOM, and QRF"
    )
  ) %>%
  # add expanded estimates as new records
  {
    d <- .
    bind_rows(
      d,
      d %>%
        filter(p_qrf < threshhold) %>%
        mutate(
          median        = median_exp,
          lower95ci     = lower95ci_exp,
          upper95ci     = upper95ci_exp,
          PopFit        = "Same",
          MetaComments  = "STADEM, DABOM, and QRF"
        )
    )
  } %>%
  # round estimates
  mutate(across(c(median, lower95ci, upper95ci), round)) %>%
  distinct() %>%
  arrange(species, popid, spawn_yr, PopFit) %>%
  # notes and protocol fields
  mutate(
    qrf_note = paste0("Percent pop coverage estimated using redd QRF dataset (See et al. 2021)."),
    PopFitNotes = case_when(
      p_qrf >= threshhold & PopFit == "Same"    ~ paste0("Estimate reflects PTAGIS site(s): ", pop_sites, " which monitor an estimated ", round(p_qrf * 100, 1), "% of available habitat. PopFit considered 'Same' if >= ", threshhold * 100, "%. ", qrf_note),
      p_qrf <  threshhold & PopFit == "Portion" ~ paste0("Estimate reflects PTAGIS site(s): ", pop_sites, " which monitor an estimated ", round(p_qrf * 100, 1), "% of available habitat. PopFit considered 'Portion' because < ", threshhold * 100, "% and estimate NOT expanded to account for unmonitored habitat. ", qrf_note),
      p_qrf <  threshhold & PopFit == "Same"    ~ paste0("Estimate reflects PTAGIS site(s): ", pop_sites, " which monitor an estimated ", round(p_qrf * 100, 1), "% of available habitat. PopFit considered 'Same' because estimate is expanded to account for unmonitored habitat. ", qrf_note),
      TRUE ~ NA_character_
    ),
    ProtMethName = case_when(
      (p_qrf >= threshhold & PopFit == "Same") | (p_qrf < threshhold & PopFit == "Portion") ~ "PIT tag Based Escapement Estimation Above Lower Granite Dam v1.0",
      (p_qrf <  threshhold & PopFit == "Same")                                              ~ "DRAFT",
      TRUE ~ NA_character_
    ),
    ProtMethURL = case_when(
      (p_qrf >= threshhold & PopFit == "Same") | (p_qrf < threshhold & PopFit == "Portion") ~ "https://www.monitoringresources.org/Document/Protocol/Details/2187",
      (p_qrf <  threshhold & PopFit == "Same")                                              ~ "DRAFT",
      TRUE ~ NA_character_
  )) %>%
  select(-qrf_note) %>%
  # assign WaterBody based on pop_sites
  mutate(WaterBody = case_when(
    MetaComments == "STADEM and DABOM" & pop_sites %in% c("ACB", "ACM")                               ~ "Asotin Creek",
    MetaComments == "STADEM and DABOM" & pop_sites == "BRC"                                           ~ "Bear Valley Creek",
    MetaComments == "STADEM and DABOM" & pop_sites == "TAY"                                           ~ "Big Creek",
    MetaComments == "STADEM and DABOM" & pop_sites %in% c("BBA", "KHS")                               ~ "Big Bear Creek",
    MetaComments == "STADEM and DABOM" & pop_sites %in% c("BSC", "BSC, CMP")                          ~ "Big Sheep Creek",
    MetaComments == "STADEM and DABOM" & pop_sites == "CCW"                                           ~ "Catherine Creek",
    MetaComments == "STADEM and DABOM" & pop_sites == "CLC"                                           ~ "Clear Creek",
    MetaComments == "STADEM and DABOM" & pop_sites %in% c("BED, CLC, LAP", "BED, EPR, LAP, LAW",
                                                          "CLC, HLM, KHS, LAP", "CLC, KHS, LAP, PCM") ~ "Clearwater River",
    MetaComments == "STADEM and DABOM" & pop_sites == "ESS"                                           ~ "East Fork South Fork Salmon River",
    MetaComments == "STADEM and DABOM" & pop_sites == "UGR"                                           ~ "Grande Ronde River",
    MetaComments == "STADEM and DABOM" & pop_sites %in% c("COC, IR1", "IR3")                          ~ "Imnaha River",
    MetaComments == "STADEM and DABOM" & pop_sites == "JOC"                                           ~ "Joseph Creek",
    MetaComments == "STADEM and DABOM" & pop_sites == "LAP"                                           ~ "Lapwai Creek",
    MetaComments == "STADEM and DABOM" & pop_sites == "LAP"                                           ~ "Lemhi River",
    MetaComments == "STADEM and DABOM" & pop_sites == "LRL"                                           ~ "Lochsa River",
    MetaComments == "STADEM and DABOM" & pop_sites == "LC1"                                           ~ "Lolo Creek",
    MetaComments == "STADEM and DABOM" & pop_sites == "LGW"                                           ~ "Lookingglass Creek",
    MetaComments == "STADEM and DABOM" & pop_sites == "MAR"                                           ~ "Marsh Creek",
    MetaComments == "STADEM and DABOM" & pop_sites == "MR1"                                           ~ "Minam River",
    MetaComments == "STADEM and DABOM" & pop_sites == "MIS"                                           ~ "Mission Creek",
    MetaComments == "STADEM and DABOM" & pop_sites == "NFS"                                           ~ "North Fork Salmon River",
    MetaComments == "STADEM and DABOM" & pop_sites == "PCA"                                           ~ "Panther Creek",
    MetaComments == "STADEM and DABOM" & pop_sites %in% c("HLM", "JUL", "KHS, PCM")                   ~ "Potlatch River",
    MetaComments == "STADEM and DABOM" & pop_sites == "ZEN"                                           ~ "Secesh River",
    MetaComments == "STADEM and DABOM" & pop_sites == "SW1"                                           ~ "Selway River",
    MetaComments == "STADEM and DABOM" & pop_sites == "SC1"                                           ~ "South Fork Clearwater River",
    MetaComments == "STADEM and DABOM" & pop_sites == "KRS"                                           ~ "South Fork Salmon River",
    MetaComments == "STADEM and DABOM" & pop_sites %in% c("MDC, UGS", "UGS")                          ~ "upper Grande Ronde River",
    MetaComments == "STADEM and DABOM" & pop_sites == "VC2"                                           ~ "Valley Creek",
    MetaComments == "STADEM and DABOM" & pop_sites %in% c("MR1, WR2", "WR1", "WR2")                   ~ "Wallowa River",
    MetaComments == "STADEM and DABOM" & pop_sites == "WEN"                                           ~ "Wenaha River",
    MetaComments == "STADEM and DABOM" & pop_sites == "WB1"                                           ~ "Whitebird Creek",
    MetaComments == "STADEM and DABOM" & pop_sites == "YFK"                                           ~ "Yankee Fork Salmon River",
    MetaComments == "STADEM and DABOM" & pop_sites %in% c("ACM, COU", "CRC, LLR", "ESS, KRS", 
                                                          "HLM, KHS, LAP", "HLM, MIS", "JA1, LAP",
                                                          "JA1, SWT", "JUL, LAP", "KHS, LAP",
                                                          "LAP, LAW, SIX", "LGW, UGR", "VC2, YFK")    ~ "Multiple",
    MetaComments == "STADEM, DABOM, and QRF"                                                          ~ "Multiple"
  )) %>%
  # final shaping for CAX NOSA
  select(-pop_sites, -n_tags, -p_qrf, -contains("_exp")) %>%
  rename(
    CommonName       = species,
    SpawningYear     = spawn_yr,
    CommonPopName    = popid,
    NOSAIJ           = median,
    NOSAIJLowerLimit = lower95ci,
    NOSAIJUpperLimit = upper95ci,
    Comments         = notes
  ) %>%
  mutate(
    EstimateType = "Escapement",
    EscapementTiming = case_when(
      CommonName == "Chinook salmon" ~ "Jun-Oct",
      CommonName == "Steelhead"      ~ "Feb-Jun",
      TRUE ~ NA_character_
    ),
    NOSAIFAlpha = 0.05,
    ContactPersonFirst = "Mike",
    ContactPersonLast  = "Ackerman",
    ContactPhone       = "208-634-5290",
    ContactEmail       = "mikea@nezperce.org",
    ContactAgency      = "Nez Perce Tribe",
    SubmitAgency       = "NPT",
    Publish            = "Yes"
  )

# write to excel, if needed
write_xlsx(srfs_to_cax_df, path = paste0("output/SnakeRiverFishStatus_Results_4_CAX_NOSA_", Sys.Date(), ".xlsx"))

### END SCRIPT




