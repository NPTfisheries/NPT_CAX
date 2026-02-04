# -----------------------
# Author(s): Mike Ackerman
# Purpose: Use NWFSC rCAX package to retrieve NOSA estimates from CAX   
# 
# Created Date: February 4, 2026
#   Last Modified: 
#
# Notes:

# clear environment
rm(list = ls())

# install rCAX
remotes:::install_github("nwfsc-cb/rCAX@*release")

# load libraries
library(rCAX)
library(tidyverse)

# rCAX package information
rcax_termsofuse()          # terms of use
rcax_version()             # check version
rcax_citation()            # view citation

# view datasets available in CAX
rcax_tables = rcax_datasets()

# retrieve metadata for NOSA fields
nosa_meta = rcax_hli("NOSA", type = "colnames")

# retrieve NOSA table
nosa_df = rcax_hli("NOSA", 
                   qlist = list(limit = 10000)) # by default, rcax_hli() only retrieves 1000 records

npt_nosa_df = nosa_df %>%
  filter(submitagency == "NPT")

### END SCRIPT
