library(readr)
library(dplyr)
library(lubridate)
library(data.table)
library(ggplot2)
library(forcats)
library(openssl)
library(glue)
library(curl)
library(janitor)

# functions to learn ----
# select()
# get()
# unnest()
# clean_names()
# 

setwd("C:/Users/TimBender/Documents/R/ncceh/projects/ce_assessment")

rm(list=ls());cat('\f')

# import data from GitHub----
clients <- read_csv("https://raw.githubusercontent.com/timbender-ncceh/ce_assessment/main/MASTER_client_deidentified.csv")
cw_vuln <- read_csv("https://raw.githubusercontent.com/timbender-ncceh/ce_assessment/main/MASTER_crosswalk_vuln.csv")
quest_vuln <- read_csv("https://raw.githubusercontent.com/timbender-ncceh/ce_assessment/main/MASTER_questions_vuln.csv")

cw_vuln
quest_vuln

# Fix NAs
clients$order_vuln[is.na(clients$order_vuln)] <- 0
clients$order_vuln.norm[is.na(clients$order_vuln.norm)] <- 0

quest_vuln$order_vuln[is.na(quest_vuln$order_vuln)] <- 0
quest_vuln$order_vuln.norm[is.na(quest_vuln$order_vuln.norm)] <- 0

# set weights
month_since_own_home                  <- 1
months_since_any_home                 <- 1
loc_sleep_last_night                  <- 1
loc_sleep_tonight                     <- 1
now_or_at.risk_violence               <- 1
leave_prev.curr_living_bc_felt_unsafe <- 1
exp_violence_close                    <- 1
exp_violence_homeless                 <- 1
hh_phys.mntl_health_conds             <- 1
hh_lung.kid.liv.heart.sud             <- 1
hard_get_doctor_rx                    <- 1
health_ins                            <- 1
hh_size                               <- 1
hh_anyone_5orUnder                    <- 1
hh_anyone_55orOver                    <- 1
hh_pregnant                           <- 1
non.hh_children                       <- 1
non.hh_adults                         <- 1

# Multiply weights by vuln_norm

# 
