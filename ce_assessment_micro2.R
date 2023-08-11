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
month_since_own_home                  <- -0.9899421#runif(n = 1, min = -1, max = 1) #sample(sample_choices,1)                                                                        
months_since_any_home                 <- -0.8785537#runif(n = 1, min = -1, max = 1) #sample(sample_choices,1)                        
loc_sleep_last_night                  <- -1.7076351#runif(n = 1, min = -1, max = 1) #sample(sample_choices,1)                                                                              
loc_sleep_tonight                     <- -1.1801855#runif(n = 1, min = -1, max = 1) #sample(sample_choices,1)                                                                      
now_or_at.risk_violence               <- -1.0692902#runif(n = 1, min = -1, max = 1) #sample(sample_choices,1)                               
leave_prev.curr_living_bc_felt_unsafe <- -0.9929337#runif(n = 1, min = -1, max = 1) #sample(sample_choices,1)                                                   
exp_violence_close                    <- -1.6955225#runif(n = 1, min = -1, max = 1) #sample(sample_choices,1)                                                   
exp_violence_homeless                 <- -1.3794149#runif(n = 1, min = -1, max = 1) #sample(sample_choices,1)                                                            
hh_phys.mntl_health_conds             <- -0.9330276#runif(n = 1, min = -1, max = 1) #sample(sample_choices,1)
hh_lung.kid.liv.heart.sud             <- -0.5711712#runif(n = 1, min = -1, max = 1) #sample(sample_choices,1)
hard_get_doctor_rx                    <- -0.8691389#runif(n = 1, min = -1, max = 1) #sample(sample_choices,1)              
health_ins                            <- -1.7896179#runif(n = 1, min = -1, max = 1) #sample(sample_choices,1)                                                                            
hh_size                               <- -0.3508039#runif(n = 1, min = -1, max = 1) #sample(sample_choices,1)                                                
hh_anyone_5orUnder                    <- -0.4714653#runif(n = 1, min = -1, max = 1) #sample(sample_choices,1)                                                                                 
hh_anyone_55orOver                    <- -0.9887942#runif(n = 1, min = -1, max = 1) #sample(sample_choices,1)                                                                                    
hh_pregnant                           <- -0.2541675#runif(n = 1, min = -1, max = 1) #sample(sample_choices,1)                                                                   
non.hh_children                       <- -0.8760057#runif(n = 1, min = -1, max = 1) #sample(sample_choices,1) 
non.hh_adults                         <- -0.6804583#runif(n = 1, min = -1, max = 1) #sample(sample_choices,1)

