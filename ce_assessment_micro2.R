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

#rm(list=ls());cat('\f')

# fingerprint----
sim.fingerprint <- openssl::md5(as.character(Sys.time())) %>%
  substr(., nchar(.) - 7, nchar(.))

git.raw.md5 <- read_file("https://raw.githubusercontent.com/timbender-ncceh/ncceh_data_tools/main/ce_assessment_micro.R") %>%
  md5
gc()

# import data from GitHub----
clients <- read_csv("https://raw.githubusercontent.com/timbender-ncceh/ce_assessment/main/MASTER_client_deidentified.csv")
cw_vuln <- read_csv("https://raw.githubusercontent.com/timbender-ncceh/ce_assessment/main/MASTER_crosswalk_vuln.csv")
quest_vuln <- read_csv("https://raw.githubusercontent.com/timbender-ncceh/ce_assessment/main/MASTER_questions_vuln.csv")
cw_qshortname <- read_csv("https://raw.githubusercontent.com/timbender-ncceh/ce_assessment/main/MASTER_cw_qshortname.csv")

#clients[is.na(clients$order_vuln),]

# fix doctor issue
lack.doctor <- "Is the lack of housing making it hard to get to a doctor office or take prescribed medications?"
clients$question[grepl("doctor", clients$question)] <- lack.doctor
cw_vuln$question[grepl("doctor", cw_vuln$question)] <- lack.doctor
quest_vuln$question[grepl("doctor", quest_vuln$question)] <- lack.doctor
cw_qshortname$question[grepl("doctor", cw_qshortname$question)] <- lack.doctor

# Fix NAs
clients$order_vuln[is.na(clients$order_vuln)] <- 0
clients$order_vuln.norm[is.na(clients$order_vuln.norm)] <- 0

quest_vuln$order_vuln[is.na(quest_vuln$order_vuln)] <- 0
quest_vuln$order_vuln.norm[is.na(quest_vuln$order_vuln.norm)] <- 0

# Vars----
max.composite.score                     <- 1000

if(!"override.weights" %in% ls()){
  # set weights
  month_since_own_home                  <- 7
  months_since_any_home                 <- 6
  loc_sleep_last_night                  <- 6
  loc_sleep_tonight                     <- 7
  now_or_at.risk_violence               <- 6
  leave_prev.curr_living_bc_felt_unsafe <- 4
  exp_violence_close                    <- 3
  exp_violence_homeless                 <- 5
  hh_phys.mntl_health_conds             <- 7
  hh_lung.kid.liv.heart.sud             <- 5
  hard_get_doctor_rx                    <- 6
  health_ins                            <- 3
  hh_size                               <- 7
  hh_anyone_5orUnder                    <- 4
  hh_anyone_55orOver                    <- 6
  hh_pregnant                           <- 6
  non.hh_children                       <- 5
  non.hh_adults                         <- 7
}



# Build weight df
weights.df <- cw_qshortname %>%
  mutate(., 
         weight_factor = NA)

for(i in 1:nrow(weights.df)){
  weights.df$weight_factor[i] <- get(weights.df$short_name[i])
}


# Multiply weights by vuln_norm
out.score <- left_join(clients, 
          select(weights.df, qnum, weight_factor)) %>%
  mutate(., 
         q_score = order_vuln.norm * weight_factor) %>%
  group_by(client_id2, 
           Region, 
           Gender, 
           Race, 
           Ethnicty) %>%
  summarise(comp_score = sum(q_score)) %>%
  .[order(.$comp_score,decreasing = T),]

# adjust scores greater than max composite score limit
out.score$comp_score[out.score$comp_score > max.composite.score] <- max.composite.score

# Clean for Maximum composite score

# label top 20
out.score$top20 <- out.score$client_id2 %in% slice_max(ungroup(out.score), 
          order_by = comp_score, 
          prop = 0.2)$client_id2


out.score$weights <- round(weights.df[order(weights.df$qnum),]$weight_factor,2) %>% 
  paste(., sep = "|", collapse = "|")
out.score$sim_fp <- sim.fingerprint %>% as.character()



# write_out 

write_csv(x = out.score, 
          file = "model_outputs2.csv", 
          append = T)
