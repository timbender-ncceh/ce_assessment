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

name.scenario <- "Optimal"
if(!"override.weights" %in% ls()){
  # set weights
  month_since_own_home                  <- 3 
  months_since_any_home                 <- 9 
  loc_sleep_last_night                  <- 9 
  loc_sleep_tonight                     <- 3 
  now_or_at.risk_violence               <- 1 
  leave_prev.curr_living_bc_felt_unsafe <- 2
  exp_violence_close                    <- 4 
  exp_violence_homeless                 <- 3 
  hh_phys.mntl_health_conds             <- 9 
  hh_lung.kid.liv.heart.sud             <- 6 
  hard_get_doctor_rx                    <- 1 
  health_ins                            <- 7 
  hh_size                               <- 7 
  hh_anyone_5orUnder                    <- 5 
  hh_anyone_55orOver                    <- 4 
  hh_pregnant                           <- 8 
  non.hh_children                       <- 8 
  non.hh_adults                         <- 9 
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


# summary chart
cw.ques <- read_csv('https://raw.githubusercontent.com/timbender-ncceh/ce_assessment/main/MASTER_cw_qshortname.csv')
plot.ques <- NULL
plot.wts  <- NULL
for(i in 1:nrow(cw.ques)){
  plot.ques <- c(plot.ques, cw.ques[i,]$short_name)
  plot.wts <- c(plot.wts, get(cw.ques[i,]$short_name))
}

plot.df <- data.frame(short_name = plot.ques, 
           weight = plot.wts, 
           qnum = (1:18)) %>%
  left_join(., 
            read_csv("https://raw.githubusercontent.com/timbender-ncceh/ce_assessment/main/MASTER_crosswalk_vuln.csv")) %>%
  as_tibble() %>%
  select(., qnum, short_name, vuln_group, weight)
plot.df$short_name_f <- factor(plot.df$short_name, 
                               levels = unique(plot.df$short_name[order(plot.df$weight)]))


ggplot(data = plot.df, 
         aes(x = short_name_f, y = weight)) + 
  geom_col()+
  facet_grid(~vuln_group, scales = "free", space = "free")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+
  scale_y_continuous(limits = c(0,10), 
                     breaks = seq(0,100,by=1), 
                     minor_breaks = seq(0,100,by=1))+
  labs(title = "Weight Values by Question and Vulnerability Category", 
       subtitle = glue("Scenario Name: {name.scenario}\nSim Fingerprint: {sim.fingerprint}"))


Sys.sleep(3)

mo.last <- read_csv("model_outputs2.csv") 
mo.last <- mo.last[mo.last$sim_fp == last(mo.last$sim_fp),]


# race = mo.last2
mo.last2 <- mo.last %>%
  group_by(top20 = ifelse(top20 == T,"top20", "top100"),Race, .drop = F) %>%
  summarise(n = n_distinct(client_id2)) %>%
  ungroup() %>%
  as.data.table() %>%
  dcast(., 
        Race ~ top20, 
        fill = 0) %>%
  as.data.frame() %>%
  mutate(makeup_top20 = top20 / sum(top20), 
         makeup_top100 = top100/ sum(top100)) 

colnames(mo.last2) <- c("Race", "n_t20", "n_t100", 
                       "makeup_t20", "makeup_t100")
mo.last2$which <- name.scenario

library(glue)

print(mo.last2)

# if(between(out.R[out.R$Race == "Black",]$makeup_t100 / 
#            out.R[out.R$Race == "Black",]$makeup_t20  ,0.97,1.07)){
#   stop("we got one - Race")
# }

select(mo.last2, Race, makeup_t20, makeup_t100, which) %>%
  as.data.table() %>%
  melt(., id.vars = c("Race", "which")) %>%
  ggplot(data = ., 
         aes(x = Race, y = value)) + 
  geom_col(position = "dodge", 
           aes(fill = variable))+
  scale_y_continuous(labels = scales::percent, 
                     breaks = seq(0, 100, by = .10), 
                     limits = c(0,1))+
  labs(title = "Vulnerability Ranking Goal Outcomes\nPercentage of Race in Top 20% of Output Ranking",
       subtitle = glue("model fingerprint: {last(mo.last$sim_fp)}")) +
  geom_point(data = data.frame(Race = c("Asian", "Black", 
                                        "Indigenous", 
                                        "Multiple Races", 
                                        "White"), 
                               goal = c(0.013, 0.423, 0.026, 0.013, 0.526)), 
             aes(x = Race, y = goal, color = "Goal"), 
             size = 4)+
  scale_color_manual(values = "black")

# ethnicity = mo.last3
mo.last3 <- mo.last %>%
  group_by(top20 = ifelse(top20 == T,"top20", "top100"),Ethnicty, .drop = F) %>%
  summarise(n = n_distinct(client_id2)) %>%
  ungroup() %>%
  as.data.table() %>%
  dcast(., 
        Ethnicty ~ top20, 
        fill = 0) %>%
  as.data.frame() %>%
  mutate(makeup_top20 = top20 / sum(top20), 
         makeup_top100 = top100/ sum(top100)) 

colnames(mo.last3) <- c("Ethnicty", "n_t20", "n_t100", 
                        "makeup_t20", "makeup_t100")
mo.last3$which <- name.scenario

library(glue)

print(mo.last3)

# if(between(out.R[out.R$Race == "Black",]$makeup_t100 / 
#            out.R[out.R$Race == "Black",]$makeup_t20  ,0.97,1.07)){
#   stop("we got one - Race")
# }

select(mo.last3, Ethnicty, makeup_t20, makeup_t100, which) %>%
  as.data.table() %>%
  melt(., id.vars = c("Ethnicty", "which")) %>%
  ggplot(data = ., 
         aes(x = Ethnicty, y = value)) + 
  geom_col(position = "dodge", 
           aes(fill = variable))+
  scale_y_continuous(labels = scales::percent, 
                     breaks = seq(0, 100, by = .10), 
                     limits = c(0,1))+
  labs(title = "Vulnerability Ranking Goal Outcomes\nPercentage of Ethnicity in Top 20% of Output Ranking",
       subtitle = glue("model fingerprint: {last(mo.last$sim_fp)}")) #+
  # geom_point(data = data.frame(Race = c("Asian", "Black", 
  #                                       "Indigenous", 
  #                                       "Multiple Races", 
  #                                       "White"), 
  #                              goal = c(0.013, 0.423, 0.026, 0.013, 0.526)), 
  #            aes(x = Race, y = goal, color = "Goal"), 
  #            size = 4)+
  #scale_color_manual(values = "black")

override.weights <- F
