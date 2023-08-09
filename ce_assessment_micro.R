library(readr)
library(dplyr)
library(lubridate)
library(data.table)
library(ggplot2)
library(forcats)
library(openssl)
library(glue)

setwd("C:/Users/TimBender/Documents/R/ncceh/projects/ce_assessment")

rm(list=ls());cat('\f')#[!ls() %in% c("ce", 
#             "ce2", 
#            "ov")]);cat('\f')
gc()
# # VI-SPDAT Level of Priority: 
# #* Highest = 15+17 points 
# #* Higher = 11-14 points 
# #* High = 8-10 
# #* Medium = 4-7  
# #* Low = 0-3
#  
# # Goal Testing Criteria: 
# # Top 20% Racial Distribution (43% Black) 
# # Overall Racial Distribution (43% Black)
# 
#  
# #https://ncceh.sharepoint.com/:x:/s/boscoccoordination/EbaXcHJZpX1Dirf1Ql7u_9YB5FTYsxfbI5uEmPHm2Z8zjg?e=e0ORVG
# 

# New Steps----
# 1. vars inputs----
# WEIGHTS
sim.fingerprint <- openssl::md5(as.character(Sys.time())) %>%
  substr(., nchar(.) - 7, nchar(.))

git.raw.md5 <- read_file("https://raw.githubusercontent.com/timbender-ncceh/ncceh_data_tools/main/ce_assessment_micro.R") %>%
  md5
gc()


n_sims <- 1 #2500

n <- 0
while(n < n_sims){
  n <- n + 1
  rm(list=ls()[!ls() %in% c("n", "n_sims", 
                            "sim.fingerprint", 
                            "git.raw.md5")]);cat('\f')#[!ls() %in% c("ce", 
  #             "ce2", 
  #            "ov")]);cat('\f')
  gc()
  sample_choices <- -3:3
  
  # as.data.frame(sum.lm.rat_w2b2$coefficients)[2:19,]$Estimate
  
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
  
  # 1a. # question groups----
  qg <- read_tsv("	Housing and Homeless History
1	How long has it been since you lived in your own place?
2	How many months have you been without a home, such as living outside or in a shelter?
3	Where did you sleep last night?
4	Where are you going to sleep tonight?


	Risks
5	Did you leave your previous or current living situation because you felt unsafe?
6	Have you experienced violence since becoming homeless?
7	Have you ever experienced violence with someone close to you?
7.1	Are you currently experiencing or feel you are at risk of experiencing violence?

	Health and Wellness
8	Does anyone in your household have any physical or mental health conditions that are treated or have been treated by a professional?
9	Do you or does anyone in the household have lung cancer, kidney or liver failure, heart disease, or a substance use disorder?
10	Covered by Health Insurance
11	Is the lack of housing making it hard to get to a doctors office or take prescribed medications?


	Family Unit
12	What is the size of your household? (including you)
13	Is anyone under 5 years old?
14	Is anyone 55 years or older?
15	Is anyone in the household pregnant?
16	How many children under the age of 18 are not currently staying with your family, but would live with you? (if you have a home)
17	How many adults 18 or older are not currently staying with your family, but would live with you? (if you have a home)",
                 col_names = F)
  
  qg2 <- NULL
  temp.group <- qg$X2[1]
  for(i in 1:nrow(qg)){
    if(is.na(qg$X1[i])){
      temp.group <- qg$X2[i]
    }else{
      qg2 <- rbind(qg2,
                   data.frame(question = qg$X2[i],
                              group    = temp.group,
                              orig.ord = qg$X1[i]))
    }
  }
  qg2 <- qg2 %>% as_tibble()
  rm(qg, i, temp.group)
  
  qg2$question
  
  
  # Questions and responses and groups----
  
  
  # # error check 1----
  # if(!all(df.weights$long_name %in% qg2$question &
  #         qg2$question %in% df.weights$long_name)){
  #   stop("ERROR 1: question language does not match between <qg2> and <df.colatts>")
  # }
  
  # 2. read data and tidy----
  # only do this once per startup
  if(!"ce" %in% ls()){
    ce <- read_tsv("Client ID	Household ID	Race	Ethnicty	Gender	Entry Date	Exit Date	Region	Provider	Provider Updating	How long has it been since you lived in your own place?	How many months have you been without a home, such as living outside or in a shelter?	Where did you sleep last night?	Where are you going to sleep tonight?	Are you currently experiencing or feel you are at risk of experiencing violence?	Did you leave your previous or current living situation because you felt unsafe?	Have you ever experienced violence with someone close to you?	Have you experienced violence since becoming homeless?	Does anyone in your household have any physical or mental health conditions that are treated or have been treated by a professional?	Do you or does anyone in the household have lung cancer, kidney or liver failure, heart disease, or a substance use disorder?	Is the lack of housing making it hard to get to a doctors office or take prescribed medications?	Covered by Health Insurance	What is the size of your household? (including you)	Is anyone under 5 years old?	Is anyone 55 years or older?	Is anyone in the household pregnant?	How many children under the age of 18 are not currently staying with your family, but would live with you? (if you have a home)	How many adults 18 or older are not currently staying with your family, but would live with you? (if you have a home)	Note
136715		White	Non-Hispanic	Male	6/20/2023	7/1/2023	R05	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	Less than 3 months	Less than 3 months	Sheltered (ES, TH)	Sheltered (ES, TH)	No	No	Yes	No	No	No	No	No (HUD)	1-2 people	No	No	No	1 or more	None	
285031		Black	Non-Hispanic	Male	6/10/2023	7/1/2023	R05	Rowan Helping Ministries - Rowan County -  Emergency Shelter - ES - Private	Rowan Helping Ministries - Rowan County -  Emergency Shelter - ES - Private	36 months (3 years) or more	36 months (3 years) or more	Sheltered (ES, TH)	Sheltered (ES, TH)	Yes	Yes	No	Yes	Yes	Yes, 1	Yes	Yes (HUD)	1-2 people	No	No	No	None	None	
297854		White	Non-Hispanic	Male	6/23/2023		R05	Rowan Helping Ministries - Rowan County -  Emergency Shelter - ES - Private	Rowan Helping Ministries - Rowan County -  Emergency Shelter - ES - Private	36 months (3 years) or more	36 months (3 years) or more	Sheltered (ES, TH)	Sheltered (ES, TH)	No	No	No	No	No	No	No	No (HUD)	1-2 people	No	No	No	None	None	
320083		White	Non-Hispanic	Female	6/3/2023	6/30/2023	R05	Rowan Helping Ministries - Rowan County -  Emergency Shelter - ES - Private	Rowan Helping Ministries - Rowan County -  Emergency Shelter - ES - Private	Less than 3 months	Less than 3 months	Sheltered (ES, TH)	Sheltered (ES, TH)	No	No	Yes	Yes	Yes	Yes, 2 or more	No	Yes (HUD)	1-2 people	No	Yes	No	None	None	
352302		White	Non-Hispanic	Male	5/5/2023		R05	Crisis Ministry of Davidson County - Davidson County - Emergency Shelter - ES - State ESG CV	Crisis Ministry of Davidson County - Davidson County - Emergency Shelter - ES - State ESG CV	Less than 3 months	12 to 35 months (1-2 years)	Sheltered (ES, TH)	Sheltered (ES, TH)	No	No	No	No	No	No	No	No (HUD)	1-2 people	No	No	No	None	None	
387916		White	Non-Hispanic	Female	6/16/2023		R07	Outreach Mission Inc. - Lee County - Women's Shelter - ES - State ESG	Outreach Mission Inc. - Lee County - Women's Shelter - ES - State ESG	36 months (3 years) or more	Less than 3 months	Sheltered (ES, TH)	Sheltered (ES, TH)	No	Yes	Yes	No	No	No	Yes	Yes (HUD)	1-2 people	No	No	No	1 or more	1 or more	
420172		White	Non-Hispanic	Male	6/3/2023	6/15/2023	R05	Crisis Ministry of Davidson County - Davidson County - Emergency Shelter - ES - State ESG CV	Crisis Ministry of Davidson County - Davidson County - Emergency Shelter - ES - State ESG CV	12 to 35 months (1-2 years)	12 to 35 months (1-2 years)	Sheltered (ES, TH)	Sheltered (ES, TH)	No	No	Yes	No	Yes	Yes, 1	No	Yes (HUD)	1-2 people	No	Yes	No	None	None	
458353		White	Non-Hispanic	Male	6/12/2023		R05	Rowan Helping Ministries - Rowan County -  Emergency Shelter - ES - Private	Rowan Helping Ministries - Rowan County -  Emergency Shelter - ES - Private	36 months (3 years) or more	Less than 3 months	Sheltered (ES, TH)	Sheltered (ES, TH)	No	No	Yes	No	Yes	No	Yes	No (HUD)	1-2 people	No	No	No	1 or more	None	
460330		Native American	Non-Hispanic	Male	6/9/2023		R05	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	3 to 5 months	3 to 5 months	Sheltered (ES, TH)	Sheltered (ES, TH)	No	Yes	Yes	No	Yes	No	No	No (HUD)	1-2 people	No	No	No	1 or more	1 or more	
471926		Black	Non-Hispanic	Male	6/20/2023		R05	Rowan Helping Ministries - Rowan County -  Emergency Shelter - ES - Private	Rowan Helping Ministries - Rowan County -  Emergency Shelter - ES - Private	Less than 3 months	Less than 3 months	Sheltered (ES, TH)	Sheltered (ES, TH)	No	No	Yes	No	No	No	No	No (HUD)	1-2 people	No	No	No	None	None	
474858		Black	Non-Hispanic	Male	6/17/2023	7/6/2023	R05	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	Less than 3 months	Less than 3 months	Sheltered (ES, TH)	Sheltered (ES, TH)	No	No	No	No	No	No	No	No (HUD)	1-2 people	No	Yes	No	None	None	
475377		Black	Non-Hispanic	Male	5/17/2023		R05	Homes of Hope - Stanly County - Stanly Community Inn - ES - State ESG	Homes of Hope - Stanly County - Stanly Community Inn - ES - State ESG	3 to 5 months	Less than 3 months	Sheltered (ES, TH)	Sheltered (ES, TH)	No	No	No	No	Yes	No	No	Yes (HUD)	1-2 people	No	No	No	None	None	Manually entered Family Size questions (single)
481262		White	Non-Hispanic	Male	5/26/2023		R05	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	12 to 35 months (1-2 years)	12 to 35 months (1-2 years)	Sheltered (ES, TH)	Sheltered (ES, TH)	No	No	No	No	Yes	No	No	No (HUD)	1-2 people	No	No	No	None	None	
483917		Black	Non-Hispanic	Male	6/13/2023	6/15/2023	R05	Rowan Helping Ministries - Rowan County -  Emergency Shelter - ES - Private	Rowan Helping Ministries - Rowan County -  Emergency Shelter - ES - Private	12 to 35 months (1-2 years)	Less than 3 months	Sheltered (ES, TH)	Sheltered (ES, TH)	No	No	No	Yes	Yes	Yes, 2 or more	No	Yes (HUD)	1-2 people	No	Yes	No	None	None	
485450		White	Non-Hispanic	Male	5/18/2023		R05	Rowan Helping Ministries - Rowan County -  Emergency Shelter - ES - Private	Rowan Helping Ministries - Rowan County -  Emergency Shelter - ES - Private	36 months (3 years) or more	36 months (3 years) or more	Sheltered (ES, TH)	Sheltered (ES, TH)	No	No	Yes	Yes	Yes	Yes, 2 or more	No	Yes (HUD)	1-2 people	No	No	No	None	None	
492513		Black	Non-Hispanic	Male	6/29/2023		R05	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	36 months (3 years) or more	36 months (3 years) or more	Sheltered (ES, TH)	Sheltered (ES, TH)	No	No	No	No	Yes	Yes, 1	Yes	Yes (HUD)	1-2 people	No	Yes	No	None	1 or more	
497653					7/3/2023		R05	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	36 months (3 years) or more	Less than 3 months	Sheltered (ES, TH)	Sheltered (ES, TH)	No	Yes	Yes	No	Yes	No	Yes	No (HUD)	3 or more people	No	No	No	1 or more	None	
499414		White	Non-Hispanic	Female	5/27/2023		R05	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	36 months (3 years) or more	6 to 11 months	Sheltered (ES, TH)	Sheltered (ES, TH)	No	Yes	Yes	No	Yes	Yes, 1	Yes	Yes (HUD)	1-2 people	No	No	No	None	None	
1000419		White	Non-Hispanic	Male	6/25/2023		R05	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	6 to 11 months	6 to 11 months	Sheltered (ES, TH)	Sheltered (ES, TH)	No	No	No	Yes	Yes	Yes, 1	Yes	No (HUD)	1-2 people	No	Yes	No	None	None	
1004773		Black	Non-Hispanic	Male	6/10/2023		R05	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	36 months (3 years) or more	36 months (3 years) or more	Sheltered (ES, TH)	Sheltered (ES, TH)	Yes	Yes	Yes	Yes	Yes	Yes, 1	Yes	No (HUD)	1-2 people	No	Yes	No	1 or more	None	
1005636		White	Non-Hispanic	Male	6/16/2023		R07	Outreach Mission Inc. - Lee County - Men's Shelter - ES - State ESG	Outreach Mission Inc. - Lee County - Men's Shelter - ES - State ESG	36 months (3 years) or more	12 to 35 months (1-2 years)	Sheltered (ES, TH)	Sheltered (ES, TH)	No	Yes	No	No	No	No	No	No (HUD)	1-2 people	No	Yes	No	None	None	
1007181		Black	Non-Hispanic	Male	6/6/2023		R07	NC Balance of State - Region 7 Coordinated Entry Project	NC Balance of State - Region 7 Coordinated Entry Project	36 months (3 years) or more	36 months (3 years) or more	Unsheltered	Unsheltered	No	No	No	No	No	No	No	No (HUD)	1-2 people	No	No	No	None	None	
1009104		Black	Non-Hispanic	Male	6/2/2023		R05	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	Less than 3 months	Less than 3 months	Sheltered (ES, TH)	Sheltered (ES, TH)	No	No	Yes	No	Yes	No	No	Yes (HUD)	1-2 people	No	No	No	1 or more	None	
1009332		White	Non-Hispanic	Male	5/25/2023	6/2/2023	R05	Crisis Ministry of Davidson County - Davidson County - Emergency Shelter - ES - State ESG CV	Crisis Ministry of Davidson County - Davidson County - Emergency Shelter - ES - State ESG CV	12 to 35 months (1-2 years)	6 to 11 months	Sheltered (ES, TH)	Sheltered (ES, TH)	No	No	No	No	No	No	No	No (HUD)	1-2 people	No	No	No	None	None	
1011517		White	Non-Hispanic	Male	5/19/2023		R05	Crisis Ministry of Davidson County - Davidson County - Emergency Shelter - ES - State ESG CV	Crisis Ministry of Davidson County - Davidson County - Emergency Shelter - ES - State ESG CV	36 months (3 years) or more	3 to 5 months	Sheltered (ES, TH)	Sheltered (ES, TH)	No	No	No	No	No	No	No	No (HUD)	1-2 people	No	No	No	None	1 or more	
1013834		White	Non-Hispanic	Male	5/10/2023	6/12/2023	R05	Rowan Helping Ministries - Rowan County -  Emergency Shelter - ES - Private	Rowan Helping Ministries - Rowan County -  Emergency Shelter - ES - Private	12 to 35 months (1-2 years)	12 to 35 months (1-2 years)	Sheltered (ES, TH)	Sheltered (ES, TH)	No	Yes	Yes	Yes	No	No	Yes	No (HUD)	1-2 people	No	No	No	None	None	
1015529		Black	Non-Hispanic	Male	5/24/2023	7/3/2023	R05	Rowan Helping Ministries - Rowan County -  Emergency Shelter - ES - Private	Rowan Helping Ministries - Rowan County -  Emergency Shelter - ES - Private	6 to 11 months	6 to 11 months	Sheltered (ES, TH)	Sheltered (ES, TH)	No	Yes	No	Yes	Yes	No	Yes	Yes (HUD)	1-2 people	No	Yes	No	None	None	
1015639		White	Non-Hispanic	Female	6/8/2023		R07	NC Balance of State - Region 7 Coordinated Entry Project	NC Balance of State - Region 7 Coordinated Entry Project	36 months (3 years) or more	36 months (3 years) or more	Sheltered (ES, TH)	Sheltered (ES, TH)	No	No	Yes	No	No	No	No	Yes (HUD)	1-2 people	No	No	No	None	None	
1016977	140789	White	Non-Hispanic	Female	5/5/2023		R05	Crisis Ministry of Davidson County - Davidson County - Emergency Shelter - ES - State ESG CV	Crisis Ministry of Davidson County - Davidson County - Emergency Shelter - ES - State ESG CV	6 to 11 months	6 to 11 months	Sheltered (ES, TH)	Sheltered (ES, TH)	No	No	Yes	No	Yes	No	Yes	Yes (HUD)	3 or more people	Yes	No	No	None	None	
1017025		White	Non-Hispanic	Female	6/22/2023		R05	Crisis Ministry of Davidson County - Davidson County - Emergency Shelter - ES - State ESG CV	Crisis Ministry of Davidson County - Davidson County - Emergency Shelter - ES - State ESG CV	36 months (3 years) or more	36 months (3 years) or more	Sheltered (ES, TH)	Sheltered (ES, TH)	No	No	Yes	No	Yes	No	No	No (HUD)	1-2 people	No	No	No	None	None	
1020242		Black	Non-Hispanic	Male	6/13/2023		R05	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	12 to 35 months (1-2 years)	12 to 35 months (1-2 years)	Sheltered (ES, TH)	Sheltered (ES, TH)	No	No	No	No	Yes	No	Yes	Yes (HUD)	1-2 people	No	Yes	No	None	None	
1022349		Black	Non-Hispanic	Female	5/10/2023		R05	Rowan Helping Ministries - Rowan County -  Emergency Shelter - ES - Private	Rowan Helping Ministries - Rowan County -  Emergency Shelter - ES - Private	12 to 35 months (1-2 years)	12 to 35 months (1-2 years)	Sheltered (ES, TH)	Sheltered (ES, TH)	No	Yes	Yes	No	No	No	No	No (HUD)	1-2 people	No	No	No	None	None	
1029115		Asian	Non-Hispanic	Female	5/13/2023		R05	NC Balance of State - Piedmont (Region 5) Coordinated Entry Project	NC Balance of State - Piedmont (Region 5) Coordinated Entry Project	12 to 35 months (1-2 years)	12 to 35 months (1-2 years)	Sheltered (ES, TH)	Sheltered (ES, TH)	No	No	No	No	No	No	No	Yes (HUD)	1-2 people	No	No	Yes	None	None	
1029786		White	Non-Hispanic	Male	5/19/2023		R05	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	36 months (3 years) or more	Less than 3 months	Sheltered (ES, TH)	Sheltered (ES, TH)	Yes	No	Yes	No	Yes	No	Yes	Yes (HUD)	1-2 people	No	No	No	None	None	
1030477	137091	White	Non-Hispanic	Male	6/11/2023		R05	Union County Community Shelter - Union County - Family Shelter - ES - State ESG	Union County Community Shelter - Union County - Family Shelter - ES - State ESG	36 months (3 years) or more	36 months (3 years) or more	Sheltered (ES, TH)	Sheltered (ES, TH)	No	No	No	No	No	No	No	No (HUD)	3 or more people	Yes	No	No	1 or more	None	
1032375		Black	Non-Hispanic	Male	5/27/2023		R05	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	36 months (3 years) or more	36 months (3 years) or more	Sheltered (ES, TH)	Sheltered (ES, TH)	No	No	No	No	Yes	No	No	No (HUD)	1-2 people	No	No	No	None	1 or more	
1033239		Black	Non-Hispanic	Male	5/18/2023		R05	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	36 months (3 years) or more	36 months (3 years) or more	Sheltered (ES, TH)	Sheltered (ES, TH)	No	No	Yes	Yes	Yes	No	Yes	No (HUD)	1-2 people	No	No	No	None	None	
1033654		White	Non-Hispanic	Male	6/27/2023		R07	NC Balance of State - Region 7 Coordinated Entry Project	NC Balance of State - Region 7 Coordinated Entry Project	36 months (3 years) or more	36 months (3 years) or more	Unsheltered	Unsheltered	No	No	Yes	Yes	Yes	Yes, 2 or more	Yes	Yes (HUD)	1-2 people	No	No	No	None	None	
1034289		White	Non-Hispanic	Female	6/26/2023		R05	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	36 months (3 years) or more	36 months (3 years) or more	Sheltered (ES, TH)	Sheltered (ES, TH)	Yes	Yes	Yes	Yes	Yes	Yes, 2 or more	Yes	Yes (HUD)	3 or more people	No	No	No	1 or more	1 or more	
1036385		White	Non-Hispanic	Male	6/9/2023		R05	Crisis Ministry of Davidson County - Davidson County - Emergency Shelter - ES - State ESG CV	Crisis Ministry of Davidson County - Davidson County - Emergency Shelter - ES - State ESG CV	12 to 35 months (1-2 years)	12 to 35 months (1-2 years)	Sheltered (ES, TH)	Sheltered (ES, TH)	No	Yes	Yes	Yes	No	No	Yes	No (HUD)	1-2 people	No	Yes	No	None	None	
1036756		White	Non-Hispanic	Male	5/22/2023	6/20/2023	R05	Crisis Ministry of Davidson County - Davidson County - Emergency Shelter - ES - State ESG CV	Crisis Ministry of Davidson County - Davidson County - Emergency Shelter - ES - State ESG CV	36 months (3 years) or more	12 to 35 months (1-2 years)	Sheltered (ES, TH)	Sheltered (ES, TH)	No	No	No	No	Yes	Yes, 1	Yes	Data not collected (HUD)	1-2 people	No	Yes	No	None	None	
1037076		Black	Non-Hispanic	Male	5/20/2023		R05	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	12 to 35 months (1-2 years)	12 to 35 months (1-2 years)	Sheltered (ES, TH)	Sheltered (ES, TH)	No	Yes	No	No	Yes	No	No	Yes (HUD)	1-2 people	No	No	No	None	None	
1037329		Black	Non-Hispanic	Male	6/4/2023	6/22/2023	R05	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	36 months (3 years) or more	Less than 3 months	Sheltered (ES, TH)	Sheltered (ES, TH)	No	No	Yes	No	Yes	No	No	No (HUD)	1-2 people	No	No	No	1 or more	1 or more	
1038154		White	Non-Hispanic	Male	6/12/2023	6/21/2023	R05	Crisis Ministry of Davidson County - Davidson County - Emergency Shelter - ES - State ESG CV	Crisis Ministry of Davidson County - Davidson County - Emergency Shelter - ES - State ESG CV	12 to 35 months (1-2 years)	12 to 35 months (1-2 years)	Sheltered (ES, TH)	Sheltered (ES, TH)	No	No	Yes	No	Yes	Yes, 1	Yes	Yes (HUD)	1-2 people	No	No	No	None	None	
1039918		White	Non-Hispanic	Male	5/24/2023		R07	Outreach Mission Inc. - Lee County - Men's Shelter - ES - State ESG	Outreach Mission Inc. - Lee County - Men's Shelter - ES - State ESG	36 months (3 years) or more	3 to 5 months	Sheltered (ES, TH)	Sheltered (ES, TH)	Yes	Yes	Yes	Yes	Yes	No	Yes	No (HUD)	1-2 people	No	No	No	None	None	
1041408		White	Hispanic	Female	6/6/2023	6/23/2023	R05	Crisis Ministry of Davidson County - Davidson County - Emergency Shelter - ES - State ESG CV	Crisis Ministry of Davidson County - Davidson County - Emergency Shelter - ES - State ESG CV	12 to 35 months (1-2 years)	12 to 35 months (1-2 years)	Sheltered (ES, TH)	Sheltered (ES, TH)	No	Yes	Yes	Yes	Yes	No	Yes	No (HUD)	1-2 people	No	No	No	1 or more	None	
1041711		White	Non-Hispanic	Male	5/8/2023		R05	NC Balance of State - Piedmont (Region 5) Coordinated Entry Project	NC Balance of State - Piedmont (Region 5) Coordinated Entry Project	3 to 5 months	3 to 5 months	Sheltered (ES, TH)	Sheltered (ES, TH)	No	No	Yes	No	No	No	Yes	No (HUD)	1-2 people	No	No	No	None	None	
1041788		White	Non-Hispanic	Male	5/9/2023		R05	NC Balance of State - Piedmont (Region 5) Coordinated Entry Project	NC Balance of State - Piedmont (Region 5) Coordinated Entry Project	12 to 35 months (1-2 years)	12 to 35 months (1-2 years)	Sheltered (ES, TH)	Sheltered (ES, TH)	No	No	Yes	No	No	No	No	No (HUD)	1-2 people	No	No	No	1 or more	None	
1041930		White	Non-Hispanic	Female	5/11/2023		R05	NC Balance of State - Piedmont (Region 5) Coordinated Entry Project	NC Balance of State - Piedmont (Region 5) Coordinated Entry Project	3 to 5 months	3 to 5 months	Sheltered (ES, TH)	Sheltered (ES, TH)	No	Yes	Yes	No	No	No	Yes	Yes (HUD)	3 or more people	Yes	No	No	None	None	
1042423		White	Non-Hispanic	Male	5/19/2023	6/29/2023	R05	Rowan Helping Ministries - Rowan County -  Emergency Shelter - ES - Private	Rowan Helping Ministries - Rowan County -  Emergency Shelter - ES - Private	6 to 11 months	6 to 11 months	Sheltered (ES, TH)	Sheltered (ES, TH)	No	No	Yes	No	Yes	Yes, 1	Yes	No (HUD)	1-2 people	Yes	No	No	1 or more	None	
1042501		Black	Non-Hispanic	Male	5/23/2023		R05	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	Less than 3 months	Less than 3 months	Sheltered (ES, TH)	Sheltered (ES, TH)	No	No	No	No	Yes	No	No	Yes (HUD)	1-2 people	No	Yes	No	None	None	
1042502	140924	Black	Non-Hispanic	Female	5/23/2023		R05	Union County Community Shelter - Union County - Family Shelter - ES - State ESG	Union County Community Shelter - Union County - Family Shelter - ES - State ESG	36 months (3 years) or more	36 months (3 years) or more	Sheltered (ES, TH)	Sheltered (ES, TH)	No	No	No	No	No	No	No	Yes (HUD)	3 or more people	Yes	No	No	None	1 or more	
1042525	140945	Black	Non-Hispanic	Female	6/13/2023		R07	NC Balance of State - Region 7 Coordinated Entry Project	NC Balance of State - Region 7 Coordinated Entry Project	3 to 5 months	3 to 5 months	Sheltered (ES, TH)	Sheltered (ES, TH)	No	No	No	No	No	Yes, 1	No	Yes (HUD)	1-2 people	No	Yes	No	None	None	
1042530		Multiple Races	Non-Hispanic	Female	5/24/2023		R07	Outreach Mission Inc. - Lee County - Women's Shelter - ES - State ESG	Outreach Mission Inc. - Lee County - Women's Shelter - ES - State ESG	3 to 5 months	Less than 3 months	Sheltered (ES, TH)	Sheltered (ES, TH)	No	No	Yes	No	Yes	No	Yes	Yes (HUD)	1-2 people	No	No	No	1 or more	1 or more	
1042559	140945	Black	Non-Hispanic	Male	6/13/2023		R07	NC Balance of State - Region 7 Coordinated Entry Project	NC Balance of State - Region 7 Coordinated Entry Project	3 to 5 months	3 to 5 months	Sheltered (ES, TH)	Sheltered (ES, TH)	No	No	No	No	No	Yes, 1	No	No (HUD)	1-2 people	No	No	No	None	None	
1042589		Black	Non-Hispanic	Female	5/25/2023		R05	Union County Community Shelter - Union County - Family Shelter - ES - State ESG	Union County Community Shelter - Union County - Family Shelter - ES - State ESG	12 to 35 months (1-2 years)	12 to 35 months (1-2 years)	Sheltered (ES, TH)	Sheltered (ES, TH)	No	No	No	No	No	No	No	Yes (HUD)	3 or more people	Yes	No	Yes	None	None	
1042605		White	Non-Hispanic	Male	5/25/2023		R05	Rowan Helping Ministries - Rowan County -  Emergency Shelter - ES - Private	Rowan Helping Ministries - Rowan County -  Emergency Shelter - ES - Private	12 to 35 months (1-2 years)	12 to 35 months (1-2 years)	Sheltered (ES, TH)	Sheltered (ES, TH)	No	No	No	No	Yes	No	Yes	Yes (HUD)	1-2 people	No	No	No	None	None	
1042611		Black	Non-Hispanic	Male	5/29/2023		R07	Outreach Mission Inc. - Lee County - Men's Shelter - ES - State ESG	Outreach Mission Inc. - Lee County - Men's Shelter - ES - State ESG	12 to 35 months (1-2 years)	Less than 3 months	Sheltered (ES, TH)	Sheltered (ES, TH)	No	No	Yes	No	No	No	No	Yes (HUD)	1-2 people	No	No	No	None	None	
1042616		Black	Non-Hispanic	Male	5/26/2023		R05	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	36 months (3 years) or more	Less than 3 months	Sheltered (ES, TH)	Sheltered (ES, TH)	No	No	No	No	Yes	No	Yes	No (HUD)	1-2 people	No	No	No	None	1 or more	
1042617		White	Non-Hispanic	Male	5/29/2023	6/13/2023	R05	Crisis Ministry of Davidson County - Davidson County - Emergency Shelter - ES - State ESG CV	Crisis Ministry of Davidson County - Davidson County - Emergency Shelter - ES - State ESG CV	36 months (3 years) or more	6 to 11 months	Sheltered (ES, TH)	Sheltered (ES, TH)	No	Yes	Yes	No	No	No	Yes	No (HUD)	1-2 people	No	No	No	None	None	
1042690	140991	White	Non-Hispanic	Female	5/31/2023		R07	Outreach Mission Inc. - Lee County - Women's Shelter - ES - State ESG	Outreach Mission Inc. - Lee County - Women's Shelter - ES - State ESG	6 to 11 months	6 to 11 months	Sheltered (ES, TH)	Sheltered (ES, TH)	No	Yes	Yes	No	No	No	No	Yes (HUD)	1-2 people	Yes	No	No	None	None	
1042705		Black	Non-Hispanic	Female	5/31/2023		R05	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	3 to 5 months	3 to 5 months	Sheltered (ES, TH)	Sheltered (ES, TH)	Yes	No	No	No	Yes	No	No	Yes (HUD)	1-2 people	No	No	No	None	None	
1042706		Native American	Non-Hispanic	Female	5/31/2023		R05	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	3 to 5 months	Less than 3 months	Sheltered (ES, TH)	Sheltered (ES, TH)	No	Yes	No	No	Yes	No	No	Yes (HUD)	1-2 people	No	Yes	No	None	None	
1042743		White	Non-Hispanic	Female	6/1/2023	6/24/2023	R05	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	3 to 5 months	Less than 3 months	Sheltered (ES, TH)	Sheltered (ES, TH)	No	No	No	No	Yes	No	Yes	No (HUD)	1-2 people	No	No	No	None	None	
1042774		Black	Non-Hispanic	Female	6/3/2023	6/28/2023	R05	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	12 to 35 months (1-2 years)	Less than 3 months	Sheltered (ES, TH)	Sheltered (ES, TH)	No	Yes	No	No	No	No	No	Data not collected (HUD)	1-2 people	No	No	No	None	None	
1042775	141011	Black	Non-Hispanic	Female	6/2/2023		R05	Union County Community Shelter - Union County - Family Shelter - ES - State ESG	Union County Community Shelter - Union County - Family Shelter - ES - State ESG	3 to 5 months	3 to 5 months	Sheltered (ES, TH)	Sheltered (ES, TH)	No	No	No	No	Yes	No	Yes	No (HUD)	3 or more people	Yes	No	No	None	None	
1042803		White	Non-Hispanic	Female	6/5/2023		R05	Crisis Ministry of Davidson County - Davidson County - Emergency Shelter - ES - State ESG CV	Crisis Ministry of Davidson County - Davidson County - Emergency Shelter - ES - State ESG CV	12 to 35 months (1-2 years)	Less than 3 months	Sheltered (ES, TH)	Sheltered (ES, TH)	No	No	No	No	No	No	No	No (HUD)	1-2 people	No	Yes	No	None	None	
1042843	141033	White	Non-Hispanic	Female	6/6/2023	6/28/2023	R05	Union County Community Shelter - Union County - Family Shelter - ES - State ESG	Union County Community Shelter - Union County - Family Shelter - ES - State ESG	3 to 5 months	3 to 5 months	Sheltered (ES, TH)	Sheltered (ES, TH)	No	No	No	No	Yes	No	No	Yes (HUD)	1-2 people	No	No	No	None	None	
1042878		Black	Non-Hispanic	Male	6/3/2023		R07	Outreach Mission Inc. - Lee County - Men's Shelter - ES - State ESG	Outreach Mission Inc. - Lee County - Men's Shelter - ES - State ESG	12 to 35 months (1-2 years)	12 to 35 months (1-2 years)	Sheltered (ES, TH)	Unsheltered	No	No	No	No	No	No	No	No (HUD)	1-2 people	No	No	No	1 or more	1 or more	
1042926	141063	Black	Non-Hispanic	Male	6/8/2023		R07	NC Balance of State - Region 7 Coordinated Entry Project	NC Balance of State - Region 7 Coordinated Entry Project	12 to 35 months (1-2 years)	12 to 35 months (1-2 years)	Unsheltered	Unsheltered	No	No	No	No	Yes	No	Yes	Yes (HUD)	1-2 people	No	No	No	1 or more	1 or more	
1042927	141063	Black	Non-Hispanic	Female	6/8/2023		R07	NC Balance of State - Region 7 Coordinated Entry Project	NC Balance of State - Region 7 Coordinated Entry Project	12 to 35 months (1-2 years)	12 to 35 months (1-2 years)	Unsheltered	Unsheltered	No	No	No	No	No	No	No	Yes (HUD)	1-2 people	No	No	No	1 or more	1 or more	
1042935		Black	Non-Hispanic	Male	6/8/2023	6/16/2023	R07	Outreach Mission Inc. - Lee County - Men's Shelter - ES - State ESG	Outreach Mission Inc. - Lee County - Men's Shelter - ES - State ESG	Less than 3 months	3 to 5 months	Unsheltered	Unsheltered	No	No	No	No	No	No	No	No (HUD)	1-2 people	No	Yes	No	None	None	
1042951		White	Non-Hispanic	Male	6/8/2023		R05	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	36 months (3 years) or more	36 months (3 years) or more	Sheltered (ES, TH)	Sheltered (ES, TH)	Yes	Yes	Yes	Yes	Yes	No	Yes	No (HUD)	1-2 people	No	No	No	None	None	
1042980	141099	Asian	Non-Hispanic	Female	6/9/2023		R05	Rowan Helping Ministries - Rowan County -  Emergency Shelter - ES - Private	Rowan Helping Ministries - Rowan County -  Emergency Shelter - ES - Private	6 to 11 months	6 to 11 months	Sheltered (ES, TH)	Sheltered (ES, TH)	No	Yes	Yes	Yes	Yes	No	No	Yes (HUD)	1-2 people	Yes	No	No	1 or more	None	
1042986		Black	Non-Hispanic	Male	6/12/2023		R07	NC Balance of State - Region 7 Coordinated Entry Project	NC Balance of State - Region 7 Coordinated Entry Project	36 months (3 years) or more	36 months (3 years) or more	Unsheltered	Unsheltered	No	Yes	Yes	No	Yes	No	Yes	Yes (HUD)	1-2 people	No	No	No	None	None	
1042998		White	Non-Hispanic	Male	6/12/2023		R07	NC Balance of State - Region 7 Coordinated Entry Project	NC Balance of State - Region 7 Coordinated Entry Project	6 to 11 months	6 to 11 months	Unsheltered	Unsheltered	No	No	No	No	No	No	No	No (HUD)	1-2 people	No	No	No	None	None	
1043012		Black	Non-Hispanic	Male	6/12/2023		R05	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	Less than 3 months	Less than 3 months	Sheltered (ES, TH)	Sheltered (ES, TH)	No	No	No	No	No	No	No	No (HUD)	1-2 people	No	No	No	None	None	
1043047		White	Non-Hispanic	Male	6/13/2023		R05	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	36 months (3 years) or more	36 months (3 years) or more	Sheltered (ES, TH)	Sheltered (ES, TH)	No	Yes	No	No	Yes	Yes, 1	Yes	Yes (HUD)	1-2 people	No	Yes	No	None	None	
1043089		White	Non-Hispanic	Male	6/15/2023	7/5/2023	R07	Outreach Mission Inc. - Lee County - Men's Shelter - ES - State ESG	Outreach Mission Inc. - Lee County - Men's Shelter - ES - State ESG	3 to 5 months	Less than 3 months	Unsheltered	Sheltered (ES, TH)	Yes	No	No	No	Yes	No	No	Yes (HUD)	1-2 people	No	No	No	None	None	
1043117		White	Non-Hispanic	Female	6/15/2023		R05	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	12 to 35 months (1-2 years)	Less than 3 months	Sheltered (ES, TH)	Sheltered (ES, TH)	No	No	Yes	No	Yes	No	Yes	No (HUD)	1-2 people	No	No	No	None	None	
1043135	141205	Black	Non-Hispanic	Female	6/16/2023		R05	Crisis Ministry of Davidson County - Davidson County - Emergency Shelter - ES - State ESG CV	Crisis Ministry of Davidson County - Davidson County - Emergency Shelter - ES - State ESG CV	Less than 3 months	Less than 3 months	Sheltered (ES, TH)	Sheltered (ES, TH)	No	No	Yes	No	No	No	No	Yes (HUD)	3 or more people	Yes	No	No	1 or more	None	
1043160		White	Non-Hispanic	Female	6/14/2023		R05	Rowan Helping Ministries - Rowan County -  Emergency Shelter - ES - Private	Rowan Helping Ministries - Rowan County -  Emergency Shelter - ES - Private	Less than 3 months	Less than 3 months	Sheltered (ES, TH)	Sheltered (ES, TH)	No	No	No	No	No	Yes, 2 or more	Yes	Yes (HUD)	1-2 people	No	No	No	None	None	
1043164		White	Non-Hispanic	Male	6/16/2023		R05	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	12 to 35 months (1-2 years)	12 to 35 months (1-2 years)	Sheltered (ES, TH)	Sheltered (ES, TH)	No	No	Yes	Yes	Yes	Yes, 1	No	No (HUD)	1-2 people	Yes	No	No	1 or more	None	
1043165		White	Non-Hispanic	Male	6/16/2023		R05	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	36 months (3 years) or more	36 months (3 years) or more	Sheltered (ES, TH)	Sheltered (ES, TH)	No	No	No	No	Yes	No	No	No (HUD)	1-2 people	No	No	No	None	None	
1043193		Asian	Non-Hispanic	Male	6/20/2023		R05	Crisis Ministry of Davidson County - Davidson County - Emergency Shelter - ES - State ESG CV	Crisis Ministry of Davidson County - Davidson County - Emergency Shelter - ES - State ESG CV	36 months (3 years) or more	Less than 3 months	Sheltered (ES, TH)	Sheltered (ES, TH)	Yes	Yes	Yes	Yes	Yes	No	No	No (HUD)	1-2 people	No	No	No	None	None	
1043274		White	Non-Hispanic	Female	6/21/2023		R05	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	36 months (3 years) or more	12 to 35 months (1-2 years)	Sheltered (ES, TH)	Sheltered (ES, TH)	No	Yes	Yes	Yes	No	No	Yes	No (HUD)	1-2 people	No	No	No	1 or more	None	
1043327		White	Non-Hispanic	Male	6/20/2023		R05	Rowan Helping Ministries - Rowan County -  Emergency Shelter - ES - Private	Rowan Helping Ministries - Rowan County -  Emergency Shelter - ES - Private	36 months (3 years) or more	36 months (3 years) or more	Sheltered (ES, TH)	Sheltered (ES, TH)	No	No	Yes	No	Yes	Yes, 2 or more	Yes	Yes (HUD)	1-2 people	No	No	No	None	None	
1043328		Black	Hispanic	Female	6/20/2023		R05	Rowan Helping Ministries - Rowan County -  Emergency Shelter - ES - Private	Rowan Helping Ministries - Rowan County -  Emergency Shelter - ES - Private	36 months (3 years) or more	36 months (3 years) or more	Sheltered (ES, TH)	Sheltered (ES, TH)	No	No	No	No	Yes	Yes, 2 or more	No	Yes (HUD)	1-2 people	No	No	No	None	None	
1043332		White	Hispanic	Male	6/22/2023		R05	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	3 to 5 months	3 to 5 months	Sheltered (ES, TH)	Sheltered (ES, TH)	No	No	No	Yes	No	No	Yes	No (HUD)	1-2 people	No	No	No	None	None	
1043333		White	Non-Hispanic	Female	6/25/2023		R05	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	3 to 5 months	3 to 5 months	Sheltered (ES, TH)	Sheltered (ES, TH)	No	No	Yes	No	Yes	No	No	Yes (HUD)	1-2 people	No	Yes	No	None	1 or more	
1043357		Black	Non-Hispanic	Female	6/23/2023		R07	NC Balance of State - Region 7 Coordinated Entry Project	NC Balance of State - Region 7 Coordinated Entry Project	3 to 5 months	3 to 5 months	Unsheltered	Unsheltered	No	No	No	No	Yes	Yes, 1	Yes	Yes (HUD)	1-2 people	No	Yes	No	None	None	
1043358		Black	Non-Hispanic	Male	6/26/2023		R05	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	Less than 3 months	Less than 3 months	Sheltered (ES, TH)	Sheltered (ES, TH)	No	No	No	No	No	No	No	No (HUD)	1-2 people	No	No	No	None	None	
1043375		White	Non-Hispanic	Male	6/26/2023		R05	Crisis Ministry of Davidson County - Davidson County - Emergency Shelter - ES - State ESG CV	Crisis Ministry of Davidson County - Davidson County - Emergency Shelter - ES - State ESG CV	36 months (3 years) or more	3 to 5 months	Sheltered (ES, TH)	Sheltered (ES, TH)	No	No	No	No	Yes	Yes, 1	Yes	No (HUD)	1-2 people	No	No	No	1 or more	1 or more	
1043474		White	Hispanic	Male	6/30/2023		R07	NC Balance of State - Region 7 Coordinated Entry Project	NC Balance of State - Region 7 Coordinated Entry Project	12 to 35 months (1-2 years)	3 to 5 months	Unsheltered	Unsheltered	No	No	No	No	Yes	Yes, 2 or more	Yes	Yes (HUD)	1-2 people	No	Yes	No	None	None	
1043525					7/1/2023		R05	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	12 to 35 months (1-2 years)	3 to 5 months	Sheltered (ES, TH)	Sheltered (ES, TH)	No	No	No	No	Yes	No	No	Yes (HUD)	3 or more people	No	No	No	1 or more	None	
1043536					7/3/2023		R05	Rowan Helping Ministries - Rowan County -  Emergency Shelter - ES - Private	Rowan Helping Ministries - Rowan County -  Emergency Shelter - ES - Private	6 to 11 months	6 to 11 months	Sheltered (ES, TH)	Sheltered (ES, TH)	No	Yes	Yes	No	No	Yes, 1	Yes	Yes (HUD)	1-2 people	No	No	No	None	None	
1043641	141355				7/6/2023		R05	Union County Community Shelter - Union County - Family Shelter - ES - State ESG	Union County Community Shelter - Union County - Family Shelter - ES - State ESG	3 to 5 months	3 to 5 months	Sheltered (ES, TH)	Sheltered (ES, TH)	Yes	No	No	No	Yes	Yes, 1	No	Yes (HUD)	3 or more people	No	No	No	1 or more	None	", 
                   #col_types = paste(c("d", "d", rep("f", 3), "c", "c", rep("f", 21), "c"), sep = "", collapse = ""))
    )
    
    ce$rid <- 1:nrow(ce)
    ce$`Entry Date` <- mdy(ce$`Entry Date`)
    ce$`Exit Date`  <- mdy(ce$`Exit Date`)
    
    # Treat NAs
    for(i in c(11:(ncol(ce)-2))){
      ce[,i] <- ifelse(is.na(unname(unlist(ce[,i]))), 
                       "na", 
                       unname(unlist(ce[,i])))
    }
  }
  
  
  # do this only 1 per session
  if(!"ce2" %in% ls()){
    ce2 <- ce %>% 
      as.data.table() %>%
      melt(., 
           id.vars = c("Client ID", 
                       "Household ID", 
                       "Race", "Ethnicty", "Gender", 
                       "Entry Date", "Exit Date", 
                       "Region", "Provider", 
                       "Provider Updating", 
                       "rid", "Note"), 
           value.name = "response", 
           variable.name = "question") %>%
      as.data.frame() %>%
      as_tibble()
  }
  
  
  
  # 3. assign number to responses----
  # order vulnerability----
  
  
  # do this only once per session
  if(!"ov" %in% ls()){
    ov <- read_tsv("How long has it been since you lived in your own place?	order_vuln
12 to 35 months (1-2 years)	3
3 to 5 months	1
36 months (3 years) or more	4
6 to 11 months	2
Less than 3 months	0
	
How many months have you been without a home, such as living outside or in a shelter?	order_vuln
12 to 35 months (1-2 years)	3
3 to 5 months	1
36 months (3 years) or more	4
6 to 11 months	2
Less than 3 months	0
	
Where did you sleep last night?	order_vuln
Sheltered (ES, TH)	1
Unsheltered	0
	
Where are you going to sleep tonight?	order_vuln
Sheltered (ES, TH)	1
Unsheltered	0
	
Are you currently experiencing or feel you are at risk of experiencing violence?	order_vuln
No	0
Yes	1
	
Did you leave your previous or current living situation because you felt unsafe?	order_vuln
No	0
Yes	1
	
Have you ever experienced violence with someone close to you?	order_vuln
No	0
Yes	1
	
Have you experienced violence since becoming homeless?	order_vuln
No	0
Yes	1
Does anyone in your household have any physical or mental health conditions that are treated or have been treated by a professional?	order_vuln
No	0
Yes	1
Do you or does anyone in the household have lung cancer, kidney or liver failure, heart disease, or a substance use disorder?	order_vuln
No	0
Yes, 1	1
Yes, 2 or more	2
Is the lack of housing making it hard to get to a doctors office or take prescribed medications?	order_vuln
No	0
Yes	1
Covered by Health Insurance	order_vuln
Data not collected (HUD)	0
No (HUD)	1
Yes (HUD)	0
What is the size of your household? (including you)	order_vuln
1-2 people	0
3 or more people	1
Is anyone under 5 years old?	order_vuln
No	0
Yes	1
Is anyone 55 years or older?	order_vuln
No	0
Yes	1
Is anyone in the household pregnant?	order_vuln
No	0
Yes	1
How many children under the age of 18 are not currently staying with your family, but would live with you? (if you have a home)	order_vuln
1 or more	1
None	0
How many adults 18 or older are not currently staying with your family, but would live with you? (if you have a home)	order_vuln
1 or more	1
None	0", col_names = F,
                   skip_empty_rows = T)
    
    ov$question <- NA
    temp.q <- NA
    for(i in 1:nrow(ov)){
      if(ov$X2[i] == "order_vuln"){
        temp.q <- ov$X1[i]
      }
      ov$question[i] <- temp.q
    }
    rm(temp.q,i)
    
    ov <- ov[ov$X2 != "order_vuln",]
    colnames(ov)[1:2] <- c("response", "order_vuln")
    
    ov$order_vuln <- as.numeric(ov$order_vuln)
    ov <- ov[order(ov$question, ov$order_vuln),]
    
    ov <- rbind(ov,data.frame(question = unique(ov$question), 
                              response = NA, 
                              order_vuln = -1)) %>%
      .[order(.$question, .$order_vuln),] %>%
      mutate(., 
             t_order = 1:length(response), 
             order_vuln = ifelse(order_vuln == -1, 0, order_vuln))
    
    ov$response[is.na(ov$response)] <- "na"
    
    # ov %>%
    #   group_by(response, order_vuln) %>%
    #   summarise(n_ov = n_distinct(order_vuln))
    
    ov$order_vuln <- ov$order_vuln + 1
    
    # ov %>%
    #   group_by(question) %>%
    #   summarise(n_d = n_distinct(order_vuln), 
    #             min_v = min(order_vuln), 
    #             max_v = max(order_vuln)) %>%
    #   mutate(., 
    #          check = n_d == (max_v - min_v +1))
    
    # 4. normalize responses----
    ov <- group_by(ov, question) %>%
      mutate(., 
             pct_v = order_vuln / max(order_vuln)) %>%
      ungroup() %>%
      .[,c("t_order", "question", "response", 
           "order_vuln", "pct_v")]
    
    ce2 <- left_join(ce2, ov)
  }
  
  
  
  # 5. weigh normalized responses----
  df.weights <- data.frame(long_name = c("How long has it been since you lived in your own place?" ,                                                                            
                                         "How many months have you been without a home, such as living outside or in a shelter?" ,                                              
                                         "Where did you sleep last night?",                                                                                                     
                                         "Where are you going to sleep tonight?",                                                                                               
                                         "Are you currently experiencing or feel you are at risk of experiencing violence?",                                                    
                                         "Did you leave your previous or current living situation because you felt unsafe?",                                                    
                                         "Have you ever experienced violence with someone close to you?",                                                                       
                                         "Have you experienced violence since becoming homeless?",                                                                              
                                         "Does anyone in your household have any physical or mental health conditions that are treated or have been treated by a professional?",
                                         "Do you or does anyone in the household have lung cancer, kidney or liver failure, heart disease, or a substance use disorder?",       
                                         "Is the lack of housing making it hard to get to a doctors office or take prescribed medications?",                                   
                                         "Covered by Health Insurance",                                                                                                         
                                         "What is the size of your household? (including you)" ,                                                                                
                                         "Is anyone under 5 years old?" ,                                                                                                       
                                         "Is anyone 55 years or older?",                                                                                                        
                                         "Is anyone in the household pregnant?" ,                                                                                               
                                         "How many children under the age of 18 are not currently staying with your family, but would live with you? (if you have a home)",     
                                         "How many adults 18 or older are not currently staying with your family, but would live with you? (if you have a home)"),
                           short_name = c("month_since_own_home" ,                                                                            
                                          "months_since_any_home" ,                                              
                                          "loc_sleep_last_night",                                                                                                     
                                          "loc_sleep_tonight",                                                                                               
                                          "now_or_at.risk_violence",                                                    
                                          "leave_prev.curr_living_bc_felt_unsafe",                                                    
                                          "exp_violence_close",                                                                       
                                          "exp_violence_homeless",                                                                              
                                          "hh_phys.mntl_health_conds",
                                          "hh_lung.kid.liv.heart.sud",       
                                          "hard_get_doctor_rx",                                   
                                          "health_ins",                                                                                                         
                                          "hh_size" ,                                                                                
                                          "hh_anyone_5orUnder" ,                                                                                                       
                                          "hh_anyone_55orOver",                                                                                                        
                                          "hh_pregnant" ,                                                                                               
                                          "non.hh_children",     
                                          "non.hh_adults"), 
                           weight = NA) %>% as_tibble()
  
  for(i in 1:nrow(df.weights)){
    df.weights$weight[i] <- get(df.weights$short_name[i])
  }
  
  ce2 <- left_join(ce2, df.weights, by = c("question" = "long_name"))
  
  
  # 6. calculate composite score----
  ce2$comp_score <- ce2$weight * ce2$pct_v
  
  
  # 7. sort by composite score----
  ce3 <- ce2 %>%
    group_by(`Client ID`,Race) %>%
    summarise(comp_score = sum(comp_score)) %>%
    .[order(.$comp_score,decreasing = T),]
  
  score.fingerprint <- unlist(lapply(df.weights$short_name, get)) %>%
    paste(., sep = "|", collapse = "|")
  
  score.sum.out <- ce3 %>%
    group_by(Race) %>%
    summarise(avg_cs = mean(comp_score)#, 
              #med_cs = median(comp_score), 
              # sd
    ) %>%
    mutate(., 
           score_fingerprint = score.fingerprint) %>%
    as.data.table() %>%
    dcast(., 
          score_fingerprint ~ Race, 
          value.var = "avg_cs")
  
  
  readr::write_csv(x = score.sum.out, 
                   file = "ce_log.csv", 
                   append = T)
  
  # plot clients----
  ggplot() + 
    geom_boxplot(data = ce3, 
                 aes(x = Race, y = comp_score, group = Race))+
    labs(title = "Composite Score by Race", 
         subtitle = glue("Sim Iteration #: {sim.fingerprint}"))
  
  # plot weights----
  ggplot() + 
    geom_col(data = left_join(df.weights,qg2[,c(1:2)],by=c("long_name"="question")), 
             aes(x = weight, 
                 y = unlist(lapply(lapply(long_name, 
                                          strwrap, 70), 
                                   paste, 
                                   collapse = "\n"))))+
    facet_grid(group~., scales = "free_y", space = "free_y")+
    labs(title = "Score Weights by Question and Group", 
         subtitle = glue("Sim Iteration #: {sim.fingerprint}"))+
    theme(strip.text.y = element_text(angle = 0))+
    scale_y_discrete(name = "Survey Question / Group")+
    scale_x_continuous(name = "Weight Factor")
  
}


library(crayon)

ggplot() + 
  geom_boxplot(data = ce3, 
               aes(x = Race, 
                   y = comp_score, 
                   group = Race))+
  labs(title = "Composite Score by Race
       Optimized to Maximize Difference in Race", 
       subtitle = glue("Sim Iteration #: {sim.fingerprint}"))



ggplot() + 
  geom_col(data = left_join(df.weights,qg2[,c(1:2)],by=c("long_name"="question")), 
           aes(x = weight, 
               y = unlist(lapply(lapply(long_name, 
                                        strwrap, 70), 
                                 paste, 
                                 collapse = "\n"))))+
  facet_grid(group~., scales = "free_y", space = "free_y")+
  labs(title = "Score Weights - Optimized to Maximize Difference 
       in Race (Unadjusted)", 
       subtitle = glue("Sim Iteration #: {sim.fingerprint}"), 
       caption = "The higher the number, the higher the vulnerability. In this chart, the negative 
       numbers have larger bars but they are lower vulnerabilities (I think). So we 
       need to adjust by adding some common value to all questions")+
  theme(strip.text.y = element_text(angle = 0))+
  scale_y_discrete(name = "Survey Question / Group")+
  scale_x_continuous(name = "Weight Factor")

ggplot() + 
  geom_col(data = left_join(df.weights,qg2[,c(1:2)],by=c("long_name"="question")), 
           aes(x = weight - min(weight), 
               y = unlist(lapply(lapply(long_name, 
                                        strwrap, 70), 
                                 paste, 
                                 collapse = "\n"))))+
  facet_grid(group~., scales = "free_y", space = "free_y")+
  labs(title = "Score Weights - Optimized to Maximize Difference 
       in Race (ADJUSTED)", 
       subtitle = glue("Sim Iteration #: {sim.fingerprint}"), 
       caption = "This is the adjusted weights (all values increased by approximately +1.5). This
       now reflect larger bars meaning larger vulnerabilites, in-line with our 
       overall analysis*. (* Needs to be peer-reviewed and confirmed that this 
       theory / approach is correct)")+
  theme(strip.text.y = element_text(angle = 0))+
  scale_y_discrete(name = "Survey Question / Group")+
  scale_x_continuous(name = "Weight Factor")


cat(bgRed("I'm not confident I have this theory [above] correct on
          dealing with negative weights\n\n"))
Sys.sleep(10)



# read_log----
out.scoresA <- read_csv("ce_log.csv") 

out.scoresA <- out.scoresA[nchar(out.scoresA$score_fingerprint) > 100,] %>%
  mutate(., 
         rat_w2b = White/Black)

plot(sort(log(out.scoresA$rat_w2b)))

out.scoresB <- out.scoresA %>%
  as.data.table() %>%
  melt(., 
       id.vars = "score_fingerprint", 
       variable.name = "Race", 
       value.name = "avg_score") %>%
  as.data.frame() %>%
  as_tibble()

out.scoresBf <- out.scoresA %>%
  .[.$Black >= .$White,] %>%
  as.data.table() %>%
  melt(., 
       id.vars = "score_fingerprint", 
       variable.name = "Race", 
       value.name = "avg_score") %>%
  as.data.frame() %>%
  as_tibble()

# ggplot() + 
#   geom_line(data = out.scoresB, color = "grey",
#             aes(x = Race, 
#                 y = avg_score, 
#                 group = score_fingerprint))+
#   geom_line(data = out.scoresBf, color = "red",
#              aes(x = Race, 
#                  y = avg_score, 
#                  group = score_fingerprint))

ggplot() + 
  geom_point(data = out.scoresA, #size = 0.1,
             aes(x = White, y = Black, color = rat_w2b))+
  scale_color_viridis_c(option = "C")+
  scale_x_continuous(limits = c(-6,6))+
  scale_y_continuous(limits = c(-6,6))+
  geom_segment(linewidth = 1, 
               aes(x = -6, xend = 6, 
                   y = -6, yend = 6))

ggplot(data = out.scoresBf[out.scoresBf$Race != "rat_w2b",]) +
  geom_violin(aes(x = Race, y = avg_score, group  =Race), 
              draw_quantiles = c(0.25, 0.5, 0.75))


out.iv_list <- strsplit(x = out.scoresA$score_fingerprint, 
                        split = "\\|") 

out.df <- NULL
for(i in 1:length(out.iv_list)){
  temp <- out.iv_list[[i]] %>%
    as.numeric() %>% 
    as.data.frame() %>% t %>% as.data.frame()
  out.df <- rbind(out.df, temp)
  rm(temp)
}

rownames(out.df) <- 1:nrow(out.df)
out.df$Black <- out.scoresA$Black
out.df$White <- out.scoresA$White
out.df$w2b   <- out.scoresA$rat_w2b

fivenum(out.df$w2b)

out.df2 <- out.df %>% as_tibble() %>%
  slice_min(., 
            order_by = w2b, 
            n = 100)

out.df3 <- out.df2[order(out.df2$w2b,decreasing = T),] %>%
  mutate(., rid = 1:length(White)) %>%
  as.data.table() %>%
  melt(., id.vars = c("Black", "White", "w2b", "rid")) %>%
  as.data.frame() 
  
out.df3 %>%
  group_by(rid) %>%
  summarise(n_val_positive = sum(value > 0), 
            sum_val = sum(value),
            avg_val = mean(value), 
            med_val = median(value), 
            sd_val  = sd(value)) %>%
  .[order(.$n_val_positive,decreasing = F),]

ggplot(data = out.df3, 
         aes(x = n_val_positive, y = sum_val)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(data = out.df3[out.df3$rid %in% c(1,3),], 
         aes(group = rid, 
             x = variable, y = value, color = factor(rid))) + 
  geom_line()+
  geom_point()

lm.rat_w2b <- lm(formula = w2b ~ V1+V2+V3+V4+V5+V6+
                 V7++V8+V9+V10+V11+V12+
                 V13+V14+V15+V16+V17+V18, 
               data = out.df)
sum.lm.rat_w2b <- summary(lm.rat_w2b)

lm.rat_w2b2 <- lm(formula = w2b ~ V1+V2+V3+V4+V5+V6+
                   V7++V8+V9+V10+V11+V12+
                   V13+V14+V15+V16+V17+V18, 
                 data = out.df2)
sum.lm.rat_w2b2 <- summary(lm.rat_w2b2)



as.data.frame(sum.lm.rat_w2b$coefficients)$Estimate[2:19] %>% 
     plot(., type = "b")

as.data.frame(sum.lm.rat_w2b2$coefficients)$Estimate[2:19] %>% 
  plot(., type = "b")


ggplot() + 
  geom_col(data = as.data.frame(sum.lm.rat_w2b2$coefficients)[2:19,], 
            aes(x = (2:nrow(as.data.frame(sum.lm.rat_w2b2$coefficients)))-1, 
                y = Estimate, 
                fill = "Top 10%: Ratio Black to White"), 
           position = "dodge") +
  geom_col(data = as.data.frame(sum.lm.rat_w2b$coefficients)[2:19,], 
            aes(x = (2:nrow(as.data.frame(sum.lm.rat_w2b$coefficients)))-1, 
                y = Estimate, 
                fill = "All Simulation Weights")) +
  theme(legend.position = "bottom") +
  labs(subtitle = glue("Sim Iteration #: {sim.fingerprint}"))






# lm.black <- lm(formula = Black ~ V1+V2+V3+V4+V5+V6+
#                  V7++V8+V9+V10+V11+V12+
#                  V13+V14+V15+V16+V17+V18, 
#                data = out.df)
# 
# lm.white <- lm(formula = White ~ V1+V2+V3+V4+V5+V6+
#                  V7++V8+V9+V10+V11+V12+
#                  V13+V14+V15+V16+V17+V18, 
#                data = out.df)
# 
# 
# 
# 
# lm.blackwhite <- lm(formula = Black + White ~ V1+V2+V3+V4+V5+V6+
#                       V7++V8+V9+V10+V11+V12+
#                       V13+V14+V15+V16+V17+V18 , 
#                     data = out.df)
# sum.b <- summary(lm.black)
# sum.w <- summary(lm.white)
# sum.bw <- summary(lm.blackwhite)
# 
# 
# sum.b$coefficients %>% as.data.frame() %>% .$Estimate %>%
#   .[2:19] %>% 
#   plot(., type = "b")
# 
# as.data.frame(sum.w$coefficients)$Estimate[2:19] %>% 
#   plot(., type = "b")
# 
# 
# data.frame(q_num = 1:18, 
#            white_weight = as.data.frame(sum.w$coefficients)$Estimate[2:19], 
#            black_weight = as.data.frame(sum.b$coefficients)$Estimate[2:19], 
#            bw_weight = as.data.frame(sum.bw$coefficients)$Estimate[2:19]) %>%
#   ggplot(data = .) + 
#   geom_segment(aes(y = q_num, yend = q_num, 
#                    x = white_weight, xend = black_weight, 
#                    color = white_weight > black_weight)) +
#   scale_x_continuous(name = "weight_value")+
#   scale_y_continuous(name = "question number", minor_breaks = seq(0,1000,by=1)) +
#   geom_point(aes(y = q_num, x = bw_weight))+
#   theme(legend.position = "bottom")
