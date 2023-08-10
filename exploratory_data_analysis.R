# exploratory data analysis----

library(readr)
library(dplyr)
library(lubridate)
library(data.table)
library(ggplot2)
library(forcats)
library(openssl)
library(glue)

library(janitor)

# functions to learn ----
# select()
# get()
# unnest()
# clean_names()
# 

setwd("C:/Users/TimBender/Documents/R/ncceh/projects/ce_assessment")
rm(list=ls());cat('\f');gc()

# Funs----
ci <- function(samp_mean, moe){
  samp_mean * moe
}

moe <- function(samp_sd, samp_size, z.score = 1.95){
  #https://www.surveymonkey.com/mp/margin-of-error-calculator/
  
  zscore.df <- data.frame(desired_conf_lvl = c(.8,.85,.9, 
                                               .95,.99), 
                          dcl_pct = NA,
                          z_score = c(1.28, 1.44,1.65,
                                      1.96,2.58)) %>%
    mutate(dcl_pct = scales::percent(desired_conf_lvl))
  print(zscore.df)
  
  z.score * (samp_sd / sqrt(samp_size))
}



# metadata pull----
sim.fingerprint <- openssl::md5(as.character(Sys.time())) %>%
  substr(., nchar(.) - 7, nchar(.))

git.raw.md5 <- read_file("https://raw.githubusercontent.com/timbender-ncceh/ncceh_data_tools/main/ce_assessment_micro.R") %>%
  md5
gc()


# read data and tidy----
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
  try(df.weights$weight[i] <- get(df.weights$short_name[i]))
}

ce2 <- left_join(ce2, df.weights, by = c("question" = "long_name"))


# 6. calculate composite score----
ce2$comp_score <- ce2$weight * ce2$pct_v


# # 7. sort by composite score----
# ce3 <- ce2 %>%
#   group_by(`Client ID`,Race) %>%
#   summarise(comp_score = sum(comp_score)) %>%
#   .[order(.$comp_score,decreasing = T),]

# try(score.fingerprint <- unlist(lapply(df.weights$short_name, get)) %>%
#   paste(., sep = "|", collapse = "|"))

# try(score.sum.out <- ce3 %>%
#   group_by(Race) %>%
#   summarise(avg_cs = mean(comp_score)#, 
#             #med_cs = median(comp_score), 
#             # sd
#   ) %>%
#   mutate(., 
#          score_fingerprint = score.fingerprint) %>%
#   as.data.table() %>%
#   dcast(., 
#         score_fingerprint ~ Race, 
#         value.var = "avg_cs"))


# DATA EXPLORATION----

# Race / Ethnicity ----
out_re <- ce2 %>%
  #.[.$question == "Where are you going to sleep tonight?",] %>%
  group_by(Race, Ethnicty) %>%
  summarise() %>%
  ungroup() %>%
  mutate(., 
         race_eth = ifelse(Race == "White" & 
                             Ethnicty == "Non-Hispanic", 
                           "White, non-HISP", NA), 
         race_eth = ifelse(is.na(race_eth) & 
                             Race == "Black" & 
                             Ethnicty == "Non-Hispanic", 
                           "Black, non-HISP", race_eth), 
         race_eth = ifelse(is.na(race_eth), 
                           "Other", race_eth))

ce2 <- left_join(ce2[ce2$question == "Where are you going to sleep tonight?",],
                 out_re)

ce2$race_eth <- factor(ce2$race_eth, 
                       levels = sort(unique(ce2$race_eth))[c(3,1,2)])

# weighted sum model explanation-----
library(stats)

#https://or.stackexchange.com/questions/7835/use-the-weighted-sum-method-in-r

df1<- structure(list(Student                = c("Student1", "Student2", "Student3", "Student4", "Student5"), 
                     CGPA                   = c(9, 7.6, 8.2, 8.5, 9.3), 
                     `Expected Stipend`     = c(12000L, 8500L, 9500L, 10000L, 14000L), 
                     `Technical Exam Score` = c(72L, 68L, 63L, 70L, 72L), 
                     `Aptitude Test Grade`  = c("B1", "B1", "B2", "A2", "A2")), 
                class     = "data.frame", 
                row.names = c(NA, -5L))

# > df1
# Student CGPA Expected Stipend Technical Exam Score Aptitude Test Grade
# 1 Student1  9.0            12000                   72                  B1
# 2 Student2  7.6             8500                   68                  B1
# 3 Student3  8.2             9500                   63                  B2
# 4 Student4  8.5            10000                   70                  A2
# 5 Student5  9.3            14000                   72                  A2

df1

# Convert the grades to point values.
df1 <- df1 |> mutate(`Aptitude Test Grade` = recode(`Aptitude Test Grade`, "A1" = 5, "A2" = 4, "B1" = 3, "B2" = 2, "C1" = 1))

df1

# Set up the vector of weights.
weights <- c(0.3, 0.2, 0.25, 0.25)
# Copy the data frame and scale each column appropriately.
scaled <- df1 |>
  mutate(CGPA = CGPA / max(CGPA),
         `Expected Stipend` = min(`Expected Stipend`) / `Expected Stipend`,
         `Technical Exam Score` = `Technical Exam Score` / max(`Technical Exam Score`),
         `Aptitude Test Grade` = `Aptitude Test Grade` / max(`Aptitude Test Grade`)
  )
# Compute the weighted scores.
scaled <- scaled |>
  rowwise() |>
  mutate(`Performance Score` = weighted.mean(c(CGPA, `Expected Stipend`, `Technical Exam Score`, `Aptitude Test Grade`), w = weights))
# Assign ranks.
scaled$Rank <- (nrow(scaled) + 1) - rank(scaled$`Performance Score`)
# View the results.
scaled %>% as.data.frame()


janitor::clean_names(scaled)
