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

rm(list=ls());cat('\f')

# Dataset----

# Data Dictionary----

qr_vuln <- read_tsv("How long has it been since you lived in your own place?	order_vuln	normalized vulnerability
12 to 35 months (1-2 years)	3	0.75
3 to 5 months	1	0.25
36 months (3 years) or more	4	1
6 to 11 months	2	0.5
Less than 3 months	0	0
		
How many months have you been without a home, such as living outside or in a shelter?	order_vuln	
12 to 35 months (1-2 years)	3	0.75
3 to 5 months	1	0.25
36 months (3 years) or more	4	1
6 to 11 months	2	0.5
Less than 3 months	0	0
		
Where did you sleep last night?	order_vuln	
Sheltered (ES, TH)	0	0
Unsheltered	1	1
		
Where are you going to sleep tonight?	order_vuln	
Sheltered (ES, TH)	0	0
Unsheltered	1	1
		
Are you currently experiencing or feel you are at risk of experiencing violence?	order_vuln	
No	0	0
Yes	1	1
		
Did you leave your previous or current living situation because you felt unsafe?	order_vuln	
No	0	0
Yes	1	1
		
Have you ever experienced violence with someone close to you?	order_vuln	
No	0	0
Yes	1	1
		
Have you experienced violence since becoming homeless?	order_vuln	
No	0	0
Yes	1	1
Does anyone in your household have any physical or mental health conditions that are treated or have been treated by a professional?	order_vuln	
No	0	0
Yes	1	1
Do you or does anyone in the household have lung cancer, kidney or liver failure, heart disease, or a substance use disorder?	order_vuln	
No	0	0
Yes, 1	1	0.5
Yes, 2 or more	2	1
Is the lack of housing making it hard to get to a doctor's office or take prescribed medications?	order_vuln	
No	0	0
Yes	1	1
Covered by Health Insurance	order_vuln	
Data not collected (HUD)	0	0
No (HUD)	1	1
Yes (HUD)	0	0
What is the size of your household? (including you)	order_vuln	
1-2 people	0	0
3 or more people	1	1
Is anyone under 5 years old?	order_vuln	
No	0	0
Yes	1	1
Is anyone in the household pregnant?	order_vuln	
No	0	0
Yes	1	1
How many children under the age of 18 are not currently staying with your family, but would live with you? (if you have a home)	order_vuln	
1 or more	1	1
None	0	0
How many adults 18 or older are not currently staying with your family, but would live with you? (if you have a home)	order_vuln	
1 or more	1	1
None	0	0", 
                    col_names = F)

qr_vuln <- mutate(qr_vuln, 
                  type = ifelse(grepl("\\?", qr_vuln$X1), "question", "response"), 
                  qnum = NA_integer_)
colnames(qr_vuln) <- c("question.response", 
                       "order_vuln", 
                       "order_vuln.norm", 
                       "type", "qnum")

qn <- 0
for(i in 1:nrow(qr_vuln)){
  if(qr_vuln$type[i] == "question"){
    qn <- qn + 1
  }
  qr_vuln$qnum[i] <- qn
}

qr_vuln

select(qr_vuln, question.response, type, qnum)

q_vuln <- qr_vuln[qr_vuln$type == "question",] %>%
  select(., question.response, qnum)
colnames(q_vuln)[1] <- "question"
qr_vuln <- left_join(qr_vuln, q_vuln) %>%
  .[.$type != "question",]
colnames(qr_vuln)[1] <- "response"
qr_vuln <- qr_vuln[!colnames(qr_vuln) %in% "type"]
qr_vuln <- qr_vuln[,c("question", "response", 
                      "qnum", "order_vuln", "order_vuln.norm")]
qr_vuln <- qr_vuln[order(qr_vuln$qnum, qr_vuln$order_vuln),]

qr_vuln$qnum <- as.numeric(qr_vuln$qnum)
qr_vuln$order_vuln <- qr_vuln$order_vuln
qr_vuln$order_vuln.norm <- qr_vuln$order_vuln.norm

# add na values for each response

# crosswalk vuln_group

cw_vuln <- select(qr_vuln, question, qnum) %>%
  .[!duplicated(.),] %>%
  mutate(., 
         vuln_group = NA)
cw_vuln$vuln_group[cw_vuln$qnum %in% 1:4] <- "Housing and Homeless History"
cw_vuln$vuln_group[cw_vuln$qnum %in% 5:8] <- "Risks"
cw_vuln$vuln_group[cw_vuln$qnum %in% 9:12] <- "Health and Wellness"
cw_vuln$vuln_group[cw_vuln$qnum %in% 13:18] <- "Family Unit"

write_csv(x = cw_vuln, 
          file = "MASTER_crosswalk_vuln.csv")

write_csv(x = qr_vuln, 
          file = "MASTER_questions_vuln.csv")


# Deidentified Data----

ce_in <- read_tsv("Client ID	Household ID	Race	Ethnicty	Gender	Entry Date	Exit Date	Region	Provider	Provider Updating	How long has it been since you lived in your own place?	How many months have you been without a home, such as living outside or in a shelter?	Where did you sleep last night?	Where are you going to sleep tonight?	Are you currently experiencing or feel you are at risk of experiencing violence?	Did you leave your previous or current living situation because you felt unsafe?	Have you ever experienced violence with someone close to you?	Have you experienced violence since becoming homeless?	Does anyone in your household have any physical or mental health conditions that are treated or have been treated by a professional?	Do you or does anyone in the household have lung cancer, kidney or liver failure, heart disease, or a substance use disorder?	Is the lack of housing making it hard to get to a doctorâ€™s office or take prescribed medications?	Covered by Health Insurance	What is the size of your household? (including you)	Is anyone under 5 years old?	Is anyone 55 years or older?	Is anyone in the household pregnant?	How many children under the age of 18 are not currently staying with your family, but would live with you? (if you have a home)	How many adults 18 or older are not currently staying with your family, but would live with you? (if you have a home)	Note
136715		White	Non-Hispanic	Male	6/20/2023	7/1/2023	R05	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	Less than 3 months	Less than 3 months	Sheltered (ES, TH)	Sheltered (ES, TH)	No	No	Yes	No	No	No	No	No (HUD)	1-2 people	No	No	No	1 or more	None	
285031		Black	Non-Hispanic	Male	6/10/2023	7/1/2023	R05	Rowan Helping Ministries - Rowan County -  Emergency Shelter - ES - Private	Rowan Helping Ministries - Rowan County -  Emergency Shelter - ES - Private	36 months (3 years) or more	36 months (3 years) or more	Sheltered (ES, TH)	Sheltered (ES, TH)	Yes	Yes	No	Yes	Yes	Yes, 1	Yes	Yes (HUD)	1-2 people	No	No	No	None	None	
297854		White	Non-Hispanic	Male	6/23/2023		R05	Rowan Helping Ministries - Rowan County -  Emergency Shelter - ES - Private	Rowan Helping Ministries - Rowan County -  Emergency Shelter - ES - Private	36 months (3 years) or more	36 months (3 years) or more	Sheltered (ES, TH)	Sheltered (ES, TH)	No	No	No	No	No	No	No	No (HUD)	1-2 people	No	No	No	None	None	
320083		White	Non-Hispanic	Female	6/3/2023	6/30/2023	R05	Rowan Helping Ministries - Rowan County -  Emergency Shelter - ES - Private	Rowan Helping Ministries - Rowan County -  Emergency Shelter - ES - Private	Less than 3 months	Less than 3 months	Sheltered (ES, TH)	Sheltered (ES, TH)	No	No	Yes	Yes	Yes	Yes, 2 or more	No	Yes (HUD)	1-2 people	No	Yes	No	None	None	
352302		White	Non-Hispanic	Male	5/5/2023		R05	Crisis Ministry of Davidson County - Davidson County - Emergency Shelter - ES - State ESG CV	Crisis Ministry of Davidson County - Davidson County - Emergency Shelter - ES - State ESG CV	Less than 3 months	12 to 35 months (1-2 years)	Sheltered (ES, TH)	Sheltered (ES, TH)	No	No	No	No	No	No	No	No (HUD)	1-2 people	No	No	No	None	None	
387916		White	Non-Hispanic	Female	6/16/2023		R07	Outreach Mission Inc. - Lee County - Women's Shelter - ES - State ESG	Outreach Mission Inc. - Lee County - Women's Shelter - ES - State ESG	36 months (3 years) or more	Less than 3 months	Sheltered (ES, TH)	Sheltered (ES, TH)	No	Yes	Yes	No	No	No	Yes	Yes (HUD)	1-2 people	No	No	No	1 or more	1 or more	
420172		White	Non-Hispanic	Male	6/3/2023	6/15/2023	R05	Crisis Ministry of Davidson County - Davidson County - Emergency Shelter - ES - State ESG CV	Crisis Ministry of Davidson County - Davidson County - Emergency Shelter - ES - State ESG CV	12 to 35 months (1-2 years)	12 to 35 months (1-2 years)	Sheltered (ES, TH)	Sheltered (ES, TH)	No	No	Yes	No	Yes	Yes, 1	No	Yes (HUD)	1-2 people	No	Yes	No	None	None	
458353		White	Non-Hispanic	Male	6/12/2023		R05	Rowan Helping Ministries - Rowan County -  Emergency Shelter - ES - Private	Rowan Helping Ministries - Rowan County -  Emergency Shelter - ES - Private	36 months (3 years) or more	Less than 3 months	Sheltered (ES, TH)	Sheltered (ES, TH)	No	No	Yes	No	Yes	No	Yes	No (HUD)	1-2 people	No	No	No	1 or more	None	
460330		Indigenous	Non-Hispanic	Male	6/9/2023		R05	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	3 to 5 months	3 to 5 months	Sheltered (ES, TH)	Sheltered (ES, TH)	No	Yes	Yes	No	Yes	No	No	No (HUD)	1-2 people	No	No	No	1 or more	1 or more	
471926		Black	Non-Hispanic	Male	6/20/2023		R05	Rowan Helping Ministries - Rowan County -  Emergency Shelter - ES - Private	Rowan Helping Ministries - Rowan County -  Emergency Shelter - ES - Private	Less than 3 months	Less than 3 months	Sheltered (ES, TH)	Sheltered (ES, TH)	No	No	Yes	No	No	No	No	No (HUD)	1-2 people	No	No	No	None	None	
474858		Black	Non-Hispanic	Male	6/17/2023	7/6/2023	R05	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	Less than 3 months	Less than 3 months	Sheltered (ES, TH)	Sheltered (ES, TH)	No	No	No	No	No	No	No	No (HUD)	1-2 people	No	Yes	No	None	None	
475377		Black	Non-Hispanic	Male	5/17/2023		R05	Homes of Hope - Stanly County - Stanly Community Inn - ES - State ESG	Homes of Hope - Stanly County - Stanly Community Inn - ES - State ESG	3 to 5 months	Less than 3 months	Sheltered (ES, TH)	Sheltered (ES, TH)	No	No	No	No	Yes	No	No	Yes (HUD)	1-2 people	No	No	No	None	None	Manually entered Family Size questions (single)
481262		White	Non-Hispanic	Male	5/26/2023		R05	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	12 to 35 months (1-2 years)	12 to 35 months (1-2 years)	Sheltered (ES, TH)	Sheltered (ES, TH)	No	No	No	No	Yes	No	No	No (HUD)	1-2 people	No	No	No	None	None	
483917		Black	Non-Hispanic	Male	6/13/2023	6/15/2023	R05	Rowan Helping Ministries - Rowan County -  Emergency Shelter - ES - Private	Rowan Helping Ministries - Rowan County -  Emergency Shelter - ES - Private	12 to 35 months (1-2 years)	Less than 3 months	Sheltered (ES, TH)	Sheltered (ES, TH)	No	No	No	Yes	Yes	Yes, 2 or more	No	Yes (HUD)	1-2 people	No	Yes	No	None	None	
485450		White	Non-Hispanic	Male	5/18/2023		R05	Rowan Helping Ministries - Rowan County -  Emergency Shelter - ES - Private	Rowan Helping Ministries - Rowan County -  Emergency Shelter - ES - Private	36 months (3 years) or more	36 months (3 years) or more	Sheltered (ES, TH)	Sheltered (ES, TH)	No	No	Yes	Yes	Yes	Yes, 2 or more	No	Yes (HUD)	1-2 people	No	No	No	None	None	
492513		Black	Non-Hispanic	Male	6/29/2023		R05	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	36 months (3 years) or more	36 months (3 years) or more	Sheltered (ES, TH)	Sheltered (ES, TH)	No	No	No	No	Yes	Yes, 1	Yes	Yes (HUD)	1-2 people	No	Yes	No	None	1 or more	
497653		Black	Non-Hispanic	Male	7/3/2023		R05	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	36 months (3 years) or more	Less than 3 months	Sheltered (ES, TH)	Sheltered (ES, TH)	No	Yes	Yes	No	Yes	No	Yes	No (HUD)	3 or more people	No	No	No	1 or more	None	
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
1042706		Indigenous	Non-Hispanic	Female	5/31/2023		R05	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	3 to 5 months	Less than 3 months	Sheltered (ES, TH)	Sheltered (ES, TH)	No	Yes	No	No	Yes	No	No	Yes (HUD)	1-2 people	No	Yes	No	None	None	
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
1043525		White	Non-Hispanic	Female	7/1/2023		R05	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	Union County Community Shelter - Union County - Emergency Adult Shelter - ES - State ESG	12 to 35 months (1-2 years)	3 to 5 months	Sheltered (ES, TH)	Sheltered (ES, TH)	No	No	No	No	Yes	No	No	Yes (HUD)	3 or more people	No	No	No	1 or more	None	
1043536		Black	Non-Hispanic	Male	7/3/2023		R05	Rowan Helping Ministries - Rowan County -  Emergency Shelter - ES - Private	Rowan Helping Ministries - Rowan County -  Emergency Shelter - ES - Private	6 to 11 months	6 to 11 months	Sheltered (ES, TH)	Sheltered (ES, TH)	No	Yes	Yes	No	No	Yes, 1	Yes	Yes (HUD)	1-2 people	No	No	No	None	None	
1043641	141355	Black	Non-Hispanic	Female	7/6/2023		R05	Union County Community Shelter - Union County - Family Shelter - ES - State ESG	Union County Community Shelter - Union County - Family Shelter - ES - State ESG	3 to 5 months	3 to 5 months	Sheltered (ES, TH)	Sheltered (ES, TH)	Yes	No	No	No	Yes	Yes, 1	No	Yes (HUD)	3 or more people	No	No	No	1 or more	None	") 

ce_in

library(openssl)

ce_in2 <- mutate(ce_in, 
                 client_id2 = sha256(x = as.character(`Client ID`), 
                                     key = sha256("ncceh", "ncceh")), 
                 rid = 1:length(`Client ID`))
ce_in2$client_id2 <- ce_in2$client_id2 %>%
  substr(., 0, 3)



colnames(ce_in2)

ce_in3 <- as.data.table(ce_in2) %>%
  melt(., id.vars = c("Client ID", "client_id2", "rid", "Note", 
                      "Provider", "Provider Updating", 
                      "Region", "Exit Date", "Entry Date", 
                      "Gender", "Ethnicty", "Race", 
                      "Household ID"), 
       variable.name = "question", 
       value.name = "response") %>%
  as.data.frame() %>%
  as_tibble() %>%
  select(., 
         client_id2, Region, Gender, Race, Ethnicty, question, response)

ce_in3$client_id2 <- ce_in3$client_id2 %>% as.character() 


ce_in4 <- left_join(ce_in3, 
                    select(qr_vuln, question,response,qnum, order_vuln, order_vuln.norm))



write_csv(ce_in4, 
          "MASTER_client_deidentified.csv")

