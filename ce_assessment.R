library(readr)
library(dplyr)
library(lubridate)
library(data.table)

rm(list=ls());cat('\f')
gc()


# WEIGHTS----
hhh   <- 30 #1  # housing and homeless history
risks <- 2    # risk factors
hw    <- 30 #1  # health wellness
fu    <- 1 #3  # family unit

# DATA IMPORT----

# VI-SPDAT Level of Priority: 
#* Highest = 15+17 points 
#* Higher = 11-14 points 
#* High = 8-10 
#* Medium = 4-7  
#* Low = 0-3
 
# Goal Testing Criteria: 
# Top 20% Racial Distribution (43% Black) 
# Overall Racial Distribution (43% Black)

 
#https://ncceh.sharepoint.com/:x:/s/boscoccoordination/EbaXcHJZpX1Dirf1Ql7u_9YB5FTYsxfbI5uEmPHm2Z8zjg?e=e0ORVG

ce <- read_tsv("Client ID	Household ID	Race	Ethnicty	Gender	Entry Date	Exit Date	Region	Provider	Provider Updating	How long has it been since you lived in your own place?	How many months have you been without a home, such as living outside or in a shelter?	Where did you sleep last night?	Where are you going to sleep tonight?	Are you currently experiencing or feel you are at risk of experiencing violence?	Did you leave your previous or current living situation because you felt unsafe?	Have you ever experienced violence with someone close to you?	Have you experienced violence since becoming homeless?	Does anyone in your household have any physical or mental health conditions that are treated or have been treated by a professional?	Do you or does anyone in the household have lung cancer, kidney or liver failure, heart disease, or a substance use disorder?	Is the lack of housing making it hard to get to a doctor's office or take prescribed medications?	Covered by Health Insurance	What is the size of your household? (including you)	Is anyone under 5 years old?	Is anyone 55 years or older?	Is anyone in the household pregnant?	How many children under the age of 18 are not currently staying with your family, but would live with you? (if you have a home)	How many adults 18 or older are not currently staying with your family, but would live with you? (if you have a home)	Note
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
#View(ce)
colnames(ce)

ce$`Entry Date` <- mdy(ce$`Entry Date`)
ce$`Exit Date`  <- mdy(ce$`Exit Date`)


# tidy to factor----
#stop("treat NAs as same numeric value as zero or mininal vulnerability")

library(forcats)

colnames(ce)

ce3 <- ce


ce3$Race
ce3$Ethnicty
ce3$Gender

# levels(ce3$`How long has it been since you lived in your own place?`) <- 
#   levels(ce3$`How long has it been since you lived in your own place?`)[c(1,4,5,3,2)] 
# 
# 
# ce3[,28] %>% unlist %>% unname %>% levels
# colnames(ce3)[28]
# 
# levels(ce3$`How many children under the age of 18 are not currently staying with your family, but would live with you? (if you have a home)`) <- 
#   levels(ce3$`How many children under the age of 18 are not currently staying with your family, but would live with you? (if you have a home)`)[c(1,2)]
# 
# levels(ce3$`Have you ever experienced violence with someone close to you?`) <- 
#   levels(ce3$`Have you ever experienced violence with someone close to you?`)[c(2,1)]
# 
# levels(ce3$`How many months have you been without a home, such as living outside or in a shelter?`) <- 
#   levels(ce3$`How many months have you been without a home, such as living outside or in a shelter?`)[c(1,4,5,3,2)]
# 
# # data not collected: do we do as NA? 
# levels(ce3$`Covered by Health Insurance`) <-
#   levels(ce3$`Covered by Health Insurance`)[c(3,1,2)]

df.colatts <- data.frame(long_name = colnames(ce3), 
                         short_name = c("rid", "client_id", "hhid", 
                                        "race", "eth", "gender", "entry_date", "exit_date", 
                                        "region", "provider", "provider_updt", 
                                        "months_since_having_own_home", 
                                        "months_without_any_home", 
                                        "loc_slept_last_night", 
                                        "loc_sleep_tonight", 
                                        "cur_violence_or_at_risk", 
                                        "leave_prev_house_bc_unsafe",
                                        "ever_violence_close", 
                                        "ever_violence_since_homeless", 
                                        "hh_phys.or.mntl_health_cond", 
                                        "hh_lung.kidney.liver.heart.substanceuse", 
                                        "hard_find_doc.or.meds_bc_homeless", 
                                        "have_health_ins", 
                                        "hh_size", 
                                        "hh_5yo_or_under", 
                                        "hh_55yo_or_over", 
                                        "hh_preg", 
                                        "non_hh_children_count", 
                                        "non_hh_adults_count", 
                                        "note"), 
                         group = NA)  %>%
  as_tibble() 

df.colatts$short_name

df.colatts[grepl("^How long has it been|^How many months|^Where did you sleep|^Where are you going", df.colatts$long_name),]$group <- "Housing and Homeless History"
df.colatts[grepl("^Did you leave|^Have you experienced|^Have you ever", df.colatts$long_name),]$group <- "Risks"
df.colatts[grepl("^Does anyone in|Health Insurance|^Is the lack|kidney", df.colatts$long_name),]$group <- "Health and Wellness"
df.colatts[grepl("^What is the|^Is anyone under|^Is anyone 55|^How many children|^How many adults", df.colatts$long_name),]$group <- "Family Unit"

df.colatts

# order vulnerability----
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
Is the lack of housing making it hard to get to a doctor's office or take prescribed medications?	order_vuln
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

# tidy ce3----



library(data.table)
ce4 <- as.data.table(ce3) %>%
  melt(., 
       id.vars = colnames(ce3)[c(1:10,30)], 
       variable.name = "question", value.name = "response") %>%
  as.data.frame() %>% 
  as_tibble()

ce4$response[is.na(ce4$response)] <- "na"

ce5 <- left_join(ce4, ov)


# normalize----
ov_n <- ov %>%
  group_by(question, order_vuln) %>%
  summarise() %>%
  ungroup() %>%
  group_by(question) %>%
  mutate(norm_vuln = order_vuln / max(order_vuln))


ce6 <- left_join(ce5, ov_n)

ce6[is.na(ce6$norm_vuln),]$question %>% unique()

ce6$norm_vuln %>% table(., useNA = "always")

ce7 <- left_join(ce6, 
          df.colatts, 
          by = c("question" = "long_name"))


ce8 <- ce7 %>%
  group_by(`Client ID`, group) %>%
  summarise(t_norm = sum(norm_vuln)) %>%
  .[!is.na(.$group),] %>%
   left_join(., 
             data.frame(group = c("Family Unit", 
                                  "Health and Wellness", 
                                  "Housing and Homeless History", 
                                  "Risks"), 
                        weight = c(fu, hw, hhh, risks))) %>%
  ungroup() 

ce9 <- ce8 %>%
  mutate(., 
         comp_score = t_norm * weight)

ce10 <- as.data.table(ce9) %>% 
  dcast(., 
       `Client ID` ~ group, value.var = "comp_score", fun.aggregate = sum) %>%
  as.data.frame() %>%
  as_tibble() %>%
  mutate(., 
         t_score = `Family Unit` + `Health and Wellness` + `Housing and Homeless History` + Risks)




ce11 <- full_join(ce[,c(1:10, 30)], 
          ce10) %>%
  .[order(.$t_score),] %>%
  mutate(., 
         rank_order = 1:length(Race))

library(ggplot2)

# ggplot() + 
#   geom_jitter(data = ce11, 
#               height = 0, width = 0.2,
#              aes(x = 0, y = rank_order, color = Race))+
#   scale_x_continuous(limits = c(-1,1))

ggplot() + 
  geom_boxplot(data = ce11, 
             aes(x = Race, y = t_score))


ce12 <- ce11[order(ce11$rank_order),]


ce12$top_20pct <- c(rep(T,19), rep(F,97-19))

ce12[ce12$top_20pct,] %>%
  group_by(Race) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(., 
         pct_r = n / sum(n))
