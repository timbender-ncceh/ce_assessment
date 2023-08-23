# ce_assessment README
## Simulation Data Logs:
All raw simulation data is stored here: https://raw.githubusercontent.com/timbender-ncceh/ce_assessment/main/model_outputs2.csv.  Each time a simulation was run a unique md5 hash was generated based on the timestamp at which the simulation was run.  This way there would be a unique ID for each simulation, because there are starting to be simulations which duplicate scores.  

## TRACKING OUTCOMES
The charts and images below show the following: 

| Index of Images | UNWEIGHTED | WEIGHTED | 
|---|---|---|
| List of weights| [link[(https://github.com/timbender-ncceh/ce_assessment/blob/main/README.md#unweighted-inputs) | (link)[] |
| Weights plotted by groups| //tbd// | //tbd//|
| Vulnerability Rankings plotted by Race| //tbd// | //tbd// |
| Vulnerability Rankings plotted by Ethnicity| //tbd// | //tbd// |


### UNWEIGHTED
#### unweighted inputs:
![image](https://github.com/timbender-ncceh/ce_assessment/assets/105810134/b70a00cc-5333-455c-9450-747b9c180a9f)
#### unweighted inputs by group:
![image](https://github.com/timbender-ncceh/ce_assessment/assets/105810134/cf727c49-b650-474c-84b0-fc0433bf2919)
#### unweighted inputs as outputs (race):
![image](https://github.com/timbender-ncceh/ce_assessment/assets/105810134/7c7c0713-a72d-4127-8956-0c68a67e61a6)
#### unweighted inputs as outputs (ethnicity): 
![image](https://github.com/timbender-ncceh/ce_assessment/assets/105810134/93d96c78-3119-4d21-9924-dcedbe1d77cd)

### OPTIMAL WEIGHTS (RACE) (as of 8/21/23)
#### optimal inputs:
![image](https://github.com/timbender-ncceh/ce_assessment/assets/105810134/ccc2665a-a7ad-43f4-bb51-c8dc4f679627)
#### optimal inputs by group:
![image](https://github.com/timbender-ncceh/ce_assessment/assets/105810134/1c8b648d-7f1b-41f2-b767-2bf865343df7)
#### optimal inputs as outputs (race):
![image](https://github.com/timbender-ncceh/ce_assessment/assets/105810134/5211eaa6-d314-487a-86d5-21963014c130)

### OPTIMAL WEIGHTS (ETHNICITY) (as of 8/21/23)
#### optimal inputs: 
![image](https://github.com/timbender-ncceh/ce_assessment/assets/105810134/ccc2665a-a7ad-43f4-bb51-c8dc4f679627)
#### optimal inputs by group:
![image](https://github.com/timbender-ncceh/ce_assessment/assets/105810134/bc3bd728-bd61-4b81-a3b6-42873adf431d)
#### optimal inputs as outputs (ethnicity):
![image](https://github.com/timbender-ncceh/ce_assessment/assets/105810134/2e7cb28a-1e13-4285-8e6f-63c3ed2cf2f5)




## IMPORTANT FILES
### Input Tables 
**(imported as data into model)**
* MASTER_questions_vuln.csv ([link](https://github.com/timbender-ncceh/ce_assessment/blob/main/MASTER_questions_vuln.csv) to view or open an issue)
* MASTER_crosswalk_vuln.csv ([link](https://github.com/timbender-ncceh/ce_assessment/blob/main/MASTER_crosswalk_vuln.csv))
* MASTER_cw_qshortname.csv
* MASTER_client_deidentified.csv
### Model Parameters
**(imported as data into the model)**
* 
### Model Code
* ce_assessment_micro2.R
* exploratory_data_analysis.R
* cd_data_tidy.R: used to clean/tidy data from excel spreadsheet so that it can be used in model.  
### Data Logs
* ce_log.csv: a data file where outputs of mote carlo simulation runs are stored
