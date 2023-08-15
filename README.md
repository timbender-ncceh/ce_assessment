# ce_assessment README

## BEST WEIGHTS SO FAR
*  month_since_own_home                  <- 3 
*  months_since_any_home                 <- 9 
*  loc_sleep_last_night                  <- 9 
*  loc_sleep_tonight                     <- 3 
*  now_or_at.risk_violence               <- 1 
*  leave_prev.curr_living_bc_felt_unsafe <- 2
*  exp_violence_close                    <- 4 
*  exp_violence_homeless                 <- 3 
*  hh_phys.mntl_health_conds             <- 9 
*  hh_lung.kid.liv.heart.sud             <- 6 
*  hard_get_doctor_rx                    <- 1 
*  health_ins                            <- 7 
*  hh_size                               <- 7 
*  hh_anyone_5orUnder                    <- 5 
*  hh_anyone_55orOver                    <- 4 
*  hh_pregnant                           <- 8 
*  non.hh_children                       <- 8 
*  non.hh_adults                         <- 9 

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
