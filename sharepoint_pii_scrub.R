## WARNING - PLEASE READ ##
## --------------------- ##

# THIS SCRIPT DELETES FILES PERMANENTLY. THERE IS ALWAYS A NON-ZERO RISK THAT
# IMPORTANT FILES MAY BE DELETED INADVERTENTLY WHEN A SCRIPT IS WRITTEN FOR THIS
# PURPOSE.

# DO NOT ASSUME THIS FILE HAS BEEN TESTED OR IS SAFE, OR DOES WHAT YOU THINK
# IT'S SUPPOSED TO DO. EVEN IF IT USED TO WORK, THINGS CHANGE AND CODE BREAKS.
# CODE THAT DELETES FILES IS VERY DANGEROUS SO PROCEED WITH CAUTION.  THIS IS
# BEST PRACTICES STUFF.

# DO NOT RUN THIS SCRIPT IF YOU DO NOT UNDERSTAND WHAT IT DOES.

# ONE LINE OF CODE IS ALL IT TAKES TO DELETE EVERY FILE FROM YOUR HARD DRIVE
# AND/OR NETWORK.  THAT LINE OF CODE IS USED IN THIS SCRIPT, BUT MODIFIED
# SLIGHTLY SO THAT INSTEAD OF DELETING EVERYTHING, IT ONLY DELETES FILES
# MATCHING A SPECIFIC (FILENAME) CRITERIA. FAILSAFES WILL BE IMPLEMENTED TO
# CHECK FOR COMMON ERRORS AND MISTAKES BUT FAILSAFES ARE NOT TO BE RELIED UPON.
# THIS IS BEST PRACTICES STUFF

## OVERVIEW - PLEASE READ ##
## ---------------------- ##

# THIS SCRIPT IS DESIGNED TO SEARCH THROUGH A SPECIFIED FILE DIRECTORY AND ALL
# SUB-DIRECTORIES THAT IT CONTAINS FOR FILES WITH FILENAMES MATCHING A SPECIFIC
# VALUE.  WHEN A MATCHING FILE IS FOUND, THE SCRIPT WILL USE THE WINDOWS
# OPERATING SYSTEM TO FIRST OVERWRITE THE PERSONALLY IDENTIFYING INFORMATION
# (PII) WITHIN THE FILE WITH NULL VALUES, SAVE THE CHANGES TO THE FILE, THEN
# PERMANENTLY REMOVE THE FILE FROM THE DIRECTORY.

## POTENTIAL ISSUES ##
## ---------------- ##

# THE CODE WILL SEARCH FOR ALL FILES NAMED "Client.csv" WHICH IS WHERE WE KNOW
# PII DATA IS LOCATED IN HMIS EXPORTS.

# ISSUE #1: FALSE POSITIVES: ANY FILES SAVED IN THE DIRECTORY NAMED "Client.csv"
# WILL BE DELETED WITHOUT CONSIDERATION FOR WHAT THE FILE CONTAINS.
# THEORETICALLY THERE SHOULD NOT BE ANY FILES WITH THE SAME NAME THAT ARE NOT A
# PRODUCT OF THE HMIS_CSV EXPORT PROCESS.  HOWEVER, THERE IS ALWAYS A CHANCE
# THAT SOMEONE HAS UNKNOWINGLY NAMED AN IMPORTANT, UNRELATED FILE "Client.csv"
# AND THIS SCRIPT WILL PERMANENTLY DELETE IT.

# ISSUE #2: FALSE NEGATIVES: ANYONE COULD CIRCUMVENT THE SECURITY MEASURES
# AFFORDED BY THIS LOGIC BY SIMPLY RENAMING THE "Client.csv" FILE TO SOME OTHER
# NAME SUCH AS "new_client.csv". THIS FILE WOULD NOT BE FOUND BY THIS SCRIPT,
# AND THUS PII WOULD REMAIN ON IN THE DIRECTORY.

# ISSUES #1 AND #2 CAN BE ADDRESSED WITH A REASONABLE LEVEL OF CERTAINITY BY
# ADDING ADDITIONAL LOGIC TO THIS CODE, BUT WE INTEND THIS TO BE A QUICK
# IMPLEMENTATION THAT CAN ALSO BE EASY TO READ AND COMPREHEND BY THE AUTHOR AND
# NON-AUTHORS ALIKE.  COMPLEXITY IS NOT A GOOD THING WHEN IT COMES TO FILE
# MANAGEMENT.

## ---- END OF OVERVIEW ---- ##

# Libraries----
library(dplyr)
library(readr)

rm(list = ls())
cat('\f')
gc()

# To Do----
# 1) Remove .7z files and .zip files from download folder

# Functions----

search.dirs <- c("C:/Users/TimBender/North Carolina Coalition to End Homelessness/PM Data Center - Documents/Reporting", 
                 "C:/Users/TimBender/Documents/R")

# Variables----

for(search.dir in search.dirs){
  search.filename <- "Client.csv"
  pii.colnames    <- c("FirstName",
                       "MiddleName",
                       "LastName",
                       "NameSuffix",
                       "SSN",
                       "DOB")
  
  # Logic----
  # convert filename to regex
  search.filename2 <-  paste("^", search.filename, "$", sep = "", collapse = "") %>%
    gsub(pattern = "\\.", replacement = "\\\\\\.")
  
  # find all instances of file
  all.files <- list.files(
    path = search.dir,
    pattern = search.filename2,
    recursive = T,
    full.names = F,
    include.dirs = T
  )
  
  all.files <-  paste(search.dir, all.files, sep = "/") %>%
    gsub(pattern = "\\/{2,}", "/", .)
  
  # loop through each filename
  
  for (i in all.files) {
    # read file
    temp <- read_csv(i)
    
    # loop through pii columns
    for (ic in pii.colnames) {
      # overwrite pii data with NA
      if (ic %in% colnames(temp)) {
        temp[, ic] <- NA
      }
    }
    
    # save as Client.csv to overwrite data
    write_csv(x      = temp,
              file   = i,
              append = F)
    
    
    # delete file(s)
    # <<< not yet added >>> ----
    
    # cleanup
    rm(temp)
  }
}



