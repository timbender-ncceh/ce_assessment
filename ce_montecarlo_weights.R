#ce_montecarlo_weights

library(dplyr)
library(readr)

setwd("C:/Users/TimBender/Documents/R/ncceh/projects/ce_assessment")

rm(list=ls());cat('\f');gc()


# fingerprints----
sim.fingerprint <- openssl::md5(as.character(Sys.time())) %>%
  substr(., nchar(.) - 7, nchar(.))

git.raw.md5 <- read_file("https://raw.githubusercontent.com/timbender-ncceh/ncceh_data_tools/main/ce_assessment_micro.R") %>%
  md5
gc()

# set weights----

# run model----
source("https://raw.githubusercontent.com/timbender-ncceh/ce_assessment/main/ce_assessment_micro2.R")
