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
override.weights <- T

weight.min <- 1
weight.max <- 10

mc.weights <- read_csv("https://raw.githubusercontent.com/timbender-ncceh/ce_assessment/main/MASTER_cw_qshortname.csv")
mc.weights$weight <- sample(weight.min:weight.max, 
                            size = nrow(mc.weights), 
                            replace = T) %>%
  log()





for(i in 1:nrow(mc.weights)){
  assign(x = mc.weights$short_name[i], 
         value = mc.weights$weight[i])
}

# run model----

source("https://raw.githubusercontent.com/timbender-ncceh/ce_assessment/main/ce_assessment_micro2.R")
rm(override.weights)
