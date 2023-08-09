#vacations

library(dplyr)
library(readxlsb)
library(data.table)
library(ggplot2)

rm(list=ls());cat('\f')
#alt.wd <- "C:/Users/TimBender/North Carolina Coalition to End Homelessness/PM Data Center - Documents/Reporting/Reporting  PIT HIC/2022 PIT HIC"
prime.wd <- "C:/Users/TimBender/Documents/R/ncceh/projects/racial_equity_assessmet"


setwd(prime.wd)

# vars----
the.state <- "NC"
the.coc   <- 503



bos_coc.i <- paste(the.state, c(the.coc), sep = "-")

# the.tools <- c("CoC-Analysis-Tool-3.0.xlsb", 
#                #"CoC-Analysis-Tool-2.1.xlsb",
#                "CoC-Analysis-Tool-2.0.xlsb",
#                "CoC-Analysis-Tool-2018.xlsb")


the.tools2 <- rbind(data.table(version = c("1.0"), 
                               filename = c("CoC-Analysis-Tool-2018.xlsb"), 
                               the.worksheets = list(c("State ACS2015 Data", 
                                                       "CoC ACS2015 Data", 
                                                       "PIT - State", 
                                                       "PIT - CoC", 
                                                       "HOW TO USE THIS TOOL", 
                                                       "DASHBOARD", 
                                                       "METHODOLOGY",
                                                       #"NEXT STEPS", 
                                                       "Data Validations", 
                                                       "Codebook")), 
                               the.namedranges = list(c("'Data Validations'!CoCs", 
                                                        "CurrentYear", 
                                                        "DASHBOARD!Print_Area", 
                                                        "'HOW TO USE THIS TOOL'!Print_Area", 
                                                        "METHODOLOGY!Print_Area", 
                                                        "DASHBOARD!Selected_Coc"))),
                    data.table(version = c("2.1"), 
                               filename = c("CoC-Analysis-Tool-2.1.xlsb"), 
                               the.worksheets = list(c("State ACS2017 Data", 
                                                       "CoC ACS2017 Data", 
                                                       "PIT 2019 - State", 
                                                       "PIT 2019 - CoC", 
                                                       "HOW TO USE THIS TOOL", 
                                                       "DASHBOARD", 
                                                       "METHODOLOGY",
                                                       "NEXT STEPS", 
                                                       "Data Validations", 
                                                       "Codebook")), 
                               the.namedranges = list(c("'Data Validations'!CoCs", 
                                                        "CurrentYear", 
                                                        "DASHBOARD!Print_Area", 
                                                        "'HOW TO USE THIS TOOL'!Print_Area", 
                                                        "METHODOLOGY!Print_Area", 
                                                        "DASHBOARD!Selected_Coc"))),
                    data.table(version = c("3.0"), 
                               filename = c("CoC-Analysis-Tool-3.0.xlsb"), 
                               the.worksheets = list(c("State ACS2019 Data", 
                                                       "CoC ACS2019 Data", 
                                                       "PIT 2021 - State", 
                                                       "PIT 2021 - CoC", 
                                                       "HOW TO USE THIS TOOL", 
                                                       "DASHBOARD", 
                                                       "METHODOLOGY",
                                                       "NEXT STEPS", 
                                                       "Data Validations", 
                                                       "Codebook")), 
                               the.namedranges = list(c("'Data Validations'!CoCs", 
                                                        "CurrentYear", 
                                                        "DASHBOARD!Print_Area", 
                                                        "'HOW TO USE THIS TOOL'!Print_Area", 
                                                        "METHODOLOGY!Print_Area", 
                                                        "DASHBOARD!Selected_Coc"))))



out.state.acs <- NULL
out.state.pit <- NULL
out.acs <- NULL
out.pit <- NULL

for(i in 1:nrow(the.tools2)){
  print(i)
  # STATE ACS DATA
  state.acs <- the.tools2$the.worksheets[[i]] %>%
    grep(pattern = "ACS", x = ., value = T) %>%
    grep(pattern = "State", x = ., value = T)
  state.acs <- read_xlsb(path = the.tools2$filename[i], 
                         sheet = state.acs)
  
  # CoC ACS DATA
  coc.acs <- the.tools2$the.worksheets[[i]] %>%
    grep(pattern = "ACS", x = ., value = T) %>%
    grep(pattern = "CoC", x = ., value = T)
  coc.acs <- read_xlsb(path = the.tools2$filename[i], 
                         sheet = coc.acs)
  
  # State PIT Data
  state.pit <- the.tools2$the.worksheets[[i]] %>%
    grep(pattern = "PIT", x = ., value = T) %>%
    grep(pattern = "State", x = ., value = T)
  state.pit <- read_xlsb(path = the.tools2$filename[i], 
                         sheet = state.pit)
  # CoC PIT Data
  coc.pit <- the.tools2$the.worksheets[[i]] %>%
    grep(pattern = "PIT", x = ., value = T) %>%
    grep(pattern = "CoC", x = ., value = T)
  coc.pit <- read_xlsb(path = the.tools2$filename[i], 
                         sheet = coc.pit)
  
  # fiter coc & state
  tool.ver <- the.tools2$version[i]
  state.acs <- state.acs[state.acs$state %in% c("NC", "North Carolina"),]
  coc.acs <- coc.acs[coc.acs$cocnum %in% bos_coc.i,] 
  
  state.pit <- state.pit[state.pit$State %in% c("NC", "North Carolina"),]
  coc.pit <- coc.pit[coc.pit$CoC.Code %in% bos_coc.i,]
  
  
  
  library(data.table)
  # slide 11: BoS ----
  # total ppl population by race (ACS)
  out.state.acs <- rbind(out.state.acs, 
                         state.acs[,grep("^state_black_total$|^state_white_total$|^state_native_total$|^state_asian_total$|^state_other_total$|^tract_black|^bg_black$|^tract_white|^bg_white$|^tract_asian|^bg_asian$|^tract_other|^bg_other$|^tract_native|^bg_native$", 
                                         colnames(state.acs), value = T)] %>%
                           as.data.table() %>%
                           melt(.) %>%
                           cbind(., data.table(ver = tool.ver, 
                                               geo = "NC", 
                                               var2 = "total pop by race (ACS)")))
  
  
  out.acs <- rbind(out.acs, 
               coc.acs[,grep("^tract_black|^bg_black$|^tract_white|^bg_white$|^tract_asian|^bg_asian$|^tract_other|^bg_other$|^tract_native|^bg_native$", 
                colnames(coc.acs), value = T)] %>%
    as.data.table() %>%
    melt(.) %>%
    cbind(., data.table(ver = tool.ver, 
                        geo = bos_coc.i, 
                        var2 = "total pop by race (ACS)")))
  
  # ppl in poverty by race (ACS)
  
  out.state.acs <- rbind(out.state.acs, 
                         state.acs[,grep("^state_black_pov$|^state_white_pov$|^state_native_pov$|^state_asian_pov$|^state_other_pov$|^tract_pov_bla|^tract_pov_whi|^tract_pov_asi|^tract_pov_oth|^tract_pov_nat|^bg_black_pov$|^bg_white_pov$|^bg_other_pov$|^bg_native_pov$|^bg_asian_pov$", 
                                         colnames(state.acs), value = T)] %>%
                           as.data.table() %>%
                           melt(.) %>%
                           cbind(., data.table(ver = tool.ver, 
                                               geo = "NC", 
                                               var2 = "poverty by race (ACS)")))
  
  out.acs <- rbind(out.acs, 
               coc.acs[,grep("^tract_pov_bla|^tract_pov_whi|^tract_pov_asi|^tract_pov_oth|^tract_pov_nat|^bg_black_pov$|^bg_white_pov$|^bg_other_pov$|^bg_native_pov$|^bg_asian_pov$", 
                             colnames(coc.acs), value = T)] %>%
                 as.data.table() %>%
                 melt(.) %>%
                 cbind(., data.table(ver = tool.ver, 
                                     geo = bos_coc.i, 
                                     var2 = "poverty by race (ACS)")))
  
  # ppl experiencing homelessness by race (PIT)
  out.state.pit <- rbind(out.state.pit, 
                         state.pit[,grep("\\.persons\\.in\\.families|\\.persons\\.in\\.households", 
                                         colnames(state.pit), ignore.case = T, value = T)] %>%
                           as.data.table() %>%
                           melt(.) %>%
                           cbind(., data.table(ver = tool.ver, 
                                               geo = "NC", 
                                               var2 = "people experiencing homelessness by race (PIT)")))
  
  
  out.pit <- rbind(out.pit, 
               coc.pit[,grep("^native.american.alaskan.persons.in.households..\\d{4,4}$|^other.multi.racial.persons.in.households..\\d{4,4}$|^asian.pacific.islander.persons.in.households..\\d{4,4}$|^white.persons.in.households..\\d{4,4}$|^black.persons.in.households..\\d{4,4}$", colnames(coc.pit), ignore.case = T, value = T)] %>%
    as.data.table() %>%
    melt(.) %>%
    cbind(., data.table(ver = tool.ver, 
                        geo = bos_coc.i, 
                        var2 = "people experiencing homelessness by race (PIT)")))
  
  # families experiencing homelessness by race (PIT)
  out.pit <- rbind(out.pit, 
               coc.pit[,grep("^native.american.alaskan.persons.in.families..\\d{4,4}$|^other.multi.racial.persons.in.families..\\d{4,4}$|^asian.pacific.islander.persons.in.families..\\d{4,4}$|^white.persons.in.families..\\d{4,4}$|^black.persons.in.families..\\d{4,4}$", colnames(coc.pit), ignore.case = T, value = T)] %>%
    as.data.table() %>%
    melt(.) %>%
    cbind(., data.table(ver = tool.ver, 
                        geo = bos_coc.i, 
                        var2 = "people experiencing homelessness by race (PIT)")))
  
  coc.pit[,colnames(coc.pit) %>%
    grep("^hispanic|^non.", ., T, value = T) %>%
    grep("persons", ., T, value = T) %>%
    grep("in.families|in.households", ., T, value = T)]
}

# # check that all is done
# out.pit
# out.acs
# out.state.pit
# out.state.acs

# race 
out.pit$race <- out.pit$variable %>%
  gsub(pattern = "\\.persons.in.households..\\d{4,4}$|\\.persons.in.families..\\d{4,4}$", "", ., ignore.case = T) %>%
  gsub("\\.", " ", .) %>%
  tolower()
out.acs$race <- out.acs$variable %>% gsub(pattern = "^bg_|^tract_|pov_|_pov", "", x = .) %>%
  tolower()

out.state.pit$race <- out.state.pit$variable %>%
  as.character() %>%
  gsub(pattern = "\\.persons\\.in\\..*$", 
       replacement = "", x = ., ignore.case = T) %>%
  gsub("\\.", " ", .) %>%
  tolower()

out.state.acs$race <- out.state.acs$variable %>%
  as.character() %>%
  gsub(pattern = "^state_|^tract_|_total$|_{0,1}pov_{0,1}", "", ., ignore.case = T) 


# tidy2----

out.pit$var2 <- gsub(pattern = "people", "people in households", x = out.pit$var2)
out.pit$var2[out.pit$variable %in% 
               grep("in\\.families", 
                    out.pit$variable, 
                    ignore.case = T, value = T)] <- gsub("households", "families", 
                                                         x = out.pit$var2[out.pit$variable %in% 
                                                                            grep("in\\.families",
                                                                                 out.pit$variable, 
                                                                                 ignore.case = T, value = T)])
out.pit$var3 <- out.pit$var2 %>%
  gsub(pattern = "experiencing.*$", "", 
       .) %>% 
  trimws()
out.pit$source <- gsub(pattern = "^.*\\(|\\).*$", "", out.pit$var2)
out.pit$year <- gsub(pattern = "^.*\\.{2,2}", "", out.pit$variable) %>%
  as.numeric()

out.pit <- out.pit %>%
  group_by(year, geo, race, var3, ver, source, 
           value) %>%
  summarise(n = n())

out.state.pit <- out.state.pit %>%
  left_join(., 
            data.frame(ver = c("1.0", "2.1", "3.0"), 
                       year = c(2019, 2017, 2015))) 
out.state.pit$var3 <- out.state.pit$variable %>% 
  as.character()  %>%
  gsub(pattern = "^.*persons\\.in", "persons in", ., ignore.case = T) %>% 
  gsub(pattern = "\\.{2,2}.*$", "", .) %>%
  gsub("\\.| {1,}", " ", .) %>% 
  tolower() %>%
  trimws()

out.state.pit <- out.state.pit %>%
group_by(year, 
           geo,
           race, 
           var3, 
           ver, 
           source = "PIT", 
         value) %>%
  summarise(n = n())

# acs

out.acs <- left_join(out.acs, 
          data.frame(ver = c("1.0", "2.1", "3.0"), 
                     year = c(2019, 2017, 2015)))

out.acs$var3 <- gsub(" (ACS).*$", "", out.acs$var2)


out.acs <- out.acs %>%
  group_by(year, 
           geo, 
           race, 
           var3, 
           ver, 
           source = "ACS", 
           value) %>%
  summarise(n = n())


out.state.acs <- out.state.acs %>%
  left_join(., 
            data.frame(ver = c("1.0", "2.1", "3.0"), 
                       year = c(2019, 2017, 2015)))

out.state.acs$var3 <- gsub("\\(ACS\\)", "", out.state.acs$var2) %>%
  trimws()

out.state.acs <- out.state.acs %>%
  group_by(year, 
           geo, 
           race, 
           var3, 
           ver, 
           source = "ACS", 
           value) %>%
  summarise(n = n())


out.acs
out.pit
out.state.acs
out.state.pit


master.out <- rbind(out.acs, out.state.acs, out.pit, out.state.pit) %>%
  group_by_all() %>%
  summarise(n1 = n())

master.out$race2 <- master.out$race
master.out$race2[master.out$race2 == "asian"] <- "asian pacific islander"
master.out$race2[master.out$race2 == "native"] <- "native american alaskan"
master.out$race2[master.out$race2 == "other"] <- "other multi racial"


write_csv(x = master.out, "master_3year.csv")

# older----
for(bos_coc in bos_coc.i){
  # import data----
  
  the.file <- "CoC-Analysis-Tool-3.0.xlsb"
  
  the.worksheets <- c("State ACS2019 Data", 
                      "CoC ACS2019 Data", 
                      "PIT 2021 - State", 
                      "PIT 2021 - CoC", 
                      "HOW TO USE THIS TOOL", 
                      "DASHBOARD", 
                      "METHODOLOGY",
                      "NEXT STEPS", 
                      "Data Validations", 
                      "Codebook")
  the.namedranges <- c("'Data Validations'!CoCs", 
                       "CurrentYear", 
                       "DASHBOARD!Print_Area", 
                       "'HOW TO USE THIS TOOL'!Print_Area", 
                       "METHODOLOGY!Print_Area", 
                       "DASHBOARD!Selected_Coc")
  
  
  
  query_tool <- function(sheet1 = NA, 
                         coc = bos_coc, 
                         file1 = "CoC-Analysis-Tool-3.0.xlsb"){
    # pull data
    out <- read_xlsb(path = file1, 
                     sheet = sheet1) %>%
      as.data.table()
    # filter coc
    out <- out[out$CoC.Code %in% coc,]
    
    
    # tidying
    if(sheet1 %in% c("PIT 2021 - CoC")){
      out <- out %>% 
        melt(., id.vars = c("CoC.Code", "CoC.Name", "Populations", 
                            "HAS.UNSHELTERED.RACE.DATA.FOR.all", 
                            "HAS.UNSHELTERED.RACE.DATA.FOR.fam", 
                            "HAS.UNSHELTERED.RACE.DATA.FOR.upyp", 
                            "HAS.UNSHELTERED.RACE.DATA.FOR.pyp", 
                            "HAS.UNSHELTERED.RACE.DATA.FOR.vet")) %>%
        group_by(CoC.Code, variable, value) %>%
        summarise()
    }
    
    # return
    return(out)
    
  }
  
  
  # RE slide # 15----
  # families by race
  census.all.fams.by.race <- read_xlsb(sheet = "CoC ACS2019 Data", 
                                       path = "CoC-Analysis-Tool-3.0.xlsb") %>%
    .[.$cocnum %in% bos_coc,] %>% 
    as.data.table() %>% 
    melt(., id.vars = c("cocnum", "state"))
  
  census.all.fams.by.race$race <- NA
  census.all.fams.by.race$race[grepl("_white$", census.all.fams.by.race$variable)] <- "white"
  census.all.fams.by.race$race[grepl("_black$", census.all.fams.by.race$variable)] <- "black"
  census.all.fams.by.race$race[grepl("_native$", census.all.fams.by.race$variable)] <- "native"
  census.all.fams.by.race$race[grepl("_asian$", census.all.fams.by.race$variable)] <- "asian"
  census.all.fams.by.race$race[grepl("_other$", census.all.fams.by.race$variable)] <- "other"
  census.all.fams.by.race$race[grepl("_hisp$", census.all.fams.by.race$variable)] <- "hisp"
  
  census.all.fams.by.race$ALL   <- grepl("tract_white|tract_black|tract_native|tract_asian|tract_other|tract_hisp", 
                                         census.all.fams.by.race$variable)
  census.all.fams.by.race$fam   <- grepl("_fam_", census.all.fams.by.race$variable)
  census.all.fams.by.race$pov   <- grepl("_pov_", census.all.fams.by.race$variable)
  census.all.fams.by.race$youth <- grepl("_youth_", census.all.fams.by.race$variable)
  census.all.fams.by.race$vet   <- grepl("_vet_", census.all.fams.by.race$variable)
  
  
  census.all.fams.by.race <- census.all.fams.by.race %>%
    .[.$fam == F & 
        .$pov == F & 
        .$youth == F & 
        .$vet == F, ] 
  
  colnames(census.all.fams.by.race) <- c("cocnum", "State", "variable", "value", 
                                         "Race", "total", "Families", "Families in Poverty", 
                                         "Youth", "Veterans")
  
  
  census.all.fams.by.race$pct_tot <- 0
  census.all.fams.by.race$pct_tot <- census.all.fams.by.race$value / 
    sum(census.all.fams.by.race$value)
  
  
  census.all.fams.by.race <- census.all.fams.by.race[,c(1,2,3,4,5,11)]
  census.all.fams.by.race$type <- "All ACS"
  
  write_csv(x = census.all.fams.by.race, 
            file = paste("andrea_all", bos_coc, ".csv", sep = "", collapse = ""))
  
  library(ggplot2)
  
  census.pov.fams.by.race <- read_xlsb(sheet = "CoC ACS2019 Data", 
                                       path = "CoC-Analysis-Tool-3.0.xlsb") %>%
    .[.$cocnum %in% bos_coc,] %>% 
    as.data.table() %>% 
    melt(., id.vars = c("cocnum", "state"))
  
  census.pov.fams.by.race$race <- NA
  census.pov.fams.by.race$race[grepl("_white$", census.pov.fams.by.race$variable)] <- "white"
  census.pov.fams.by.race$race[grepl("_black$", census.pov.fams.by.race$variable)] <- "black"
  census.pov.fams.by.race$race[grepl("_native$", census.pov.fams.by.race$variable)] <- "native"
  census.pov.fams.by.race$race[grepl("_asian$", census.pov.fams.by.race$variable)] <- "asian"
  census.pov.fams.by.race$race[grepl("_other$", census.pov.fams.by.race$variable)] <- "other"
  census.pov.fams.by.race$race[grepl("_hisp$", census.pov.fams.by.race$variable)] <- "hisp"
  
  census.pov.fams.by.race$ALL   <- grepl("tract_white|tract_black|tract_native|tract_asian|tract_other|tract_hisp", 
                                         census.pov.fams.by.race$variable)
  census.pov.fams.by.race$fam   <- grepl("_fam_", census.pov.fams.by.race$variable)
  census.pov.fams.by.race$pov   <- grepl("_pov_", census.pov.fams.by.race$variable)
  census.pov.fams.by.race$youth <- grepl("_youth_", census.pov.fams.by.race$variable)
  census.pov.fams.by.race$vet   <- grepl("_vet_", census.pov.fams.by.race$variable)
  
  
  census.pov.fams.by.race <- census.pov.fams.by.race %>%
    .[.$fam == F & 
        .$pov == T & 
        .$youth == F & 
        .$vet == F,] 
  
  colnames(census.pov.fams.by.race) <- c("cocnum", "State", "variable", "value", 
                                         "Race", "total", "Families", "Families in Poverty", 
                                         "Youth", "Veterans")
  
  
  census.pov.fams.by.race$pct_tot <- 0
  census.pov.fams.by.race$pct_tot <- 
    census.pov.fams.by.race$value /
    sum(census.pov.fams.by.race$value)
  
  
  census.pov.fams.by.race <- census.pov.fams.by.race[,c(1,2,3,4,5,11)]
  census.pov.fams.by.race$type <- "in poverty ACS"
  
  write_csv(x = census.pov.fams.by.race, 
            file = paste("andrea_poverty", bos_coc, ".csv", sep = "", collapse = ""))
  
  # pit
  pit.fams.by.race        <- read_xlsb(sheet = "PIT 2021 - CoC", 
                                       path = "CoC-Analysis-Tool-3.0.xlsb") %>% 
    .[.$CoC.Code %in% bos_coc,] %>% 
    as.data.table() %>% 
    melt(., id.vars = c("CoC.Code", 
                        "CoC.Name", "Populations")) %>% as.data.frame() %>%
    .[!grepl("^SHELTERED|^UNSHELTERED", .$variable, ),] %>%
    .[grepl("PERSONS.IN.HOUSEHOLDS..", .$variable),]
  #pit.fams.by.race$ethnicity <- !grepl("HISPANIC", pit.fams.by.race$variable)
  
  pit.fams.by.race$Race <- NA
  pit.fams.by.race$Race[grepl("BLACK", pit.fams.by.race$variable)] <- "black"
  pit.fams.by.race$Race[grepl("WHITE", pit.fams.by.race$variable)] <- "white"
  pit.fams.by.race$Race[grepl("ASIAN.PACIFIC.ISLANDER", pit.fams.by.race$variable)] <- "asian"
  pit.fams.by.race$Race[grepl("NATIVE.AMERICAN.ALASKAN", pit.fams.by.race$variable)] <- "native"
  pit.fams.by.race$Race[grepl("OTHER.MULTI.RACIAL", pit.fams.by.race$variable)] <- "other"
  pit.fams.by.race$Race[grepl("^HISPANIC", pit.fams.by.race$variable)] <- "hispanic"
  pit.fams.by.race$Race[grepl("^NON.HISPANIC", pit.fams.by.race$variable)] <- "non-hispanic"
  pit.fams.by.race$value <- as.numeric(pit.fams.by.race$value)
  
  pit.fams.by.race <- pit.fams.by.race[!is.na(pit.fams.by.race$Race),]
  
  pit.fams.by.race$pct_tot <- pit.fams.by.race$value / sum(pit.fams.by.race$value)
  
  
  write_csv(x = pit.fams.by.race, 
            file = paste("andrea_pit", bos_coc, ".csv", sep = "", collapse = ""))
  
  
}

andrea2all <- NULL
for(i in list.files(pattern = "^andrea_all")){
  andrea2all <- rbind(andrea2all, 
                      read_csv(i))
}
write_csv(andrea2all, "andrea2_all.csv")

andrea2pit <- NULL
for(i in list.files(pattern = "^andrea_pit")){
  andrea2pit <- rbind(andrea2pit, 
                      read_csv(i))
}
write_csv(andrea2pit, "andrea2_pit.csv")


andrea2pov <- NULL
for(i in list.files(pattern = "^andrea_pov")){
  andrea2pov <- rbind(andrea2pov, 
                      read_csv(i))
}
write_csv(andrea2pov, "andrea2_pov.csv")


ggplot() + 
  geom_col(data = census.all.fams.by.race, 
           aes(y = "Census - Families", x = pct_tot, fill = Race))+
  geom_col(data = census.pov.fams.by.race, 
           aes(y = "In Poverty - Families", x = pct_tot, fill = Race))+
  geom_col(data = pit.fams.by.race, 
           aes(y = "PIT - Families", x = pct_tot, fill = Race))+
  theme(legend.position = "right")+
  scale_x_continuous(labels=scales::percent)+
  labs(title = "Families Experiencing Homelessness", 
       subtitle = bos_coc)

# RE slide # 16----
# families by ethnicity
census.all.fams.by.race$ethnicity <- ifelse(census.all.fams.by.race$Race == "hisp", 
                                            "Hispanic", "Non-Hispanic")
census.all.fams.by.race$pct_e <- census.all.fams.by.race$value / sum(census.all.fams.by.race$value)
census.pov.fams.by.race$ethnicity <- ifelse(census.pov.fams.by.race$Race == "hisp", 
                                            "Hispanic", "Non-Hispanic")
census.pov.fams.by.race$pct_e <- census.pov.fams.by.race$value / sum(census.pov.fams.by.race$value)

pit.fams.by.race        <- read_xlsb(sheet = "PIT 2021 - CoC", 
                                     path = "CoC-Analysis-Tool-3.0.xlsb") %>% 
  .[.$CoC.Code %in% bos_coc,] %>% 
  as.data.table() %>% 
  melt(., id.vars = c("CoC.Code", 
                      "CoC.Name")) %>% as.data.frame() %>%
  .[!grepl("^SHELTERED|^UNSHELTERED", .$variable, ),] %>%
  .[grepl("PERSONS.IN.FAMILIES..", .$variable),]

pit.fams.by.race$value <- as.numeric(pit.fams.by.race$value)

pit.fams.by.race$ethnicity <- NA
pit.fams.by.race$ethnicity[grepl("^HISPANIC", pit.fams.by.race$variable)] <- "Hispanic"
pit.fams.by.race$ethnicity[grepl("^NON.HISPANIC", pit.fams.by.race$variable)] <- "Non-Hispanic"
pit.fams.by.race <- pit.fams.by.race[!is.na(pit.fams.by.race$ethnicity),]
pit.fams.by.race$pct_e <- pit.fams.by.race$value / sum(pit.fams.by.race$pct_e <- pit.fams.by.race$value)


ggplot() + 
  geom_col(data = census.all.fams.by.race, 
           aes(y = "Census - Families", x = pct_e, fill = ethnicity))+
  geom_col(data = census.pov.fams.by.race, 
           aes(y = "In Poverty - Families", x = pct_e, fill = ethnicity))+
  geom_col(data = pit.fams.by.race, 
           aes(y = "PIT - Families", x = pct_e, fill = ethnicity))+
  theme(legend.position = "right")+
  scale_x_continuous(labels=scales::percent)+
  labs(title = "Families Experiencing Homelessness")


# pit coc----
pic_coc <- query_tool("PIT 2021 - CoC") %>% as.data.frame()
pic_coc$variable <- as.character(pic_coc$variable)

pic_coc$sheltered_type <- NA
pic_coc$sheltered_type[grepl("^SHELTERED", pic_coc$variable)] <- "sheltered"
pic_coc$sheltered_type[grepl("^UNSHELTERED", pic_coc$variable)] <- "unsheltered"

pic_coc$persons_in <- NA
pic_coc$persons_in[grepl(".PERSONS.IN.YOUTH.HOUSEHOLDS", pic_coc$variable)] <- "youth households"
pic_coc$persons_in[grepl(".PERSONS.IN.YOUTH.FAMILIES", pic_coc$variable)] <- "youth families"
pic_coc$persons_in[grepl(".PERSONS.IN.VETERAN.HOUSEHOLDS", pic_coc$variable)] <- "veteran households"
pic_coc$persons_in[grepl(".PERSONS.IN.VETERAN.FAMILIES", pic_coc$variable)] <- "veteran families"
pic_coc$persons_in[grepl(".PERSONS.IN.HOUSEHOLDS", pic_coc$variable)] <- "households"
pic_coc$persons_in[grepl(".PERSONS.IN.FAMILIES", pic_coc$variable)] <- "families"


pic_coc %>%
  group_by(sheltered_type, 
           persons_in, 
           #has_na_val = is.na(value)
  ) %>%
  summarise(sumval = sum(value, na.rm = T))


# older----
state_acs19_data <- read_xlsb(path = the.file, 
                              sheet = "State ACS2019 Data") %>%
  as.data.table()

coc_acs19_data <- read_xlsb(path = the.file, 
                            sheet = the.worksheets[2]) %>%
  as.data.table()

pit_2021_state <- read_xlsb(path = the.file, 
                            sheet = the.worksheets[3]) %>%
  as.data.table()
pit_2021_coc <- read_xlsb(path = the.file, 
                          sheet = the.worksheets[4]) %>%
  as.data.table()


dashboard <- read_xlsb(path = the.file, 
                       sheet = the.worksheets[6]) %>%
  as.data.table()
next_steps <- read_xlsb(path = the.file, 
                        sheet = the.worksheets[8]) %>%
  as.data.table()
data_validations <- read_xlsb(path = the.file, 
                              sheet = the.worksheets[9]) %>%
  as.data.table()
codebook <- read_xlsb(path = the.file, 
                      sheet = the.worksheets[10]) %>%
  as.data.table()

rm(dashboard)

# Filter data----

coc_acs19_data[coc_acs19_data$cocnum %in% bos_coc,] %>% t()
codebook
data_validations[data_validations$CoC.IDs %in% bos_coc,] %>% t()
t(pit_2021_coc[pit_2021_coc$CoC.Code %in% bos_coc,]) 

t(pit_2021_state[pit_2021_state$State == "NC",])
state_acs19_data[state_acs19_data$state == "NC",] %>% t()


# output data

name.heirarchy <- names(coc_acs19_data) %>% 
  strsplit(., "_")

name.heirarchy2 <- NULL
for(i in 1:length(name.heirarchy)){
  
  #name.heirarchy[[i]]
  name.heirarchy2 <- rbind(name.heirarchy2, 
                           data.frame(nth = i, 
                                      level = 1:length(name.heirarchy[[i]]), 
                                      level.name = name.heirarchy[[i]]))
}

name.heirarchy3 <- name.heirarchy2 %>%
  as.data.table() %>%
  dcast(., nth~level) %>%
  as.data.frame()

name.heirarchy3 <- name.heirarchy3[name.heirarchy3$`1` %in% c("tract"),]

name.heirarchy3

for(ab in 1:2 ){
  for(i in 1:nrow(name.heirarchy3)){
    for(r1 in ncol(name.heirarchy3[i,]):4){
      if(is.na(name.heirarchy3[i,r1])){
        name.heirarchy3[i,r1] <- name.heirarchy3[i,r1-1]
      }
    }
  }
}

for(ab in 1:2){
  for(i in 1:nrow(name.heirarchy3)){
    # 3 vs 4
    if(name.heirarchy3[i,3] == name.heirarchy3[i,4]){
      name.heirarchy3[i,3] <- "-"
    }
    # 4 vs 5
    if(name.heirarchy3[i,4] == name.heirarchy3[i,5]){
      name.heirarchy3[i,4] <- "-"
    }
  }
}


name.heirarchy3$col_name <- names(coc_acs19_data)[3:length(names(coc_acs19_data))]

coc_acs19_data %>%
  melt(., 
       id.vars = c("cocnum", "state"))

