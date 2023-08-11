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


# import data----
mo.all <- read_csv("model_outputs2.csv") %>%
  .[!duplicated(.),]


# filter to last model written
for(i in unique(mo.all$sim_fp)){
  mo <- mo.all[mo.all$sim_fp == i,]
  
  mo$weights %>%  unique()
  
  
  ggplot() + 
    geom_boxplot(data = mutate(rbind(mutate(mo, top20 = F),mo[mo$top20,]),
                               color1 = ifelse(top20, "Top 20% of Pop", 
                                               "100% of Pop")), 
                 aes(x = Race, y = comp_score, #group = Race, 
                     color = color1), 
                 position = "dodge")+
    scale_color_discrete(name = "Population Category")+
    labs(title = "Unweighted Baseline Scores", 
         subtitle = NULL)
  
  
  out.R <- mo %>%
    group_by(top20,Race) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    as.data.table() %>%
    dcast(., 
          Race ~ top20, 
          fill = 0) %>%
    as.data.frame() %>%
    mutate(makeup_top20 = `TRUE` / sum(`TRUE`), 
           makeup_top100 = `FALSE` / sum(`FALSE`))
  
  colnames(out.R) <- c("Race", "n_t100", "n_t20", 
                       "makeup_t20", "makeup_t100")
  library(glue)
  
  print(out.R)
  
  if(between(out.R[out.R$Race == "Black",]$makeup_t100 / 
             out.R[out.R$Race == "Black",]$makeup_t20  ,0.97,1.07)){
    stop("we got one - Race")
  }
  
  select(out.R, Race, makeup_t20, makeup_t100) %>%
    as.data.table() %>%
    melt(., id.vars = "Race") %>%
    ggplot(data = ., 
           aes(x = Race, y = value, fill = variable)) + 
    geom_col(position = "dodge")+
    scale_y_continuous(labels = scales::percent, 
                       breaks = seq(0, 100, by = .10))+
    labs(title = "Unweighted Outcomes", 
         subtitle = glue("model fingerprint: {unique(mo$sim_fp)}"))
  
  
  out.E <- mo %>%
    group_by(top20,Ethnicty) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    as.data.table() %>%
    dcast(., 
          Ethnicty ~ top20, 
          fill = 0) %>%
    as.data.frame() %>%
    mutate(makeup_top20 = `TRUE` / sum(`TRUE`), 
           makeup_top100 = `FALSE` / sum(`FALSE`))
  
  colnames(out.E) <- c("Ethnicty", "n_t100", "n_t20", 
                       "makeup_t20", "makeup_t100")
  library(glue)
  
  print(out.E)
  
  if(between(out.E[out.E$Ethnicty == "Hispanic",]$makeup_t100 / 
             out.E[out.E$Ethnicty == "Hispanic",]$makeup_t20  ,0.97,1.07)){
    stop("we got one - Race")
  }
  
  
  select(out.E, Ethnicty, makeup_t20, makeup_t100) %>%
    as.data.table() %>%
    melt(., id.vars = "Ethnicty") %>%
    ggplot(data = ., 
           aes(x = Ethnicty, y = value, fill = variable)) + 
    geom_col(position = "dodge")+
    scale_y_continuous(labels = scales::percent, 
                       breaks = seq(0, 100, by = .10))+
    labs(title = "Unweighted Outcomes", 
         subtitle = glue("model fingerprint: {unique(mo$sim_fp)}"))
  
  mo$sim_fp %>% unique
}
mo.all %>%
  group_by(sim_fp,top20,Race) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  group_by(sim_fp) %>%
  as.data.table() %>%
  dcast(., 
        sim_fp + Race ~ top20, 
        fill = 0) %>%
  as.data.frame() %>%
  mutate(makeup_top20 = `TRUE` / sum(`TRUE`), 
         makeup_top100 = `FALSE` / sum(`FALSE`)) %>%
  .[.$Race == "Black",] %>%
  mutate(., 
         t20divbyt100 = makeup_top20/makeup_top100)

out10 <- mo.all %>%
  group_by(sim_fp,top20,Race) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  group_by(sim_fp) %>%
  as.data.table() %>%
  dcast(., 
        sim_fp + Race ~ top20, 
        fill = 0)
colnames(out10) <- c("sim_fp", "Race", "bottom80", "top20")
out10$sim_fp <- factor(out10$sim_fp)


out10 <- as.data.frame(out10) %>% ungroup() 

for(i in 1:nrow(out10)){
  out10$race100[i]      <- out10$bottom80[i] + out10$top20[i]
  out10$pctR_in_t20[i]  <- out10$top20[i]    / sum(out10$top20[out10$sim_fp == out10$sim_fp[i]])
  out10$pctR_in_t100[i] <- out10$race100[i]  / sum(out10$race100[out10$sim_fp == out10$sim_fp[i]])
}

out10[out10$Race=="Black",] %>%
  ggplot(data = ., 
         aes(x = Race, y = pctR_in_t20, 
             group = Race)) + 
  geom_violin()+
  geom_jitter(height = 0)+
  scale_y_continuous(limits = c(0,1), 
                     breaks = seq(0,1,by=0.1), 
                     labels = scales::percent) +
  geom_hline(aes(yintercept = 0.43), 
             linewidth = 5, 
             alpha = 0.3, 
             color = "orange")


out10[between(out10$pctR_in_t20,0.42,0.44),] 

scales::percent(7:9/39)
97*.2 * .43

scales::percent((0:19)/19)

