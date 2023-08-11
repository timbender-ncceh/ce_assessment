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
mo <- read_csv("model_outputs2.csv")


# filter to last model written
mo <- mo[mo$sim_fp == last(mo$sim_fp),]



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
