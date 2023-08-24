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

# Funs----
con_weights <- function(w.in = mo.all$weights[sample(1:nrow(mo.all),size = 1)]){
  out.wts <- w.in %>%
    strsplit(x = .,
             split = "\\|") %>%
    unlist() %>%
    as.numeric()

  out <- data.frame(qnum = 1:length(out.wts),
                    weight = out.wts)

  return(out)
}

# import data----
mo.all <- read_csv("model_outputs2.csv") %>%
  .[!duplicated(.),]

# clean up duplicated simulations----


# narrow down duplicated weights to 1 weight <--> sim_fp combo 
mo.all <- mo.all %>%
  group_by(weights, sim_fp,) %>%
  summarise() %>%
  ungroup() %>%
  group_by(weights) %>%
  slice_sample(., n = 1) %>%
  inner_join(., mo.all) 


# get unweighted output----
mo.unw <- mo.all[mo.all$sim_fp == first(mo.all$sim_fp),] 

ag.fp <- mo.all %>%
  .[.$top20,] %>%
  group_by(sim_fp, Race) %>%
  summarise(n = n_distinct(client_id2)) %>%
  .[.$n == 8 & 
      .$Race == "Black",] %>%
  .$sim_fp %>% unique()

mo.allgoals <- mo.all[mo.all$sim_fp %in% ag.fp,] 

# # filter to last model written
# 
# mo <- mo.all[mo.all$sim_fp == last(mo.all$sim_fp),]
# 
# mo$weights %>%  unique()
# 
# 
# ggplot() + 
#   geom_boxplot(data = mutate(rbind(mutate(mo, top20 = F),mo[mo$top20,]),
#                              color1 = ifelse(top20, "Top 20% of Pop", 
#                                              "100% of Pop")), 
#                aes(x = Race, y = comp_score, #group = Race, 
#                    color = color1), 
#                position = "dodge")+
#   scale_color_discrete(name = "Population Category")+
#   labs(title = "Scores", 
#        subtitle = NULL)


out.unw <- mo.unw %>%
  group_by(top20 = ifelse(top20 == T,"top20", "top100"),Race, .drop = F) %>%
  summarise(n = n_distinct(client_id2)) %>%
  ungroup() %>%
  as.data.table() %>%
  dcast(., 
        Race ~ top20, 
        fill = 0) %>%
  as.data.frame() %>%
  mutate(makeup_top20 = top20 / sum(top20), 
         makeup_top100 = top100/ sum(top100)) 

colnames(out.unw) <- c("Race", "n_t100", "n_t20", 
                     "makeup_t20", "makeup_t100")
out.unw$which <- "unweighted"

library(glue)

print(out.unw)

# if(between(out.R[out.R$Race == "Black",]$makeup_t100 / 
#            out.R[out.R$Race == "Black",]$makeup_t20  ,0.97,1.07)){
#   stop("we got one - Race")
# }

select(out.unw, Race, makeup_t20, makeup_t100, which) %>%
  as.data.table() %>%
  melt(., id.vars = c("Race", "which")) %>%
  ggplot(data = ., 
         aes(x = Race, y = value, fill = variable)) + 
  geom_col(position = "dodge")+
  scale_y_continuous(labels = scales::percent, 
                     breaks = seq(0, 100, by = .10), 
                     limits = c(0,1))+
  labs(title = "Outcomes - Unweighted", 
       subtitle = glue("model fingerprint: {unique(mo.unw$sim_fp)}"))


#Sys.sleep(10)

# best solutions----
mo.allgoals$weights %>% unique %>% sort()
mo.allgoals <- mo.allgoals %>%
  .[!grepl("\\.", .$weights),] 

out.allgoals <- mo.allgoals %>%
  group_by(sim_fp, top20 = ifelse(top20 == T,"top20", "top100"),Race, .drop = F) %>%
  summarise(n = n_distinct(client_id2)) %>%
  ungroup() %>%
  as.data.table() %>%
  dcast(., 
        sim_fp + Race ~ top20, 
        fill = 0) %>%
  as.data.frame() %>%
  group_by(sim_fp) %>%
  mutate(makeup_top20 = top20 / sum(top20), 
         makeup_top100 = top100/ sum(top100)) 

colnames(out.allgoals) <- c("sim_fp", "Race", "n_t100", "n_t20", 
                       "makeup_t20", "makeup_t100")
out.allgoals$which <- out.allgoals$sim_fp

library(glue)

print(out.allgoals)

# if(between(out.R[out.R$Race == "Black",]$makeup_t100 / 
#            out.R[out.R$Race == "Black",]$makeup_t20  ,0.97,1.07)){
#   stop("we got one - Race")
# }


goal.fp <- out.allgoals[out.allgoals$Race == "Black" & 
               out.allgoals$makeup_t20 == 8/19,]$sim_fp

#not.fp <- out.allgoals[out.allgoals$n_t20 == 0,]$sim_fp
#goal.fp <- goal.fp[!goal.fp %in% not.fp]

out.allgoals %>%
  .[.$sim_fp %in% goal.fp,] %>%
  #.[!.$sim_fp %in% not.fp,]
  as.data.table() %>%
  dcast(., 
        sim_fp ~ Race, fill = 0, 
        value.var = "makeup_t20", 
        ) %>%
  .[.$sim_fp %in% goal.fp,]


out.allgoals %>%
  .[.$sim_fp %in% goal.fp,] %>%
  ggplot(data = ., 
         aes(x = Race, group = Race, y = makeup_t20)) + 
  #geom_jitter(height = 0) +
  geom_boxplot() +
  scale_y_continuous(breaks = seq(0,1,by=0.1), 
                     labels = scales::percent)


out.weights <- out.allgoals %>%
  .[.$sim_fp %in% goal.fp,] %>%
  left_join(., 
            summarise(group_by_all(select(mo.all, sim_fp, weights)))) %>%
  .[!duplicated(.),]



# get and assign random wts----
new.wts <- full_join(con_weights(sample(out.weights$weights, size = 1)), 
          select(read_csv('https://raw.githubusercontent.com/timbender-ncceh/ce_assessment/main/MASTER_cw_qshortname.csv'), 
                 short_name, qnum))

for(i in 1:nrow(new.wts)){
  assign(x = new.wts$short_name[i], 
         value = new.wts$weight[i])
}

override.weights <- T

