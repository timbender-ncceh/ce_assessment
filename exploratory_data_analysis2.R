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


# filter to last model written

  mo <- mo.all[mo.all$sim_fp == last(mo.all$sim_fp),]
  
  mo$weights %>%  unique()
  
  
  ggplot() + 
    geom_boxplot(data = mutate(rbind(mutate(mo, top20 = F),mo[mo$top20,]),
                               color1 = ifelse(top20, "Top 20% of Pop", 
                                               "100% of Pop")), 
                 aes(x = Race, y = comp_score, #group = Race, 
                     color = color1), 
                 position = "dodge")+
    scale_color_discrete(name = "Population Category")+
    labs(title = "Scores", 
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
    labs(title = "Outcomes", 
         subtitle = glue("model fingerprint: {unique(mo$sim_fp)}"))
  

Sys.sleep(10)

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
#}
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

mo.all$weights %>% unique() %>% sort

out10 <- mo.all %>%
  .[.$weights != "1|1|1|1|1|1|1|1|1|1|1|1|1|1|1|1|1|1",] %>%
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


best.sims <- out10[between(out10$pctR_in_t20,0.42,0.44),]$sim_fp

scales::percent(7:9/39)
97*.2 * .43

scales::percent((0:19)/19)


# analyze all----
mq <- read_csv("https://raw.githubusercontent.com/timbender-ncceh/ce_assessment/main/MASTER_crosswalk_vuln.csv")
cw_sn <- read_csv("https://raw.githubusercontent.com/timbender-ncceh/ce_assessment/main/MASTER_cw_qshortname.csv")


in_range_scores <- NULL
for(i in best.sims){
  temp <- con_weights(w.in = unique(mo.all$weights[mo.all$sim_fp == i])) %>%
    mutate(., 
           fp = i)
  in_range_scores <- rbind(in_range_scores, 
                           temp)
  rm(temp)
}

# sort into log() weights and not log() weights

in_range_scores$weights_type <- ifelse(round(in_range_scores$weight,0) == 
                                        in_range_scores$weight, 
                                      "1:10", 
                                      "log(1:10)")

in_range_scores$weight %>% unique()

in_range_scores %>%
  left_join(., cw_sn) %>% 
  left_join(., select(mq, vuln_group, qnum)) %>%
  as_tibble() %>%
  .[.$weights_type == "1:10",] %>%
  ggplot(data = ., aes(x = short_name, 
                       y = factor(weight, levels = as.character(0:10)))) + 
  geom_bin2d()+
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1, vjust = 1))+
  scale_fill_viridis_c(option = "C")+
  facet_grid(~vuln_group, 
             scales = "free", space = "free")

in_range_scores %>%
  left_join(., cw_sn) %>% 
  left_join(., select(mq, vuln_group, qnum)) %>%
  as_tibble() %>%
  .[.$weights_type == "1:10",] %>%
  ggplot(data = ., aes(x = short_name, 
                       y = weight, 
                       group = short_name)) + 
  geom_boxplot() +
  #geom_jitter(height = 0)+
  facet_grid(~vuln_group, 
             scales = "free", space = "free")+
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1, vjust = 1))
  


# sd analysis----

sd.plot <- in_range_scores %>% 
  .[.$weights_type == "1:10",] %>%
  left_join(.,cw_sn) %>% 
  left_join(., mq) %>% as_tibble() %>%
  group_by(qnum, short_name, vuln_group) %>%
  summarise(avg_w = mean(weight), 
            sd_w = sd(weight)) 
sd.plot$short_name_f <- factor(sd.plot$short_name, 
                               levels = unique(sd.plot$short_name[order(sd.plot$sd_w)]))

sd.plot$vuln_group_f <- factor(sd.plot$vuln_group, 
                               levels = unique(sd.plot$vuln_group[order(sd.plot$sd_w)]))

ggplot(data = sd.plot, 
         aes(x = short_name_f, y = sd_w, 
             fill = avg_w)) + 
  scale_fill_viridis_c(option = "D", 
                       limits = c(NA,NA), 
                       name = "Average\nWeight")+
  scale_y_continuous(name = "Standard Deviation of Weight") +
  scale_x_discrete(name = "Questions\nFrom Least Deviation of Average to Most")+
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(title = "Simulations that Achieved 42% of 20% Goal: Output Weights.", 
         subtitle = "Illustrating the Variability of Weights (Standard Deviation) and Average Weight by Question.")+
  facet_grid(~vuln_group_f, scales = "free", space = "free")

ggplot(data = sd.plot, 
       aes(x = short_name_f, y = avg_w, 
           fill = sd_w)) + 
  scale_fill_viridis_c(option = "D", 
                       limits = c(NA,NA), 
                       name = "SD of\nWeight")+
  scale_y_continuous(name = "Average Weight\n(Higher = more vulnerable)") +
  scale_x_discrete(name = "Questions\nFrom Least Deviation of Average to Most")+
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(title = "Simulations that Achieved 42% of 20% Goal: Output Weights.", 
       subtitle = "Illustrating the Variability of Weights (Standard Deviation) and Average Weight by Question")+
  facet_grid(~vuln_group_f, scales = "free", space = "free")

# show how the questions match up----
library(igraph)


irs <- in_range_scores %>%
  left_join(., cw_sn) %>% 
  left_join(., select(mq, vuln_group, qnum)) %>%
  .[.$weights_type == "1:10",] %>%
  as_tibble()

irs %>%
  group_by(fp, 
           vuln_group) %>%
  summarise(avg_wt = mean(weight), 
            sd_wt = sd(weight)) %>%
  group_by(fp) %>%
  summarise(sd.sd_wt = sd(sd_wt)) %>%
  .[complete.cases(.),] %>%
  .[order(.$sd.sd_wt,decreasing = F),] %>%
  ungroup() %>%
  slice_min(., 
            order_by = sd.sd_wt, 
            n = 25) %>%
  left_join(.,  irs) %>%
  ggplot(data = ., 
         aes(x = short_name, 
             y = weight, group = 
               short_name)) + 
  geom_boxplot()+
  facet_grid(~vuln_group, scales = "free", space = "free")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
