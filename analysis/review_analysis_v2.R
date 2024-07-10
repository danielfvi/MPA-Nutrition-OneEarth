##############Review analysis########

library(gdata)
library(readr)
library(tidyverse) # for general data wrangling and plotting
library(rgeos)
library(scales)
library(gdata)
library(summarytools)
library(ggh4x)
library(ggpubr)
review_data <- read_csv("review/review_data.csv")

####Clean data
rev.data = review_data %>% 
  select(Eco_simple, Study_design_2, MPA_age_cat_2, Multiple_Zones, Stake_simple, CPUE, Catch, Income) 

rev.data = reshape2::melt(rev.data, id.vars = c("Eco_simple", "Study_design_2", "MPA_age_cat_2", "Multiple_Zones", "Stake_simple"))

rev.data_2 = rev.data %>% 
  drop_na() %>% 
  group_by(Eco_simple, Study_design_2, MPA_age_cat_2, Stake_simple, Multiple_Zones, variable) %>% 
  count(value)

rev.data_total = rev.data %>% 
  drop_na() %>% 
  group_by(variable) %>% 
  count(Stake_simple) %>% 
  rename(total=n)

rev.data_2 = left_join(rev.data_2, rev.data_total)

rev.data_2 = rev.data_2 %>% 
  ungroup() %>% 
  mutate(perc = 100*n/total,
         Multiple_Zones = recode(Multiple_Zones, 
                                 "N" = "No-take",
                                 "Y" = "Multple-Use"),
         Stake_simple = recode(Stake_simple,
                               "Coastal community" = "Community")) %>% 
  rename("MPA type" = "Multiple_Zones",
         "Outcome" = "value",
         "MPA age" = "MPA_age_cat_2",
         "Ecosystem" = "Eco_simple",
         "Study design" = "Study_design_2")

rev.data_2$Outcome = factor(rev.data_2$Outcome, levels = c("Increase", "No change", "Decrease"))

rev.data_2$Stake_simple = factor(rev.data_2$Stake_simple, levels = c("Fisheries", "Tourism", "Community"))

##Simple plot
p.all = ggplot(data = rev.data_2,
               aes(x=perc, y = variable))+
  geom_bar(stat="identity", aes(fill=Outcome, alpha = `MPA type`))+
  #geom_bar(stat="identity", aes(fill=Outcome, alpha = `MPA age`))+
  #facet_wrap(~variable)
  facet_grid(Outcome~Stake_simple, scales = "free", space="free")+
  labs(x = "Percentage of studies", y = "") +
  xlim(0,100)+
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size=15, color = "black", face = "bold"),
        axis.text = element_text(color="black", margin = margin(b = 10, l=10, t = 0, r = 0)))+
  scale_alpha_discrete(range = c(0.4,1))

p.all

ggplot(data = rev.data_2,
       aes(x=perc, y = variable))+
  geom_bar(stat="identity", aes(fill=Outcome, alpha = `MPA type`), position = "dodge")+
  #geom_bar(stat="identity", aes(fill=Outcome, alpha = `MPA age`))+
  #facet_wrap(~variable)
  facet_grid(~Stake_simple, scales = "free", space="free") +
  labs(x = "Percentage of studies", y = "") +
  xlim(0,100) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size=15, color = "black", face = "bold"),
        axis.text = element_text(color="black", margin = margin(b = 10, l=10, t = 0, r = 0)))+
  scale_alpha_discrete(range = c(0.4,1))



##Plot based on MPA age
p.all = ggplot(data = rev.data_2,
               aes(x=perc, y = variable))+
  geom_bar(stat="identity", aes(fill=Outcome, alpha = `MPA type`))+
  #geom_bar(stat="identity", aes(fill=Outcome, alpha = `MPA age`))+
  #facet_wrap(~variable)
  facet_nested(Stake_simple+`MPA age`~Outcome)+
  labs(x = "Percentage of studies", y = "") +
  xlim(0,100)+
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size=15, color = "black", face = "bold"),
        axis.text = element_text(color="black", margin = margin(b = 10, l=10, t = 0, r = 0)))+
  scale_alpha_discrete(range = c(0.4,1))

p.all

##Plot based on Study design
p.2 = ggplot(data = rev.data_2,
               aes(x=perc, y = variable))+
  geom_bar(stat="identity", aes(fill=Outcome, alpha = `MPA type`))+
  #geom_bar(stat="identity", aes(fill=Outcome, alpha = `MPA age`))+
  #facet_wrap(~variable)
  facet_nested(Stake_simple+`Study design`~Outcome)+
  labs(x = "Percentage of studies", y = "") +
  xlim(0,100)+
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size=15, color = "black", face = "bold"),
        axis.text = element_text(color="black", margin = margin(b = 10, l=10, t = 0, r = 0)))+
  scale_alpha_discrete(range = c(0.4,1))

p.2

##Plot based on Study design
p.3 = ggplot(data = rev.data_2,
             aes(x=perc, y = variable))+
  geom_bar(stat="identity", aes(fill=Outcome, alpha = `MPA type`))+
  #geom_bar(stat="identity", aes(fill=Outcome, alpha = `MPA age`))+
  #facet_wrap(~variable)
  facet_nested(Stake_simple+`Ecosystem`~Outcome)+
  labs(x = "Percentage of studies", y = "") +
  xlim(0,100)+
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size=15, color = "black", face = "bold"),
        axis.text = element_text(color="black", margin = margin(b = 10, l=10, t = 0, r = 0)))+
  scale_alpha_discrete(range = c(0.4,1))

p.3

ggplot(data = rev.data_2,
       aes(x=perc, y = variable)) + 
  geom_col(position = position_dodge(width = 0.9), aes(fill = `MPA type`)) +
  geom_col(position = position_dodge(width = 0.9), aes(alpha = `MPA age`)) +
  facet_grid(Stake_simple~Outcome)+
  labs(x = "Percentage of studies", y = "") +
  xlim(0,100)+
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size=15, color = "black", face = "bold"),
        axis.text = element_text(color="black", margin = margin(b = 10, l=10, t = 0, r = 0)))+
  scale_alpha_discrete(range = c(0.4,1))




g1 <- ggplot(data = rev.data_2,
             aes(x=perc)) +
  geom_bar(position="identity", width=0.3, aes(y=as.numeric(variable) - 0.15, fill=Outcome, alpha=`MPA type`)) +
  geom_bar(
              aes(y=as.numeric(variable) + 0.15, fill=Outcome, alpha=`MPA age`),
              position="stack", width=0.3)

g1




###########Seperate plots
p1 = ggplot(data = fisheries.table,
       aes(x=value*100, y = key))+
  geom_bar(stat="identity")+
  #facet_wrap(~variable)
  facet_grid(.~variable)+
  labs(x = "Percentage of studies", y = "Fisheries Outcomes") +
  xlim(0,100)+
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size=15, color = "black"),
        axis.text = element_text(color="black", margin = margin(b = 10, l=10, t = 0, r = 0)))

p2 = ggplot(data = tourism.table,
            aes(x=value*100, y = key))+
  geom_bar(stat="identity")+
  #facet_wrap(~variable)
  facet_grid(.~variable)+
  labs(x = "Percentage of studies", y = "Tourism Outcomes") +
  xlim(0,100)+
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size=15, color = "black"),
        axis.text = element_text(color="black", margin = margin(b = 10, l=10, t = 0, r = 0)))


p3 = ggplot(data = commun.table,
            aes(x=value*100, y = key))+
  geom_bar(stat="identity")+
  #facet_wrap(~variable)
  facet_grid(.~variable)+
  labs(x = "Percentage of studies", y = "Community Outcomes") +
  xlim(0,100)+
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size=15, color = "black"),
        axis.text = element_text(color="black", margin = margin(b = 10, l=10, t = 0, r = 0)))



mutate(freq = 100*n/sum(n)) %>% 
  

fisheries_catch = review_data %>% 
  filter(Stake_simple=="Fisheries") %>%
  group_by(Catch_change, )
  
  
  select(Catch_change) %>% 
  drop_na() %>% 
  group_by(Catch_change) %>% 
  tally() %>% 
  mutate(freq = 100*n/sum(n))
  
  
  %>% 
    group_by(Catch_change) %>% 
    tally() %>% 
    select(Catch_change,n) %>% 
    spread(Catch_change, n) %>% 
    mutate(n = Decrease + Increase + `No change`) %>% 