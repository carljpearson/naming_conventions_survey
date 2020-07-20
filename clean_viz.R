library(tidyverse)
library(here)
data <- read_csv(here("/data.csv"),col_names = T)

data <- data[3:nrow(data),] %>% filter(Status != "Survey Preview")

df <- data %>%
  select(rh,role,contains("vote"),contains("open"))

zval = 1.440

df %>%
  select(rh,contains("vote")) %>%
  pivot_longer(contains("vote")) %>%
  group_by(rh,name) %>%
  mutate(total=n()) %>%
  group_by(rh,name,value,total) %>%
  count(name = "count") %>%
  mutate(prop = count / total, #get cis
         n= total, #rename
         prop = count / n, #exact proportion from succesess/trials
         laplace = (count + 1) / (n + 2), #laplace point estimate
         p_adj = (n * prop + (zval * zval) / 2) / (n + (zval * zval)), #adjust p for wald calculation
         n_adj = n + (zval * zval), #adjust n for wald calculation
         marg =  zval * sqrt(p_adj * (1 - p_adj) / n_adj), #wald margin value
         lowerci = p_adj - marg, #lower wald ci
         lowerci = ifelse(lowerci <= 0, 0, lowerci), #keep lower ci above 0
         upperci = p_adj + marg, #upper wald ci
         upperci = ifelse(upperci >= 1, 1, upperci))  %>% #keep upper ci below 1
  ggplot(aes(y=prop,x=value,fill=rh)) +
  geom_bar(stat="identity",position = position_dodge()) +
  geom_errorbar(aes(ymin=lowerci,ymax=upperci),color="gray",width=.5,position = position_dodge(width = .9)) +
  facet_wrap(~name,scales = "free_y",ncol = 1) +
  coord_flip() +
  ggsci::scale_fill_npg()+
  ggthemes::theme_tufte(base_family="sans")
  
df %>%
  select(rh,contains("vote")) %>%
  pivot_longer(contains("vote")) %>%
  group_by(name) %>%
  mutate(total=n()) %>%
  group_by(name,value,total) %>%
  count(name = "count") %>%
  mutate(prop = count / total, #get cis
         n= total, #rename
         prop = count / n, #exact proportion from succesess/trials
         laplace = (count + 1) / (n + 2), #laplace point estimate
         p_adj = (n * prop + (zval * zval) / 2) / (n + (zval * zval)), #adjust p for wald calculation
         n_adj = n + (zval * zval), #adjust n for wald calculation
         marg =  zval * sqrt(p_adj * (1 - p_adj) / n_adj), #wald margin value
         lowerci = p_adj - marg, #lower wald ci
         lowerci = ifelse(lowerci <= 0, 0, lowerci), #keep lower ci above 0
         upperci = p_adj + marg, #upper wald ci
         upperci = ifelse(upperci >= 1, 1, upperci))  %>% #keep upper ci below 1
  ggplot(aes(y=prop,x=value)) +
  geom_bar(stat="identity",position = position_dodge()) +
  geom_errorbar(aes(ymin=lowerci,ymax=upperci),color="gray",width=.5,position = position_dodge(width = .9)) +
  facet_wrap(~name,scales = "free_y",ncol = 1) +
  coord_flip() +
  ggsci::scale_fill_npg()+
  ggthemes::theme_tufte(base_family="sans")
