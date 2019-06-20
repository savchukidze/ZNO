library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)

all_data <- data.table::fread("OpenData2018.csv")

students <- all_data %>%
   group_by(TERNAME) %>%
   distinct(OUTID) %>% 
   summarise(count = n())

data <- all_data %>% 
   select(1:15, contains("Ball100", ignore.case = T))
   
rivne <- data %>% 
   filter(REGNAME == "Рівненська область") %>% 
   gather("subject", "value", 16:26) %>% 
   mutate(value = as.numeric(value))
   
values_subjects <-  rivne %>% 
   filter(value != "NA", value != 0) %>% 
   group_by(subject) %>% 
   summarise(count = n(),
             mean = mean(value),
             max = max(value)) %>% 
   mutate(mean = round(mean, 1))

values_localities <- rivne %>% 
   filter(value != "NA", value != 0) %>% 
   group_by(EOTYPENAME) %>% 
   summarise(count = n(),
             mean = mean(value),
             max = max(value))

values_schools <- rivne %>% 
   filter(value != "NA", value != 0, TERNAME == "м.Рівне") %>% 
   group_by() %>%
   summarise(count = n(),
             mean = mean(value),
             max = max(value)) %>% 
   arrange(desc(mean))
