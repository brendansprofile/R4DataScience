# https://r4ds.had.co.nz/workflow-scripts.html 

# CHAPTER 6  -- SCRIPTS
#install.packages("dplyr")
library(dplyr)
library(nycflights13)

not_cancelled <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(mean = mean(dep_delay))

# execute the complete script in one step: Cmd/Ctrl + Shift + S

# Note, however, that you should never include install.packages() or 
# setwd() in a script that you share.