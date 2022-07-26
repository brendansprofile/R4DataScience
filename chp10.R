# https://r4ds.had.co.nz/tibbles.html

# CHP 10 - TIBBLES
library(tidyverse)

# 10.1 INTRODUCTION

# Tibbles are data frames, but they tweak some older behaviors to make life a little easier
vignette("tibble")


#-------------------------------------------------------------------------------
# 10.2 CREATING TIBBLES

# coerce a df into  tibble with
as_tibble()

tibble(
  x = 1:5,
  y = 1,
  z = x ^ 2 + y
)

# you need to use backticks when working w/ nonsynactic var names and vars from other packages (ggplot2,tidyr,dplyr)
tb <- tibble(
  `:)` = "smile", 
  ` ` = "space",
  `2000` = "number"
)

# TRIBBLE = transposed tibble ... 
  # customised for data entry in code: column headings are defined by formulas 
  # (i.e. they start with ~), and entries are separated by commas.
tribble(
  ~x, ~y, ~z,
  #--\---\---
  "A", 2, 3.6
)


#-------------------------------------------------------------------------------
# 10.3 TIBBLES VS DATAFRAMES

# two main differences
  # 1) PRINTING`
  # 2) SUBSETTING

# PRINTING ---------------------------
#note: only 10 rows, all cols that fit, var types
tibble(
  a = lubridate::now() + runif(1e3) * 86400,
  b = lubridate::today() + runif(1e3) * 30,
  c = 1:1e3,
  d = runif(1e3),
  e = sample(letters, 1e3, replace = TRUE)
)

nycflights13::flights %>% 
  print(n = 10, width = Inf) # n = num rows, width = Inf -> all cols

options(tibble.print_min = Inf) # to always show all rows.

options(tibble.width = Inf) # to always print all columns, regardless of the width of the screen.

#scrollable view of dataset, useful for long chain manipulations
nycflights13::flights %>% 
  View()

# SUBSETTING -----------------------------------
  # pulling out a single variable
    # [[""]] <-  extract by name or position
    # $ <-  extract by name
df <- tibble(
  x = runif(5),
  y = rnorm(5)
)

# extract by name
df$x  

df[["x"]]
  
#extract by position
df[[1]]

# to use in a pipe you need the . placerholder
df %>% 
  .$x
df %>% 
  .[["x"]]


#-------------------------------------------------------------------------------
# 10.4 INTERACTING WITH OLDER CODE

# some older functions dont work with tibbles
  as.data.frame() # turns tibble -> data frame
class(as.data.frame(df))


#-------------------------------------------------------------------------------
# 10.5 EXERCISES

  # 1 - how can tell if object is tibble?
print(mtcars) # does not print like a tibble (tibble = only 10 rows, data type)
class(mtcars) # "data.frame"
  # OR
is_tibble(mtcars) # FALSE

  # 2 - Compare and contrast the following operations on a data.frame and equivalent tibble.
df <- data.frame(abc = 1, xyz = "a")

df$x
  # [1] "a"   .... not an exact match
df[,"xyz"]
  # [1] "a"    ..... 
df[, c("abc", "xyz")]

  # 3 - name of a variable stored in an object, e.g. var <- "mpg", how can you extract the reference variable from a tibble?
var <- "mpg"
class(var)
as_tibble(var) %>% 
  .[[1]]

  # 5 
#function tibble::enframe() converts named vectors to a data frame with names and values


  