# https://r4ds.had.co.nz/factors.html

# CHP 15 FACTORS


# factors are used to work with categorical variables, variables that have a 
# fixed and known set of possible values

library(tidyverse)

# ------------------------------------------------------------------------------
# 15.2 

  # want to create variable that records month
x1 <- c("Dec", "Apr", "Jan", "Mar")

# using a string to record has two problems
  # 1. only 12 possible months, nothing saving you from typos
x2 <- c("Dec", "Apr", "Jam", "Mar")

  # 2. does not sort in a useful way
sort(x1)

# FIX BOTH w/ a factor
  # to start, must FIRST create a list of the valid levels
month_levels <- c(
  "Jan", "Feb", "Mar", "Apr", "May", "Jun", 
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
)

y1 <- factor(x1, levels = month_levels)
y1
# [1] Dec Apr Jan Mar
# Levels: Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec

sort(y1)

# any values not in set silent convert to NA
(y2 <- factor(x2, levels = month_levels))
  # [1] Dec  Apr  <NA> Mar 
  # Levels: Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec

# If you want a warning, you can use readr::parse_factor():
(y2 <- parse_factor(x2, levels = month_levels))
  # Warning: 1 parsing failure.
  # row col           expected actual
  # 3  -- value in level set    Jam

# If omit levels, alphabetical order
factor(x1)

# Sometimes you'd prefer that the order of the levels match the order of the first appearance in the data
  # unique(x)
  # fct_inorder()

(f1 <- factor(x1, levels = unique(x1)))
    # [1] Dec Apr Jan Mar
    # Levels: Dec Apr Jan Mar
(f2 <- x1 %>% factor() %>% fct_inorder())
    # [1] Dec Apr Jan Mar
    # Levels: Dec Apr Jan Mar

# to access set of levels directly
levels(f2)
#   [1] "Dec" "Apr" "Jan" "Mar"



#------------------------------------------------------------------------------
# 15.3 GENERAL SOCIETY SURVEY
gss_cat
?gss_cat

# when factors are stored in a tibble, cant see factors easily ... use count()
gss_cat %>% 
  count(race)
# OR
gss_cat %>% 
  ggplot(aes(race)) +
  geom_bar()

# dont drop any values
ggplot(gss_cat, aes(race)) +
  geom_bar() +
  scale_x_discrete(drop = FALSE)

# When working with factors, the two most common operations are changing the 
# order of the levels, and changing the values of the levels.


# EXERCISES
  # 1 - Explore the distribution of rincome (reported income). What makes the 
# default bar chart hard to understand? How could you improve the plot?
rincome_plot <- gss_cat %>% 
  ggplot(aes(rincome)) +
  geom_bar() # current plot not readable with labels 

rincome_plot + coord_flip()
# tilted x labs
rincome_plot +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


gss_cat %>%
  filter(!rincome %in% c("Not applicable")) %>%
  mutate(rincome = fct_recode(rincome,
                              "Less than $1000" = "Lt $1000"
  )) %>%
  mutate(rincome_na = rincome %in% c("Refused", "Don't know", "No answer")) %>%
  ggplot(aes(x = rincome, fill = rincome_na)) +
  geom_bar() +
  coord_flip() +
  scale_y_continuous("Number of Respondents", labels = scales::comma) +
  scale_x_discrete("Respondent's Income") +
  scale_fill_manual(values = c("FALSE" = "black", "TRUE" = "gray")) +
  theme(legend.position = "None")

# 2 -
gss_cat %>% 
  count(relig) %>% 
  arrange(desc(n))

gss_cat %>% 
  count(partyid) %>% 
  arrange(desc(n))

# 3 - 
levels(gss_cat$denom)

gss_cat %>% 
  count(denom) %>% 
  arrange(desc(n))



#------------------------------------------------------------------------------
# 15.4 MODIFYING FACTOR ORDER

# often useful to change order of factor variables

relig_summary <- gss_cat %>% 
  group_by(relig) %>% 
  summarise(
    age = mean(age, na.rm = TRUE),
    tvhours = mean(tvhours, na.rm = TRUE),
    n = n()
  )

ggplot(relig_summary,aes(tvhours,relig, colour = age)) + geom_point()
  # difficult to interpret this graph bc theres overall no pattern
    # reorder the levels of 'relig' using fct_reorder()

# fct_reorder() takes
  # f, the factors whose levels you want to modify
  # x , a numeric vector used to reorder the levels
  # fun(optional), a function used if multiple x for each f (default = median)

ggplot(relig_summary, aes(tvhours,fct_reorder(relig,tvhours), colour = age)) + 
  geom_point()

# more complicated transformations, I'd recommend moving them out of 
# aes() and into a separate mutate() step

relig_summary %>%
  mutate(
    relig = fct_reorder(relig, tvhours)
  ) %>% 
  ggplot(aes(tvhours,relig))+
    geom_point()
    

# how age varies w/ income
rincome_summary <- gss_cat %>%
  group_by(rincome) %>%
  summarise(
    age = mean(age, na.rm = TRUE),
    tvhours = mean(tvhours, na.rm = TRUE),
    n = n()
  )
#> `summarise()` ungrouping output (override with `.groups` argument)

ggplot(rincome_summary, aes(age, fct_reorder(rincome, age))) + geom_point()
  # Here, arbitrarily reordering the levels isn't a good idea! That's because 
  # rincome already has a principled order that we shouldn't mess with. 
  # Reserve fct_reorder() for factors whose levels are arbitrarily ordered.

ggplot(rincome_summary, aes(age, fct_relevel(rincome, "Not applicable"))) +
  geom_point()
    # ruse fct_relevel(). It takes a factor, f, and then any number of levels that you want to move to the front of the line.


# Reordering is useful when you are colouring the lines on a plot. 
# fct_reorder2() reorders the factor by the y values associated with the 
# largest x values. 

# This makes the plot easier to read because the line colours line up with the legend.

by_age <- gss_cat %>% 
  filter(!is.na(age)) %>% 
  count(age,marital) %>% 
  group_by(age) %>% 
  mutate(
    prop = n / sum(n)
  )

ggplot(by_age, aes(age, prop, colour = marital)) +
  geom_line(na.rm = TRUE)

#reorder so colors matchup
ggplot(by_age, aes(age, prop, colour = fct_reorder2(marital, age, prop))) +
  geom_line() +
  labs(colour = "marital")


# reorder bar charts to order levels of increasing freq
gss_cat %>% 
  mutate(marital = marital %>% fct_infreq() %>% fct_rev()) %>% 
  ggplot(aes(marital)) +
  geom_bar()


# EXERCISES
  # 1 - There are some suspiciously high numbers in tvhours. Is the mean a good summary?

gss_cat %>% 
  select(tvhours) %>% 
  summary()

gss_cat %>% 
  filter(!is.na(tvhours)) %>% 
  ggplot(aes(x = tvhours)) +
  geom_histogram(binwidth = 1)
# median probably better  


  # 2 - For each factor in gss_cat identify whether the order of the levels is arbitrary or principled.
keep(gss_cat, is.factor) %>% names()
# [1] "marital" "race"    "rincome" "partyid" "relig"   "denom"  

levels(gss_cat[["marital"]])
# [1] "No answer"     "Never married" "Separated"     "Divorced"      "Widowed"       "Married"      


levels(gss_cat[["race"]])
# [1] "Other"          "Black"          "White"          "Not applicable"

gss_cat %>% 
  ggplot(aes(race)) +
  geom_bar()
# default ordered by number of observations


  # 3 - Why did moving "Not applicable" to the front of the levels move it to the bottom of the plot?
# Because that gives the level "Not applicable" an integer value of 1



# -----------------
# 15.5 MODIFYING FACTOR LEVELS

  # about changing levels values
# fct_recode() allows you to recode or change the value of each level

gss_cat %>% count(partyid)
#> # A tibble: 10 x 2
#>   partyid                n
#>   <fct>              <int>
#> 1 No answer            154
#> 2 Don't know             1
#> 3 Other party          393
#> 4 Strong republican   2314
#> 5 Not str republican  3032
#> 6 Ind,near rep        1791

  # levels are terse and inconsistent

gss_cat %>% 
  mutate(partyid = fct_recode(partyid,
                              "Republican, strong"    = "Strong republican",
                              "Republican, weak"      = "Not str republican",
                              "Independent, near rep" = "Ind,near rep",
                              "Independent, near dem" = "Ind,near dem",
                              "Democrat, weak"        = "Not str democrat",
                              "Democrat, strong"      = "Strong democrat"                     
    
  )) %>% 
  count(partyid)

# To combine groups, you can assign multiple old levels to the same new level:
gss_cat %>%
  mutate(partyid = fct_recode(partyid,
                              "Republican, strong"    = "Strong republican",
                              "Republican, weak"      = "Not str republican",
                              "Independent, near rep" = "Ind,near rep",
                              "Independent, near dem" = "Ind,near dem",
                              "Democrat, weak"        = "Not str democrat",
                              "Democrat, strong"      = "Strong democrat",
                              "Other"                 = "No answer",
                              "Other"                 = "Don't know",
                              "Other"                 = "Other party"
  )) %>%
  count(partyid)


# TO collapse a lot of levels use fct_collapse()
gss_cat %>% 
  mutate(partyid = fct_collapse(partyid,
                                other = c("No answer", "Don't know", "Other party"),
                                rep = c("Strong republican", "Not str republican"),
                                ind = c("Ind,near rep", "Independent", "Ind,near dem"),
                                dem = c("Not str democrat", "Strong democrat")
  )) %>% 
  count(partyid)


# Sometimes want to lump together a small group to make a plot simpler
  # --> fct_lump()

gss_cat %>% 
  mutate(relig = fct_lump(relig)) %>% 
  count(relig)
  #  too collapsed!!!

gss_cat %>% 
  mutate(relig = fct_lump(relig, n = 10)) %>% 
  count(relig, sort = TRUE) %>% 
  print(n = Inf)
#> # A tibble: 10 x 2
#>    relig                       n
#>    <fct>                   <int>
#>  1 Protestant              10846
#>  2 Catholic                 5124
#>  3 None                     3523
#>  4 Christian                 689
#>  5 Other                     458
#>  6 Jewish                    388
#>  7 Buddhism                  147
#>  8 Inter-nondenominational   109
#>  9 Moslem/islam              104
#> 10 Orthodox-christian         95


# EXERCISES
   # 1 - How have the proportions of people identifying as Democrat, Republican, and Independent changed over time?
gss_cat %>% 
  mutate(
    partyid = fct_collapse(partyid,
                Other = c("No answer", "Don't know", "Other party"),
                Republican = c("Strong republican", "Not str republican"),
                Independent = c("Ind,near rep", "Independent", "Ind,near dem"),
                Democrat = c("Not str democrat", "Strong democrat"))
  ) %>%
  count(year,partyid) %>%
  group_by(year) %>% 
  mutate(prop = n / sum(n)) %>% 
  ggplot(aes(year, prop, colour = fct_reorder2(partyid, year,prop))) + 
    geom_point() +
    geom_line() +
    labs(colour = "Party ID.") + ylab("Proportion") + xlab("Year")


  # 2 - How could you collapse rincome into a small set of categories?

library("stringr")
gss_cat %>%
  mutate(
    rincome =
      fct_collapse(
        rincome,
        `Unknown` = c("No answer", "Don't know", "Refused", "Not applicable"),
        `Lt $5000` = c("Lt $1000", str_c(
          "$", c("1000", "3000", "4000"),
          " to ", c("2999", "3999", "4999")
        )),
        `$5000 to 10000` = str_c(
          "$", c("5000", "6000", "7000", "8000"),
          " to ", c("5999", "6999", "7999", "9999")
        )
      )
  ) %>%
  ggplot(aes(x = rincome)) +
  geom_bar() +
  coord_flip()





