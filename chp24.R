# https://r4ds.had.co.nz/model-building.html


# CHP 24 MODEL BUILDING ---------------------------------------------------

#INTRODUCTION --------------------

# Chapter will focus on real data, showing you how you can progressively build up
# a model to aid your understanding of the data.

# you can think about a model partitioning your data into pattern and residuals.

# a more machine learning approach is simply to focus on the predictive ability of the model (tend to be black boxes)

# For most real models, I'd expect you to use some combination of this approach and a more classic automated approach.

# It's a challenge to know when to stop. You need to figure out when your model is good enough, 
# and when additional investment is unlikely to pay off.

# "A poor seamstress makes many mistakes. A good seamstress works hard to correct 
# those mistakes. A great seamstress isn't afraid to throw out the garment and start over.


# https://www.reddit.com/r/datascience/comments/4irajq/mistakes_made_by_beginningaspiring_data_scientists/ 


library(tidyverse)
library(modelr)
options(na.action = na.warn)

library(nycflights13)
library(lubridate)

install.packages('broom') # for glance function()
library(broom)
install.packages("caTools")
library(caTools)


# 24.2 WHY ARE LOW QUALITY DIAMONDS MORE EXPENSIVE ------------------------

# low quality diamonds (poor cuts, bad colours, and inferior clarity) have higher prices.
ggplot(diamonds, aes(cut,price)) +
  geom_boxplot()
ggplot(diamonds, aes(color,price)) +
  geom_boxplot()
ggplot(diamonds, aes(clarity,price)) +
  geom_boxplot()


# PRICE AND CARAT ----------------------
  # lower quality diamonds = higher price b/c important confounding variable
    # ==> the weight (carat)

# carat is single most important factor determining price
ggplot(diamonds, aes(carat, price)) +
  geom_hex(bins = 50)


# before modeling, lets make some tweaks to diamonds
  # 1. focus on diamonds smaller than 2.5 carats (99.7% of data)
  # 2. Log-transform the carat and price variables (makes the pattern linear!! )

diamonds2 <- diamonds %>% 
  filter(carat <= 2.5) %>% 
  mutate(
    lprice = log2(price), 
    lcarat = log2(carat)
  )

# changes make it easier to see relationship carat v. price
ggplot(diamonds2, aes(lcarat, lprice)) +
  geom_hex(bins = 50)

# make the pattern explicit
mod_diamond <- lm(lprice ~ lcarat, data = diamonds2)

# look at what model tells us about the data 
  # back transform the predictions, undoing log, to overlay predictions on raw data
grid <- diamonds2 %>% 
  data_grid(carat = seq_range(carat, 20)) %>% 
  mutate(lcarat = log2(carat)) %>% 
  add_predictions(mod_diamond, "lprice") %>% 
  mutate(price = 2 ^ lprice) #back transforming predictions

ggplot(diamonds2, aes(carat, price)) +
  geom_hex(bins = 50) +
  geom_line(data = grid, colour = "red", size = 1)
    # interesting ... large diamonds not as expensive as expected, none over 19K
      # b/c we filtered the data

# look at the residuals
diamonds2 <- diamonds2 %>% 
  add_residuals(mod_diamond, "lresid")

# verify we have removed the strong linear pattern
ggplot(diamonds2, aes(carat, lresid)) +
  geom_hex(bins = 50)

# redo motivating plots using the residuals -- now appears to make sense
diamonds2 %>% 
  ggplot(aes(cut, lresid)) + geom_boxplot() 
diamonds2 %>% 
  ggplot(aes(color, lresid)) + geom_boxplot() 
diamonds2 %>% 
  ggplot(aes(clarity, lresid)) + geom_boxplot() 
# as the quality of the diamond increases, so too does its relative price

# interpret the y axis, need to think about what the residuals are telling us, 
# and what scale they are on ....

# A residual of -1 indicates that lprice was 1 unit lower than a prediction 
# based solely on its weight. 2 exp(-1) is 1/2, points with a value of -1 are half 
# the expected price, and residuals with value 1 are twice the predicted price.



# MORE COMPLICATED MODEL -------------------------
  
# can continue to build up our model
mod_diamond2 <- lm(lprice ~ lcarat + color + cut + clarity, data = diamonds2)

# now we have 4 predictors, harder to vis ... but all indep so cant plot indiv
grid <- diamonds2 %>% 
  data_grid(cut, .model = mod_diamond2) %>%  # use .model to make easier
  add_predictions(mod_diamond2)
grid
    # # A tibble: 5 x 5
    # cut       lcarat color clarity  pred
    # <ord>      <dbl> <chr> <chr>   <dbl>
    #   1 Fair      -0.515 G     VS2      11.2
    # 2 Good      -0.515 G     VS2      11.3
    # 3 Very Good -0.515 G     VS2      11.4
    # 4 Premium   -0.515 G     VS2      11.4
    # 5 Ideal     -0.515 G     VS2      11.4
ggplot(grid, aes(cut, pred)) +
  geom_point()

# adding residuals to the diamonds2 set
diamonds2 <- diamonds2 %>% 
  add_residuals(mod_diamond2, "lresid2")

# plot residuals
ggplot(diamonds2, aes(lcarat, lresid2)) +
  geom_hex(bins = 50)
# plot indicates some very large residuals (resid of 2 -> 4x more expensive)

# look at unusual values
diamonds2 %>% 
  filter(abs(lresid2) > 1) %>% 
  add_predictions(mod_diamond2) %>% 
  mutate(pred = round(2 ^ pred)) %>% # convert down from log
  select(price, pred, carat:table, x:z) %>% 
  arrange(price)
  # would be worth taking a look at these irl


# EXERCISES -----------------------
diamonds2 <- diamonds %>%
  filter(carat <= 2.5) %>%
  mutate(
    lprice = log2(price),
    lcarat = log2(carat)
  )

mod_diamond2 <- lm(lprice ~ lcarat + color + cut + clarity, data = diamonds2)

diamonds2 <- add_residuals(diamonds2, mod_diamond2, "lresid2")

# 1 - 
# in plot of lcarat vs lprice some bright vert strips, what are they
ggplot(diamonds2, aes(lcarat, lprice)) +
  geom_hex(bins = 50)
  # ==> whole numbers are more common for diamonds

# 2 - 
# If log(price) = a_0 + a_1 * log(carat), what does that say about the 
# relationship between price and carat?

# fit log2 model
mod_log <- lm(log2(price) ~ log2(carat), data = diamonds)
  # mod_log
  # Call:
  #   lm(formula = log2(price) ~ log2(carat), data = diamonds)
  # 
  # Coefficients:
  #   (Intercept)  log2(carat)  
  # 12.189        1.676  

# graph
tibble(carat = seq(0.25, 5, by = 0.25)) %>% 
  add_predictions(mod_log) %>% 
  ggplot(aes(x = carat, y = 2^pred)) +
  geom_line() +
  labs(x = "carat", y = "price")

# relationship between carat and price is not linear
# The exact relationship in this model is if  
# x increases   r times, then   y increases r^a1 times
2^coef(mod_log)[2]
  # log2(carat) 
# 3.195002 

# PROOF
logb(y) = a0 + a1*logb(x)

# understand how diff in y relates to a diff in x
logb(y0) = a0 + a1*logb(x0)
logb(y1) = a0 + a1*logb(x1)

# diff in val logb(y1) - logb(y0)
logb(y1) - logb(y0) = (a0 + a1*logb(x1)) - (a0 + a1*logb(x0))
# ....
y1/y0 = (x1/x0)^a1
s = r^a1


# 3 - 
diamonds2 %>% 
  filter(abs(lresid2) > 1) %>% 
  add_predictions(mod_diamond2) %>% 
  mutate(pred = round(2 ^ pred)) %>% # convert down from log
  select(lresid2, price, pred, carat:table, x:z) %>% 
  arrange(desc(abs(lresid2)))

# 4 - mod_diamond2 do a good job at predicting?

# PLOT - slight downward bias looking at residual graph
ggplot(diamonds2, aes(lcarat, lresid2)) +
  geom_hex(bins = 50)


# ACCURACY MEASURES - 
lresid2_summary <- diamonds2 %>% 
  summarise(
  rmse = sqrt(mean(lresid2^2)), # root mean square error
  mae = mean(abs(lresid2)),
  p025 = quantile(lresid2, 0.025),
  p975 = quantile(lresid2, 0.975)
)
lresid2_summary
    # # A tibble: 1 x 4
    # rmse   mae   p025  p975
    # <dbl> <dbl>  <dbl> <dbl>
    #   1 0.192 0.149 -0.369 0.384
# remember to transform the above from log





# 24.3 WHAT AFFECTS # OF DAILY FLIGHTS ------------------------------------

# Let's get started by counting the number of flights per day and visualize it with ggplot2.
daily <- flights %>% 
  mutate(date = make_date(year, month, day)) %>% 
  group_by(date) %>% 
  summarise(n = n())
daily

ggplot(daily, aes(date,n)) +
  geom_line()


# DAY OF THE WEEK ----
# understanding long term trend challenge b/c weekly effect dominates
daily <- daily %>% 
  mutate(
    wday = wday(date, label = TRUE)
  )
ggplot(daily, aes(wday, n)) + 
  geom_boxplot()

# one way to remove this strong pattern is to overlay a model
mod <- lm(n ~ wday, data = daily)

# create the grid
grid <- daily %>% 
  data_grid(wday)
  # A tibble: 7 x 1
  # wday 
  # <ord>
  #   1 Sun  
  # 2 Mon  
  # 3 Tue  
  # 4 Wed  
  # 5 Thu  
  # 6 Fri  
  # 7 Sat  

# add the predictions to the grid from the model "mod"
grid <- grid %>% 
  add_predictions(mod, "n")
    # # A tibble: 7 x 2
    # wday      n
    # <ord> <dbl>
    #   1 Sun    891.
    # 2 Mon    975.
    # 3 Tue    951.
    # 4 Wed    963.
    # 5 Thu    966.
    # 6 Fri    967.
    # 7 Sat    745.

# just graph it
ggplot(daily, aes(wday, n)) +
  geom_boxplot() +
  geom_point(data = grid, colour = "red", size = 4)


# now, compute and visualize the RESIDUALS
daily <- daily %>% 
  add_residuals(mod)
# graph -- now seeing resid(the deviation from expected # flights, given day of week)
daily %>% 
ggplot(aes(date, resid)) +
  geom_ref_line(h = 0) +
  geom_line()

# MODEL ASSESS --------

# 1 - the model begins to fail around June, strong pattern model not capturing
ggplot(daily, aes(date, resid, colour = wday))+ # add lines to show wday
  geom_ref_line(h = 0)+
  geom_line()
# ==> model poorly assess Saturdays: summer more flights than expect, fall less

# 2 - some days w/ far fewer flights than expected
daily %>% 
  filter(resid < -100) %>% 
  arrange(resid)
# ==> New Year's day, July 4th, Thanksgiving and Christmas, Labor day, Mem day

# 3 - smoother long term trend over year use geom_smooth()
daily %>% 
  ggplot(aes(date, resid)) +
  geom_ref_line(h = 0) +
  geom_line(colour = "grey50") +
  geom_smooth(se = FALSE, span = 0.2)



# SEASONAL SATURDAY EFFECT  ------ 

# investigate saturday flight data
daily %>% 
  filter(wday == "Sat") %>% 
  ggplot(aes(date, n)) +
  geom_point() +
  geom_line() +
  scale_x_date(NULL, date_breaks = "1 month", date_labels = "%b")
# ==> pattern likely described by summer holidays June - Fall
# more trips on Sat in spring than fall .... school terms / fall holidays


# create a term to capture the seasonality trend
term <- function(date){
  cut(date,
      breaks = ymd(20130101, 20130605, 20130825, 20140101),
      labels = c("spring", "summer", "fall") 
  )
}

# apply function to daily df
daily <- daily %>% 
  mutate(
    term = term(date)
  )
  # # A tibble: 365 x 5
  # date           n wday   resid term  
  # <date>     <int> <ord>  <dbl> <fct> 
  #   1 2013-01-01   842 Tue   -109.  spring
  #   2 2013-01-02   943 Wed    -19.7 spring

# graph the findings
daily %>% 
  filter(wday == "Sat") %>% 
  ggplot(aes(date,n, color = term)) +
  geom_point() +
  geom_line() +
  scale_x_date(NULL, date_breaks = "1 month", date_labels = "%b")

# how does this variable affect other days of week
daily %>% 
  ggplot(aes(wday, n, colour = term)) +
  geom_boxplot()

# significant variation across the terms, fitting separate day of week effect reasonable
mod1 <- lm(n ~ wday, data = daily)
mod2 <- lm(n ~ wday * term, data = daily) # * = interaction

# gather_residuals -> adds a column with the rows being the model name
daily %>% 
  gather_residuals(without_term = mod1, with_term = mod2) %>% 
  ggplot(aes(date, resid, color = model), se = TRUE) +
  geom_line(alpha = 0.75)

# we can see the problem by overlaying the predictions onto the model
grid <- daily %>% 
  data_grid(wday, term) %>% 
  add_predictions(mod2, "n")

ggplot(daily, aes(wday, n)) +
  geom_boxplot() +
  geom_point(data = grid, color = "red") +
  facet_wrap(~ term)

# *** Our model is finding the MEAN EFFECT, but we have a lot of big outliers,
# so the mean tends to be far way from the typical value

# use a model which is robust to the effect of outliers-> 
  # MASS:rlm() fits a linear model using M estimator
mod3 <- rlm(n ~ wday * term, data = daily)

daily %>% 
  add_residuals(model = mod3, "resid") %>% 
  ggplot(aes(date, resid)) +
  geom_hline(yintercept = 0, size = 2, colour = "white") +
  geom_line()
# => now much easier to see the longterm trend


# COMPUTED VARIABLES ---------------
  # if dealing with many models and many visualizations, good idea to 
  # bundle the creation of variables up into a function no chance of applying
  # different transform elsewhere

# example
compute_vars <- function(data) {
  data %>% 
    mutate(
      term = term(date),
      wday = wday(date, label = TRUE)
    )
}

# another option to put the transform directly in the model formula
wday2 <- function(x) wday(x, label = TRUE)
mod3 <- lm(n ~ wday2(date) * term(date), data = daily)



# TIME OF THE YEAR: ALT APPROACH ------------------------
  # before we used domain knowledge about the US school year

# we can use more flexible model to capture the pattern of interest ...
  # use a natural spline to fit a smooth curve across the year:

library(splines)
mod <- rlm(n ~ wday * ns(date, 5), data = daily)

daily %>% 
  data_grid(wday, date = seq_range(date, n = 13)) %>% 
  add_predictions(mod) %>% 
  ggplot(aes(date, pred, colour = wday)) + 
  geom_line() +
  geom_point()
# strong pattern in # of Saturday flights, also saw pattern in raw (good sign)


# EXERCISES ---------------
# 1 - brainstorm why Jan20, May26, Sep1 less flights
  # (Monday holidays)
daily %>%
  filter(wday == "Sun") %>% 
  ggplot(aes(date,resid)) +
  geom_point() +
  geom_line()

daily %>% 
  filter(wday == "Sun" & resid < -20) %>% 
  arrange(resid)

# 2 - what do 3 days with highest pos residuals represent... how to generalize to another year
daily %>% 
  add_predictions(mod) %>% 
  slice_max(n = 3, resid)

# represent where the model underpredicted and there were more flights than expect
# these days are around thanksgiving and Christmas 

# 3 - Create a new variable that splits the wday variable into terms, but only
# for Saturdays, i.e. it should have Thurs, Fri, but Sat-summer, Sat-spring, 
# Sat-fall. How does this model compare with the model with every combination of wday and term?
daily <- daily %>%
  mutate(
    wday2 =
      case_when(
        wday == "Sat" & term == "summer" ~ "Sat-summer",
        wday == "Sat" & term == "fall" ~ "Sat-fall",
        wday == "Sat" & term == "spring" ~ "Sat-spring",
        TRUE ~ as.character(wday)
      )
  ) 


mod3 <- lm(n ~ wday2, data = daily)

# hard to tell
daily %>%
  gather_residuals(sat_term = mod3, all_interact = mod2) %>%
  ggplot(aes(date, resid, colour = model)) +
  geom_line(alpha = 0.75)

# plot the differences using spread_residuals() to add one column per model
daily %>% 
  spread_residuals(sat_term = mod3, all_interact = mod2)%>% 
  mutate(resid_diff = sat_term - all_interact) %>% 
    # # A tibble: 365 x 8
    # date           n wday   resid term   sat_term wday2      all_interact
    # <date>     <int> <ord>  <dbl> <fct>     <dbl> <chr>             <dbl>
    #   1 2013-01-01   842 Tue   -109.  spring   -109.  Tue              -98.3 
    # 2 2013-01-02   943 Wed    -19.7 spring    -19.7 Wed               -8.64
    # 3 2013-01-03   914 Thu    -51.8 spring    -51.8 Thu              -51.4 
  ggplot(aes(date, resid_diff)) +
  geom_line(alpha = 0.7)

# compare models numerically
glance(mod3) %>%
  dplyr::select(r.squared, sigma, AIC, df, p.value)
  # # A tibble: 1 x 5
  # r.squared sigma   AIC    df  p.value
  # <dbl> <dbl> <dbl> <dbl>    <dbl>
  #   1     0.736  47.4 3863.     8 5.43e-98
glance(mod2) %>%
  dplyr::select(r.squared, sigma, AIC, df, p.value)
  # # A tibble: 1 x 5
  # r.squared sigma   AIC    df  p.value
  # <dbl> <dbl> <dbl> <dbl>    <dbl>
  #   1     0.757  46.2 3856.    20 6.72e-93


# 4- 

holidays_2013 <-
  tribble(
    ~holiday, ~date,
    "New Year's Day", 20130101,
    "Martin Luther King Jr. Day", 20130121,
    "Washington's Birthday", 20130218,
    "Memorial Day", 20130527,
    "Independence Day", 20130704,
    "Labor Day", 20130902,
    "Columbus Day", 20131028,
    "Veteran's Day", 20131111,
    "Thanksgiving", 20131128,
    "Christmas", 20131225
  ) %>%
  mutate(date = lubridate::ymd(date))

# how to handle day before and after
daily <- daily %>%
  mutate(
    wday3 =
      case_when(
        date %in% (holidays_2013$date - 1L) ~ "day before holiday",
        date %in% (holidays_2013$date + 1L) ~ "day after holiday",
        date %in% holidays_2013$date ~ "holiday",
        .$wday == "Sat" & .$term == "summer" ~ "Sat-summer",
        .$wday == "Sat" & .$term == "fall" ~ "Sat-fall",
        .$wday == "Sat" & .$term == "spring" ~ "Sat-spring",
        TRUE ~ as.character(.$wday)
      )
  )

mod4 <- lm(n ~ wday3, data = daily)

daily %>%
  spread_residuals(resid_sat_terms = mod3, resid_holidays = mod4) %>%
  mutate(resid_diff = resid_holidays - resid_sat_terms) %>%
  ggplot(aes(date, resid_diff)) +
  geom_line(alpha = 0.75)

glance(mod4) %>%
  dplyr::select(r.squared, sigma, AIC, df, p.value)


# 7 - hypoth that ppl leaving on sunday more likely business traveler
# explore and see how break down by dist and time
# if true, should see more sunday evening flights to places far

glimpse(flights2)

flights2 <- flights %>% 
  mutate(
    wday = wday(time_hour, label = TRUE),
    evening = 
      ifelse((hour > 17 ),1,0)
  )

flights2 %>% 
  group_by(wday,evening) %>% 
  summarise(
    avg_dist = mean(distance)
  )

