# https://r4ds.had.co.nz/transform.html

# CHAPTER 5 -- DATA TRANSFORMATION


# 5.1 - INTRODUCTION
# focus on the dplyr package (part of tidyverse)

install.packages("nycflights13")
library(tidyverse)
library(nycflights13)

glimpse(flights)
view(flights) #opens new window

#Tibbles are data frames, but slightly tweaked to work better in the tidyverse.

#DATA TYPES
# int stands for integers.
# dbl stands for doubles, or real numbers.
# chr stands for character vectors, or strings.
# dttm stands for date-times (a date + a time)
# lgl stands for logical, vectors that contain only TRUE or FALSE.
# fctr stands for factors, which R uses to represent categorical variables with
# # fixed possible values.
# date stands for dates


# 5 KEY FUNCTIONS
# Pick observations by their values (filter()).
# Reorder the rows (arrange()).
# Pick variables by their names (select()).
# Create new variables with functions of existing variables (mutate()).
# Collapse many values down to a single summary (summarise()).

# These can all be used in conjunction with group_by() which changes the scope of 
# each function from operating on the entire dataset to operating on it 
# group-by-group. These six functions provide the verbs for a language of data 
# manipulation.
# 
# All verbs work similarly:
#   
# The first argument is a data frame.
# 
# The subsequent arguments describe what to do with the data frame, using the 
# variable names (without quotes).
# 
# The result is a new data frame.
# 
# Together these properties make it easy to chain together multiple simple steps 
# to achieve a complex result. Let's dive in and see how these verbs work.



#-------------------------------------------------------------------------------
# 5.2 - FILTER

jan1 <- filter(flights, month == 1, day == 1)

## Comparisons ... make sure to use ==

## Every number is approx ... so use near()
### ex: near(sqrt(2) ^ 2,  2)
#> [1] TRUE


## Logical operators
#   or = | ; and = &; ! = not; xor = xor

# De Morgan's law: !(x & y) is the same as !x | !y, 
# and !(x | y) is the same as !x & !y. 

# OR LOGIC SHORTCUT
#   %in% c(X,Y,Z)

# find flights that werent delayed (arrival and depart) by more than 2 hr
filter(flights, !(arr_delay > 120 | dep_delay > 120))
filter(flights, arr_delay <= 120, dep_delay <= 120)


# Missing Values
x <- NA
is.na(x)
# filter() excludes FALSE and NA

# 5.2 EXERCISES
##  1 - Find all flights that:

    # Had an arrival delay of two or more hours
filter(flights, arr_delay >= 120)
    # Flew to Houston (IAH or HOU)
glimpse(
  filter(flights, dest == "IAH" | dest == "HOU")
  )
    # Were operated by United, American, or Delta
glimpse(
  filter(flights, carrier %in% c("AA","DL","UA"))
  )
    # Departed in summer (July, August, and September)
filter(flights, month >= 7 & month <= 9)
    # Arrived more than two hours late, but didn't leave late
filter(flights, arr_delay > 120 & dep_delay <= 0)
    # Were delayed by at least an hour, but made up over 30 minutes in flight
filter(flights, dep_delay >= 60 & arr_delay < 30)
    # Departed between midnight and 6am (inclusive)
summary(filter(flights, dep_time <= 600 | dep_time == 2400))

## 2 - between()
filter(flights, between(month,7,9))


## 3 - How many flights have a missing dep_time? What other variables are
##    missing? What might these rows represent?

(missdep <- length(filter(flights, is.na(dep_time) == TRUE)))

#The output of the function summary() includes the number of missing values for
#all non-character variables.


# 5.3 - ARRANGE with arrange()
    # arrange() works similarly to filter() except that instead of selecting \
    # rows,it changes their order.

## changes the ORDER OF THE ROWS!!!!
arrange(flights, year, month, day)

arrange(flights, desc(dep_delay))

#missing values always sorted at the end
tail(
  arrange(flights, desc(dep_delay))
)


## 5.3 EXERCISES 
    ### 1- How could you use arrange() to sort all missing values to the start?
    ###(Hint: use is.na()).
arrange(flights, !is.na(dep_delay))
arrange(flights, desc(is.na(dep_time)), dep_time)

    ### 2 - Sort flights to find the most delayed flights. Find the flights 
    ### that left earliest.
arrange(flights, desc(dep_delay))
arrange(flights, dep_delay)

    ### 3 - highest speed
glimpse(flights)
head(arrange(flights, desc(distance / (air_time/60))))

    ### 4 - traveled longest / short
glimpse(arrange(flights, desc(distance))) # long
glimpse(arrange(flights, distance)) # short



# 5.4 - SELECT columns with select()

  # select() allows you to rapidly zoom in on a useful subset using operations
  # based on the names of the variables.

select(flights, day, month, year)

# Select all columns between year and day (inclusive)
select(flights, year:day)

# Select all columns except those from year to day (inclusive)
select(flights, -(year:day))


#useful functions
# starts_with("abc"): matches names that begin with "abc".
# 
# ends_with("xyz"): matches names that end with "xyz".
# 
# contains("ijk"): matches names that contain "ijk".
# 
# matches("(.)\\1"): selects variables that match a regular expression. 
# This one matches any variables that contain repeated characters. You'll 
# learn more about regular expressions in strings.
# 
# num_range("x", 1:3): matches x1, x2 and x3.


# use the rename() function
rename(flights, tail_num = tailnum)
glimpse(flights)


#use select with everything()
head(select(flights, time_hour, air_time, everything()))


## 5.4 EXERCISES
    ### 1 -Brainstorm as many ways as possible to select
    ### dep_time, dep_delay, arr_time, and arr_delay from flights.
glimpse(flights)
select(flights, starts_with("arr") | starts_with("dep"))
select(flights,dep_time:arr_delay, -starts_with("sched"))

    ### 2 - what happens if you select a var twices
select(flights, tailnum, tailnum)
select(flights, tailnum, everything())

    ### 3  - any_of()?
vars <- c("year", "month", "day", "dep_delay", "arr_delay")
select(flights, any_of(vars))


    ### 4 - 
select(flights, contains("TIME"))
# case dont matter
select(flights, contains("TIME", ignore.case = FALSE))
# returns 0



#------------------------------------------------------------------------------
# 5.5 - ADD NEW VARIBALES WITH MUTATE

## mutate() always adds to the end of the df

(flights_sml <- select(flights,
  year:day,
  ends_with("delay"),
  distance,
  air_time
))

mutate(flights_sml,
       gain = dep_delay - arr_delay,
       speed = distance / air_time * 60,
       hours = air_time / 60,
       gain_per_hour = gain / hours
       )

## to only keep the new vars use transmutate()

(trans <- transmute(flights,
  gain = dep_delay - arr_delay,
  hours = air_time / 60,
  gain_per_hour = gain / hours
))


## USEFUL FUNCS

# Arithmetic operators: +, -, *, /, ^. These are all vectorised, using the so
# called "recycling rules". If one parameter is shorter than the other, it will
# be automatically extended to be the same length. This is most useful when one
# of the arguments is a single number: air_time / 60, hours * 60 + minute, etc.
# 
# Arithmetic operators are also useful in conjunction with the aggregate functions
# you'll learn about later. For example, x / sum(x) calculates the proportion of a
# total, and y - mean(y) computes the difference from the mean.
# 
# Modular arithmetic: %/% (integer division) and %% (remainder), 
# where x == y * (x %/% y) + (x %% y). Modular arithmetic is a handy tool because
# it allows you to break integers up into pieces. For example, in the flights 
# dataset, you can compute hour and minute from dep_time with:


(arithdiv <- transmute(flights,
                      dep_time,
                      hour = dep_time %/% 100,   #arith div
                      minute = dep_time %/% 100
                      ))


## USEFUL
  # LOGS
    # Logs: log(), log2(), log10(). Logarithms are an incredibly useful
    # transformation for dealing with data that ranges across multiple orders
    # of magnitude.
      #log2() because it's easy to interpret: a difference of 1 on the log scale
      # corresponds to doubling on the original scale and a difference of
      # -1 corresponds to halving.

  # OFFSETS
    # Offsets: lead() and lag() allow you to refer to leading or lagging values.
    # This allows you to compute running differences (e.g. x - lag(x)) or find 
    # when values change (x != lag(x)). They are most useful in conjunction with
    # group_by(), which you'll learn about shortly.
(x <- 1:10)
#>  [1]  1  2  3  4  5  6  7  8  9 10
lag(x)
#>  [1] NA  1  2  3  4  5  6  7  8  9
lead(x)
#>  [1]  2  3  4  5  6  7  8  9 10 NA


  # CUMULATIVE FUNCS
    # Cumulative and rolling aggregates: R provides functions for running sums, 
    # products, mins and maxes: cumsum(), cumprod(), cummin(), cummax(); and dplyr
    # provides cummean() for cumulative means. 
x
#>  [1]  1  2  3  4  5  6  7  8  9 10
cumsum(x)
#>  [1]  1  3  6 10 15 21 28 36 45 55
cummean(x)
#>  [1] 1.0 1.5 2.0 2.5 3.0 3.5 4.0 4.5 5.0 5.5
#>  

  # LOGICAL FUNC

  # RANKS
    # min_rank(). It does the most usual type of ranking (e.g. 1st, 2nd, 3rd..


## 5.5 EXERCISES
  ### 1 - Convert dep_time/sched_dep_time to a more convenient representation
  ### of number of minutes since midnight
flight_times <- mutate(flights,
  dep_time_mins = 
    (dep_time %/% 100 * 60 + dep_time %% 100) %% 1440,
  sched_dep_time_mins = 
    (sched_dep_time %/% 100 * 60 +sched_dep_time %% 100) %% 1440
)
# view only relevant columns
select(
  flights_times, dep_time, dep_time_mins, sched_dep_time,
  sched_dep_time_mins
)
#> # A tibble: 336,776 x 4
#>   dep_time dep_time_mins sched_dep_time sched_dep_time_mins
#>      <int>         <dbl>          <int>               <dbl>
#> 1      517           317            515                 315
#> 2      533           333            529                 329
#> 3      542           342            540                 340
#> 4      544           344            545                 345
#> 5      554           354            600                 360
#> 6      554           354            558                 358
#> # . with 336,770 more rows

    ### 2 - Compare air_time with arr_time - dep_time
transmute(flights,
       air_time,
       bad_dur = arr_time - dep_time,
       arr_time,
       dep_time)
# not in mins
# ...
#define a function
time2mins <- function(x) {
  (x %/% 100 * 60 + x %% 100) %% 1440
}

flight_airtime <- transmute(flights,
       dep_time_mins = time2mins(dep_time),
       arr_time_mins = time2mins(arr_time),
       air_time,
       airtime_diff = arr_time_mins - dep_time_mins
       )
# ...


    ### 3 - 
flights_deptime <- flights %>% 
  select(dep_time, sched_dep_time, dep_delay) %>% 
  mutate(dep_time_min = time2mins(dep_time),
         sched_dep_time_min = time2mins(sched_dep_time),
         calc_delay = dep_time_min - sched_dep_time_min,
         checker = dep_delay - calc_delay
         )

ggplot(
    filter(flights_deptime, checker > 0),
    aes(x= sched_dep_time_min, y = checker)
    ) +
    geom_point()
# ones around midnight

    ### 4 - top 10 delayed flights
flights %>% 
  select(dep_delay) %>% 
  arrange(desc(dep_delay)) %>% 
  top_n(10,dep_delay)



# ------------------------------------------------------------------------------
# 5.6 - `GROUPED SUMMARIES with summarise()`
summarise(flights, delay = mean(dep_delay, na.rm = TRUE))
## not too useful without groupby
by_day <- group_by(flights, year, month, day)  ## avg delay per date
summarise(by_day, delay = mean(dep_delay, na.rm = TRUE))

## COMBINING MULTIPLE OPERATIONS WITH THE PIPE

### helps min lines of codes and adding var names  ..... %>% = "THEN"
### we want to explore relationship between flight length and delays
delays <- flights %>% 
  group_by(dest) %>% 
  summarise(
    count = n(),
    distance = mean(distance, na.rm = TRUE),
    delay = mean(dep_delay, na.rm = TRUE)
  ) %>% 
  filter(count > 20, dest != "HNL" )
# group then summarize then filter 

# x %>% f(y) turns into f(x, y), 
# and x %>% f(y) %>% g(z) turns into g(f(x, y), z) and so on.


## MISSING VALUES
flights %>% 
  group_by(year, month, day) %>% 
  summarise(mean = mean(dep_delay))
# <-  alot of NAs bc of flights that were cancelled

flights %>% 
  group_by(year, month, day) %>% 
  summarise(mean = mean(dep_delay, na.rm = TRUE))
# :)

## create a new df by removing cancelled flights
not_cancelled <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))


## COUNTS

  # Whenever you do any aggregation, it's always a good idea to include either 
  # a \ count (n()), or a count of non-missing values (sum(!is.na(x))). 
  # That way you can check that you're not drawing conclusions based on very 
  # small amounts of data.

delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(
    delay = mean(arr_delay)
  )
ggplot(data = delays, mapping = aes(x = delay)) +
  geom_freqpoly(binwidth = 10)
# story a little more nuanced .... we dont know the sample size of mean!!
  ## lets look at number of flights vs average delay

delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(
    delay = mean(arr_delay, na.rm = TRUE),
    n = n()
  )
ggplot(data = delays, mapping = aes(x = n, y = delay)) +
  geom_point(alpha = 1/10)
        
## makes more sense.... the plane with a 300 min delay had 1 flight

  # The shape of this plot is very characteristic: whenever you plot a mean
  # (or other summary) vs. group size, you'll see that the variation decreases
  # as the sample size increases.

## filter out the groups with not a lot of observations

delays %>% 
  filter(n>25) %>% 
  ggplot(mapping = aes(x = n, y = delay)) +
    geom_point(alpha = 1/10)

### LETS LOOK AT NEW DATA FROM BATTING 
install.packages("Lahman")
batting <- as.tibble(Lahman::Batting)

batters <- batting %>% 
  group_by(playerID) %>% 
  summarise(
    ba = sum(H, na.rm = TRUE) / sum(AB, na.rm = TRUE),
    ab = sum(AB, na.rm = TRUE)
  )

batters %>%
  filter(ab > 100) %>% 
  ggplot(data = batters, mapping = aes(x = ab, y = ba)) +
    geom_point() +
    geom_smooth(se = FALSE)

batters %>% 
  #filter(ab > 100) %>% 
  arrange(desc(ba))  # ppl are lucky, important rank nuance

### other useful summary() functions
    # LOC -- median(), mean()
  not_cancelled %>% 
    group_by(year, month, day) %>% 
    summarise(
      avg_delay = mean(arr_delay),
      avg_delay_pos = mean(arr_delay[arr_delay > 0])  # add logical stmnt
    )
    
  # SPREAD -- sd(x), IQR(x), mad(x)
    # why is distance to some destinations more variable than others
  not_cancelled %>% 
    group_by(dest) %>% 
    summarise(distance_sd = sd(distance)) %>% 
    arrange(desc(distance_sd))
  
  # RANK - min, max, quantile(x, 0.25)
    # when so firast and last flights depart
  not_cancelled %>% 
    group_by(year,month,day) %>% 
    summarise(
      first = min(dep_time),
      last = max(dep_time)
  )
  
  # Measures of position: first(x), nth(x, 2), last(x)
  
  # Counts: You've seen n(), which takes no arguments, and returns the size of
  # the current group. To count the number of non-missing values, use sum(!is.na(x)).

  # Counts and proportions of logical values: sum(x > 10), mean(y == 0)
  
  
## GROUPING BY MULT VARIABLES
daily <- group_by(flights, year, month, day)
(per_day <- summarise(daily, flights = n()))

(per_month <- summarise(per_day, flights = sum(flights)))


## UNGROUPING
daily %>% 
  ungroup() %>%           # no longer grouped by date
  summarise(flights = n())  #all flights


# 5.6 EXERCISES
## 1 - 
    # A flight is 15 minutes early 50% of the time, 
    # and 15 minutes late 50% of the time.
not_cancelled <- flights %>% 
  filter(!is.na(air_time))

not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(
    count = n(),
    early_15 = mean(arr_delay[arr_delay < -15]),
    late_15 = mean(arr_delay[arr_delay > 15]),
    early_15_frq = -1 * early_15 / count,
    late_15_frq = late_15 / count
  ) %>% 
  filter(early_15_frq > 0.5 | late_15_frq > 0.5)

not_cancelled %>%
  group_by(tailnum) %>% 
  mutate(
    count = n(),
    median_arr_delay = median(arr_delay),
    median_dep_delay = median(dep_delay)
  ) %>% 
  filter(count > 30) %>%  #mroe than 30 flights
  arrange(median_arr_delay,median_dep_delay) %>% 
  select(tailnum,count, median_arr_delay,median_dep_delay)
# ?????????????????????


    ## flight is always 10 min late
(late10 <- not_cancelled %>% 
  group_by(tailnum, origin, dest) %>% 
  summarise(
    count = n(),
    arr_delay_10 = mean(arr_delay[arr_delay > 10], na.rm = TRUE)
  ) %>% 
  filter(count > 20) %>% 
  arrange(desc(arr_delay_10)))
ggplot(data = late10, mapping = aes(x = count, y = arr_delay_10)) +
    geom_point(alpha = 1/5) +
    labs(
      x = '# of Flights',
      y = 'Avg Delay',
      title = 'Flight Paths Averaging > 10 min late '
      )

    ## 99% of the time a flight is on time. 1% of the time it's 2 hours late
not_cancelled %>% 
  group_by(tailnum,origin,dest) %>% 
  summarise(
    count = n(),
    on_time = mean(arr_delay < 1),
    late2hr = mean(arr_delay > 120),
  ) %>% 
  filter(on_time >= 0.99 & late2hr < 0.1) %>% 
  arrange(desc(on_time))

### IDK


## 2 - 
#   Come up with another approach that will give you the same output as
not_cancelled %>%     #counts of dest arrivals
  count(dest) %>% 
  arrange(desc(n))
#-->
not_cancelled %>%
  group_by(dest) %>% 
  summarise(
    n = n()
  ) %>% 
  arrange(desc(n))
  

not_cancelled %>% 
  count(tailnum, wt = distance) #sum dist of flight by tailnum
#-->
not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(
    n = n(),
    sum_dist = sum(distance,na.rm = TRUE),
    avg_dist = sum_dist / n
  ) %>% 
  arrange(desc(sum_dist))

`## 3 -
      # Our definition of cancelled flights 
      # (is.na(dep_delay) | is.na(arr_delay) ) is slightly suboptimal. 
      # Why? Which is the most important column?

# arr_delay is most important ... if it never departs it never lands... could crash/reroute
filter(flights, !is.na(dep_delay), is.na(arr_delay)) %>%
  select(dep_time, arr_time, sched_arr_time, dep_delay, arr_delay)

  ## 4 - look at number of cancelled flights per day, pattern?
        # is prop of cancelled flights related to avg delay

        # cancelled vs # of flights
(cancel_per_day <- 
  flights %>% 
  mutate(cancelled = (is.na(arr_delay) | is.na(dep_delay))) %>% 
  group_by(year,month,day) %>% 
  summarise(
    cancelled_num = sum(cancelled),
    flights_num = n()
  ) %>% 
  arrange(desc(cancelled_num)))
ggplot(data = cancel_per_day, mapping = aes(x = flights_num, y = cancelled_num)) +
    geom_point(alpha = 1/2) 


        # cancel # vs avg delay
# cancel_delays <- 
#   flights %>% 
#   group_by(year,month,day) %>% 
#   summarise(
#     cancell
#   )


#-------------------------------------------------------------------------------
## 5.7 GROUPED MUTATES (and FILTERS)


# find the worst members of a group
flights_sml %>% 
  group_by(year,month,day) %>% 
  filter(rank(desc(arr_delay)) < 10)

# find all groups bigger than a threshold
popular_dests <- flights %>% 
  group_by(dest) %>% 
  filter(n() > 365)

# standardize to compute group metrics
popular_dests %>% 
  filter(arr_delay > 0) %>% 
  mutate(prop_delay = arr_delay / sum(arr_delay)) %>% 
  select(year:day, dest, arr_delay, prop_delay)

# A grouped filter is a grouped mutate followed by an ungrouped filter. 
# I generally avoid them except for quick and dirty manipulations: otherwise 
# it's hard to check that you've done the manipulation correctly.
# 
# Functions that work most naturally in grouped mutates and filters are known as 
# window functions (vs. the summary functions used for summaries). You can learn 
# more about useful window functions in the corresponding vignette: 
#   vignette("window-functions")


## 5.7 -- EXERCISES
  ## 1 - 
# Summary functions (mean()), offset functions (lead(), lag()), ranking functions 
# (min_rank(), row_number()), operate within each group when used with group_by() 
# in mutate() or filter(). Arithmetic operators (+, -), logical operators (<, ==),
# modular arithmetic operators (%%, %/%), logarithmic functions (log) are not 
# affected by group_by.

# Summary functions like mean(), median(), sum(), std() and others covered in the
# section Useful Summary Functions calculate their values within each group when
# used with mutate() or filter() and group_by().

tibble(x = 1:9,
       group = rep(c("a", "b", "c"), each = 3)) %>%
  mutate(x_mean = mean(x)) %>%  # before group() mean by whole group
  group_by(group) %>%
  mutate(x_mean_2 = mean(x))  # after group() mean by groups a, b, c

  # arith operators NOT affected by group
tibble(x = 1:9,
       group = rep(c("a", "b", "c"), each = 3)) %>% 
         mutate(y = x + 2) %>% 
         group_by(group) %>% 
         mutate(z = x + 2)
       )
  # The modular arithmetic operators %/% and %% are not affected by group_by()
  
  # The logarithmic functions log(), log2(), and log10() are not affected by group_by().

  # The offset functions lead() and lag() respect the groupings in group_by(). 
  # The functions lag() and lead() will only return values within each group
tibble(x = 1:9,
       group = rep(c("a", "b", "c"), each = 3)) %>% 
  group_by(group) %>% 
  mutate(x_lag = lag(x),
         lead_x = lead(x))

# The cumulative and rolling aggregate functions cumsum(), cumprod(), cummin(), 
# cummax(), and cummean() calculate values within each group.

# Logical comparisons, <, <=, >, >=, !=, and == are not affected by group_by()

# Ranking functions like min_rank() work within each group when used with group_by().

# Though not asked in the question, note that arrange() ignores groups when sorting values.


  ## 2 - which TAILNUM has the worst on time record
# focus on arrival more important
flights %>%
  filter(!is.na(arr_delay)) %>%
  group_by(tailnum) %>%
  summarise(
    arr_delay = mean(arr_delay),
    n = n()
    ) %>%
  filter(n >= 20) %>%
  filter(min_rank(desc(arr_delay)) == 1)
#N203FR

flights %>% 
  filter(tailnum == 'N203FR') %>% 
  ggplot(mapping = aes(x = arr_delay)) + 
  geom_freqpoly()

  ## 3 - What time of day should you fly if you want to avoid delays as much as possible?

flight_times %>% 
  filter(!is.na(arr_delay)) %>% 
  group_by(hour) %>% 
  summarise(
    n = n(),
    avg_delay_min_per_hr = mean(arr_delay,na.rm = TRUE)
  ) %>% 
  filter(n > 100) %>% 
  arrange(avg_delay_min_per_hr) %>% 
  ggplot() +
  geom_point(mapping = aes(x = hour, y = avg_delay_min_per_hr))

  ## 4 - For each destination, compute the total minutes of delay. For each 
  ## flight, compute the proportion of the total delay for its destination.

flights %>%
  filter(arr_delay > 0) %>%
  group_by(dest) %>%
  mutate(
    arr_delay_total = sum(arr_delay),
    arr_delay_prop = arr_delay / arr_delay_total
  ) %>%
  select(dest, month, day, dep_time, carrier, flight,
         arr_delay, arr_delay_prop) %>%
  arrange(dest, desc(arr_delay_prop))


## 5 - 
# Using lag(), explore how the delay of a flight is related to the delay of \
# the immediately preceding flight

(lagged_delays <- flights %>%
  arrange(origin, month, day, dep_time) %>%
  group_by(origin) %>%
  mutate(dep_delay_lag = lag(dep_delay)) %>%
  filter(!is.na(dep_delay), !is.na(dep_delay_lag))
) %>% 
  select(origin, month, day, dep_time, dep_delay_lag, arr_delay)

lagged_delays %>%
  group_by(dep_delay_lag) %>%
  summarise(dep_delay_mean = mean(dep_delay)) %>%
  ggplot(aes(y = dep_delay_mean, x = dep_delay_lag)) +
  geom_point() +
  scale_x_continuous(breaks = seq(0, 1500, by = 120)) +
  labs(y = "Departure Delay", x = "Previous Departure Delay")
  
lagged_delays %>%
  group_by(origin, dep_delay_lag) %>%
  summarise(dep_delay_mean = mean(dep_delay)) %>%
  ggplot(aes(y = dep_delay_mean, x = dep_delay_lag)) +
  geom_point() +
  facet_wrap(~ origin, ncol=1) +
  labs(y = "Departure Delay", x = "Previous Departure Delay")

  ## 6 - 
# Look at each destination. Can you find flights that are suspiciously fast? 
# (i.e. flights that represent a potential data entry error). 
# Compute the air time of a flight relative to the shortest flight to that 
# destination. Which flights were most delayed in the air?
flights %>% 
  filter(!is.na(arr_delay) | !is.na(air_time)) %>% 
  group_by(dest) %>% 
  mutate(
    speed = distance / (air_time*(1/60)),
  ) %>% 
  filter(speed > 550) %>% 
  arrange(desc(speed)) %>% 
  select(origin,dest,speed,distance,air_time,dep_time:arr_delay)

# NO

  ## 7 - 
# Find all destinations that are flown by at least two carriers. 
# Use that information to rank the carriers.


  ## 8 - 
# For each plane, count the number of flights before the first delay of greater than 1 hour.
  
  
  




