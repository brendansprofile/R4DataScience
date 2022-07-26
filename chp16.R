# https://r4ds.had.co.nz/dates-and-times.html

library(tidyverse)

library(lubridate)
library(nycflights13)


# ------------------------------------------------------------------------------
# 16.2 CREATING DATE/TIMES

# A date. Tibbles print this as <date>.
# 
# A time within a day. Tibbles print this as <time>.
# 
# A date-time is a date plus a time: it uniquely identifies an instant in time
# (typically to the nearest second). Tibbles print this as <dttm>.


# current day / time
date()
now()
today()

# -------------
# FROM STRINGS

  # creating dates
ymd("2017-01-31")
  # [1] "2017-01-31"
mdy("January 31st, 2017")
  # [1] "2017-01-31"
dmy("31-Jan-2017")
  # [1] "2017-01-31"

  # also take unquoted numbers
ymd(20170131)

  # creating date-times adding _hms
ymd_hms("2017-01-31 20:11:59")
# [1] "2017-01-31 20:11:59 UTC"
mdy_hm("01/31/2017 08:01")
# [1] "2017-01-31 08:01:00 UTC"

  # also you can just supply a timezone 
ymd(20170131, tz = "UTC")
# [1] "2017-01-31 UTC"


# -----------
# FROM INDIVIDUAL COMPONTENTS
flights %>% 
  select(year, month, day, hour, minute)
  # # A tibble: 336,776 x 5
  # year month   day  hour minute
  # <int> <int> <int> <dbl>  <dbl>
  #   1  2013     1     1     5     15

# use make_date()  or  make_datetime()
flights %>% 
  select(year, month, day, hour, minute) %>% 
  mutate(departure = make_datetime(year,month,day,hour,minute))
  # # A tibble: 336,776 x 6
  # year month   day  hour minute departure          
  # <int> <int> <int> <dbl>  <dbl> <dttm>             
  #   1  2013     1     1     5     15 2013-01-01 05:15:00


# do for the rest
make_datetime_100 <- function(year, month, day, time) {
  make_datetime(year, month, day, time %/% 100, time %% 100)
}

flights_dt <- flights %>% 
  filter(!is.na(dep_time) | !is.na(arr_time)) %>% 
  mutate(
    dep_time = make_datetime_100(year, month, day, dep_time),
    arr_time = make_datetime_100(year, month, day, arr_time),
    sched_dep_time = make_datetime_100(year, month, day, sched_dep_time),
    sched_arr_time = make_datetime_100(year, month, day, sched_arr_time)
  ) %>% 
  select(origin, dest, ends_with("delay"), ends_with("time"))
flights_dt

# visualize departures across the year
flights_dt %>% 
  ggplot(aes(dep_time)) +
  geom_freqpoly(binwidth = (60*60*24)) # 1 day

# visualize a single day
flights_dt %>% 
  filter(dep_time < ymd(20130102)) %>% 
  ggplot(aes(dep_time)) +
  geom_freqpoly(binwidth = (60*10)) # 10 min 


# ----------------
# FROM OTHER TYPES

# Switch between a date-time and a date. as_datetime() and as_date():

as_datetime(today())
#> [1] "2020-10-09 UTC"
as_date(now())
#> [1] "2020-10-09"


# EXERCISES
  # 1 - invalid dates?
ymd(c("2010-10-10", "banana"))
  # [1] "2010-10-10" NA          
  # Warning message:
  #   1 failed to parse. 

  # 2 - tzone for today() ?
today()
today(tzone = "EST") # specifies day based on TZ

  # 3 - use correct lubridate
d1 <- "January 1, 2010"
d2 <- "2015-Mar-07"
d3 <- "06-Jun-2017"
d4 <- c("August 19 (2015)", "July 1 (2015)")
d5 <- "12/30/14" # Dec 30, 2014

mdy(d1)
# [1] "2010-01-01"
ymd(d2)
# [1] "2015-03-07"
dmy(d3)
# [1] "2017-06-06"
mdy(d4)
# [1] "2015-08-19" "2015-07-01"
mdy(d5)
# [1] "2014-12-30"



# ------------------------------------------------------------------------------
# 16.3 DATE TIME COMPONENTS

# GETTING COMPONENTS
datetime1 <- ymd_hms("2016-07-08 12:34:56")

year(datetime1)
#> [1] 2016
month(datetime1)
#> [1] 7
mday(datetime1)
#> [1] 8

yday(datetime1)# day of the year
#> [1] 190

wday(datetime1) # week of the year
#> [1] 6
wday(datetime1, label = TRUE)
# [1] Fri
# Levels: Sun < Mon < Tue < Wed < Thu < Fri < Sat

month(datetime1, label = TRUE)
#> [1] Jul
#> 12 Levels: Jan < Feb < Mar < Apr < May < Jun < Jul < Aug < Sep < ... < Dec


# visualize flights by day of week
flights_dt %>% 
  mutate(wday = wday(dep_time, label = TRUE)) %>% 
  ggplot(aes(x = wday)) + 
    geom_bar()

# visualize avg departure delay by minute within hour
flights_dt %>% 
  mutate(minute = minute(dep_time)) %>% 
  group_by(minute) %>% 
  summarise(
    avg_delay = mean(arr_delay, na.rm = TRUE),
    n = n()) %>% 
  ggplot(aes(minute, avg_delay)) +
  geom_line() # looks like there is less delays 20-30 min on hour....hmmmmmm


# look at sched dep time ..... not really a pattern
sched_dep <- flights_dt %>% 
  mutate(minute = minute(sched_dep_time)) %>% 
  group_by(minute) %>% 
  summarise(
    avg_delay = mean(arr_delay, na.rm = TRUE),
    n = n()
  )
ggplot(sched_dep, aes(minute, avg_delay)) +
  geom_line()

# uncover the "trend" --> there is a strong pattern of flights leaving at "nice" times
ggplot(sched_dep, aes(minute, n)) +
  geom_line()


# ------------
# ROUNDING

  # alternative to plotting individual components -> round date to nearby unit of time
    # floor_date() -  round down
    # round_date()
    # ceiling_date()  - round up
      # -> Each function takes a vector of dates to adjust + name of the unit round (ex:"week")

flights_dt %>% 
  count(week = floor_date(dep_time, "week")) %>% 
  ggplot(aes(week, n)) +
  geom_line()
    # looks at # of flights per week


# ------------------
# SETTING COMPONENTS
(datetime <- ymd_hms("2016-07-08 12:34:56"))
#> [1] "2016-07-08 12:34:56 UTC"

#modify a date
year(datetime) <- 2020
datetime
# [1] "2020-07-08 12:34:56 UTC"   #changed to 2020

month(datetime) <- 01    # changed to jan
datetime
# [1] "2020-01-08 12:34:56 UTC"

hour(datetime) <- hour(datetime) + 1
datetime
#> [1] "2020-01-08 13:34:56 UTC"


# can use update() to change multiple vals of a date
update(datetime, year = 2020, month = 2, mday = 2, hour = 2)
  # [1] "2020-02-02 02:34:56 UTC"

# if values are too big, it will roll them over
ymd("2015-02-01") %>% 
  update(mday = 30)
#> [1] "2015-03-02"

ymd("2015-02-01") %>% 
  update(hour = 400)
#> [1] "2015-02-17 16:00:00 UTC"


# update() to show the distribution of flights across the course of the day for every day of the year:
flights_dt %>% 
  mutate(dep_hour = update(dep_time, yday = 4)) %>% 
  ggplot(aes(dep_hour)) +
    geom_freqpoly(binwidth = 300)
# Setting larger components of a date to a constant is a powerful technique 
# that allows you to explore patterns in the smaller components.


# EXERCISES
# 1 - How does the distribution of flight times within a day change over the course of the year?
flights_dt %>% 
 filter(!is.na(dep_time)) %>% 
  mutate(dep_hour = update(dep_time, yday = 1)) %>% 
  mutate(month = factor(month(dep_time))) %>%
  ggplot(aes(dep_hour, color = month)) +
  geom_freqpoly(binwidth = 60 * 60)

# 2 - Compare dep_time, sched_dep_time and dep_delay. Are they consistent? Explain your findings.
flights_dt %>%
  mutate(
    calc_dep_time = sched_dep_time + dep_delay*60,
    check = (calc_dep_time == dep_time)
    ) %>% 
  select(sched_dep_time, dep_delay, dep_time, calc_dep_time, check) %>% 
  filter(check == FALSE)
# errors with dates over midnight, didnt account for in make_datetime100

# 3 - Compare air_time with the duration between the departure and arrival.
  # Explain your findings. (Hint: consider the location of the airport.)
flights_dt %>%
  mutate(
    flight_duration = as.numeric(arr_time - dep_time),
    air_time_mins = air_time,
    diff = flight_duration - air_time_mins
  ) %>%
  select(origin, dest, flight_duration, air_time_mins, diff) %>% 
  filter(diff > 1 | diff < -1)

# 4 - How does average delay time change over course of a day? Should you use dep_time or sched_dep_time? Why?
flights_dt %>% 
  filter(!is.na(dep_time)) %>% 
  mutate(hour = hour(sched_dep_time)) %>% 
  group_by(hour) %>% 
  mutate(avg_delay = mean(dep_delay)) %>% 
    ggplot(aes(hour,avg_delay)) +
    geom_point()+
    geom_smooth()

# 5 - What day of week to minimize delays
flights_dt %>% 
  mutate(dow = wday(sched_dep_time, label = TRUE)) %>% 
  group_by(dow) %>% 
  summarise(
    dep_delay = mean(dep_delay, na.rm = TRUE),
    arr_delay = mean(arr_delay, na.rm = TRUE)
    ) %>% 
  ggplot(aes(dow,dep_delay)) +
  geom_bar(stat = "identity")

# 6 -

# 7 - Confirm my hypothesis that the early departures of flights in minutes 
# 20-30 and 50-60 are caused by scheduled flights that leave early.
# Hint: create a binary variable that tells you whether or not a flight was delayed.

flights_dt %>% 
  mutate(
    minute = minute(dep_time),
    early = dep_delay < 0
    ) %>% 
  group_by(minute) %>% 
  summarise(
    early = mean(early, na.rm = TRUE),
    n = n()
  ) %>% 
  ggplot(aes(minute,early)) +
  geom_line()




# ------------------------------------------------------------------------------
# 16.4 TIME SPANS

# important classes that rep time spans
    # durations,   represent exact number of seconds
    # periods,  which represent human units like weeks and months.
    # intervals,  which represent a starting and ending point.


# -----------
# DURATIOnS
  # in R whe you subtract dates you get a difftime obj

# How old is Hadley?
h_age <- today() - ymd(19791014)
h_age
#> Time difference of 14971 days

# lubridate provides an alternative which always uses seconds: the duration.
as.duration(h_age)
  # [1] "1345420800s (~42.63 years)"


dseconds(15)
#> [1] "15s"
dminutes(10)
#> [1] "600s (~10 minutes)"
dhours(c(12, 24))
#> [1] "43200s (~12 hours)" "86400s (~1 days)"
ddays(0:5)
#> [1] "0s"                "86400s (~1 days)"  "172800s (~2 days)"
#> [4] "259200s (~3 days)" "345600s (~4 days)" "432000s (~5 days)"
dweeks(3)
#> [1] "1814400s (~3 weeks)"
dyears(1)
#> [1] "31557600s (~1 years)"


# add/ multiply durations
2 * dyears(1)
#> [1] "63115200s (~2 years)"
dyears(1) + dweeks(12) + dhours(15)
#> [1] "38869200s (~1.23 years)"


# add durations from days
tomorrow <- today() + ddays(1)
last_year <- today() - dyears(1)

# durations rep a number of sec, may get a weird result
one_pm <- ymd_hms("2016-03-12 13:00:00", tz = "America/New_York")

one_pm
#> [1] "2016-03-12 13:00:00 EST"
one_pm + ddays(1)
#> [1] "2016-03-13 14:00:00 EDT"   # why 1hr later --> daylight save time
 

# --------
# PERIODS
  # time spans but no fix length in seconds, work with "human times"
one_pm
#> [1] "2016-03-12 13:00:00 EST"
one_pm + days(1)
#> [1] "2016-03-13 13:00:00 EDT"


seconds(15)
#> [1] "15S"
minutes(10)
#> [1] "10M 0S"
hours(c(12, 24))
#> [1] "12H 0M 0S" "24H 0M 0S"
days(7)
#> [1] "7d 0H 0M 0S"
months(1:6)
#> [1] "1m 0d 0H 0M 0S" "2m 0d 0H 0M 0S" "3m 0d 0H 0M 0S" "4m 0d 0H 0M 0S"
#> [5] "5m 0d 0H 0M 0S" "6m 0d 0H 0M 0S"
weeks(3)
#> [1] "21d 0H 0M 0S"
years(1)
#> [1] "1y 0m 0d 0H 0M 0S"


# add/multiply periods
10 * (months(6) + days(1))
#> [1] "60m 10d 0H 0M 0S"
days(50) + hours(25) + minutes(2)
#> [1] "50d 25H 2M 0S"


# add to dates, more what you will expect
# A leap year
ymd("2016-01-01") + dyears(1)
#> [1] "2016-12-31 06:00:00 UTC"
ymd("2016-01-01") + years(1)
#> [1] "2017-01-01"

# Daylight Savings Time
one_pm + ddays(1)
#> [1] "2016-03-13 14:00:00 EDT"
one_pm + days(1)
#> [1] "2016-03-13 13:00:00 EDT"


# use periods to fix oddity with flight dates
flights_dt %>% 
  filter(arr_time < dep_time)
# A tibble: 10,633 x 9
# origin dest  dep_delay arr_delay dep_time            sched_dep_time      arr_time           
# <chr>  <chr>     <dbl>     <dbl> <dttm>              <dttm>              <dttm>             
#   1 EWR    BQN           9        -4 2013-01-01 19:29:00 2013-01-01 19:20:00 2013-01-01 00:03:00
# 2 JFK    DFW          59        NA 2013-01-01 19:39:00 2013-01-01 18:40:00 2013-01-01 00:29:00
# 3 EWR    TPA          -2         9 2013-01-01 20:58:00 2013-01-01 21:00:00 2013-01-01 00:08:00

  # -> fix by adding days(1)
flights_dt %>% 
  mutate(
    overnight = arr_time < dep_time, #binary
    arr_time = arr_time + days(overnight * 1),
    sched_arr_time = sched_arr_time + days(overnight * 1)
  ) %>% 
  filter(overnight, arr_time < dep_time) 
# 0 results , fixed



# -----------
# INTERVALS
years(1) / days(1)
#> [1] 365.25

# interval is a duration with a starting point: 
    # that makes it precise so you can determine exactly how long it is:
next_year <- today() + years(1)
(today() %--% next_year) / ddays(1)
#> [1] 365

# to find out how many periods in an interval, integer division
(today() %--% next_year) %/% days(1)
#> [1] 365


# -------------
# SUMMARY

# As always, pick the simplest data structure that solves your problem. 
  # If you only care about physical time, use a duration; 
  # if you need to add human times, use a period; 
  # if you need to figure out how long a span is in human units, use an interval.

            # SEE CHART

# EXERCISES
#   Why is there months() but no dmonths()?
#   Explain days(overnight * 1) to someone who has just started learning R. How does it work?
#   Create a vector of dates giving the first day of every month in 2015. Create a vector of dates giving the first day of every month in the current year.
#   Write a function that given your birthday (as a date), returns how old you are in years.
#   Why can't (today() %--% (today() + years(1))) / months(1) work?



# ------------------------------------------------------------------------------
# 16.5 TIME ZONES

  # my  zone
Sys.timezone() 
#[1] "America/New_York"

# time zone list
length(OlsonNames())
#> [1] 594
head(OlsonNames())
#> [1] "Africa/Abidjan"     "Africa/Accra"       "Africa/Addis_Ababa"
#> [4] "Africa/Algiers"     "Africa/Asmara"      "Africa/Asmera"


# ...........
# *** COME BACK TO








