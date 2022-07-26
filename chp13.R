# https://r4ds.had.co.nz/relational-data.html

# CHP 13 RELATIONAL DATA -------------------------------------------------------


# 13.1 Intro

  # typically you have multiple tables worth of data
    # => collectively, multiple tables of data called relational data

# Need verbs to work with  relational data

  # 1) Mutating Joins
        # add new variables to one data frame from matching observations
        # in another
  # 2) Filtering Joins
        # filter observations from one data frame based on whether or not
        # they match an observation in another table

  # 3) Set Operations 
        # treat observations as if they were set elements

# relational database management system (RDBMS) ... SQL
  # dplyr is slightly easier than SQL bc it is built for data analysis

library(tidyverse)
library(nycflights13)

# ------------------------------------------------------------------------------
# 13.2 nycflights13

  # four tibbles related to 'flights'
airlines   # look up full carrier name from abbrev code
airports   # id by 'faa' airport code
planes     # id by 'tailnum'
weather    # weather at each NYC airport by each hour

# *** relational data diagram (ERD) ***
    # flights connects to planes via a single variable, tailnum.
    
    # flights connects to airlines through the carrier variable.
     
    # flights connects to airports in two ways: via the origin and dest variables.

    # flights connects to weather via origin (the location), and year, month, day and hour (the time).


# 12.1 EXERCISES

  # 1 - Imagine you wanted to draw (approximately) the route each plane flies 
  # from its origin to its destination. What variables would you need? 
  # What tables would you need to combine?
    
    # flights(origin,dest,distance,flight,tailnum) + airports(lat, long)
flights_latlon <- flights %>%
  inner_join(select(airports, origin = faa, origin_lat = lat, origin_lon = lon),
             by = "origin"
  ) %>%
  inner_join(select(airports, dest = faa, dest_lat = lat, dest_lon = lon),
             by = "dest"
  )
flights_latlon %>%
  slice(1:100) %>%
  ggplot(aes(
    x = origin_lon, xend = dest_lon,
    y = origin_lat, yend = dest_lat
  )) +
  borders("state") +
  geom_segment(arrow = arrow(length = unit(0.1, "cm"))) +
  coord_quickmap() +
  labs(y = "Latitude", x = "Longitude")

  # 2 -
glimpse(weather)
glimpse(airports)

  # 3 -


  # 4 - 
special_days <- tribble(
  ~year, ~month, ~day, ~holiday,
  2013, 01, 01, "New Years Day",
  2013, 07, 04, "Independence Day",
  2013, 11, 29, "Thanksgiving Day",
  2013, 12, 25, "Christmas Day"
)


#-------------------------------------------------------------------------------
# 13.3 KEYS

# variables used to connect each pair of tables are called keys. 
  # A key is a variable (or set of variables) that uniquely identifies an observation. 

# Two types of keys

  # Primary Key - uniquely identifies an observation in its own table
    # ex: planes$tailnum is a primary key bc it uniquely identifies each plane in the planes table.
  
  # Foreign Key - uniquely identifies an observation in another table
    # flights$tailnum is a foreign key bc it appears in the flights table where it matches each flight to a unique plane.
  
    # a variable can be both primary and foreign key

# once you have identified primary keys, verify they uniquely ID each observation
  # count() the primary keys and look where 'n" > 1

planes %>% 
  count(tailnum) %>% 
  filter(n > 1)

weather %>% 
  count(year, month, day, hour, origin) %>% 
  filter(n > 1)

# sometimes a table does not have an explicit primary key
  # ex: what's the PK in flights
flights %>% 
  count(year, month, day, flight) %>% # NOT UNIQUE
  filter(n > 1)
      # A tibble: 29,768 x 5                
      # year month   day flight     n
      # <int> <int> <int>  <int> <int>
      # 1  2013     1     1      1     2
      # 2  2013     1     1      3     2
      # 3  2013     1     1      4     2
      # 4  2013     1     1     11     3

flights %>% 
  count(year, month, day, tailnum) %>% 
  filter(n > 1)
#> # A tibble: 64,928 x 5
#>    year month   day tailnum     n
#>   <int> <int> <int> <chr>   <int>
#> 1  2013     1     1 N0EGMQ      2
#> 2  2013     1     1 N11189      2


#  If table lacks primary key, sometimes useful to add one with mutate() and row_number() 
  # => SURROGATE KEY

# NOTE: a primary key and corresponding foreign key in another table form a RELATION
  # RELATIONS typically 1:Many (1:M)
    # example, each flight has one plane, but each plane has many flights

# 13.3 EXERCISEs
  # 1 - add a surrogate key to 'flights'
flights %>% 
  arrange(year, month, day, sched_dep_time, carrier, flight) %>% 
  mutate(flight_id = row_number()) %>% 
  glimpse()

  # 2 - 
  Lahman::Batting
#    playerID yearID stint teamID lgID  G  AB  R  H X2B X3B HR RBI SB CS BB SO IBB HBP SH SF GIDP
#   abercda01   1871     1    TRO   NA  1   4  0  0   0   0  0   0  0  0  0  0  NA  NA NA NA    0
  Lahman::Batting %>%
    count(playerID, yearID, stint) %>%
    filter(n > 1) %>%
    nrow()   # KEYS = playerID, yearID, stint
  
  # babynames::babynames
  # nasaweather::atmos
  # fueleconomy::vehicles
  
  ggplot2::diamonds %>%
    distinct() %>%
    nrow()   # NO PRIMARY KEY
  
  # 3 -
install.packages("Lahman")
library(Lahman)

glimpse(Batting)
glimpse(People)
glimpse(Salaries)

  # People
  # Primary key: playerID
   
  # Batting
  # Primary key: playerID, yearID, stint
  # Foreign keys:
  #  playerID = Master$playerID (many-to-1)
  
  # Salaries
  # Primary key: yearID, teamID, playerID
  # Foreign keys:
  #   playerID = Master$playerID (many-to-1)

# SALARIES              # PEOPLE             # BATTING
  # yearID          ---- # playerID ---->     # playerID
  # teamID                                    # yearID
  # playerID <------                          # stintID


# install.packages("dm")
# library(dm)
# 
# dm1 <- dm_from_data_frames(list(
#   Batting = Lahman::Batting,
#   Master = Lahman::Master,
#   Salaries = Lahman::Salaries
# )) %>%
#   dm_set_key("Batting", c("playerID", "yearID", "stint")) %>%
#   dm_set_key("Master", "playerID") %>%
#   dm_set_key("Salaries", c("yearID", "teamID", "playerID")) %>%
#   dm_add_references(
#     Batting$playerID == Master$playerID,
#     Salaries$playerID == Master$playerID
#   )
# 
# dm_create_graph(dm1, rankdir = "LR", columnArrows = TRUE) %>%
#   dm_render_graph()



# ------------------------------------------------------------------------------
# 13.4 MUTATING JOINS
  
  # A mutating join allows you to combine variables from two tables
    # first matches observations by their keys, then copies across variables from one table to the other.

flights2 <- flights %>% 
  select(year:day, hour, origin, dest, tailnum, carrier)
flights2

flights2 %>% 
  select(-origin, -dest) %>% 
  left_join(airlines, by = "carrier")
      # # A tibble: 336,776 x 7
      # year month   day  hour tailnum carrier name                    
      # <int> <int> <int> <dbl> <chr>   <chr>   <chr>                   
      #   1  2013     1     1     5 N14228  UA      United Air Lines Inc.
# result = additonal variable: 'name'
  
# could have also used a mutate()
flights2 %>%
  select(-origin, -dest) %>% 
  mutate(name = airlines$name[match(carrier, airlines$carrier)])



# 13.4.1 Understanding joins
  # SEE GRAPHIC
library(tidyverse)
x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  3, "x3"
)
y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2",
  4, "y3"
)


# 13.4.2 Inner join (equijoin)
  # inner join matches pairs of observations whenever their keys are equal:
  # output of an inner join is a new data frame that contains the key, the x values, and the y values.
  
x %>% 
  inner_join(y, by = "key")

  # NOTE: UNMATCHED ROWS ARE NOT INCLUDED IN THE RESULT
    # too easy to lose observations :///


# 13.4.3 Outer joins
  # outer join keeps observations that appear in at least one of the tables. 
  # There are three types of outer joins:
    # 1) left join = keep all observations in x
    # 2) right join = keep all observation in y
    # 3) full join = keeps all observations in x and y

  #  joins work by adding an additional "virtual" observation to each table.
    # if not match => 'NA'

  # DEFAULT SHOULD BE LEFT JOIN
    # use when you lookup data from another table, preserves orginal if no match


# 13.4.4 Duplicate keys
  # one table has duplicate keys
x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  2, "x3",
  1, "x4"
)
y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2"
)
left_join(x, y, by = "key") # 1:M relationship

  # both tables duplicate keys
    # usually an error bc in neither table do the keys uniquely ID an observation


# 13.4.5 Defining the key columns
 
 # by = NULL, uses all variables that appear in both tables, the so called natural join.
library(nycflights13)
flights2 %>% 
  left_join(weather)

# by = "x" 
flights2 %>% 
  left_join(planes, by = "tailnum")

  #year.x disambiguatd bc both tables have year
# A tibble: 336,776 x 16
# year.x month   day  hour origin dest  tailnum carrier year.y type    manufacturer model engines seats speed engine
# <int> <int> <int> <dbl> <chr>  <chr> <chr>   <chr>    <int> <chr>   <chr>        <chr>   <int> <int> <int> <chr> 
#   1   2013     1     1     5 EWR    IAH   N14228  UA        1999 Fixed ~ BOEING       737-~       2   149    NA Turbo~

# by = c("a" = "b")
  # matches a variable 'a' in x to a variable 'b' in y ... variables in x used as output

flights2 %>% 
  left_join(airports, c("dest" = "faa"))  # need tp specify which airport to join to


#13.4.6 EXERCISES

  # 1 - compute average delay by destination, then join on aiports df to show 
      # spatial dist of delays
head(flights)

avg_dest_delays <- flights %>% 
  group_by(dest) %>% 
  mutate(
    avg_delay = mean(dep_delay, na.rm = TRUE)
  ) %>% 
  inner_join(airports, by = c("dest" = "faa"))

avg_dest_delays %>% 
  ggplot(aes(lon, lat), color = delay) +
    borders("state") +
    geom_point() +
    coord_quickmap()

  # 2 - add the location of the origin AND dest (lat lon) to flights
airport_location <- airports %>% 
  select(faa, lat, lon)

flights %>%
  select(year:day, hour, origin, dest) %>%
  left_join(
    airport_location,
    by = c("origin" = "faa")
  ) %>%
  left_join(
    airport_location,
    by = c("dest" = "faa"),
    suffix = c("_origin", "_dest")
    # existing lat and lon variables in tibble gain the _origin suffix
    # new lat and lon variables are given _dest suffix
  )

# 3 - Is there a relationship between the age of a plane and its delays?
glimpse(planes)
# avg_dest_delays %>% 
#   left_join(
#     planes,
#     by = "tailnum",
#     suffix = c("_delays","_planes")
#     ) %>% 
#   filter(!is.na(year_planes)) %>% 
#   select(tailnum,avg_delay, year_planes) %>% 
#   ggplot(aes(year_planes, avg_delay)) +
#   geom_point()

plane_cohort <- inner_join(flights,
  select(planes, tailnum, plane_year = year),
  by = "tailnum"
) %>% 
  mutate(age = year - plane_year) %>% 
  filter(!is.na(age)) %>% 
  mutate(age = if_else(age > 25, 25L, age)) %>% 
  group_by(age) %>% 
  summarise(
    dep_delay_mean = mean(dep_delay, na.rm = TRUE),
    dep_delay_sd = sd(dep_delay, na.rm = TRUE),
    arr_delay_mean = mean(arr_delay, na.rm = TRUE),
    arr_delay_sd = sd(arr_delay, na.rm = TRUE),
    n_arr_delay = sum(!is.na(arr_delay)),
    n_dep_delay = sum(!is.na(dep_delay))
  )

ggplot(plane_cohort, aes(age, dep_delay_mean)) +
  geom_point() +
  scale_x_continuous("Age of plane in years", breaks = seq(0, 30, by = 10)) +
  scale_y_continuous("Mean Departure Delay")


ggplot(plane_cohort, aes(age, arr_delay_mean)) +
  geom_point() +
  scale_x_continuous("Age of plane in years", breaks = seq(0, 30, by = 10)) +
  scale_y_continuous("Mean Departure Delay")


# 4 - What weather conditions make it more likely to see a delay?

glimpse(weather)
summary(weather)
count(weather, origin)

flight_weather <- 
  flights %>% 
  inner_join(
    weather,
    by = c(
      "origin" = "origin",
      "year" = "year",
      "month" = "month",
      "day" = "day",
      "hour" = "hour"
    )
  )

flight_weather %>% 
  group_by(precip) %>% 
  summarise(delay = mean(dep_delay, na.rm = TRUE)) %>% 
  ggplot(aes(x = precip, y = delay)) +
  geom_line() + geom_point()


flight_weather %>% 
  ungroup() %>% 
  mutate(visib_cat = cut_interval(visib, n = 10)) %>% 
  group_by(visib_cat) %>% 
  summarise(
    delay = mean(dep_delay, na.rm = TRUE)
  ) %>% 
  ggplot(aes(x = visib_cat, y = delay)) + 
  geom_point()
  

  # 5 - what happened on Jun 13 2013
jun13_2013 <- 
  flights %>% 
  filter(
    day == 13,
    month == 6,
    year == 2013
  ) %>% 
  group_by(dest) %>% 
  summarise(delay = mean(arr_delay, na.rm = TRUE)) %>% 
  inner_join(airports, by = c("dest" = "faa"))

ggplot(jun13_2013, aes(x = lon, y = lat, colour = delay)) +
borders("state") +
geom_point(aes(size = delay)) +
coord_quickmap()+
scale_color_viridis_b()


# 13.4.7 - DPLYR VS SQL
  #           dplyr	                                SQL
  # inner_join(x, y, by = "z")	  SELECT * FROM x INNER JOIN y USING (z)
  # left_join(x, y, by = "z")	    SELECT * FROM x LEFT OUTER JOIN y USING (z)
  # right_join(x, y, by = "z")	  SELECT * FROM x RIGHT OUTER JOIN y USING (z)
  # full_join(x, y, by = "z")	    SELECT * FROM x FULL OUTER JOIN y USING (z)
  
  # Joining different variables between the tables, 
  # e.g. inner_join(x, y, by = c("a" = "b")) uses a slightly different syntax
# in SQL: SELECT * FROM x INNER JOIN y ON x.a = y.b.



# ------------------------------------------------------------------------------
# 13.5  FILTERING JOINS

  # Filtering joins match observations in the same way as mutating joins, 
  # but affect the observations, not the variables. There are two types:
      # semi_join(x, y) keeps all observations in x that have a match in y.
      # anti_join(x, y) drops all observations in x that have a match in y.

# NOTE: Semi-joins are useful for matching filtered summary tables back to the original rows.
# ex:
  # found top 10 spots
top_dest <- flights %>% 
  count(dest, sort = TRUE) %>% 
  head(10)
top_dest
  # now want to find each flight that went there
flights %>% 
  filter(dest %in% top_dest$dest)

  # OR INSTEAD -- semi_join --> only keeps rows in x with a match in y
flights %>% 
  semi_join(top_dest)

  # FILTER JOINS NEVER DUPLICATE ROWS LIKE MUTATING JOINS DO


# INVERSE of FILTER JOIN => ANTIJOIN - KEEPS ROWS WITH NO MATCH
  # antijoins useful to diagnose join mismatches

# ex: which flights  dont have a match in plane
flights %>% 
  anti_join(planes, by = "tailnum") %>% 
  count(tailnum, sort = TRUE)


#13.5.1 EXERCISES

  # 1- what does it mean for a flight to have a missing tailnum? 
    # What do the tail numbers that dont have a match record in planes have in common?
    # Hint: 1 variable explains ~90% of problems

flights %>% 
  filter(is.na(tailnum))

flights %>% 
  filter( 
    (is.na(tailnum) & !is.na(air_time)) 
    )
# a flight w/ a missing tailnum means the flight was likely cancelled

flights %>% 
  anti_join(planes, by = "tailnum") %>% 
  count(carrier, sort = TRUE) %>% 
  mutate(
    prop = n / sum(n)
  )
# AA and MQ are mainly the culprits ... msut use a diff nomeclature


  # 2 - filter 'flights' to only show flights with planes that have flown >= 100x

planes_gte_100 <- flights %>%
  filter(!is.na(tailnum)) %>% 
  group_by(tailnum) %>% 
  mutate(
    total_flights = n()
  ) %>% 
  arrange() %>% 
  filter(total_flights >= 100) %>% 
  semi_join(
    planes,
    by = "tailnum"
  )
planes_gte_100 %>% 
  select(total_flights) %>% 
  arrange(total_flights) # GOOD


  # 4 - find the 48 hours with worst delays. Cross ref w/ weather. Any pattern?

worst_hours <- flights %>%
  mutate(hour = sched_dep_time %/% 100) %>%
  group_by(origin, year, month, day, hour) %>%
  summarise(dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(dep_delay)) %>%
  slice(1:48)

weather_most_delayed <- semi_join(weather, worst_hours, 
                                  by = c("origin", "year",
                                         "month", "day", "hour"))
  
select(weather_most_delayed, temp, wind_speed, precip) %>%
  print(n = 48)

ggplot(weather_most_delayed, aes(x = precip, y = wind_speed, color = temp)) +
  geom_point()

  # 5 - nah im done eff this




# -----------------------------------------------------------------------------
# 13.6 JOIN PROBLEMS

# few things to do to make sure joins go smoothly 

  # 1) identify primary key
      # should do this based on your understanding of the data
  # 2) check that none of the variables in the primary key are missing
      # if a val is missing then cannot id 
  # 3) check your foreign keys match primary keys in another table
      # best way --> anti_join()
          # often keys dont mathc bc of data entry error

# Be aware that simply checking the number of rows before and after the join is
# not sufficient to ensure that your join has gone smoothly. If you have an 
# inner join with duplicate keys in both tables, you might get unlucky as the
# number of dropped rows might exactly equal the number of duplicated rows!



# -----------------------------------------------------------------------------
# 13.7 SET PROBLEMS

    # intersect(x, y): return only observations in both x and y.
    # union(x, y): return unique observations in x and y.
    # setdiff(x, y): return observations in x, but not in y.

df1 <- tribble(
  ~x, ~y,
  1,  1,
  2,  1
)
df2 <- tribble(
  ~x, ~y,
  1,  1,
  1,  2
)

intersect(df1, df2)
#> # A tibble: 1 x 2
#>       x     y
#>   <dbl> <dbl>
#> 1     1     1

# Note that we get 3 rows, not 4
union(df1, df2)
#> # A tibble: 3 x 2
#>       x     y
#>   <dbl> <dbl>
#> 1     1     1
#> 2     2     1
#> 3     1     2

setdiff(df1, df2)
#> # A tibble: 1 x 2
#>       x     y
#>   <dbl> <dbl>
#> 1     2     1

setdiff(df2, df1)
#> # A tibble: 1 x 2
#>       x     y
#>   <dbl> <dbl>
#> 1     1     2

