# https://r4ds.had.co.nz/tidy-data.html

# CHAPTER 12 TIDY DATA

#install.packages("tidyverse")
library(tidyverse)


# 12.2 TIDY DATA

## TIDY RULES
  # 1) Each variable must have its own column.
  # 2) Each observation must have its own row.
  # 3) Each value must have its own cell.

  # ==>
      # Put each dataset in a tibble.
      # Put each variable in a column

# packages in tidyverse are designed to work with tidy data

# compute rates per 10,000
table1 %>% 
  mutate(rate = cases / population * 10000)
# compute cases per year
table1 %>% 
  count(year, wt = cases)
# visualize changes over time
library(ggplot2)
ggplot(table1, aes(year, cases)) +
  geom_line(aes(group = country), colour = "grey50") +
  geom_point(aes(colour = country))


# 12.2 EXERCISES

  # 1 - 
  
  # 2 - 
glimpse(table2)
table2x <- table2 %>% 
  group_by(country, year) %>% 
  summarise(
    cases_per_yr = sum(count[type == "cases"], na.rm = TRUE),
    pop_per_yr = sum(count[type == "population"], na.rm = TRUE),
    rate_per_10k = cases_per_yr / pop_per_yr * 10000
  )

# table 4 xxxxxxx

  # 3 - 
  ggplot(data = table2x, aes(x = year, y = cases_per_yr))+
    geom_point(aes(colour = country))+
    geom_line(aes(group = country))

# -----------------------------------------------------------------------------
# 12.3 PIVOTING

# Data is typically not tidy right away
  
# Step 1 is always to figure out what are variables and what are observations
  
# Step 2 is to resolve one of two commons problems
  # A) one variable might be spread across multiple columns
  # B) one observation might be scattered across multiple rows
    
    # TWO MOST IMPORTANT FUNCTIONS IN TIDYR
    # pivot_longer()
    # pivot_wider()

# -----------------------------
# 12.3.1 LONGER
    # common problem is dataset where columns are not name of variables,
    # but they are vaues of a variable
        # e.g. 
          table4a
          #> # A tibble: 3 x 3
          #>   country     `1999` `2000`
          #> * <chr>        <int>  <int>
          #> 1 Afghanistan    745   2666
          #> 2 Brazil       37737  80488
          #> 3 China       212258 213766

  # need to pivot into a new pair of variables... need 3 parameters
    # 1) set of columns where names are values, not variables (1999, 2000)
    # 2) name of the variable to move column names to (year)
    # 3) name of variable to move columns values to (cases)

tidy4a <- table4a %>% 
  pivot_longer(c(`1999`, `2000`), names_to = "year", values_to = "cases")

tidy4b <- table4b %>% 
  pivot_longer(c(`1999`, `2000`), names_to = "year", values_to = "population")

left_join(tidy4a, tidy4b)  

#------------------------------------
  # 12.3.2 WIDER
# pivot_wider() opposite of pivot_longer()
  # USE WHEN AN OBSERVATION IS SCATTERED ACROSS MULTIPLE ROWS

table2 # ---> EX: OBSERVATION IS COUNTRY & YEAR
#> # A tibble: 12 x 4
#>   country      year type           count
#>   <chr>       <int> <chr>          <int>
#> 1 Afghanistan  1999 cases            745   # HERE
#> 2 Afghanistan  1999 population  19987071   # HERE
#> 3 Afghanistan  2000 cases           2666
#> 4 Afghanistan  2000 population  20595360
#> 5 Brazil       1999 cases          37737

  # need 2 things to pivot
    # 1) the COLUMN to take the VARIABLE NAMES from (type)
    # 2) the COLUMN names to take VALUES from (count)

table2 %>% 
  pivot_wider(names_from = type, values_from = count)


# 12.3,3 EXERCISES
  # 1 - why are pivot_longer() and pivot_wider not perf symmetrical?
stocks <- tibble(
  year   = c(2015, 2015, 2016, 2016),
  half  = c(   1,    2,     1,    2),
  return = c(1.88, 0.59, 0.92, 0.17)
) 

stocks %>% 
  pivot_wider(names_from = year, values_from = return) %>% 
  pivot_longer(`2015`:`2016`, names_to = "year", values_to = "return")  

  # not perfectly symmetrical because column type information is lost when a
  # data frame is converted from wide to long. 

stocks %>% 
  pivot_wider(names_from = year, values_from = return) %>% 
  pivot_longer(`2015`:`2016`, names_to = "year", values_to = "return",
               names_ptypes = list(year = double())) 
              # error
stocks %>% 
  pivot_wider(names_from = year, values_from = return) %>% 
  pivot_longer(`2015`:`2016`, names_to = "year", values_to = "return",
    names_transform = list(year = as.numeric)) # coerces
  
  # 2 - Why does this code fail?
table4a %>% 
  pivot_longer(c(1999, 2000), names_to = "year", values_to = "cases")
# 1999, 2000 are non-synactic variables => should be `1999`, `2000`

  # 3 - what happens if widen? why? how to add new col to uniquely ID
people <- tribble(
  ~name,             ~key,  ~values,
  #-----------------|--------|------
  "Phillip Woods",   "age",       45,
  "Phillip Woods",   "height",   186,
  "Phillip Woods",   "age",       50, # two ages for Phillip
  "Jessica Cordero", "age",       37,
  "Jessica Cordero", "height",   156
)
glimpse(people)

people %>% 
  pivot_wider(names_from = "key", values_from = "values")
# name            age       height   
# <chr>           <list>    <list>   
#   1 Phillip Woods   <dbl [2]> <dbl [1]>
#   2 Jessica Cordero <dbl [1]> <dbl [1]>
#   Warning message:
#   Values from `values` are not uniquely identified; output will contain list-cols.

# ADD A DISTINCT OBSERVATION COUNT
  (people2 <- people %>%
    group_by(name, key) %>% 
    mutate(
      obs = row_number()
    ))
people2 %>% 
  pivot_wider(names_from = name, values_from = values)


  # 4 - tidy. what are the vars? wider or longer?
preg <- tribble(
  ~pregnant, ~male, ~female,
  "yes",     NA,    10,
  "no",      20,    12
)
preg %>% 
  pivot_longer(c(male, female), names_to = "gender", values_to = "count",
               values_drop_na = TRUE)



#------------------------------------------------------------------------------
# 12.4 SEPARATING AND UNITING

# One column that contains 2 variables

  # > table3
  # # A tibble: 6 x 3
  # country      year rate             
  # * <chr>       <int> <chr>            
  #   1 Afghanistan  1999 745/19987071     
  # 2 Afghanistan  2000 2666/20595360 

table3 %>% 
  separate(rate, into = c("cases","population"))
    # country      year cases  population
    # <chr>       <int> <chr>  <chr>              # CHR colums XXX
    #   1 Afghanistan  1999 745    19987071 

table3 %>% 
  separate(rate, into = c("cases","population"), sep = "/") #auto detects but you can specify

table3 %>% 
  separate(rate, into = c("cases","population"), convert = TRUE) #converts to guess var type

table3 %>% 
  separate(year, into = c("century", "year"), sep = 2)  # splits a number in half

# ---------
  #  UNITE
table5 %>% 
  unite(new, century, year)
    # country     new   rate             
    # <chr>       <chr> <chr>            
    #   1 Afghanistan 19_99 745/19987071

table5 %>% 
  unite(new, century, year, sep = "")
  

# 12.4.3
  # 1 - extra and fill arguments in separate() ???
?separate

tibble(x = c("a,b,c", "d,e,f,g", "h,i,j")) %>% 
  separate(x, c("one", "two", "three"), extra = "merge") #warn, drop, merge

tibble(x = c("a,b,c", "d,e", "f,g,i")) %>% 
  separate(x, c("one", "two", "three"), fill = "right") #warn, (right, left - where the NA goes


  # 2 - remove argument in unite() and separate() ? WHy set it to FALSE?
tibble(x = c("a,b,c", "d,e,f,g", "h,i,j")) %>% 
  separate(x, c("one", "two", "three"), remove = FALSE) #keeps the old variable

  # 3 -



# ------------------------------------------------------------------------------
# 12.5 MISSING VALUES

  # Explicit - marked w/ NA
  # Implicit - simply not present in data
# An explicit missing value is the presence of an absence; 
#an implicit missing value is the absence of a presence.

stocks <- tibble(
  year   = c(2015, 2015, 2015, 2015, 2016, 2016, 2016), #IMPLICIT - missing 2016 q4
  qtr    = c(   1,    2,    3,    4,    2,    3,    4),
  return = c(1.88, 0.59, 0.35,   NA, 0.92, 0.17, 2.66) #EXPLICT NA
)

# can make impicit values explicit -> put years in the columns
stocks %>% 
  pivot_wider(names_from = year, values_from = return)

# can turn explicit values implicit if not important
stocks %>% 
  pivot_wider(names_from = year, values_from = return) %>% 
  pivot_longer(
    cols = c(`2015`,`2016`),
    names_to = "year",
    values_to = "return",
    values_drop_na = TRUE
  )

# making missing values explicit in tidy data is complete()

stocks %>% 
  complete(year, qtr)
    # takes a set of columns, and finds all unique combinations. It then ensures 
    # the original dataset contains all those values, filling in explicit NAs 
    # where necessary.

# Sometimes when a data source has primarily been used for data entry, missing 
# values indicate that the previous value should be carried forward:
treatment <- tribble(
  ~ person,           ~ treatment, ~response,
  "Derrick Whitmore", 1,           7,
  NA,                 2,           10,
  NA,                 3,           9,
  "Katherine Burke",  1,           4
)

treatment %>% 
  fill(person)
#  fill() takes a set of columns where you want missing values to be replaced by
# the most recent non-missing value (sometimes called last observation 
# carried forward).


#12.5 EXERCISES
   # 1 -
# The values_fill argument in pivot_wider() and the fill argument to complete()
# both set vales to replace NA. Both arguments accept named lists to set values
# for each column. Additionally, the values_fill argument of pivot_wider() accepts
# a single value. In complete(), the fill argument also sets a value to replace
# NAs but it is named list, allowing for different values for different variables.
# Also, both cases replace both implicit and explicit missing values.

stocks <- tibble(
  year   = c(2015, 2015, 2015, 2015, 2016, 2016, 2016),
  qtr    = c(   1,    2,    3,    4,    2,    3,    4),
  return = c(1.88, 0.59, 0.35,   NA, 0.92, 0.17, 2.66)
)
stocks %>% 
  pivot_wider(names_from = year, values_from = return,
              values_fill = 0)

stocks %>% 
  complete(year, qtr, fill=list(return=0))

  # 2 - direction argument to fill()
treatment %>% 
  fill(person, .direction = "up")
# which way the NAs are fille up or down



#------------------------------------------------------------------------------
# 12.6 CASE STUDY

  # The tidyr::who dataset contains tuberculosis (TB) cases broken down by
  # year, country, age, gender, and diagnosis method

who
glimpse(who)
head(who)
tail(who)

# 1) START WITH LOOKING AT COLUMNS THAT ARE NOT VARIABLES

  # counttry, iso2, iso3 are 3 VARIABLES all redundantly identify country
  # year is a VARIABLE
  # other cols are likely values not variables bc of names

# 2) 
  # we dont know what the col names are yet -> name 'key'
  # we know the cells represent count of cases -> 'cases'
  # lot of missing values, for now we will drop

who1 <- who %>% 
  pivot_longer(
    cols = new_sp_m014:newrel_f65,
    names_to = "key",
    values_to = "cases",
    values_drop_na = TRUE
  )
who1

# count values in the key column to understand the structure
who1 %>% 
  count(key)


    # The first three letters of each column denote whether the column contains new or old cases of TB. In this dataset, each column contains new cases.
    # 
    # The next two letters describe the type of TB:
    #   
    #   rel stands for cases of relapse
    # ep stands for cases of extrapulmonary TB
    # sn stands for cases of pulmonary TB that could not be diagnosed by a pulmonary smear (smear negative)
    # sp stands for cases of pulmonary TB that could be diagnosed by a pulmonary smear (smear positive)
    # The sixth letter gives the sex of TB patients. The dataset groups cases by males (m) and females (f).
    # 
    # The remaining numbers gives the age group. The dataset groups cases into seven age groups:
    #   
    #   014 = 0 - 14 years old
    # 1524 = 15 - 24 years old
    # 2534 = 25 - 34 years old
    # 3544 = 35 - 44 years old
    # 4554 = 45 - 54 years old
    # 5564 = 55 - 64 years old
    # 65 = 65 or older


# names are slightly inconsistent because instead of new_rel we have newrel
who2 <- who1 %>% 
  mutate(
    key = stringr::str_replace(key, "newrel", "new_rel1")
  )
who2

# first split at each underscore
who3 <- who2 %>% 
  separate(key, c("new","type","sexage"), sep = "_")
who3

# drop redundant cols
who3 %>% 
  count(new) # may as well drop because constant
who4 <- who3 %>%  
  select(-new, -iso2, -iso3) # remove redundant cols

# separate sexage into sex and age
who5 <- who4 %>% 
  separate(sexage, c("sexage","age"), sep = 1)
who5

# WOOO TIDY!!! 

# relly we should use a pipe,,,,
tidy_who <- who %>%
  pivot_longer(
    cols = new_sp_m014:newrel_f65, 
    names_to = "key", 
    values_to = "cases", 
    values_drop_na = TRUE
  ) %>% 
  mutate(
    key = stringr::str_replace(key, "newrel", "new_rel")
  ) %>%
  separate(key, c("new", "var", "sexage")) %>% 
  select(-new, -iso2, -iso3) %>% 
  separate(sexage, c("sex", "age"), sep = 1)

#12.6 EXERCISES
  # 1 - was it reasonable to drop NA values? how are missing values represented
    # in this dataset? iumplicit? diff between NA and 0?
tidy_who_NA <- who %>%
  pivot_longer(
    cols = new_sp_m014:newrel_f65, 
    names_to = "key", 
    values_to = "cases", 
    values_drop_na = FALSE
  ) %>% 
  mutate(
    key = stringr::str_replace(key, "newrel", "new_rel")
  ) %>%
  separate(key, c("new", "var", "sexage")) %>% 
  select(-new, -iso2, -iso3) %>% 
  separate(sexage, c("sex", "age"), sep = 1)

# NA could mean there were no cases detected in this country or he data is not available
tidy_who_NA %>% 
  filter(cases == 0 | is.na(cases) == TRUE) %>% 
  count(cases)
# 0s are likely explicit meaning 0 posittives cases 
# Explicit NA represents missing data for (country, year)
# Implicit mising values is missing data for a country that didnt exist

  # 2 - 
  # 3 - iso2, iso3 redundant w/ country... confirm this

who3 %>% 
  select(country:iso3) %>% 
  distinct() %>% 
  group_by(country) %>% 
  filter(n() > 1)

  # 4 - 
tidy_who %>% 
  group_by(country, year, sex) %>% 
  filter(year > 2000) %>% 
  summarise(
    cases = sum(cases)
  ) %>% 
  unite(country_sex, country, sex, remove = FALSE) %>% 
  ggplot(aes(year,cases, group = country_sex, colour = sex)) +
    geom_line()
