# https://r4ds.had.co.nz/data-import.html 

# CHP 11 DATA IMPORT

library(tidyverse)

# ------------------------------------------------------------------------------
# 11.2 GETTING STARTED
  # Most of readr's functions are concerned with turning flat files into data frames

# read_csv() <- reads comma delim, read_csv2() <- reads semicolon
# read_tsv() <- reads tab delim, read_delim() <- any
# read_fwf() <- reads fixed width files
  # specify widths with fwf_widths() or their position with fwf_positions()
# read_table() <- common var of fixed width cols are sep by whitespace

heights <- read_csv("data/heights.csv")
#run read_csv() it prints out a column specification that gives the name and type of each column.
# uses first line of data for col names

# 1) sometimes a few lines of metadata at the top
read_csv("The first line of metadata
  The second line of metadata
  x,y,z
  1,2,3", skip = 2)

read_csv("# A comment I want to skip
  x,y,z
  1,2,3", comment = "#") # skip any lines that begin with '#'

# 2) the data may not have column names 
read_csv("1,2,3\n4,5,6", col_names = FALSE) #col_names = FALSE !
    # "\n" is a convenient shortcut for adding a new line

read_csv("1,2,3\n4,5,6", col_names = c("x", "y", "z")) #specify col names

read_csv("a,b,c\n1,2,.", na = ".") # na = specifies value used IN FILE to represent NA

# ------------------------------------
# 10.2 EXERCISES
  # 1 - read a file where fields were separated with "|"?
read_delim("a|b|c\n1|2|3", delim = "|" )

  # 2 - what arguments do read_tsv and read_csv have in common
intersect(names(formals(read_csv)), names(formals(read_tsv)))

  # 3 - most important arguments to read_fwf()
  # col_positions which tells the function where data columns begin and end

  # 4 - hat argument to read_csv() do you need to specify to read the following text into a data frame?
    # "x,y\n1,'a,b'"
x <- "x,y\n1,'a,b'" 
  read_csv(x, quote = "'")

  # 5 - 


# ------------------------------------------------------------------------------
# 11.3 PAARSING A VECZTOR
  
  # must learn about the parse_*() functions
    # functions take a CHAR vector and return more specialized vector (logical, integer, date)

str(parse_logical(c("TRUE", "FALSE")))
str(parse_integer(c("1","2")))
str(parse_date(c("2010-01-01", "1979-10-14")))

parse_integer(c("1","2","."), na = ".") # spec what is treated as na

x <- parse_integer(c("123", "345", "abc", "123.45"))
  # if fails, will tell you of the parsing failures
  # fails missing in the output
problems(x)
  # returns a tibble, which you can then manipulate with dplyr


# TOP 8 PARSERS ---->
  # parse_logical() and parse_integer() parse logicals and integers respectively. 
  # There's basically nothing that can go wrong with these parsers so I won't 
  # describe them here further.
  # 
  # parse_double() is a strict numeric parser, and parse_number() is a flexible 
  # numeric parser. These are more complicated than you might expect because 
  # different parts of the world write numbers in different ways.
  # 
  # parse_character() seems so simple that it shouldn't be necessary. But one 
  # complication makes it quite important: character encodings.
  # 
  # parse_factor() create factors, the data structure that R uses to represent
  # categorical variables with fixed and known values.
  # 
  # parse_datetime(), parse_date(), and parse_time() allow you to parse various 
  # date & time specifications. These are the most complicated because there are 
  # so many different ways of writing dates.

#-------------
  # 11.3.1 NUMBERS
# 3 problems that can make numbers tricky
  # 1) diff countries write them differently (1.25 vs 1,25)
parse_double("1.23")
parse_double("1,23", locale = locale(decimal_mark = ",")) #locale argument for diff areas

  # 2) characters around the numbers (e.g. $100 or 100%)
parse_number("$100")  #ignores non numeric chars
parse_number("20%") 
parse_number("that will be $149.99")

  # 3) grouping caracters making them easier (1,000,000)
parse_number("123,456,789")  #USA
parse_number("123.456.789", locale = locale(grouping_mark = "."))  #EUROPE


#-------------
# 11.3.2  STRINGS

# parse_character() should be really simple - it could just return its input. Unfortunately life isn't so simple

charToRaw("Hadley")
# [1] 48 61 64 6c 65 79
  # Each hexadecimal number represents a byte of information: 48 is H...

# diff languages have diff encoding default is UTF-8
  # guess_ecnoding() is decent if unsure

parse_character(x1, locale = locale(encoding = "Latin1"))
#> [1] "El Niño was particularly bad this year"


# -----------
# 11.3.3 FACTORS

fruit <- c("apple", "banana")
parse_factor(c("apple", "banana", "bananana"), levels = fruit)
  # Warning: 1 parsing failure.
  # row col           expected   actual
  # 3  -- value in level set bananana


# -----------
# 11.3.4 DATES, TIMES

parse_datetime("2010-10-01T2010")   # expects ISO8601 date-time(y-m-d-h-s)
  # [1] "2010-10-01 20:10:00 UTC"

parse_date("2022-05-04")   #expects 4 digit year -/ month -/ day
# [1] "2022-05-04"

library(hms)
parse_time("01:10 am")   #expects the hour, :, minutes, optionally : and seconds, and an optional am/pm specifier:
  # 01:10:00


#supply your own date-time format, built up of the following pieces:

  # Year
# %Y (4 digits).
# %y (2 digits); 00-69 -> 2000-2069, 70-99 -> 1970-1999.
  # Month
# %m (2 digits).
# %b (abbreviated name, like "Jan").
# %B (full name, "January").
  # Day
# %d (2 digits).
# %e (optional leading space).
  # Time
# %H 0-23 hour.
# %I 0-12, must be used with %p.
# %p AM/PM indicator.
  # %M minutes.
# %S integer seconds.
# %OS real seconds.
# %Z Time zone (as name, e.g. America/Chicago). Beware of abbreviations: if you're American, note that "EST" is a Canadian time zone that does not have daylight savings time. It is not Eastern Standard Time! We'll come back to this time zones.
# %z (as offset from UTC, e.g. +0800).
# Non-digits
# %. skips one non-digit character.
# %* skips any number of non-digits.

parse_date("01/02/15", "%d/%m/%y")
#> [1] "2015-02-01"


# 11.3.5 EXERCISES
#
  # 1 - what are the most important args to locale()
# locale = locale(X = "y")
    # date and time formats: date_names, date_format, and time_format
    # time zone: tz
    # numbers: decimal_mark, grouping_mark
    # encoding: encoding

  # 2 - 
parse_number("123.456.789", locale = locale(grouping_mark = ".", decimal_mark = "."))
  # Error: `decimal_mark` and `grouping_mark` must be different
locale(decimal_mark = ",")
  # then the grouping mark is set to the period "."
locale(grouping_mark = ".")
  # decimal mark is set to a comma


  # 3 - data_format, time_format options to locale()
?locale()
  # defualt date and time formats

locale_custom <- locale(date_format = "Day %d Mon %M Year %y",
                        time_format = "Sec %S Min %M Hour %H")
date_custom <- c("Day 01 Mon 02 Year 03", "Day 03 Mon 01 Year 01")
  parse_date(date_custom, locale = locale_custom)  # custom date function

time_custom <- c("Sec 01 Min 02 Hour 03", "Sec 03 Min 02 Hour 01")
  parse_time(time_custom, locale = locale_custom)

  # 5 - read_csv VS read_csv2
      # read_csv2() = semicolon delim  --> EUROPE
  
  # 6 - common encodings across globe ...
    # UTF-8 is standard now, and ASCII has been around forever.
    # https://en.wikipedia.org/wiki/Character_encoding 
  
  # 7 - correct format string to parse each
  d1 <- "January 1, 2010"
parse_date(d1, "%B %d, %Y")
#[1] "2010-01-01"

  d2 <- "2015-Mar-07"
parse_date(d2, "%Y-%b-%d")
# [1] "2015-03-07"
  d3 <- "06-Jun-2017"
  d4 <- c("August 19 (2015)", "July 1 (2015)")
  d5 <- "12/30/14" # Dec 30, 2014
  t1 <- "1705"
  t2 <- "11:15:10.12 PM" 
parse_time(t2, "%H:%M:%OS %p")
# 23:15:10.12


#-----------------------------
# 11.4 PARSING A FILE

  # 11.4.1 Strategy
# readr reads first 100 rows of a col and uses heuristics to figure the type
# --> guess_parser()

guess_parser("2010-10-01")
#> [1] "date"
guess_parser("15:01")
#> [1] "time"
guess_parser(c("TRUE", "FALSE"))
#> [1] "logical"
guess_parser(c("1", "5", "9"))
#> [1] "double"
guess_parser(c("12,352,561"))
#> [1] "number"
  
  # 11.4.2 Problems

#1) first 100 rows may be a special case

#2) col may contain a lot of missing values

challenge <- read_csv(readr_example("challenge.csv"))

problems(challenge)
tail(challenge) 
  
challenge <- read_csv(
  readr_example("challenge.csv"), 
  col_types = cols(
    x = col_double(),
    y = col_date()
  )
)
tail(challenge)  
class(challenge)


  # 11.4.3 Other strategies
challenge2 <- read_csv(readr_example("challenge.csv"), guess_max = 1001)
challenge2 <- read_csv(readr_example("challenge.csv"), 
                       col_types = cols(.default = col_character())
) # read all cols in as char vectors to diagnose
glimpse(challenge2)


df <- tribble(
  ~x,  ~y,
  "1", "1.21",
  "2", "2.32",
  "3", "4.56"
)
df  
type_convert(df) # type_convert(), which applies the parsing heuristics to the character columns in a data frame.



# ------------------------------------------------------------------------------
# 11.5 WRITING TO A FILE

# useful functions to write back to disk: write_csv()  &  write_tsv
  # 1) always encode strings in UTF-8
  # 2) save dates + date-times in ISO8601

# write_excel_csv() <- export a csv to Excel

write_csv(challenge,"challenge.csv") 
  #note: type of info is lost when saved (var types)
# ....
  # you need to recreate the column specification every time you load in. There are two alternatives:
    # 1) write_rds() and rad_rds() are uniform wrappers around readRDS() and saveRDS()
        # store data in R's custom binary format called RDS
write_rds(challenge, "challenge.rds")
read_rds("challenge.rds")
  # conserves var types + SUPPORTS LIST COLUMNS

  # 2) the feather pack implements fast binary file format shared across program langs
install.packages("feather")
library(feather)
write_feather(challenge,"challenge.feather")
read_feather("challenge.feather")


# 11.6 OTHER DATA TYPES

# useful tidyverse packages
  # haven -- SPSS, Stata, SAS files
  # readxl - excel files
  # DBI + database specific backend(RMySQL, RSQLite, RPostgreSQL)
      # allows you to run SQL queries against a database and return a df
  