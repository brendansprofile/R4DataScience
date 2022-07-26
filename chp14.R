# https://r4ds.had.co.nz/strings.html

# CHP 14 - STRINGS

  # focus of this chapter will be in regular expressions (REGEXPS)

library(tidyverse)
# stringr


# ------------------------------------------------------------------------------
# 14.2 STRING BASICS

string1 <- "this is a string"
string2 <- 'If I want to include a "quote" inside a string, I use single quotes'

# printed representation of a string is not the same as string itself, 
# because the printed representation shows the escapes. 

# see the raw contents --> writeLines()
x <- c("\"", "\\")
x
#> [1] "\"" "\\"
writeLines(x)
#> "
#> \

# special character
  # \n  =  new line
  # \t  =  tab
  # ": ?'"'   COMPLETE LIST

# multiple strings often stored in a character vector
c("one", "two", "three")
#> [1] "one"   "two"   "three"


# STRING FUNCTIONS --> all start with "str_"
str_length(c("a", "R for data science", NA))
  # [1]  1 18 NA

# COMBINING STRINGS
str_c("x", "y")
  # [1] "xy"

str_c("x","y", sep = ",")
  # [1] "x,y"

# if you want to print them as "NA: --> str_replace_na()
x <- c("abc", NA)
str_c("|-", x, "-|")
#> [1] "|-abc-|" NA
str_c("|-", str_replace_na(x), "-|")
#> [1] "|-abc-|" "|-NA-|"

# str_c() is vectorised, and it automatically recycles shorter vectors to the same length as the longest:
str_c("prefix-", c("a","b","c"), "-suffix")
#[1] "prefix-a-suffix" "prefix-b-suffix" "prefix-c-suffix"


#Objects of length 0 are silently dropped. This is particularly useful in conjunction with if:\
name <- "Hadley"
time_of_day <- "morning"
birthday <- FALSE

str_c(
  "Good ", time_of_day, " ", name,
  if (birthday) " and HAPPY BIRTHDAY",
  "."
)
#> [1] "Good morning Hadley."


# to collapse a vecotr string into a single string
str_c(c("x", "y", "z"), collapse = ", ")
  # [1] "x, y, z"

# STRING SUBSETTING
  # extract parts of a string using string_sub()
x <- c("Apple", "Banana", "Pear")
str_sub(x, 1, 3) # (string, start, end) arguments
  # [1] "App" "Ban" "Pea"

str_sub(x, -3, -1) # neg numbers count back from the end
  # [1] "ple" "ana" "ear"

str_sub("a", 1, 5) # WONT Fail if string too short, as much as can
  # [1] "a"

# assignment form of str_sub() to modify strings:
str_sub(x,1,1) <- str_to_upper(str_sub(x, 1, 1))
x
# [1] "Apple"  "Banana" "Pear"  


# LOCALES
  # defining locales for diff languages bc diff rules on upper/lowercase

# Turkish has two i's: with and without a dot, and it
# has a different rule for capitalising them:
str_to_upper(c("i", "i"))
#> [1] "I" "I"
str_to_upper(c("i", "i"), locale = "tr")
#> [1] "I" "I"

x <- c("apple", "eggplant", "banana")
str_sort(x, locale = "en")  # English
#> [1] "apple"    "banana"   "eggplant"
str_sort(x, locale = "haw") # Hawaiian
#> [1] "apple"    "eggplant" "banana"


# 14.2 EXERCISES

  # 1 - in non 'stringr' code you see paste() and paste0()
  # whats the diff? what stringr function are they equiv to?

  # The function paste() separates strings by spaces by default, 
  # while paste0() does not separate strings with spaces by default

paste("foo", "bar")
#> [1] "foo bar"

paste0("foo", "bar")
#> [1] "foobar"
str_c("foo", "bar")
#> [1] "foobar"

  # 2 - difference between the sep and collapse arguments to str_c().
# The sep argument is the string inserted between arguments to str_c(), 
# while collapse is the string used to separate any elements of the character 
# vector into a character vector of length one

  # 3 - Use str_length() and str_sub() to extract the middle character from a string. 
  # What will you do if the string has an even number of characters?
x <- c("a", "abc", "abcd", "abcde", "abcdef")
L <- str_length(x)
m <- ceiling(L / 2)
str_sub(x, m, m)

  # 4 - str_wrap() ? when to use?
?str_wrap
  # paragraph wraps text using an algo: width = chars/line, indent = indent of first line
cat(str_wrap("hi my name is kevin and i like to ski", width = 10, indent = 2),"\n")
  # This is useful for wrapping long strings of text to be typeset.

  # 5 - 
# str_trim() trims the whitespace from a string
str_trim(" abc ")
#> [1] "abc"
str_trim(" abc ", side = "left")
#> [1] "abc "

#str_pad() which adds characters to each side.
str_pad("abc", 5, side = "both")
#> [1] " abc "
str_pad("abc", 4, side = "right")
#> [1] "abc "

  # 6 - Write a function that turns (e.g.) a vector c("a", "b", "c") 
# into the string a, b, and c. Think carefully about what it should do if given 
# a vector of length 0, 1, or 2.
str_commasep <- function(x, delim = ",") {
  n <- length(x)
  if (n == 0) {
    ""
    
  } else if (n == 1) {
    x
    
  } else if (n == 2) {
    # no comma before and when n == 2
    str_c(x[[1]], "and", x[[2]], sep = " ")
    
  } else {
    # commas after all n - 1 elements
    not_last <- str_c(x[seq_len(n - 1)], delim)
    # prepend "and" to the last element
    last <- str_c("and", x[[n]], sep = " ")
    # combine parts with spaces
    str_c(c(not_last, last), collapse = " ")
  }
}



#-------------------------------------------------------------------------------
# 14.3 Matching patterns with regular expressions

  # To learn regular expressions, we'll use str_view() and str_view_all()
#install.packages("htmlwidgets")
library(htmlwidgets)


# 14.3.1 Basic matches
  # simplest pattern
x <- c("apple", "banana", "pear")
str_view(x, "an")

  # '.' which matches any character (except a newline):
str_view(x, ".a.")

  # what if we need the . character
      #  Like strings, regexps use the backslash, \, to escape special behaviour
# To create the regular expression, we need \\
dot <- "\\."

# But the expression itself only contains one:
writeLines(dot)
#> \.

# And this tells R to look for an explicit .
str_view(c("abc", "a.c", "bef"), "a\\.c")

  # what if we need the \ character
x <- "a\\b"
writeLines(x)
# a\b
str_view(x, "\\\\")

# EXERCISE
  # 1 - Explain why each of these strings don't match a \: "\", "\\", "\\\".

# "\": This will escape the next character in the R string.
# "\\": This will resolve to \ in the regular expression, which will escape the
# next character in the regular expression.

# "\\\": The first two backslashes will resolve to a literal backslash in the 
# regular expression, the third will escape the next character. So in the regular 
# expression, this will escape some escaped character

  # 2 - How would you match the sequence "'\?
str_view("\"'\\", "\"'\\\\", match = TRUE)

  # 3 - What patterns will the regular expression \..\..\.. match? 
# How would you represent it as a string?
str_view(c(".a.b.c", ".a.b", "....."), c("\\..\\..\\.."), match = TRUE)
  # any patterns dot folow by any char rep 3x  


# ANCHORS
  # often useful to anchor the regular expression so that it matches from the start or end of the string.
# ^ to match the start of the string.
# $ to match the end of the string.

x <- c("apple", "banana", "pear")
str_view(x,"^a")
str_view(x,"a$")
  # if you begin with power (^), you end up with money ($)

# force a regexp to match a complete string
x <- c("apple pie", "apple", "apple cake")
str_view(x,"apple")
# force a regexp to only match a complete string
str_view(x,"^apple$")

# EXERCISES
  # 1 - How would you match the literal string "$^$"?
str_view(c("$^$", "ab$^$sfas"), "^\\$\\^\\$$", match = TRUE)

  # 2 - nah


# CHARACTER CLASSES AND ALTERNATIVES
  # . matches any char besides a new line
  # \d: matches any digit.
  # \s: matches any whitespace (e.g. space, tab, newline).
  # [abc]: matches a, b, or c.
  # [^abc]: matches anything except a, b, or c.

# Look for a literal character that normally has special meaning in a regex
str_view(c("abc", "a.c", "a*c", "a c"), "a[.]c")

str_view(c("abc", "a.c", "a*c", "a c"), ".[*]c")

str_view(c("abc", "a.c", "a*c", "a c"), "a[ ]")

str_view(c("grey", "gray"), "gr(e|a)y")


# 1 - 
# a) words that start w vowel
str_subset(stringr::words, "^[aeiou]")
# b) words that contain only consonants
str_subset(stringr::words, "[aeiou]", negate=TRUE)
str_view(stringr::words, "[aeiou]", match=FALSE)

# 2 - Empirically verify the rule "i before e except after c".
length(str_subset(stringr::words, "(cei|[^c]ie)"))
length(str_subset(stringr::words, "(cie|[^c]ei)"))

# 5 - Create a regular expression that will match telephone numbers as commonly 
# written in your country.

str_subset(c("1-500-123-4567","12-55-124-3000"), 
           "^[1][-]\\d\\d\\d[-]\\d\\d\\d[-]\\d\\d\\d\\d$")


# REPETITION
  # how many times a pattern matches:

      # ?: 0 or 1
      # +: 1 or more
      # *: 0 or more

x <- "1888 is the longest year in Roman numerals: MDCCCLXXXVIII"
str_view(x, "CC?")

str_view(x, "CC+")

str_view(x, 'C[LX]+')

# You can also specify the number of matches precisely:
#   
#   {n}: exactly n
# {n,}: n or more
# {,m}: at most m
# {n,m}: between n and m



# COMPLETE LATER .......

