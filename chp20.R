# https://r4ds.had.co.nz/vectors.html 

# CHP 20 VECTORS
library(tidyverse)
# -------------------------------------------------------------------------

# 20.2 VECTOR BASICS

# Two types of vectors
  # 1. ATOMIC VECTORS - logical, numeric (integer, double), character, complex, raw
        # homogeneous
  # 2. LISTS - (aka Recursive Vectors b/c they can contain other lists)
        # can be heterogeneous
  # (NULL) - absence of a vector

    # *** FIGURE HEIRARCHY OF R VECTORS


# Every vector has 2 key properties
  # 1. TYPE
      # -> determine with  typeof()
typeof(letters)
# [1] "character"
typeof(1:10)
# [1] "integer"

  # 2. LENGTH
      # -> determine with  length()

# Vectors can also contain arbitrary additional metadata in the form of attributes. 
# These attributes are used to create augmented vectors 

  # Augmented Vector types:
    # 1. Factors are built on top of integer vectors.
    # 2. Dates and date-times are built on top of numeric vectors.
    # 3. Data frames and tibbles are built on top of lists.




# -------------------------------------------------------------------------

# 20.3 IMPORTANT TYPES OF ATOMIC VECTORS

# --------
# LOGICAL
  # simplest type ... can only take on: FALSE, TRUE, NA
# typically constructed with comparison operators

# can make manually
1:10 %% 3 == 0
#  [1] FALSE FALSE  TRUE FALSE FALSE  TRUE FALSE FALSE  TRUE FALSE

c(TRUE, TRUE, FALSE, NA)


# --------
# NUMERIC

# Note: In R, numbers are DOUBLES by default
typeof(1)
# [1] "double"

# place a 'L' after to coerce to INTEGER
typeof(1L)
# [1] "integer"


# IMPORTANT DIFFERENCE DOUBLE vs INTEGER
  # 1. Doubles = approximation
x <- sqrt(2) ^ 2
x
#> [1] 2
x - 2
#> [1] 4.440892e-16

  # TIP -- USE near() !!!

  # 2. Integers have one special value: NA, 
      # while doubles have four: NA, NaN, Inf and -Inf. 
c(-1, 0, 1) / 0
#> [1] -Inf  NaN  Inf

# TIP -- use helper functions
  # is.finite(), is.infinite(), is.na(),  is.nan()



# ------------
# CHARACTER
  # most complex type b/c each element of a char vect is a string (which contains arbitrarty amt of data)

  # NOTE: R uses a global string pool. This means that each unique string is only 
    # stored in memory once, and every use of the string points to that representation. 

x <- "This is a reasonably long string."
pryr::object_size(x)
#> Registered S3 method overwritten by 'pryr':
#>   method      from
#>   print.bytes Rcpp
#> 152 B

y <- rep(x, 1000)
pryr::object_size(y)
#> 8.14 kB

# ... A pointer is 8 bytes, so 1000 pointers to a 152 B string is 8 * 1000 + 152 = 8.14 kB.


# -----------------
# MISSING VALUES
  # each type of atomic vector has own missing value
NA            # logical
#> [1] NA
NA_integer_   # integer
#> [1] NA
NA_real_      # double
#> [1] NA
NA_character_ # character
#> [1] NA


# EXERCISES
  # 1 - Describe the difference between is.finite(x) and !is.infinite(x).
x <- c(Inf,-Inf, NaN, NA)
is.finite(x)
# [1] FALSE FALSE FALSE FALSE

!is.infinite(x)
# [1] FALSE FALSE  TRUE  TRUE
  is.infinite(x)
  # [1]  TRUE  TRUE FALSE FALSE

  # 2 - Read the source code for dplyr::near() 
    # (Hint: to see the source code, drop the ()). How does it work?
dplyr::near
function (x, y, tol = .Machine$double.eps^0.5) 
{
  abs(x - y) < tol
}
#<bytecode: 0x0000021d88e041b0>
  #<environment: namespace:dplyr>

# --> tolerance is set to the square root of .Machine$double.eps, which is the smallest floating point number that the computer can represent.
near(1,1.00000001)
#[1] TRUE
near(1,1.0000001)
#[1] FA


  # 3 - A logical vector can take 3 possible values. How many possible values 
# can an integer vector take? How many possible values can a double take? 
# Use google to do some research.


# INTEGERS -----------------------------
  # R uses a 32-bit representation. This means can represent up to  2^32 different values with integers.
  # ->  integer range: 2^32 - 1[bc of the +/-]
.Machine$integer.max
#> [1] 2147483647
 
.Machine$integer.max + 1L
#> Warning in .Machine$integer.max + 1L: NAs produced by integer overflow
#> [1] NA

# DOUBLES ------------------------------
  # 64 bit --> 2^64 values exactly
  # range : +/-2*10^308
.Machine$double.xmax
#> [1] 1.8e+308


  # 4 - 

  # 5 - 
# parse_logical() parses logical values
parse_logical(c("TRUE", "FALSE", "1", "0", "true", "t", "NA"))
  # [1]  TRUE FALSE  TRUE FALSE  TRUE  TRUE    NA

parse_integer(c("1345","3534","NA"))
  # [1] 1345 3534   NA

# parse_number() parses numeric values
parse_number(c("1.0", "3.5", "$1,000.00", "NA", "ABCD12234.90", "1234ABC", "A123B", "A1B2C"))
  #> [1]     1.0     3.5  1000.0      NA 12234.9  1234.0   123.0     1.0
  



# -------------------------------------------------------------------------

# 20.4 USING ATOMIC VECTORS

# Important tools for working with atomic vectors  

  # How to convert from one type to another, and when that happens automatically.

  # How to tell if an object is a specific type of vector.
  
  # What happens when you work with vectors of different lengths.
  
  # How to name the elements of a vector.
  
  # How to pull out elements of interest.


# COERCION -------------------------------
  # 1. EXPLICIT COERCION
    # call a function --> as.logical(), as.integer(), as.double(), or as.character(). 
      # TIP: always check to make fix upstream (tweak readr col_types)

  # 2. Implicit coercion happens when you use a vector in a specific context that expects a certain type of vector.
    # ex: you use a double vector where an integer vector is expected.

# IMPLICIT COERCION ----------

x <- sample(20, 100, replace = TRUE)
# [1] 17 15 20 12 12 20 10  7  2 17  7  1 12  3 19  4  7 19 20  7 19 13 18  1  6 12  3 14 13  8  4  5 20  3  5  2 15 12 15 17
# [41]  2  2  9 15 10 20 11 20  5 12 10 15  6 20  2 12 16 17 19 12  4 13  3  3  9 14 13 15 10  1 19 19 10 14 19 14  5  6  5  2
# [81] 20 15  1 11  9 17 16 20 20 14  1  9 13 12  6 20  3  5  3  2

# coercion using a logical vector inn numeric context ---> 1 = TRUE, 0 = FALSE
y <- x > 10
y
sum(y)
# [1] 54
mean(y) # what prop are > 10
# [1] 0.54


# integer to logical
if (length(x) > 0) {
  # do something
}

# creating a vector with multiple types ... most complex will win
typeof(c(TRUE, 1L))
# integer
typeof(c(1L, 1.5))
# double
typeof(c(1.5, "a"))
#> [1] "character"


# ---------------
# TEST FUNCTIONS

library(purrr)

              lgl	int	dbl	chr	list
is_logical()	x				
is_integer()		  x			
is_double()		      	x		
is_numeric()		  x	  x		
is_character()				x	
is_atomic()	  x 	x	  x	  x	
is_list()					              x
is_vector()	 x	  x	  x	  x	    x



# -----------------
# SCALARS AND RECYCLING RULES

# R will implicitly coerce the length of vectors --> RECYCLING

# R does not have scalars (vector of length 1) ... functions are vectorized

sample(10) + 100
#  [1] 109 110 101 104 108 107 103 105 106 102
runif(10) > 0.5
#  [1] FALSE FALSE  TRUE FALSE  TRUE  TRUE FALSE FALSE FALSE FALSE

# add vectors of different length ???
1:10 + 1:2
  # R expanded the shorter vector to the length of the longer one

# .... unless not a multiple
1:10 + 1:3
#> Warning in 1:10 + 1:3: longer object length is not a multiple of shorter object
#> length
#>  [1]  2  4  6  5  7  9  8 10 12 11


# vectorised functions will give an error if recycling other than scalar
tibble(x = 1:4, y = 1:2)
# Error:
#   ! Tibble columns must have compatible sizes.
# * Size 4: Existing data.
# * Size 2: Column `y`.
# i Only values of size one are recycled.
# Run `rlang::last_error()` to see where the error occurred.


# if you DO want to recycle use rep()
tibble(x = 1:4, y = rep(1:2,2))

# A tibble: 4 x 2
# x     y
# <int> <int>
# 1     1     1
# 2     2     2
# 3     3     1
# 4     4     2


# --------------------
# NAMING VECTORS

# you can name vectors during c()
c(x = 1, y = 2, z = 4)

# OR after
purrr::set_names(1:3,c("a","b","c"))


# --------------------
# SUBSETTING

dplyr::filter()
  # ONLY WORKS FOR TIBBLES

# FOR VECTORS --->  ' [ '
  # -> SUBSETTING FUNCTION

# 4 things you can subset a vector with ....

  # 1. Numeric vector containing only integers (must be all pos, all neg, or zero)

# subsetting with positive integers keeps elements at those positions
x <- c("one", "two", "three", "four", "five")
x[c(3,2,5)]
# [1] "three" "two"   "five" 

# repeating a position, make longer
x[c(1, 1, 5, 5, 5, 2)]
#> [1] "one"  "one"  "five" "five" "five" "two"

# NEGATIVE DROPS at the position
x[c(-1,-3,-5)]
# [1] "two"  "four"

x[0]
# character(0)

  # 2. Subsetting with a logical vector keeps all values corresponding to a TRUE
x <- c(10,3,NA,5,8,1,NA)

# all non missing x
x[!is.na(x)]
# [1] 10  3  5  8  1

# all even or missing values of x
x[x %% 2 == 0]
# [1] 10 NA  8 NA


  # 3. if you named a vector you can subset it w/ a character vector
x <- c(abc = 1, def = 2, xyz = 5)
x[c("xyz", "def")]
# xyz def 
# 5   2 

  # 4. simplest type of subsetting is nothing, x[] ... useful for matrix -- all rows OR al cols

#example, if x is 2d, x[1, ] selects the first row and all the columns, 
# and x[, -1] selects all rows and all columns except the first.


# -----------------------------
# EXERCISES

  # 1 - What does mean(is.na(x)) tell you about a vector x? What about sum(!is.finite(x))?
x <- c(2,4,6,NA,NA)
is.na(x)
mean(is.na(x)) # THE PROPORTION OF NA VALUES 0 0 0 1 1

y <- (c(-Inf, 5, 10, NA))
!is.infinite(y)
sum(!is.infinite(y)) # NUMBER OF VALUES NOT INFINITE (INCLUDING NA)


  # 2 - Carefully read the documentation of is.vector(). What does it actually test for? 
  # Why does is.atomic() not agree with the definition of atomic vectors above?

?is.vector()
  # checks whether the object has no attributes other than names. Thus a list is a vector:

is.vector(list(a = 1, b = 2))
# TRUE
x <- 1:10
attr(x, "something") <- TRUE
x
is.vector(x)
# [1] FALSE

# is.atomic checks whether an obj is one of the atomic typed (log,int,num,complex,char,raw) or NULL
is.atomic(1:5)
# TRUE

list(a = 1) %>% 
  is.atomic()
# [1] FALSE


  #  3 - 
setNames(1:4, c("a", "b", "c", "d"))
#> a b c d 
#> 1 2 3 4

purrr::set_names(1:4, "a", "b", "c", "d")
#> a b c d 
#> 1 2 3 4

# The biggest difference between set_names() and setNames() is that set_names() 
# allows for using a function or formula to transform the existing names.
purrr::set_names(c(a = 1, b = 2, c = 3), toupper)
# A B C 
# 1 2 3 


  # 4 - 
# Create functions that take a vector as input and returns:


#   The last value. Should you use [ or [[?
(last <- x[[length(x)]])
  # TIP!!! The function uses [[ in order to extract a single element.


#   The elements at even numbered positions.
even <- function(x) {
  if (length(x)) {
    x[seq_along(x) %% 2 == 0]
  } else {
    x
  }
}

#   Every element except the last value.
not_last <- function(x) {
  if (length(x)) {
    x[-length(x)]
  } else {
    x
  }
}

#   Only even numbers (and no missing values).
even_notNA <- function(x) {
  if (length(x)) {
    y <- x[!is.na(x)]
    y[y %% 2 == 0]
  } else {
    x
  }
}
# OR ....
even_numbers2 <- function(x) {
  x[!is.nan(x) & (x %% 2 == 0)]
}


# 5 - *** COME BACK TO *****
# 6 - 



# -------------------------------------------------------------------------

# 20.5 RECURSIVE VECTORS

# lists can contain other lists ... good for hierarchical or tree-like structure
x <- list(1,2,3)
  # [[1]]
  # [1] 1
  # 
  # [[2]]
  # [1] 2
  # 
  # [[3]]
  # [1] 3

# useful tool is str() --> focuses on the STRucture !
str(x)
#> List of 3
#>  $ : num 1
#>  $ : num 2
#>  $ : num

x_named <- list(a = 1, b = 2, c = 3)
str(x_named)
  # List of 3
  # $ a: num 1
  # $ b: num 2
  # $ c: num 3

# unlike atomic vectors, list() can contain a mix of objects
y <- list("a", 1L, 1.5, TRUE)
str(y)
  # List of 4
  # $ : chr "a"
  # $ : int 1
  # $ : num 1.5
  # $ : logi TRUE

# lists contain other lists
z <- list(list(1, 2), list(3, 4))
str(z)
#> List of 2
#>  $ :List of 2
#>   ..$ : num 1
#>   ..$ : num 2
#>  $ :List of 2
#>   ..$ : num 3
#>   ..$ : num 4


# --------------------------
# VISUALIZING LISTS
x1 <- list(c(1, 2), c(3, 4))
x2 <- list(list(1, 2), list(3, 4))
x3 <- list(1, list(2, list(3)))
str(x3)
  # *** SEE FIGURE ****


# --------------------------
# SUBSETTING

a <- list(a = 1:3, b = "a string", c = pi, d = list(-1, -5)) # working list

# 3 ways to subset a list

# 1. [ extracts a sub-list. The result will always be a list.
str(a[1:2])
  # List of 2
  # $ a: int [1:3] 1 2 3
  # $ b: chr "a string"
str(a[4])
  # List of 1
  # $ d:List of 2
  # ..$ : num -1
  # ..$ : num -5

# 2. [[ extracts a single component from a list. It removes a level of hierarchy from the list.
str(a[[1]])
#  int [1:3] 1 2 3
str(a[[4]])
#> List of 2
#>  $ : num -1
#>  $ : num -5

# 3. $ is a shorthand for extracting named elements of a list
a$a
# [1] 1 2 3
a[["a"]]
# [1] 1 2 3


# ---------------------------
# LIST OF CONDIMENTS ...  [ vs [[

# let x be a pepper shaker w/ pepper packetS in it ... it is our LIST

# list is x, x[1] is a pepper shaker with a SINGLE pepper packet

  # x[2] is the same but the SECOND packet
  # x[1:2] is the pepper shaker with 2 packets inside

  # x[[1]] is a single pepper packet  (no shaker)

  # if you want the content of pepper packet
    # --> x[[1]][[1]]



# EXERCISES
  # 1 - Draw the following lists as nested sets:
list(a, b, list(c, d), list(e, f))
list(list(list(list(list(list(a))))))


# 2 - What happens if you subset a tibble as if you're subsetting a list?
  # What are the key differences between a list and a tibble?



# -------------------------------------------------------------------------

# 20.6 ATTRIBUTES
  # vector can contain arbitrary additional metadata through its attributes


# ...... come back











