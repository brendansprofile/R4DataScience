# https://r4ds.had.co.nz/functions.html 

# CHP 19 FUNCTIONS

# Functions allow you to automate common tasks in a more powerful and general
`# way than copy-and-pasting

# 3 Main Advantages of Functions
  # You can give a function an evocative name that makes your code easier to understand.

  # As requirements change, you only need to update code in one place, instead of many.

  # You eliminate the chance of making incidental mistakes when you copy and paste
  # (i.e. updating a variable name in one place, but not in another).



# --------------
# 19.2 WHEN TO WRITE A FUNCITON

# Consider writing a function when you copied & pasted block of code more than twice 
# ex: 
df <- tibble::tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

df$a <- (df$a - min(df$a, na.rm = TRUE)) / 
  (max(df$a, na.rm = TRUE) - min(df$a, na.rm = TRUE))
df$b <- (df$b - min(df$b, na.rm = TRUE)) / 
  (max(df$b, na.rm = TRUE) - min(df$a, na.rm = TRUE)) # MISTAKE
df$c <- (df$c - min(df$c, na.rm = TRUE)) / 
  (max(df$c, na.rm = TRUE) - min(df$c, na.rm = TRUE))
df$d <- (df$d - min(df$d, na.rm = TRUE)) / 
  (max(df$d, na.rm = TRUE) - min(df$d, na.rm = TRUE))

# 1) How many inputs does the code have?
(df$a - min(df$a, na.rm = TRUE)) /
  (max(df$a, na.rm = TRUE) - min(df$a, na.rm = TRUE))
    # --> just one, df$a

# 2) rewrite the code with temporary variables w/ general names to make more clear
x <- df$a
(x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
#>  [1] 0.2892677 0.7509271 0.0000000 0.6781686 0.8530656 1.0000000 0.1716402
#>  [8] 0.6107464 0.6116181 0.6008793

# 3) some duplication of code, compute range 3x, do in one step
rng <- range(x, na.rm = TRUE)
(x - rng[1]) / (rng[2] - rng[1])
# [1] 0.2322392 1.0000000 0.4504517 0.1127375 0.1880706 0.4724134 0.4464457 0.6422948 [9] 0.0000000 0.9291498

# 4) Now we can turn into func
rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}
rescale01(c(0, 5, 10))
# [1] 0.0 0.5 1.0


# 3 KEY STEPS TO CREATING A NEW FUNCTION
# 1. You need to pick a name for the function. Here I've used rescale01 because this 
# function rescales a vector to lie between 0 and 1.

# 2. You list the inputs, or arguments, to the function inside function. Here we 
# have just one argument. If we had more the call would look like function(x, y, z).

# 3. You place the code you have developed in body of the function, a { block 
#   that immediately follows function(...)

# ...
# we can now simplify the original example bc we have a func
df$a <- rescale01(df$a)
df$b <- rescale01(df$b)
df$c <- rescale01(df$c)
df$d <- rescale01(df$d)

# ... if we discover our requirements change, we just change that function
x <- c(1:10, Inf)
rescale01(x)
#>  [1]   0   0   0   0   0   0   0   0   0   0 Na

#  --> 
rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE, finite = TRUE) # FIX
  (x - rng[1]) / (rng[2] - rng[1])
}
rescale01(x)
# [1] 0.0000000 0.1111111 0.2222222 0.3333333 0.4444444 0.5555556 0.6666667 0.7777778
# [9] 0.8888889 1.0000000       Inf


# *** DRY principle = Dont Repeat Yourself in code, creates bugs down road


# EXERCISES
  # 1 - Why is TRUE not a parameter to rescale01()? What would happen if x 
  # contained a single missing value, and na.rm was FALSE?

  # 2 - In the second variant of rescale01(), infinite values are left unchanged.
  # Rewrite rescale01() so that -Inf is mapped to 0, and Inf is mapped to 1.
rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE, finite = TRUE)
  y <- (x - rng[1]) / (rng[2] - rng[1])
  y[y == -Inf] <- 0
  y[y == Inf] <- 1
  y
}
rescale01(c(Inf, -Inf, 0:5, NA))
# [1] 1.0 0.0 0.0 0.2 0.4 0.6 0.8 1.0  NA


# 3 - Practice turning the following code snippets into functions. Think about 
  # What each function does. What would you call it? How many arguments does it need? 
  # Can you rewrite it to be more expressive or less duplicative?
  x <- c(2,4,NA)
  
mean(is.na(x)) # determines proportion of NA values
# ...
mean_na <- function(x){
  y <- is.na(x)
  mean(y)
}


x / sum(x, na.rm = TRUE) # normalizes vector to sum to 1
# ...
vec_div <- function(x){
  y <- sum(x, na.rm = TRUE)
  x / y
}


sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE) # coefficicent of variation
# ...

coef_variation <- function(x, na.rm = FALSE) { # input x and optional na.rm
  sd(x, na.rm = na.rm) / mean(x, na.rm = na.rm)
}

# 4 - write your own functions to compute the variance and skewness of a numeric vector. 
  # Variance is defined as

variance <- function(x, na.rm = TRUE){
  n = length(x)
  x_bar <- sum(x) / n
  deg_free <- 1 / (n - 1)
  (var_x <- deg_free * sum((x - x_bar)^2))
}

  # skewness is defined as
skewness <- function(x, na.rm = FALSE) {
  n <- length(x)
  m <- mean(x, na.rm = na.rm)
  v <- var(x, na.rm = na.rm)
  (sum((x - m) ^ 3) / (n - 2)) / v ^ (3 / 2)
}


# 5 - Write both_na(), a function that takes two vectors of the same length and 
  # returns the number of positions that have an NA in both vectors.

both_na <- function(x,y){
  ifelse(
    length(x) != length(y),
    print("vectors must be of the same length!"),
    print(length(is.na(x)),length(is.na(y)))
  )
}

# OR
both_na <- function(x, y) {
  sum(is.na(x) & is.na(y))
}

# 



# ------------------------------------------------------------------------- CTRL SHIFT R
# ------------------------------------------------------------------------------
# 19.3 FUNCTIONS ARE FOR HUMANS AND COMPUTERS

# Generally, function names should be verbs, and arguments should be nouns.

# If you have a family of functions that do similar things, make sure they have consistent names and arguments
# Good
input_select()
input_checkbox()
input_text()

# comments, lines starting with #, to explain the "why" of your code

# use breaks! (CTRL + SHIFT + R)
# -------------------------------------------------------------------------


# EXERCISES
# 1 - figure out functions and rename
has_prefix() <- function(string, prefix) {
  substr(string, 1, nchar(prefix)) == prefix
}


drop_last <- function(x) {
  if (length(x) <= 1) return(NULL)
  x[-length(x)]
}

f3 <- function(x, y) {
  rep(y, length.out = length(x))
}


# 3 - 
?rnorm()
?MASS::mvrnorm()

# rnorm() samples from the univariate normal distribution, 
# while MASS::mvrnorm samples from the multivariate normal distribution.


# 4 - norm_r() & norm_d() VS rnorm() & dnorm()



# -------------------------------------------------------------------------
# 19.4 CONDITIONAL EXECUTION

if (condition) {
  # code executed when condition is TRUE
} else {
  # code executed when condition is FALSE
}

# return a logical vector describing whether or not each element of a vector is named.
has_name <- function(x) {
  nms <- names(x)
  if (is.null(nms)) {
    rep(FALSE, length(x))
  } else {
    !is.na(nms) & nms != ""
  }
}
# takes advantage of the standard return rule: a function returns the last value that it computed


# -----------
# CONDITIONS
  # MUST evaluate to true or false
    # if vector --> warning
    # if NA --> error

# ( x || y) OR
# ( x && y) AND

# DONT USE | & in the if statement; vectorized operations for mult vals
  # if have a logical vector --> any() or all() to collapse()

# EQUALITY
   # == is also vectorized
    # identical() is non-vect and always returns single TRUE or FALSE
identical(0L,0)
# FALSE

# comparing non exact dpyr::near()


# ------
# MULT COMPARISONS

if (this) {
  # do that
} else if (that) {
  # do something else
} else {
  # 
}

# if too many if/else
  # --> switch()

# to discrete continuous variables
   # --> cut()


# -----------
# CODE STYLE

# Good
if (y < 0 && debug) {
  message("Y is negative")
}


if (y == 0) {
  log(x)
} else {
  y ^ x
}

# An opening curly brace should never go on its own line and should always be 
# followed by a new line. A closing curly brace should always go on its own 
# line, unless it's followed by else. Always indent the code inside curly braces.


# EXERCISES
  # 1- if() vs ifelse()
if
  # tests a single condition
?ifelse()
  # tests each element

y <- 10
ifelse(y > 5, y * 5, y / 5)
# [1] 50

if(y > 5){
  y * 5
} else {
  y / 5
}
# [1] 50


# 2 - function that greets based on time
library(lubridate)

greeter <- function(time = lubridate::now()) {
  hr <- hour(now()) # hour 0-24
  if (hr > 0 && hr < 12) {
    print("Good morning")
  } else if (hr < 17) {
    print("Good afternoon")
  } else {
    print("Good evenging")
  }
}

> hour(now())
#[1] 21
> greeter()
#[1] "Good evenging"


  # 3 - 
# fizzbuzz function. It takes a single number as input. If the number is 
# divisible by three, it returns "fizz". If it's divisible by five it returns
# "buzz". If it's divisible by three and five, it returns "fizzbuzz". Otherwise,
# it returns the number.

fizzbuzz <- function(x) {
  
  stopifnot(length(x) == 1) # input valid
  stopifnot(is.numeric(x)) # input valid
  
  if (x %% 3 == 0 && x %% 5 == 0) { # div by 3 and 5
    print("fizzbuzz")
  } else if (x %% 3 == 0) { # div by 3
    print("fizz")
  } else if (x %% 5 == 0) { # div by 5
    print("buzz")
  } else { # none
    as.character(x)
  }
}


# 4 - How could you use cut() to simplify this set of nested if-else statements?
  
  if (temp <= 0) {
    "freezing"
  } else if (temp <= 10) {
    "cold"
  } else if (temp <= 20) {
    "cool"
  } else if (temp <= 30) {
    "warm"
  } else {
    "hot"
  }

temp <- seq(-10,50, by = 5)
cut(temp, c(-Inf,0,10,20,30,Inf),
    right = FALSE, # open intervals 
    labels = c("freezing", "cold", "cool", "warm", "hot")
    )


# 5 - What happens if you use switch() with numeric values?
switch (object,
        case = action
)

switch(1, "apple", "banana", "cantaloupe")
#> [1] "apple"
switch(2, "apple", "banana", "cantaloupe")
#> [1] "banana"

# 6 - 
switcheroo <- function(x) {
  switch(x,
         a = ,
         b = "ab",
         c = ,
         d = "cd"
  )
}

switcheroo("a")
#> [1] "ab"
switcheroo("b")
#> [1] "ab"
switcheroo("c")
#> [1] "cd"
switcheroo("d")
#> [1] "cd"
switcheroo("e")
switcheroo("f")

# The switch() function returns the first non-missing argument value for the 
# first name it matches. Thus, when switch() encounters an argument with a
# missing value, like a = ,, it will return the value of the next argument with 
# a non missing value, which in this case is b = "ab". If object in 
# switch(object=) is not equal to the names of any of its arguments, switch()
# will return either the last (unnamed) argument if one is present or NULL.
# Since "e" is not one of the named arguments in switch() (a, b, c, d), and no
# other unnamed default value is present, this code will return NULL.




# -------------------------------------------------------------------------

# 19.5 FUNCTIONS AND ARGUMENTS

# arguments to functions typically fall into two categories
  # 1) data - what to compute on
  # 2) details - argument that controls details of the computation


# In log(), the data is x, and the detail is the base of the logarithm.

# In mean(), the data is x, and the details are how much data to trim from 
# the ends (trim) and how to handle missing values (na.rm).

# In t.test(), the data are x and y, and the details of the test are 
# alternative, mu, paired, var.equal, and conf.level.

# In str_c() you can supply any number of strings to ..., and the details of 
# the concatenation are controlled by sep and collapse.


# RULE : data first, then details (generally)

# Compute confidence interval around mean using normal approximation
mean_ci <- function(x, conf = 0.95) {
  se <- sd(x) / sqrt(length(x))
  alpha <- 1 - conf
  mean(x) + se * qnorm(c(alpha / 2, 1 - alpha / 2))
}

x <- runif(100)
mean_ci(x)
#> [1] 0.4976111 0.6099594
mean_ci(x, conf = 0.99)
#> [1] 0.4799599 0.6276105


# default value should always be the most common
# If you override the default, call it explicitly 

mean(1:10, na.rm = TRUE)


# ---------------
# CHOOSING NAMES

# generally longer more descriptive names
  # BUT, important short names:

    # x, y, z: vectors.
    # w: a vector of weights.
    # df: a data frame.
    # i, j: numeric indices (typically rows and columns).
    # n: length, or number of rows.
    # p: number of columns.


# ---------------
# CHECKING VALUES
  # eventually may not remember how your function works
    # useful to make constraints specific

# ex:
wt_mean <- function(x, w) {
  sum(x * w) / sum(w)
}

  # what if x and w not same length...?
wt_mean(1:6, 1:3)
#> [1] 7.666667

# add a stop
wt_mean <- function(x,w) {
  if (length(x) != length(w)) {
    stop("'x' and 'w' must be the same length", call. = FALSE)
  }
  sum(w * x) / sum(w)
}
# > wt_mean(1:6, 1:3)
# Error: 'x' and 'w' must be the same length


# TIP: dont go overboard!!!
  # can use stopifnot()
    # stopifnot() you assert what should be true rather than checking for what might be wrong.


# --------------------
# THE DOT DOT DOT (...)
  # some functions take an arbitrary number of inputs

sum(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
#[1] 55

# use special argument
  # (...)  -  captures any number of arguments that arent othw matched

commas <- function(...) stringr::str_c(..., collapse = ", ")
commas(letters[1:10])
#> [1] "a, b, c, d, e, f, g, h, i, j"

rule <- function(..., pad = "-") {
  title <- paste0(...)
  width <- getOption("width") - nchar(title) - 5
  cat(title, " ", stringr::str_dup(pad, width), "\n", sep = "")
}
rule("HEADER")
# HEADER ------------------------------------------------------------------------


# ---------------
# LAZY EVALUATION
# arguments not computed until they're needed. 
# That means if they're never used, they're never called


# EXERCISES
  # 1 -
  # 2 - 
  # 3 - What does the trim argument to mean() do? When might you use it?
x <- c(-1000,1:10)
mean(x)
# [1] -85.90909
mean(x, trim = 1/10)
# [1] 5

# The trim arguments trims a fraction of observations from each end of the vector
# (meaning the range) before calculating the mean. This is useful for calculating
# a measure of central tendency that is robust to outliers.



# -------------------------------------------------------------------------
# 19.6 RETURN VALUES

# 2 things you should consider when returning a value:
    # 1) Does returning early make your function easier to read?
    # 2) Can you make your function pipeable?


# -------------------------
# EXPLICIT RETURN STATEMENTS

  # function returned is typically the last statement evaluated
    # --> choose to return early with return()

# TIP: best to save the use of return() to signal that you can return 
  # early with a simpler solution

  # ex 1: inputs are empty
complicated_function <- function(x, y, z) {
  if (length(x) == 0 || length(y) == 0) {
    return(0)
  }
  # Complicated code here
}

  # ex 2: if statement with one complex block and one simple block
f <- function() {
  if (!x) {
    return(something_short)
  }
  
  # Do 
  # something
  # that
  # takes
  # many
  # lines
  # to
  # express
}


# --------------------------
# WRITING PIPEABLE FUNCTIONS

# to write a pipeable function, important to think about the output
  # --> knowing the return ouput OBJECT TYPE

# There are two basic types of pipeable functions: 
  # --> transformations and side-effects

    # transformations: an object is passed to function's first argument and a 
      # modified object is returned

    # side-effects: the passed object is not transformed. Instead, the function 
      # performs an action on the object, like drawing a plot or saving a file
        # -> should "invisibly" return the first argument, so that while 
        # they're not printed they can still be used in a pipeline

show_missings <- function(df) {
  n <- sum(is.na(df))
  cat("Missing values: ", n, "\n", sep = "")
  
  invisible(df)  # does not get printed out
}

show_missings(mtcars)

# its still there
x <- show_missings(mtcars) 
#> Missing values: 0
class(x)
#> [1] "data.frame"
dim(x)
#> [1] 32 11

# can use it in a pipe!
mtcars %>% 
  show_missings() %>% 
  mutate(mpg = ifelse(mpg < 20, NA, mpg)) %>% 
  show_missings()
# Missing values: 0
# Missing values: 18




# -------------------------------------------------------------------------

# 19.7 ENVIORNMENT

# Environment of a function controls how R finds the value associated with a name.

f <- function(x) {
  x + y
} 

# R uses rules called lexical scoping to find the value associated with a name.
# Since y is not defined inside the function, R will look in the environment 
# where the function was defined:

y <- 100
f(10)
#> [1] 110






