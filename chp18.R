# https://r4ds.had.co.nz/pipes.html 

#-------------------------------------------------------------------------------
# CHP 17 INTRO
  # Programming is a cross-cutting skill needed for all data science work:
  # Programming produces code, and code is a tool of communication.

  # After solving a data analysis challenge, it's often worth looking at your
  # code and thinking about whether or not it's obvious what you've done.



# ------------------------------------------------------------------------------
# CHP 18 PIPES

# Pipes are a powerful tool for clearly expressing a sequence of multiple operations
library(magrittr)
  # library(tidyverse) contains this



# ------------------------------------------------------------------------------
# 18.2  PIPING ALTERNATIVES

# too see why pipe is so useful, explore number of ways writing same code

foo_foo <- little_bunny()

# use a function for each key verb: hop(), scoop(), and bop()

# 1) INTERMEDIATE STEPS
foo_foo_1 <- hop(foo_foo, through = forest)
foo_foo_2 <- scoop(foo_foo_1, up = field_mice)
foo_foo_3 <- bop(foo_foo_2, on = head)

  # forces you to name each intermediate step
  # if there are natural names, good idea
    # if NOT, you add numeric suffixes to make unique
      # Problem 1) - code cluttered with unimportant names
      # Problem 2) - carefully increment the suffix

# if you think it will take up to much space, NAH --> shared columns!!!!
install.packages("pryr")
library(pryr)

diamonds <- ggplot2::diamonds
diamonds2 <- diamonds %>% 
  dplyr::mutate(price_per_carat = price / carat)

pryr::object_size(diamonds)
#> Registered S3 method overwritten by 'pryr':
#>   method      from
#>   print.bytes Rcpp
#> 3.46 MB
pryr::object_size(diamonds2)
#> 3.89 MB
pryr::object_size(diamonds, diamonds2)
#> 3.89 MB


# 2) OVERWRITE TE ORIGINAL
foo_foo <- hop(foo_foo, through = forest)
foo_foo <- scoop(foo_foo, up = field_mice)
foo_foo <- bop(foo_foo, on = head)
  
  # Debugging is painful: if you make a mistake you'll need to re-run the complete pipeline from the beginning.
  # The repetition of the object being transformed (we've written foo_foo six times!) obscures what's changing on each line.


# 3) FUNCTION COMPOSITION
bop(
  scoop(
    hop(foo_foo, through = forest),
    up = field_mice
  ), 
  on = head
)
  # NO


# 4) PIPE
foo_foo %>%
  hop(through = forest) %>%
  scoop(up = field_mice) %>%
  bop(on = head)
  # focus on verbs, not nouns -- Foo Foo hops, then scoops, then bops

# pipe works by performing a "lexical transformation"
  # magrittr does something like this:
my_pipe <- function(.) {
  . <- hop(., through = forest)
  . <- scoop(., up = field_mice)
  bop(., on = head)
}
my_pipe(foo_foo)

# PIPE will NOT work for two classes of functions
  # 1) Function that use the current environment (get(), load(), assign() )
assign("x", 10)
x
#> [1] 10

"x" %>% assign(100)
x
#> [1] 10

  # 2) Functions that use lazy evaluation.(trycatch, suppressWarning,...)



# ------------------------------------------------------------------------------
# 18.3 WHEN NOT TO USE PIPE

#  Pipes are most useful for rewriting a fairly short linear sequence of operations

# Should use something else when ...
  # 1. pipe longer than ~10 steps
    # --> create intermediate objects
  
  # 2. multiple inputs/outputs
    #two or more objects being combined together, don't use the pipe

  # 3. 



# ------------------------------------------------------------------------------
# 18.4 OTHER TOOLS WITH MAGRITTR

# %T>%  works like pipe but returns the left hand side
rnorm(100) %>%
  matrix(ncol = 2) %>%
  plot() %>%
  str()
# NULL
rnorm(100) %>%
  matrix(ncol = 2) %T>%
  plot() %>%
  str()
#  num [1:50, 1:2] -1.073 1.333 -0.999 -0.167 0.272 ...

# %$>% explodes out variables in a df so you can refer to them explicitly
mtcars %$%
  cor(disp, mpg)
#> [1] -0.8475514




