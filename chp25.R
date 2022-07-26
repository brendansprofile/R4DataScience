# https://r4ds.had.co.nz/many-models.html

# CHAPTER 25 - MANY MODELS


# INTRODUCTION ------------------------------------------------------------

# 3 powerful ideas that help you to work with large numbers of models with ease:
  # 1. using many simple models to better understand complex data sets
  # 2. using list-columns to store arbitrary data structures in a data frame.
        # ex: allow you to have column that contains linear models
  # 3. using BROOM package, turn models into tidy data. powerful technique for
    # working with large numbers of models b/c once have data tidy, can apply all techniques


# In list-columns, you'll learn more about the list-column data structure, and why 
# it's valid to put lists in data frames.
# 
# In creating list-columns, you'll learn the three main ways in which you'll create 
# list-columns.
# 
# In simplifying list-columns you'll learn how to convert list-columns back to 
# regular atomic vectors (or sets of atomic vectors) so you can work with them more easily.
# 
# In making tidy data with broom, you'll learn about the full set of tools provided
# by broom, and see how they can be applied to other types of data structure.


library(modelr)
library(tidyverse)



# 25.2 gapminder ----------------------------------------------------------

#install.packages("gapminder")
library(gapminder)
gapminder

# focus on just three variables to answer the question :
# "How does life expectancy (lifeExp) change over time (year) for each country (country)?"

gapminder %>% 
  ggplot(aes(year, lifeExp, group = country)) +
  geom_line(alpha = 1/3)
# life expectancy has been steadily improve...some dont follow pattern ....

# strong signal (linear growth) that makes it hard to see subtler trends

# tease these factors apart by fitting a model with a linear trend. 
# The model captures steady growth over time, and the residuals will show what's left.

nz <- filter(gapminder, country == "New Zealand")
nz %>% 
  ggplot(aes(year, lifeExp)) + 
  geom_line() + 
  ggtitle("Full data = ")

nz_mod <- lm(lifeExp ~ year, data = nz)
nz %>% 
  add_predictions(nz_mod) %>%
  ggplot(aes(year, pred)) + 
  geom_line() + 
  ggtitle("Linear trend + ")

nz %>% 
  add_residuals(nz_mod) %>% 
  ggplot(aes(year, resid)) + 
  geom_hline(yintercept = 0, colour = "white", size = 3) + 
  geom_line() + 
  ggtitle("Remaining pattern")

# nz %>% 
#   add_predictions(nz_mod) %>% 
#   add_residuals(nz_mod) %>% 
#   select(year, lifeExp, pred, resid) %>% 
#   arrange(resid)


# NESTED DATA ---------------------------

# How can we easily fit the model to every country ????
# Extract out the common code with a function and repeat using a map function from purrr

# To create a nested data frame we start with a grouped data frame, and "nest" it:
by_country <- gapminder %>% 
  group_by(country, continent) %>% 
  nest()

by_country
#> # A tibble: 142 x 3
#> # Groups:   country, continent [142]
#>   country     continent data             
#>   <fct>       <fct>     <list>           
#> 1 Afghanistan Asia      <tibble [12 × 4]>
#> 2 Albania     Europe    <tibble [12 × 4]>

    # 1 row per country, and 'data' is a list of df(tibbles)

by_country$data[[1]]
#> # A tibble: 12 x 4
#>    year lifeExp      pop gdpPercap
#>   <int>   <dbl>    <int>     <dbl>
#> 1  1952    28.8  8425333      779.
#> 2  1957    30.3  9240934      821.
#> 3  1962    32.0 10267083      853.

# NOTE: grouped data frame, each row is an observation; in a nested data frame, each row is a group.


# LIST COLUMNS -----------------

# model fit function
country_model <- function(df){
  lm(lifeExp ~ year, data = df)
}

# want to apply to every df... df are in a list, use purrr:map() to apply mod to each
models <- map(by_country$data, country_model)

# better to store it as a column in the by_country data frame. 
# instead of creating a new object in the global environment, we're going to 
  # create a new variable in the by_country data frame


by_country <- by_country %>% 
  mutate(model = map(data, country_model))
by_country
#> # A tibble: 142 x 4
#> # Groups:   country, continent [142]
#>   country   continent data              model 
#>   <fct>       <fct>     <list>            <list>
#> 1 Afghanistan Asia      <tibble [12 × 4]> <lm>  
#> 2 Albania     Europe    <tibble [12 × 4]> <lm>  
#> 3 Algeria     Africa    <tibble [12 × 4]> <lm> 

  # TIP: this is BIG advantage, all related objects stored together,
    # dont need to manually keep in sync when filtering or arranging

by_country %>% 
  filter(continent == "Europe")
#> # A tibble: 30 x 4
#> # Groups:   country, continent [30]
#>   country                continent data              model 
#>   <fct>                  <fct>     <list>            <list>
#> 1 Albania                Europe    <tibble [12 × 4]> <lm>  
#> 2 Austria                Europe    <tibble [12 × 4]> <lm> 

by_country %>% 
  arrange(continent, country)
#> # A tibble: 142 x 5
#> # Groups:   country, continent [142]
#>   country     continent data              model  resids           
#>   <fct>       <fct>     <list>            <list> <list>           
#> 1 Afghanistan Asia      <tibble [12 × 4]> <lm>   <tibble [12 × 5]>
#> 2 Albania     Europe    <tibble [12 × 4]> <lm>   <tibble [12 × 5]>


# UNNESTING --------------------

# situation: now have 142 df and 142 models
# compute residuals need to call add_residuals() to each pair
by_country <- by_country %>% 
  mutate(
    resids = map2(data, model, add_residuals)
  )
by_country

# how to plot list of data frames??? ... instead use unnest()
resids <- unnest(by_country, resids)
resids
#> # A tibble: 1,704 x 9
#> # Groups:   country, continent [142]
#>   country    continent data        model  year lifeExp     pop gdpPercap   resid
#>   <fct>      <fct>     <list>      <lis> <int>   <dbl>   <int>     <dbl>   <dbl>
#> 1 Afghanist. Asia      <tibble [1. <lm>   1952    28.8  8.43e6      779. -1.11  
#> 2 Afghanist. Asia      <tibble [1. <lm>   1957    30.3  9.24e6      821. -0.952 
#> 3 Afghanist. Asia      <tibble [1. <lm>   1962    32.0  1.03e7      853. -0.664 

# each col is repeated once for each row of nested tibble

# now, we can plot
resids %>% 
  ggplot(aes(year,resid)) +
  geom_line(aes(group = country), alpha = 1/3) +
  geom_smooth(se = FALSE)

# facet by continent
resids %>% 
  ggplot(aes(year, resid, group = country)) +
  geom_line(alpha = 1 / 3) + 
  facet_wrap(~continent)

# we've missed some mild patterns, esp in africa



# MODEL QUALITY -----------------

  # instead of looking at resid, we can look at measures of model qual
  # broom::

broom::glance(nz_mod)
#> # A tibble: 1 x 12
#>   r.squared adj.r.squared sigma statistic p.value    df logLik   AIC   BIC
#>       <dbl>         <dbl> <dbl>     <dbl>   <dbl> <dbl>  <dbl> <dbl> <dbl>
#> 1     0.954         0.949 0.804      205. 5.41e-8     1  -13.3  32.6  34.1
#> # . with 3 more variables: deviance <dbl>, df.residual <int>, nobs <int>

# use mutate() and unnest() to create a df for each country
by_country %>% 
  mutate(glance = map(model, broom::glance)) %>% 
  unnest(glance)
# A tibble: 142 x 17
  #> # Groups:   country, continent [142]
  #>   country continent data  model resids r.squared adj.r.squared sigma statistic
  #>   <fct>   <fct>     <lis> <lis> <list>     <dbl>         <dbl> <dbl>     <dbl>
  #> 1 Afghan. Asia      <tib. <lm>  <tibb.     0.948         0.942 1.22      181. 
  #> 2 Albania Europe    <tib. <lm>  <tibb.     0.911         0.902 1.98      102. 

glance <- by_country %>% 
  mutate(glance = map(model, broom::glance)) %>% 
  unnest(glance, .drop = TRUE) #suppress single list columns
glance

# now we have the df, we can look at models that dont fit well
glance %>% 
  arrange(r.squared)
#> # A tibble: 142 x 17
#> # Groups:   country, continent [142]
#>   country continent data  model resids r.squared adj.r.squared sigma statistic
#>   <fct>   <fct>     <lis> <lis> <list>     <dbl>         <dbl> <dbl>     <dbl>
#> 1 Rwanda  Africa    <tib. <lm>  <tibb.    0.0172      -0.0811   6.56     0.175
#> 2 Botswa. Africa    <tib. <lm>  <tibb.    0.0340      -0.0626   6.11     0.352

# worst models appear to be in Africa, double check w/ a plot
glance %>% 
  ggplot(aes( r.squared, continent)) +
  geom_jitter() # small number of observations & discrete variable (avoid overplot)

# pull out the countries with bad R^2 
bad_fit <- filter(glance, r.squared < 0.25)

gapminder %>% 
  semi_join(bad_fit, by = "country") %>% 
  ggplot(aes(year, lifeExp, colour = country)) +
  geom_line()
# HIV and Rwandan geonocide :((


# EXERCISES ---------------------------



