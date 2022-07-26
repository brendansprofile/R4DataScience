# https://r4ds.had.co.nz/exploratory-data-analysis.html

# CHAPTER 7 - Exploratory Data Analysis

# EDA is an iterative cycle. You:
#   
# 1 - Generate questions about your data.
# 
# 2 - Search for answers by visualizing, transforming, and modelling your data.
# 
# 3 - Use what you learn to refine your questions and/or generate new questions.


# 7.1 
install.packages("tidyverse")
library(tidyverse)

#-------------------------------------------------------------------------------
# 7.2 - QUESTIONS

## large quantity of questions ....

## Two useful starts
  # 1 - What type of variation occurs within my variables?

  # 2 - What type of covariation occurs between my variables?


#-------------------------------------------------------------------------------
# 7.3 - VARIABLES


## A variable is CATEGORICAL if it can only take one of a small set of values.
  # BAR CHARTS are good for categorical
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut))
  # compute manually
diamonds %>% 
  count(cut)

## A variable is CONTINUOUS if it can take any of an infinite set of ordered values
  # To examine the distribution of a continuous variable --> HISTOGRAM
ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = carat), binwidth = 0.5)
  # compute manually
diamonds %>% 
  count(cut_width(carat,0.5))


## TYPICAL VALUES

# adj the bins to look at more paterns
smaller <- diamonds %>% 
  filter(carat < 3)

ggplot(data = smaller)+
  geom_histogram(mapping = aes(x = carat), binwidth = 0.1)

## NOTE: overlay multiple histograms in the same plot, I recommend using geom_freqpoly()
ggplot(data = smaller, mapping = aes(x = carat, colour = cut)) +
  geom_freqpoly(binwidth = 0.1)

## NOTE: ask ... what are typical values? why? what could be misleading? patterns? ...
ggplot(data = smaller, mapping = aes(x = carat)) +
  geom_histogram(binwidth  = 0.01)
    # why more diamonds at whole carats?
    # why more just to the right of peak
    
## NOTE:  To understand the subgroups, ask:
  
  # How are the observations within each cluster similar to each other?
  # 
  # How are the observations in separate clusters different from each other?
  # 
  # How can you explain or describe the clusters?
  # 
  # Why might the appearance of clusters be misleading?

ggplot(data = faithful, mapping = aes(x = eruptions)) +
  geom_histogram(binwidth = 0.25)


# UNUSUAL VALUES
ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = y), binwidth = 0.5)
      # only evidence of outliers is large x-axis

  ## need to zoom in to observe odd values
ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = y), binwidth = 0.5) +
  coord_cartesian(ylim = c(0,50))
  ## ... we see odd values at 30 and 60 ish
(unusal <- diamonds %>% 
  filter(y < 3 | y > 20) %>% # y is the width in mm
  select(price, x, y, z) %>% 
  arrange(y))
  ## last values the diamonds are huge but not that expensive

## NOTE: repeat analysis with and WITHOUT outliers ....
  # If they have minimal effect on the results, and you can't figure out why they're
  # there, it's reasonable to replace them with missing values, and move on. 
  # However, if they have a substantial effect on your results, you shouldn't drop 
  # them without justification. 

## 7.3 EXERCISES -->
  # 1 - explore the distributions of x,y,z which is length, width, depth
diamonds %>% 
  select(x,y,z) %>% 
  summary()

ggplot(data = diamonds, mapping = aes(x = x)) +
  geom_histogram(binwidth = 0.01)

ggplot(data = diamonds, mapping = aes(x = y)) +
  geom_histogram(binwidth = 0.01)

ggplot(data = diamonds, mapping = aes(x = z)) +
  geom_histogram(binwidth = 0.01)

  # x,y > z(IQR: 2.9-4)
  # outliers present, especially in y&z b/c of the scale
  # right skew (positive)
  # outliers x,y,z have 0 vals as minimum
    filter(diamonds, x == 0 | y == 0 | z == 0)
  # very high values in y & z
    diamonds %>% 
      arrange(desc(y)) %>% 
      head()
        # probably an error
  
  # let's look at outliers bivariately
    ggplot(data = diamonds, mapping = aes(x = x, y = y)) +
      geom_point()
    ggplot(data = diamonds, mapping = aes(x = x, y = z)) +
      geom_point()
    
  
  ## 2 - explore the distribution of price
diamonds %>% 
  select(price) %>% 
  summary()
# seems cheap...

ggplot(data = diamonds, mapping = aes(x = price)) +
  geom_histogram(binwidth = 1000)    
# right skew to very cheap diamonds ... why a gap at $1700??

ggplot(data = diamonds, mapping = aes(x = price, colour = cut)) +
  geom_freqpoly(binwidth = 1000) #+
  # geom_histogram(binwidth = 100, alpha = 1/5) +
  # facet_wrap(~ cut)

ggplot(data = diamonds, mapping = aes(x = carat, y = price)) +
  geom_jitter(alpha = 1/5)


  ## 3 - How many diamonds are 0.99 carat?
diamonds %>% 
  filter(carat == 0.99) %>% 
  count(carat)

  ##4 - Expirement with coor_catesian and xlim/ylim
# ggplot(data = diamonds, mapping = aes(x = cut)) +
#   geom_histogram(stat = "count")
ggplot(data = diamonds, mapping = aes(x = price)) +
  geom_histogram() +
  coord_cartesian(xlim = c(100,5000), ylim = c(0,3000)) #no good

ggplot(diamonds) +
  geom_histogram(mapping = aes(x = price)) +
  xlim(100, 5000) +
  ylim(0, 3000) # better since these calc the limits before hand

(thisissick <- ggplot(diamonds, aes(carat, price)) +
  stat_bin2d(bins = 25, colour = "white"))



#-------------------------------------------------------------------------------
# 7.4 MISSING VALUES

# If you encounter usual values in data 
# diamonds2 <- diamonds %>% 
#   filter(between(y, 3, 20)) # NOT GOOD - doing this var by var can run out of data

  # 2) Replace unusal values with missing values (NA)
(diamonds2 <- diamonds %>% 
  mutate(y = ifelse(y < 4, NA, y)))
  
# NOTE: ifelse() ... casewhen() useful when u need to mutate complex combos of vars

ggplot(data = diamonds2, mapping = aes(x = x, y = y)) +
  geom_point()
# gives a warning about NA values, use na.rm to remove it
#ggplot(data = diamonds2
  # 1) Drop row with strange valeues
# , mapping = aes(x = x, y = y)) +
#   geom_point(na.rm = TRUE)

# other times NA values may be meaningful ex: flight is cancel
flights %>% 
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min / 60
  ) %>% 
  ggplot(mapping = aes(sched_dep_time)) +
    geom_freqpoly(mapping = aes(colour = cancelled), binwidth = 1/4)

## 7.4 - QUESTIONS
  # 1 - What happens to missing values in a histogram...bar...why the difference
diamonds2 %>% 
  filter(is.na(y))

ggplot(data = diamonds2, mapping = aes(x = y)) +
  geom_histogram() # AUTO REMOVES NA VALUES

diamonds2 %>% 
  mutate(
    cut2 = ifelse(cut == 'Fair', NA, cut) 
  ) %>% 
  ggplot(mapping = aes(x = cut2)) +
   geom_bar()

## 2 - na.rm in Mean() and sum()
  # --> just removes the NA values before calculating


#-------------------------------------------------------------------------------
# 7.5 COVARIATION

  # variation = behavior within one variable
  # covariation = behavior between variables

# Covariation is the tendency for the values of two or more variables to vary 
# together in a related way


# ----------------------------
# CATEGORICAL + CONT. VARIABLE
# ...

# explore how price of diamonds varies with quality
ggplot(data = diamonds, mapping = aes(x = price)) +
  geom_freqpoly(mapping = aes(colour = cut), binwidth = 500)
  # ^ default appearance not super helpful bc height is given by count, some groups have more than others

# *HARD TO SEE THE DIFFERENCE IN DISTRIBUTION BC COUNTS DIFFER SO MUCH ... we dont care about the count
ggplot(diamonds) +
  geom_bar(mapping = aes(x = cut))

# to make the comparison easier must swap whats on y axis

  # use DENSITY = which is the count standardised so that the area under each frequency polygon is one
ggplot(data = diamonds, mapping = aes(x = price, y= ..density..)) +
  geom_freqpoly(mapping = aes(color = cut), binwidth = 500)
  # appears that fair diamonds(lowest qual) have the highest avg price!!! kinda hard to inpret tho


# another way to display the distribution of a continuous variable broken down by categorial var is boxplot
  # BOXPLOT =  type of visual shorthand for a distribution of values that is popular among statisticians.
    # A box that stretches from the 25th percentile of the distribution to the 75th percentile, a distance known as the interquartile range (IQR
    # Visual points that display observations that fall more than 1.5 times the IQR from either edge of the box
    # A line (or whisker) that extends from each end of the box and goes to the farthest non-outlier point in the distribution

ggplot(data = diamonds, mapping = aes(x = cut, y = price)) +
  geom_boxplot()
  # supports that better quality diamonds are cheaper .... why!?


# may want top reorder how the variables appear
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
  geom_boxplot()
# to make the trend easier to see we can reorder 'class' based on median of 'hwy'
ggplot(data = mpg) +
  geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = median), y = hwy))
  # if long variable names, works better if you flip
ggplot(data = mpg) +
  geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = median), y = hwy)) +
  coord_flip()

# 7.5.1.1 EXERCISES
  # 1 - use what learned to improve visualization of depart times cancelled v non cancelled flights
flights %>% # OG code .... proper shite
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min / 60
  ) %>% 
  ggplot(mapping = aes(sched_dep_time)) +
  geom_freqpoly(mapping = aes(colour = cancelled), binwidth = 1/4)

# improved
flights %>%
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min / 60
  ) %>% 
  # ggplot() +
  #   geom_freqpoly(
  #     mapping = aes(x = sched_dep_time, y = ..density.., color = cancelled),
  #     binwwidth = 1/4
  #     )
  ggplot() +
    geom_boxplot(mapping = aes(x = cancelled, y = sched_dep_time))

  # 2 - what variable in diamondss is most important for predicting price?
        # How is that variable correlated with cut? 
        # Why does the combination of those two relationships lead to lower quality diamonds being more expensive?

# CARAT
ggplot(diamonds) +
  geom_point(mapping = aes(x = carat, y = price))
# alot of data points ... lets use a boxplot and binning w/ carat
ggplot(data = diamonds, mapping = aes(x = carat, y = price)) +
  geom_boxplot(mapping = aes(group = cut_width(carat, 0.1)), orientation = "x")

# COLOR - weak negative
diamonds %>% 
  mutate(color = fct_rev(color)) %>% #reverse the order of the color levels so they will be in increasing order of quality on the x-axis
  ggplot(aes(x = color, y = price)) +
  geom_boxplot()
# CLARITY - weak negative
ggplot(data = diamonds) +
  geom_boxplot(mapping = aes(x = clarity, y = price))

# CARAT IS THE WINNER!! ...

# cut vs carat
ggplot(diamonds, aes(x = cut, y = carat)) +
  geom_boxplot()
  # fair (lowest qual CUT) diamonds have the highest CARAT(size)
    # larger diamonds can be profitably solds with lower quality cut!

  # 3 - 
install.packages("ggstance")
library(ggstance)
diamonds %>% 
  ggplot() +
    geom_boxploth(mapping = aes(y = price, x = carat, group = cut_width(carat, 0.1)), orientation = "x")
# i guess the order you list the vars changes which is displayed vert vs horiz

  # 4 - lvplot
install.packages("lvplot")
library(lvplot)
  # NOTE: boxplots were not developed in days of BIG DATA
    # lv (letter-value) plots good for big data bc
        # 1 - large datsets can give precise estimates of quantiles
        # 2- larger sets have more outliers
      #
ggplot(data = diamonds, aes(x = cut, y = price)) +
  geom_lv() 

  # 5 - geom_violin() VS facetted geom_histogram() VS colored geom_freqpoly()
ggplot(data = diamonds) +
  geom_violin(aes(x = cut, y = price)) +
  coord_flip()

ggplot(data = diamonds) + 
  geom_histogram(mapping = aes(x = price), binwidth = 1000) +
  facet_wrap(~cut, ncol = 1, scales = "free_y")
  
ggplot(data = diamonds) + 
  geom_freqpoly(mapping = aes(x = price, y= ..density.., color = cut), binwidth = 500)

  # 6 - 
  # If you have a small dataset, it's sometimes useful to use geom_jitter() to see 
  # the relationship between a continuous and categorical variable. 
  # The ggbeeswarm package provides a number of methods similar to geom_jitter(). 
  # List them and briefly describe what each one does.




# ----------------------------
# Two categorical variables
# ...

# To visualise the covariation between categorical variables, you'll need to count
# the number of observations for each combination. One way to do that is to rely on the built-in geom_count()


ggplot(data = diamonds) + 
  geom_count(mapping = aes(x = cut, y = color))

#compute w/ dplyr
diamonds %>% 
  count(color,cut)

#visualise w/ geom_tile()
diamonds %>% 
  count(color, cut) %>% 
  ggplot(mapping = aes(x = color, y = cut)) +
           geom_tile(mapping = aes(fill = n))

# 7.5.2.1 EXERCISES
  # 1 - how to rescale the dataset above to clearly show distr of cut within color?

diamonds %>% 
  count(color, cut) %>% 
  group_by(color) %>% 
  mutate(
    prop = n / sum(n)
  ) %>% 
  ggplot(mapping = aes(x = color, y = cut)) +
    geom_tile(mapping = aes(fill = prop))

  # 2 - Use geom_tile() together with dplyr to explore how average flight delays
  # vary by destination and month of year. What makes the plot difficult to read?
  # How could you improve it?

installed.packages("nycflights13")
library(nycflights13)
glimpse(flights)
  # arr_delay, dest, month
flights %>%
  group_by(month, dest) %>% 
  mutate(
    avg_delay = mean(arr_delay, na.rm = TRUE)
  ) %>% 
  ggplot(mapping = aes(x = month, y = dest)) +
    geom_tile(mapping = aes(fill = avg_delay))

# ... improve
flights %>%
  group_by(month, dest) %>%                                 # This gives us (month, dest) pairs
  summarise(dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
  group_by(dest) %>%                                        # group all (month, dest) pairs by dest ..
  filter(n() == 12) %>%                                     # and only select those that have one entry per month 
  ungroup() %>%
  mutate(dest = reorder(dest, dep_delay)) %>%
  ggplot(aes(x = factor(month), y = dest, fill = dep_delay)) +
  geom_tile() +
  labs(x = "Month", y = "Destination", fill = "Departure Delay")
#> `summarise()` regrouping output by 'month' (override with `.groups` argument)

  #3 - why use aes(x = color, y = cut) rather than aes(x = cut, y = color)
# --> categorical var with shorter names on the bottom


# ------------------------------
# 7.5.3 Two continuous variables

# scatterplots are good method but as amount of data increases less helpful
## one way to combat this is to add transparency
ggplot(data = diamonds) +
  geom_point(mapping = aes(x = carat , y = price), alpha = 1/50)
  # transparency still challenging for very large sets

# we can use bin -- geom_bin2d() & geom_hex
  # divide the coordinate plane into 2d bins and then use a fill color to display how many points fall into each bin
ggplot(data = smaller) +
  geom_bin2d(mapping = aes(x = carat, y = price))

#install.packages("hexbin")
ggplot(data = smaller) +
  geom_hex(mapping = aes(x = carat, y = price))


# Another option is to bin one continuous variable so it acts like a categorical variable.
ggplot(data = smaller, mapping = aes(x = carat, y= price)) +
  geom_boxplot(mapping = aes(group = cut_width(carat, 0.1)))
  ## ... difficult to tell that each boxplot summarises a different number of points
  ## varwidth = TRUE
ggplot(data = smaller, mapping = aes(x = carat, y= price)) +
  geom_boxplot(mapping = aes(group = cut_width(carat, 0.1)), varwidth = TRUE)

## OR ... display the same number of points in each bin
ggplot(data = smaller, mapping = aes(x = carat, y = price)) +
  geom_boxplot(mapping = aes(group = cut_number(carat, 20)))


## 7.5.3.1 Exercises
  ## 1 - Instead of summarising the conditional distribution with a boxplot, 
# you could use a frequency polygon. What do you need to consider when using 
# cut_width() vs cut_number()? How does that impact a visualisation of the 2d 
# distribution of carat and price?

# -> When using cut_width(), we need to choose the width, and the number of bins
# will be calculated automatically. When using cut_number(), we need to specify
# the number of bins, and the widths will be calculated automatically.

# <- want to choose the bin widths and number to be large enough to aggregate 
# observations to remove noise, but not so large as to remove all the signal.
ggplot(data = diamonds, 
       mapping = aes(color = cut_number(carat, 5), x = price)) +
  geom_freqpoly() +
  labs(x = "Price", y = "Count", color = "Carat")

  ## 2 - disttribution of carat partitioned by price
ggplot(diamonds, aes(x = cut_number(price, 10), y = carat)) +
  geom_boxplot() +
  coord_flip() +
  xlab("Price")

  ## 3 - price distr as size increases
# <- `more variable

  ## 4 - visualize: cut, carat, price
ggplot(data = diamonds, mapping = aes(x = carat, y = price)) +
  geom_hex() +
  facet_wrap(~ cut, 5)

ggplot(diamonds, aes(x = cut_number(carat, 5), y = price, colour = cut)) +
  geom_boxplot()

  ## 5 - 2D plots reveal outliers not visible in 1D plots... ex:
  ## why is a scatter better than a bin plot in this case
ggplot(data = diamonds) +
  geom_point(aes(x = x, y = y)) +
  coord_cartesian(xlim = c(4,11), ylim = c(4, 11))


#-----------------------------------------------------------------------------
# 7.6 PATTERNS and MODELS

# if you spot a pattern... ask yourself:
  # Could this pattern be due to coincidence (i.e. random chance)?
  # How can you describe the relationship implied by the pattern?
  # How strong is the relationship implied by the pattern?
  # What other variables might affect the relationship?
  # Does the relationship change if you look at individual subgroups of the data?

# Old Faitful Eruption lengths vs wait time between
ggplot(data = faithful) +
  geom_point(mapping = aes(x = eruptions, y = waiting))
# look at those clusters!!!

# NOTE: Patterns provide one of the most useful tools for data scientists because they reveal covariation

# If you think of variation as a phenomenon that creates uncertainty, covariation
# is a phenomenon that reduces it. If two variables covary, you can use the values
# of one variable to make better predictions about the values of the second. If 
# the covariation is due to a causal relationship (a special case), then you can 
# use the value of one variable to control the value of the second

# Models are a tool for extracting patterns out of data

# fits a model that predicts price from carat and then computes the residuals
library(modelr)

mod <- lm(log(price) ~ log(carat), data = diamonds)

diamonds2 <- diamonds %>% 
  add_residuals(mod) %>% 
  mutate(resid = exp(resid))

ggplot(data = diamonds2) +
  geom_point(mapping = aes(x = carat, y = resid))

# The residuals give us a view of the price of the diamond, once the effect of carat has been removed.


# -----------------------------------------------------------------------------
# 7.7 ggplot2 CALLS

#ggplot2 code so far has been very explicit
ggplot(data = faithful, mapping = aes(x = eruptions)) + 
  geom_freqpoly(binwidth = 0.25)
# --> SIMPLER MOVING FORWARD
ggplot(faithful, aes(eruptions)) + 
  geom_freqpoly(binwidth = 0.25)

# end of a pipeline of data transformation into a plot
diamonds %>% 
  count(cut, clarity) %>% 
  ggplot(aes(clarity, cut, fill = n)) + 
  geom_tile()