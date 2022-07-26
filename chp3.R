# https://r4ds.had.co.nz/data-visualisation.html
# Chp 3 - DATA VISUALISZATION
install.packages("tidyverse")
library(tidyverse)


#3.2 FIRST STEPS

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy))

# GRAPHING TEMPLATE
#ggplot(data = <DATA>) + 
  #<GEOM_FUNCTION>(mapping = aes(<MAPPINGS>))

#Exercises
#2  
  # Rows = length(mpg)
  # Cols = ddply(mpg)
#4
#ggplot(data = mpg) + 
 # geom_point(mapping = aes(x = hwy, y = cyl))



# ------------------------------------------------------------------------------
#3.3 AESTHETIC MAPPING
# --> adding anaesthetic like addinga third variable to the data

ggplot(data = mpg) + 
  geom_point(mapping = aes(x= displ, y =hwy,  color = class))

#messing around with different aesthetic options -- CALLED SCALING
# Left
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, alpha = class))

# Right
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, shape = class))

#manually set an aesthetic ... ex: blue dots
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy), color = "blue")

### 3.3.1 Exercises
  #2 - mpg ; glimpse(mpg)
  #3 - 
# ggplot(data = mpg) +
#     geom_point(mapping = aes(x = displ, y = hwy, shape = displ))  #Error cont. var to shape
ggplot(data = mpg) + 
    geom_point(mapping = aes(x = displ, y = hwy, color = displ))  #shades of blue
  #4 - 
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = displ, size = displ))  #overwrites
 #5 - stroke
ggplot(mtcars, aes(wt, mpg)) +
  geom_point(shape = 21, colour = "black", fill = "white", size = 5, stroke = 5)
  #6 - 
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = displ < 5))  # shows true or false



# ------------------------------------------------------------------------------
# 3.5 FACETS
  # splitting into facets or subplots showing a data subset

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class,nrow = 2)  # breaks the plots into subplots on class, 2 rows

# facet on a combo of two variables
ggplot(data = mpg) +
    geom_point(mapping = aes(x = displ, y = hwy)) + 
    #facet_grid(drv ~ cyl)
    facet_grid(. ~ cyl)

### 3.5.1 EXECRCISES
  # 1 - facet on a cont variable??
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  facet_grid(~. cty)
  # 2 - empty cells 
ggplot(data = mpg) + 
    geom_point(mapping = aes(x = hwy, y = cty)) +
    facet_grid(drv ~ cyl)
#vs
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = drv, y = cyl))
  # 3 - what this do? ... --> . ignores that dimension 
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv ~ .)
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(. ~ cyl)
  # 4 -   



# ------------------------------------------------------------------------------
# 3.6 GEOMETRIC OBJECTS
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy))  # control with GEOM

ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv))


#grouping aesthetics
ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy, group = drv))

ggplot(data = mpg) + 
  geom_smooth(
    mapping = aes(x = displ, y = hwy, color = drv),
    show.legend = FALSE
              )

#multiple geoms in the same plot
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  geom_smooth(mapping = aes( x = displ, y = hwy))
## we are duplicating code above ... lets simplify -- PASS mapping to ggplot
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_smooth() +
  geom_point()

#if you place mappings in a geom function, ggplot2 will treat them as local mappings
# DISPLAY DIFF ASE IN DIFF LAYERS
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(color = class)) +
  geom_smooth()

# specify different data for each layer 
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(color = class)) +
  geom_smooth(data = filter(mpg, class == "subcompact"), se = FALSE) #pass diff data here

# 3.6 EXERCISES
  ## 1 - line chart, boxlpot, histo, area chart?
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_line()
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_boxplot() 
ggplot(data = mpg, mapping = aes(x = hwy)) + 
  geom_histogram()
#geom_area()

  ## 2 - what this do?
ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = drv)) + 
  geom_point() + 
  geom_smooth(se = FALSE)

## 3 - what does show.legend do
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(color = class), show.legend = TRUE)

## 4 - se ??
# --> se shows the confidence intervals
ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = drv)) + 
  geom_smooth(se = TRUE)

## 5 - 
## 6 - create 

#b 
# ggplot(data = mpg, mapping = aes(x = displ, y = hwy, group = drv, color = drv)) +
#   geom_smooth(mapping = aes(group = drv, color = drv), se = TRUE) +
#   geom_point()
#c
ggplot(data = mpg, 
       mapping = aes(x = displ, y = hwy, group = drv, color = drv)) +
        geom_smooth(se = FALSE) +
        geom_point(se = FALSE)



# ------------------------------------------------------------------------------
# 3.7 STATISTICAL TRANSFORMATIONS

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut))

# same as
ggplot(data = diamonds) +
  stat_count(mapping = aes(x = cut))

# can use the stat functions to override

# display a bar chart of proportion
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, y = stat(prop), group = 1))

# display unique stats of y for each group x
ggplot(data = diamonds) +
  stat_summary(
    mapping = aes(x = cut, y = depth),
    fun.min = min,
    fun.max = max,
    fun = median
  )

# 3.7 EXERCISES 

## 1 - rewrite above using default geom of stat_summ = 
ggplot(data = diamonds) + 
  geom_pointrange(
    mapping = aes(x = cut, y = depth),
    stat = "summary",
    fun.min = min,
    fun.max = max,
    fun = median
  )
## 2 - geom_bar VS geom_col
  # geom_col uses deault geom stat_identity and expects x and y(height of bar)
  # geom_bar uses default geom stat_count and only needs x
ggplot(data = diamonds) +
  geom_col(
    mapping = aes(x = cut, y = depth)
  )
ggplot(data = diamonds) +
  geom_bar(
    mapping = aes(x = cut)
  )
## 3 - 

# Complementary geoms and stats
# geom            	stat
# geom_bar()	          stat_count()
# geom_bin2d()	        stat_bin_2d()
# geom_boxplot()	      stat_boxplot()
# geom_contour_filled()	stat_contour_filled()
# geom_contour()	      stat_contour()
# geom_count()	        stat_sum()
# geom_density_2d()	    stat_density_2d()
# geom_density()	      stat_density()
# geom_dotplot()	      stat_bindot()
# geom_function()	      stat_function()
# geom_sf()	            stat_sf()
# geom_sf()	            stat_sf()
# geom_smooth()	        stat_smooth()
# geom_violin()	        stat_ydensity()
# geom_hex()	          stat_bin_hex()
# geom_qq_line()	      stat_qq_line()
# geom_qq()	            stat_qq()
# geom_quantile()	      stat_quantile()

#ggplot2 geom layers and their default stats.
# geom	      default stat	  shared docs
# geom_abline()	stat_identity()	
# geom_area()	stat_identity()	
# geom_bar()	stat_count()	x
# geom_bin2d()	stat_bin_2d()	x
# geom_blank()	None	
# geom_boxplot()	stat_boxplot()	x
# geom_col()	stat_identity()	
# geom_count()	stat_sum()	x
# geom_countour_filled()	stat_countour_filled()	x
# geom_countour()	stat_countour()	x
# geom_crossbar()	stat_identity()	
# geom_curve()	stat_identity()	
# geom_density_2d_filled()	stat_density_2d_filled()	x
# geom_density_2d()	stat_density_2d()	x
# geom_density()	stat_density()	x
# geom_dotplot()	stat_bindot()	x
# geom_errorbar()	stat_identity()	
# geom_errorbarh()	stat_identity()	
# geom_freqpoly()	stat_bin()	x
# geom_function()	stat_function()	x
# geom_hex()	stat_bin_hex()	x
# geom_histogram()	stat_bin()	x
# geom_hline()	stat_identity()	
# geom_jitter()	stat_identity()	
# geom_label()	stat_identity()	
# geom_line()	stat_identity()	
# geom_linerange()	stat_identity()	
# geom_map()	stat_identity()	
# geom_path()	stat_identity()	
# geom_point()	stat_identity()	
# geom_pointrange()	stat_identity()	
# geom_polygon()	stat_identity()	
# geom_qq_line()	stat_qq_line()	x
# geom_qq()	stat_qq()	x
# geom_quantile()	stat_quantile()	x
# geom_raster()	stat_identity()	
# geom_rect()	stat_identity()	
# geom_ribbon()	stat_identity()	
# geom_rug()	stat_identity()	
# geom_segment()	stat_identity()	
# geom_sf_label()	stat_sf_coordinates()	x
# geom_sf_text()	stat_sf_coordinates()	x
# geom_sf()	stat_sf()	x
# geom_smooth()	stat_smooth()	x
# geom_spoke()	stat_identity()	
# geom_step()	stat_identity()	
# geom_text()	stat_identity()	
# geom_tile()	stat_identity()	
# geom_violin()	stat_ydensity()	x
# geom_vline()	stat_identity()	

# ggplot2 stat layers and their default geoms.
# stat	        default geom	    shared docs
# stat_bin_2d()	geom_tile()	
# stat_bin_hex()	geom_hex()	x
# stat_bin()	geom_bar()	x
# stat_boxplot()	geom_boxplot()	x
# stat_count()	geom_bar()	x
# stat_countour_filled()	geom_contour_filled()	x
# stat_countour()	geom_contour()	x
# stat_density_2d_filled()	geom_density_2d()	x
# stat_density_2d()	geom_density_2d()	x
# stat_density()	geom_area()	
# stat_ecdf()	geom_step()	
# stat_ellipse()	geom_path()	
# stat_function()	geom_function()	x
# stat_function()	geom_path()	
# stat_identity()	geom_point()	
# stat_qq_line()	geom_path()	
# stat_qq()	geom_point()	
# stat_quantile()	geom_quantile()	x
# stat_sf_coordinates()	geom_point()	
# stat_sf()	geom_rect()	
# stat_smooth()	geom_smooth()	x
# stat_sum()	geom_point()	
# stat_summary_2d()	geom_tile()	
# stat_summary_bin()	geom_pointrange()	
# stat_summary_hex()	geom_hex()	
# stat_summary()	geom_pointrange()	
# stat_unique()	geom_point()	
# stat_ydensity()	geom_violin()	x


## 4 - stat_smooth  ( --> geom_smooth)
#   Computed variables
stat_smooth() #provides the following variables, some of which depend on the orientation:
#   
#   y or x
# predicted value
# 
# ymin or xmin
# lower pointwise confidence interval around the mean
# 
# ymax or xmax
# upper pointwise confidence interval around the mean
# 
# se
# standard error

## 5 - setting group=1 ... what is the prob?
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, y = after_stat(prop)))
#all the same height
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = color, y = after_stat(prop)))

# fixed --> group=1
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, y = after_stat(prop), group = 1))



# ------------------------------------------------------------------------------
# 3.8 -- POSITION ADJUSTMENTS

ggplot(data = diamonds) +
  geom_bar(
    mapping = aes(x = cut, color = cut)
  )
ggplot(data = diamonds) +
  geom_bar(
    mapping = aes(x = cut, fill = cut)
  )
# adding a diff variable to fill aes
ggplot(data = diamonds) +
  geom_bar(
    mapping = aes(x = cut, fill = clarity)
  )


# The stacking is performed automatically by the position adjustment specified 
# by the position argument. If you don't want a stacked bar chart, you can use 
# one of three other options: "identity", "dodge" or "fill".


# position = "identity" will place each object exactly where it falls in 
# the context of the graph.
ggplot(data = diamonds) +
  geom_bar(
    mapping = aes(x = cut, fill = clarity),
    alpha = 2/5,  #transparency
    position = "identity"
  )
ggplot(data = diamonds, mapping = aes(x = cut, color = clarity)) +
  geom_bar(
    fill = NA,
    position = "identity"
  )

# position = "fill" works like stacking, but makes each set of stacked bars the
# same height. This makes it easier to compare proportions across groups.
ggplot(data = diamonds) +
  geom_bar(
    mapping  = aes( x = cut, fill = clarity),
    position = "fill"
  )


# position = "dodge" places overlapping objects directly beside one another. 
# This makes it easier to compare individual values.
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, fill = clarity),
           position = "dodge"
  )

# back to scatterplots, to avoid overfitting use position = "jitter"
ggplot(data = mpg) + 
  geom_point(
    mapping = aes(x = displ, y = hwy),
    position = "jitter"  # adds small amt of random noise each point
  )
ggplot(data = mpg) +
  geom_jitter(
    mapping = aes(x = displ, y = hwy)
  )
# Adding randomness seems like a strange way to improve your plot, but while it
# makes your graph less accurate at small scales, it makes your graph more 
# revealing at large scales


# VS old overplot on grid
ggplot(data = mpg) + 
  geom_point(
    mapping = aes(x = displ, y = hwy),
  )


## 3.8 EXERCISES

### 1 - whats wrong how to improve?
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + 
  geom_point()
#   overplotting - show where more observations lie
  ggplot(data = mpg)+
    geom_point(
      mapping = aes(x = cty, y = hwy),
      position = "jitter"
    )
  
### 2 - params of geom_jitter to control the jitter
  # width (defaults to 40%)
  # height (defaults to 40%)
ggplot(data = mpg) +
  geom_jitter(
    mapping = aes(x = cty, y = hwy),
    position = position_jitter()
  )

# change so there is no horizontal jitter
ggplot(data = mpg) +
  geom_jitter(
    mapping = aes(x = cty, y = hwy),
    width = 0
    # height = 0 NO VERT JITTER
  )

### 3 - geom_jitter VS geom_count
ggplot(data = mpg) +
  geom_count(     #geom_count thiccifies the areas with lots of observations
    mapping = aes(x = cty, y = hwy)
)
  #geom_jitter WITH COLOR
ggplot(data = mpg, mapping = aes(x = cty, y = hwy, color = class)) +
  geom_jitter()

### 4 - default position adjustment for geom_boxplot
    # defaults to dodge2 <- position_dodge2
ggplot(data = mpg) +
  geom_boxplot(
    mapping = aes(x = drv, y = hwy, color = class)
  )


# ------------------------------------------------------------------------------
# 3.9 COORDINATE SYSTEMS

## coord systems most complicated part of ggplot2

## COORD_FLIP swithces x and y axes
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
  geom_boxplot()

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
  geom_boxplot() +
  coord_flip()

# COORD_QUICKMAP deals with spatial data 
nz <- map_data("nz")

ggplot(nz, aes(long,lat, group = group)) +
  geom_polygon(fill = "green", color = "red")

ggplot(nz, aes(long,lat, group = group)) +
  geom_polygon(fill = "green", color = "red") +
  coord_quickmap()

# coord_polar() uses polar coordinates. Polar coordinates reveal an 
# interesting connection between a bar chart and a Coxcomb chart.
bar <- ggplot(data = diamonds) +
  geom_bar(
    mapping = aes(x = cut, fill = cut),
    show.legend = FALSE,
    width = 1
  ) +
  theme(aspect.ratio = 1) + 
  labs(x = NULL, y = NULL)

bar + coord_flip()
bar + coord_polar()


## 3.9 EXERCISES
### 1 - Turn a stacked bar chart into a pie chart
ggplot(data = diamonds) +
  geom_bar(
    mapping = aes(x = cut, fill = clarity)
  ) + 
  coord_polar()

### 2 - LABS() ???
# gives charts labels
ggplot(data = diamonds) +
  geom_bar(
    mapping = aes(x = cut, fill = cut),
    show.legend = FALSE,
    width = 1
  ) +
  theme(aspect.ratio = 1) + 
  labs(x = "this is x",
       y = "this is y",
       title = "Hello World",
       subtile = "this is a subtile",
       caption = "this is a caption") +
  coord_flip()

### 3 - COORD_QUICKMAP vs COORD_MAP ???
      # coord_map uses more accurate mercuator proj 3D to 2D

### 4 - 
      # positive coor between cty and hwy
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_jitter() +
  geom_abline() +
  coord_fixed()   # ENSURES LINE IS AT 45 deg ANGLE



# ------------------------------------------------------------------------------
# 3.10 LAYERED GRAMMAR OF GRAPHICS

#GGPLOT2 TEMPLATE


ggplot(data = <DATA>) + 
  <GEOM_FUNCTION>(
    mapping = aes(<MAPPINGS>),
    stat = <STAT>, 
    position = <POSITION>
  ) +
  <COORDINATE_FUNCTION> +
  <FACET_FUNCTION>

