# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ------------------------- SBC LTER Data Workshop ------------------------
# ---------- Part 1. Downloading and visually exploring datasets ----------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Learning objectives
# By the end of this section of the workshop, you will be able to:
# 1. use R packages to download LTER data to your own computer
# 2. visualize data as a first step to exploring LTER datasets

# In this section of the workshop, we'll work with the seasonal timeseries data.
# The full citation is: Reed, D. and R. Miller. 2024. SBC LTER: Reef: Seasonal 
# Kelp Forest Community Dynamics: biomass of kelp forest species, ongoing since
# 2008 ver 1. Environmental Data Initiative. 
# https://doi.org/10.6073/pasta/2e3b1cf934ec4f4a9293ba117aad37f5. 

# At the end of the workshop, we'll have a visualization of purple and red
# urchin biomass through time at Naples Reef from 2014-2024.

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ------------------------------- 1. set up -------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# The `tidyverse` package is a general use package for cleaning and visualizing
# data. See https://www.tidyverse.org/ for details on what packages are in the
# tidyverse!
library(tidyverse) 

# The `janitor` package contains a bunch of different functions to help you 
# clean up your data. See the website for more details:
# https://cran.r-project.org/web/packages/janitor/vignettes/janitor.html
library(janitor)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ------------------------ 2. geting data from EDI ------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# In this section, we'll download the data from EDI and save it as an object
# called `dt1`. This is following the code that is provided on every dataset
# on EDI. You can find it on the dataset page under Code Generation.

# getting the dataset URL and saving it as an object called `inUrl1`
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-sbc/182/1/8bd628cbfbb5bef0d9b1a7ead8aab832" 

# creating an object called `infile1` that is a temporary file
infile1 <- tempfile()

# download the dataset specified in `inUrl1` and store it in `infile1`
try(download.file(inUrl1,infile1,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")

# read in the dataset using the function `read.csv()`
# store the dataset as an object called `dt1`
dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "YEAR",     
                 "MONTH",     
                 "DATE",     
                 "SITE",     
                 "TRANSECT",     
                 "VIS",     
                 "SP_CODE",     
                 "PERCENT_COVER",     
                 "DENSITY",     
                 "WM_GM2",     
                 "DRY_GM2",     
                 "SFDM",     
                 "AFDM",     
                 "SCIENTIFIC_NAME",     
                 "COMMON_NAME",     
                 "TAXON_KINGDOM",     
                 "TAXON_CLASS",     
                 "TAXON_PHYLUM",     
                 "TAXON_ORDER",     
                 "TAXON_FAMILY",     
                 "TAXON_GENUS",     
                 "GROUP",     
                 "MOBILITY",     
                 "GROWTH_MORPH"    ), check.names=TRUE)

# unlink the temporary file
unlink(infile1)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ------------------ 3. getting a first look at the data ------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# The first thing to do when working with data is to look at it. This is an
# underappreciated but crucial step! By looking at the data, you'll have a 
# sense of what the columns and rows are so you can then build your intuition
# for how to work with it down the line.

# One way to quickly look at the data is to use the `glimpse()` function, which
# comes from the `dplyr` package within the `tidyverse`. 

glimpse(dt1)

# From this, you know there are 84,316 rows in the dataset, and 24 columns.
# This function also gives you information about what is in the dataset; for 
# example, there is a column called `YEAR` and it has integers (<int>) in it,
# and you can see that there are years in the column (e.g. 2008). 

# You can also look directly at the object by typing `View(dt1)` in your 
# console, or clicking on `dt1` in the Environment tab.

# Given that we're interested in the *biomass* of *red and purple urchins* at
# *Naples* through *time* from *2014-2024*, which columns would be most 
# relevant to us?

# YEAR, DATE, SITE, DRY_GM2, SCIENTIFIC_NAME (could be others too!)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# -------------------------- 4. cleaning the data -------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# In this section, we'll filter the dataset and clean it up a bit so that 
# it's easier to use. Remember that we're only interested in purple and red
# urchins at Naples, but this dataset includes biomass for all species covered
# in the surveys at all the sites. We'll need to filter the dataset to include
# only the species and site of interest, but we'll also do a couple of cleaning
# steps to make the dataset easier to use.

# we'll create a new object called `urchins`, and start first with `dt1`
urchins <- dt1 |> 
  # using this function from `janitor` to make the column names lower case
  clean_names() |>
  # select columns of interest
  select(year, date, site, dry_gm2, common_name) |>
  # make sure the date column is read in as a date
  mutate(date = as.Date(date)) |>
  # make site and common name lower case
  mutate_at(c("site", "common_name"), str_to_lower) |>
  # filter to only include observations between 2014 and 2024
  filter(between(year, 2014, 2024)) |>
  # filter to only include red and purple urchins
  filter(common_name %in% c("red urchin", "purple urchin")) |>
  # filter to only include Naples
  filter(site == "napl")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ------------------------ 5. visualizing the data ------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Now the most exciting part: visualizing the data! Remember that we want to
# create a timeseries plot for urchin biomass. That means we'll have time (in
# this case, this is the column `date`) on the x-axis, and biomass (column name
# `dry_gm2`) on the y-axis.

# To visualize the data, we'll use the function `ggplot()` and its associated
# functions from the `ggplot2` package in the `tidyverse`. See 
# https://ggplot2-book.org/index.html for a user manual to `ggplot2`. But don't
# stop there! Lots of people think deeply about data visualization. I (An) love
# thinking about visualizing data!! So if you ever want to chat about making
# visualizations, I am super happy to talk!

# Each ggplot visualization has the same 3 basic parts:
# 1. the "global call": this tells R that you want to use ggplot by calling the
# function `ggplot()`
# 2. the "aesthetics": this outlines what you want the axes, colors, etc. to be
# by calling the function `aes()` within the global `ggplot()` call
# 3. the "geometries": these define the shapes (e.g. points, lines), colors,
# etc. that you want to draw on your plot. These functions all start with 
# `geom_`.

# To make your visualization more beautiful, you can adjust things like:
# a. colors (using `scale_color_manual()` or other functions),
# b. the overall "look" (using `theme_` functions), and/or
# c. labels (using `labs()`)
# but again, there is much more that these options!

# 1. global call
ggplot(data = urchins, # specifying data
       
       # 2. aesthetics
       aes(x = date, # x-axis
           y = dry_gm2, # y-axis
           color = common_name, # what we want the colors to be
           shape = common_name)) + 
  
  # 3. geometries
  # making points
  geom_point(size = 2) +
  # making lines to connect the points
  geom_line() + 
  
  # extra stuff!
  # a. changing the colors
  scale_color_manual(values = c("purple urchin" = "darkorchid4",
                                "red urchin" = "firebrick2")) +
  # b. changing the theme
  theme_minimal() +
  # c. changing the labels (note that the x, y, color, and shape arguments
  # are the same as the aes() call above)
  labs(x = "Date",
       y = "Dry biomass (g/m2)",
       color = "Species",
       shape = "Species",
       title = "Timeseries of purple and red urchin biomass, 2014-2024")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This concludes part 1 of the workshop! If you have any questions or want
# to nerd out about data viz, please feel free to contact me (An Bui,
# an_bui@ucsb.edu).
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
