library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

#### READ IN DATA ####
# working off NEveg outputs here; our template will be different
# these have cover, density, and height in the same sheet
# and an extra row at the top for which marsh zone each plant "belongs to"
# anything that does NOT have 'Canopy Ht' or 'Density' in the column name 
# once identifying info stops is percent cover
grb_in <- here::here("data", "GRB Data Packet 6-8-21.xlsx")
wqb_in <- here::here("data", "WQB Data Packet 2011-2020.xlsx")

# work with GRB first
dat <- read_xlsx(grb_in, sheet = "Veg",
                 skip = 1)

# bust up into data types, like we'll get in the template
id_cols <- dat %>% 
    select("Reserve Code":"Marsh Zone")
density <- dat %>% 
    select(contains("Density"))
height <- dat %>% 
    select(contains("Canopy Ht"))
cover <- dat %>% 
    select(-("Reserve Code":"Marsh Zone"),
           -contains("Density"),
           -contains("Canopy Ht"))

# make sure the numbers add up
ncol(dat) == ncol(id_cols) + ncol(density) + ncol(height) + ncol(cover)

# for now, just work with cover
cover <- cover %>% 
    select(-contains("Unknown"),
           -contains("Notes")) %>% 
    mutate_all(as.numeric) %>% 
    cbind(id_cols, .)

# pivot to long
cover_long <- pivot_longer(cover,
                           cols = (ncol(id_cols) + 1):ncol(cover),
                           names_to = "Species",
                           values_to = "pct_cover") %>% 
    janitor::clean_names()

# cringe to do this, but turn NAs into 0s, because if all data was entered properly,
# anything that wasn't recorded should be 0.
# will be important to check individual data points in some graphs
# to make sure there aren't any mistaken 0s.

cover_long <- cover_long %>% 
    mutate(pct_cover = case_when(is.na(pct_cover) ~ 0,
                                 TRUE ~ pct_cover))


#### LOOK at the data ####

# qaqc - make sure 'TOTAL W/O Water' adds up to 100

ggplot(filter(cover_long, species == "TOTAL W/O Water")) +
    geom_point(aes(x = date, y = pct_cover))
# mostly. hm.

# how to see this in different ways, because eventually
# something like the below should get included in a shiny app
# so people can see it for whichever species they want

ggplot(filter(cover_long, species == "TOTAL W/O Water")) +
    geom_point(aes(x = date, y = pct_cover)) +
    facet_grid(site ~ transect_number)

ggplot(filter(cover_long, species == "TOTAL W/O Water")) +
    geom_point(aes(x = date, y = pct_cover, col = as.factor(transect_number))) +
    facet_wrap(site~., ncol = 1) +
    labs(title = "TOTAL W/O Water",  # make same as species used above
         x = "year",
         y = "percent cover",
         color = "Transect")

## start making it programmatic
species_of_interest <- "Spartina patens"
# species_of_interest <- "Spartina alterniflora"
# species_of_interest <- "Spartina patens"

ggplot(filter(cover_long, species == species_of_interest)) +
    geom_point(aes(x = date, y = pct_cover, col = as.factor(transect_number))) +
    facet_wrap(site~., ncol = 1) +
    labs(title = species_of_interest,  
         x = "year",
         y = "percent cover",
         color = "Transect")

cover_long %>% 
    filter(species == species_of_interest) %>% 
    group_by(site, date) %>% 
    summarize(mean = mean(pct_cover, na.rm = TRUE),
              sd = sd(pct_cover, na.rm = TRUE)) %>% 
    ggplot() +
    geom_point(aes(x = date, y = mean)) + 
    facet_wrap(site~., ncol = 1) +
    labs(title = species_of_interest,  
         x = "year",
         y = "mean percent cover")
