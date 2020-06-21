# Begin a log file.
sink(file = "ec50_emp_proj_1_p2.txt", split = TRUE)

# Install packages (if necessary) and load required libraries
if (!require(tidyverse)) install.packages("tidyverse"); library(tidyverse)
if (!require(haven)) install.packages("haven"); library(haven)
if (!require(ggplot2)) install.packages("ggplot2"); library(ggplot2)
if (!require(statar)) install.packages("statar"); library(statar)

# Load in cleaned atlas data 
atlas <- read_dta("atlas.dta")
# atlas <- na.omit(atlas)


# Narrowing down by county 
ga_rows = atlas[atlas$state == 13, ]
fulton_county_rows = ga_rows[ga_rows$county == 121, ]

# black share mean and standard deviation in Fulton County
mean(fulton_county_rows$share_black2010, na.rm = TRUE)
sd(fulton_county_rows$share_black2010, na.rm = TRUE)

# create black share rank to each census in Fulton county 
fulton_county_rows$share_black2010_rank = (rank(fulton_county_rows$share_black2010))/length(rank(fulton_county_rows$share_black2010)) * 100 
fulton_county_rows$share_black2000_rank = (rank(fulton_county_rows$share_black2000))/length(rank(fulton_county_rows$share_black2000)) * 100 

# create rental prices rank 
fulton_county_rows$rent_twobed2015_rank = (rank(fulton_county_rows$rent_twobed2015))/length(rank(fulton_county_rows$rent_twobed2015)) * 100 

# correlation percent black with rental prices, then share black and blac
cor(fulton_county_rows$share_black2010_rank, fulton_county_rows$kfr_black_pooled_p25, use = "complete.obs")
cor(fulton_county_rows$share_black2000_rank, fulton_county_rows$kfr_black_pooled_p25, use = "complete.obs")



## regression of black_share rank with rental prices 
# Scatter plot first
g <- ggplot(fulton_county_rows, aes(x = share_black2010_rank , y = rent_twobed2015_rank)) 
g + geom_point(shape=1, alpha=1/2) 

# Add Bins
g <- ggplot(fulton_county_rows, aes(x = share_black2010_rank , y = rent_twobed2015_rank)) 
g + geom_point(shape=1, alpha=1/2) + geom_vline(xintercept = c(5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95))

# Line
g <- ggplot(county_rows, aes(x = share_black2010_rank , y = rent_twobed2015_rank)) 
g + stat_binmean(n = 20) + stat_smooth(method = "lm", se = FALSE) + xlim(0,100) + ylim(0,100) + geom_point(shape=1, alpha=1/8) + geom_vline(xintercept = c(5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95))

# correlation 
cor(fulton_county_rows$share_black2010_rank, fulton_county_rows$rent_twobed2015_rank, use = "complete.obs")




## regression of rental prices with upward mobility 
g <- ggplot(fulton_county_rows, aes(x = rent_twobed2015_rank , y = kfr_black_pooled_p25)) 
g + geom_point(shape=1, alpha=1/2) 

# Add Bins
g <- ggplot(fulton_county_rows, aes(x = rent_twobed2015_rank , y = kfr_black_pooled_p25)) 
g + geom_point(shape=1, alpha=1/2) + geom_vline(xintercept = c(5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95))

# Line
g <- ggplot(fulton_county_rows, aes(x = rent_twobed2015_rank , y = kfr_black_pooled_p25)) 
g + stat_binmean(n = 20) + stat_smooth(method = "lm", se = FALSE) + xlim(0,100) + ylim(.3,.4) + geom_point(shape=1, alpha=1/8) + geom_vline(xintercept = c(5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95))

# correlation 
cor(fulton_county_rows$rent_twobed2015_rank, fulton_county_rows$kfr_black_pooled_p25, use = "complete.obs")



