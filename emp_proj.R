# Begin a log file.
sink(file = "ec50_emp_proj_1.txt", split = TRUE)

# Install packages (if necessary) and load required libraries
if (!require(tidyverse)) install.packages("tidyverse"); library(tidyverse)
if (!require(haven)) install.packages("haven"); library(haven)
if (!require(ggplot2)) install.packages("ggplot2"); library(ggplot2)
if (!require(statar)) install.packages("statar"); library(statar)

# Load in cleaned atlas data 
atlas <- read_dta("atlas.dta")
# atlas <- na.omit(atlas)

# Make variable for kfr_pooled_pooled_p25 in my census tract
state_rows = atlas[atlas$state == 13, ]
county_rows = state_rows[state_rows$county == 121, ]
my_tract = county_rows[county_rows$tract ==  010106, ]
my_tract$kfr_pooled_pooled_p25
mean(atlas$kfr_pooled_pooled_p25, na.rm = TRUE)

# 6, standard deviation question
sd(county_rows$kfr_pooled_pooled_p25, na.rm = TRUE)
sd(state_rows$kfr_pooled_pooled_p25, na.rm = TRUE)
sd(atlas$kfr_pooled_pooled_p25, na.rm = TRUE)

# 7. Histogram
hist(county_rows$kfr_pooled_pooled_p25)

# 8. tract upward mobility by demographic compared to national average 
my_tract$kfr_black_pooled_p25
mean(atlas$kfr_black_pooled_p25, na.rm = TRUE)
my_tract$kfr_hisp_pooled_p25
my_tract$kfr_white_pooled_p25
mean(atlas$kfr_white_pooled_p25, na.rm = TRUE)
my_tract$kfr_black_male_p25
mean(atlas$kfr_black_male_p25, na.rm = TRUE)
my_tract$kfr_black_female_p25
mean(atlas$kfr_black_female_p25, na.rm = TRUE)


# sd's by demographic 
sd(county_rows$kfr_black_pooled_p25, na.rm = TRUE)
sd(state_rows$kfr_black_pooled_p25, na.rm = TRUE)
sd(atlas$kfr_black_pooled_p25, na.rm = TRUE)

sd(county_rows$kfr_white_pooled_p25, na.rm = TRUE)
sd(state_rows$kfr_white_pooled_p25, na.rm = TRUE)
sd(atlas$kfr_white_pooled_p25, na.rm = TRUE)

sd(county_rows$kfr_black_male_p25, na.rm = TRUE)
sd(state_rows$kfr_black_male_p25, na.rm = TRUE)
sd(atlas$kfr_black_male_p25, na.rm = TRUE)

sd(county_rows$kfr_black_female_p25, na.rm = TRUE)
sd(state_rows$kfr_black_female_p25, na.rm = TRUE)
sd(atlas$kfr_black_female_p25, na.rm = TRUE)

# 9 
mean(county_rows$jail_black_male_p25, na.rm = TRUE)
mean(atlas$jail_black_male_p25, na.rm = TRUE)

mean(county_rows$singleparent_share2010, na.rm = TRUE)
mean(atlas$singleparent_share2010, na.rm = TRUE)
cor(county_rows$singleparent_share2010, county_rows$kfr_black_male_p25, use = "complete.obs")

mean(county_rows$ann_avg_job_growth_2004_2013, na.rm = TRUE)
mean(atlas$ann_avg_job_growth_2004_2013, na.rm = TRUE)
cor(county_rows$ann_avg_job_growth_2004_2013, county_rows$kfr_black_male_p25, use = "complete.obs")

# 10 
# create rank of single parent share in 2010 in county, then scaling it by 100
county_rows$county_single_parent_share = (rank(county_rows$singleparent_share2010))/length(rank(county_rows$singleparent_share2010)) * 100 

# Scatter plot first
g <- ggplot(county_rows, aes(x = county_single_parent_share , y = kfr_black_male_p25)) 
g + geom_point(shape=1, alpha=1/2) 

# Add Bins
g <- ggplot(county_rows, aes(x = county_single_parent_share , y = kfr_black_male_p25)) 
g + geom_point(shape=1, alpha=1/2) + geom_vline(xintercept = c(5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95))

# Binscatter single parent share in county vs. kfr_black_male_p25 in county
g <- ggplot(county_rows, aes(x = county_single_parent_share , y = kfr_black_male_p25)) 
g + stat_binmean(n = 20) + stat_smooth(method = "lm", se = FALSE) + xlim(0,100) + ylim(0,1) + geom_point(shape=1, alpha=1/8) + geom_vline(xintercept = c(5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95))

# now we do the same for job growth in the county 
county_rows$county_job_growth = (rank(county_rows$ann_avg_job_growth_2004_2013))/length(rank(county_rows$ann_avg_job_growth_2004_2013)) * 100 

# Scatter plot first
g <- ggplot(county_rows, aes(x = county_job_growth , y = kfr_black_male_p25)) 
g + geom_point(shape=1, alpha=1/2) 

# Add Bins
g <- ggplot(county_rows, aes(x = county_job_growth , y = kfr_black_male_p25)) 
g + geom_point(shape=1, alpha=1/2) + geom_vline(xintercept = c(5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95))

# Binscatter county job growth vs. kfr_black_male_p25 in county
g <- ggplot(county_rows, aes(x = county_job_growth , y = kfr_black_male_p25)) 
g + stat_binmean(n = 20) + stat_smooth(method = "lm", se = FALSE) + xlim(0,100) + ylim(0,1) + geom_point(shape=1, alpha=1/8) + geom_vline(xintercept = c(5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95))

