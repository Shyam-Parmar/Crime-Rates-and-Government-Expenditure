###############################
# Crime Rates Project

rm(list=ls())

###############################
# Load Libraries
pacman::p_load(dplyr, tidyr, caret, ggplot2, caTools, MLmetrics, mlbench, mlTools, corrplot, expss, PerformanceAnalytics, 
               AER, MASS, stargazer, pscl, jtools, Hmisc, ggcorrplot, rpart, rpart.plot, readxl, ROCR, lme4)

##############################
# Load crime data set
crime_df <- read.csv("C:/Users/Scott/Documents/Shyam_Personal/SDM Assignment/Project/combined_crime_data.csv")

# Load expenditure data set
exp_df <- read.csv('C:/Users/Scott/Documents/Shyam_Personal/SDM Assignment/Project/Expenditure_2010-2017_Final.csv')

##############################
# Merge

# Ensure datatypes are matching for joining
str(crime_df)
str(exp_df)

# Check for Na values
colSums(is.na(crime_df))

# Drop NA values from the year column
crime_df <- crime_df[complete.cases(crime_df$year), ] # Drop incomplete rows

# Convert the Year column to Integer for joining
crime_df$Year <- as.integer(crime_df$Year)

# Merge the two datasets
df <- merge(crime_df, exp_df, by=c("state", "city", "year"))

# Check for any NA values
sum(is.na(df))

############################
# Clean datasets
str(df)

# Convert values to factors
cols_factor <- c("state", "city", "year")
df[cols_factor] <- (lapply(df[cols_factor], factor))

levels(df$year)

# convert variables to integer
cols_num <- c("population", "violent_crime", "murder_and_nonnegligent_manslaughter", "forcible_rape", "robbery", "aggravated_assault", "property_crime", "burglary", "larceny_theft", "motor_vehicle_theft")
df[cols_num] <- (lapply(df[cols_num], as.integer))

# Create a calculated columns to see the total crimes in each city for that year
total_crimes <- df$violent_crime + df$murder_and_nonnegligent_manslaughter + df$forcible_rape + df$robbery + df$aggravated_assault + df$property_crime + df$burglary + df$larceny_theft + df$motor_vehicle_theft

#############################
# Visualization

hist(df$violent_crime)
hist(log(df$violent_crime))

hist(df$non_violent_crime)
hist(log(df$non_violent_crime))

hist(total_crimes)
hist(log(total_crimes))

chart.Correlation(df[as.integer(which(sapply(df,class)=="integer"))]) # Plot for numeric variables

############################
# Models

################
# Violent crimes

# Baseline 
m1 <- lmer(log(violent_crime) ~ 1 + (1 | city), data=df, REML=FALSE)
summary(m1)

# Model using mixed level
vio_m2 <- lmer(log(violent_crime) ~ capital_outlay + education_capital_expenditure + higher_education_capital_expenditure + elementary_._secondary_capital_expenditure + 
             hospitals_capital_expenditure + highways_capital_expenditure + correction_capital_expenditure + 
             natural_resources_capital_expenditure + parks_and_recreation_capital_expenditure + sewerage_capital_expenditure +
             solid_waste_management_capital_expenditure + other_general_capital_expenditure + utility_capital_outlay + (1 | city), data=df, REML=FALSE)
summary(vio_m2)

# Model using poisson method
m3 <- glm(violent_crime ~ capital_outlay + education_capital_expenditure + higher_education_capital_expenditure + elementary_._secondary_capital_expenditure + 
             hospitals_capital_expenditure + highways_capital_expenditure + correction_capital_expenditure + 
             natural_resources_capital_expenditure + parks_and_recreation_capital_expenditure + sewerage_capital_expenditure +
             solid_waste_management_capital_expenditure + other_general_capital_expenditure + utility_capital_outlay, data=df, family = poisson (link = log))
summary(m3)

# Over-dispersed
m3 <- glm.nb(violent_crime ~ capital_outlay + education_capital_expenditure + higher_education_capital_expenditure + elementary_._secondary_capital_expenditure + 
            hospitals_capital_expenditure + highways_capital_expenditure + correction_capital_expenditure + 
            natural_resources_capital_expenditure + parks_and_recreation_capital_expenditure + sewerage_capital_expenditure +
            solid_waste_management_capital_expenditure + other_general_capital_expenditure + utility_capital_outlay, data=df)
summary(m3)

################
# Non-Violent crimes

# Baseline 
m1 <- lmer(log(non_violent_crime) ~ 1 + (1 | city), data=df, REML=FALSE)
summary(m1)

# Model using mixed level
non_v_m2 <- lmer(log(non_violent_crime) ~ capital_outlay + education_capital_expenditure + higher_education_capital_expenditure + elementary_._secondary_capital_expenditure + 
             hospitals_capital_expenditure + highways_capital_expenditure + correction_capital_expenditure + 
             natural_resources_capital_expenditure + parks_and_recreation_capital_expenditure + sewerage_capital_expenditure +
             solid_waste_management_capital_expenditure + other_general_capital_expenditure + utility_capital_outlay + (1 | city), data=df, REML=FALSE)
summary(non_v_m2)
ranef(non_v_m2)

# Model using poisson method
m3 <- glm(non_violent_crime ~ capital_outlay + education_capital_expenditure + higher_education_capital_expenditure + elementary_._secondary_capital_expenditure + 
            hospitals_capital_expenditure + highways_capital_expenditure + correction_capital_expenditure + 
            natural_resources_capital_expenditure + parks_and_recreation_capital_expenditure + sewerage_capital_expenditure +
            solid_waste_management_capital_expenditure + other_general_capital_expenditure + utility_capital_outlay, data=df, family = poisson (link = log))
summary(m3)

# Over-dispersed
m3 <- glm.nb(non_violent_crime ~ capital_outlay + education_capital_expenditure + higher_education_capital_expenditure + elementary_._secondary_capital_expenditure + 
               hospitals_capital_expenditure + highways_capital_expenditure + correction_capital_expenditure + 
               natural_resources_capital_expenditure + parks_and_recreation_capital_expenditure + sewerage_capital_expenditure +
               solid_waste_management_capital_expenditure + other_general_capital_expenditure + utility_capital_outlay, data=df)
summary(m3)

library(stargazer)
options(max.print=10000)
stargazer(vio_m2, non_v_m2, type="text", single.row=TRUE)
