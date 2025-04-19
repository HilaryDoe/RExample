###############################################################################
# Purpose:  Combine county-level American Community Survey data, Texas Child
#           Care data, and rural/urban county information to see the distribution
#           of child care supply in Texas and it's association with county
#           characteristics
#
# Created by:   Hilary Doe
#
# Date Created: 04/07/2025
#
################################################################################

# Install and Library Pacackages ####
#install.packages("tidyverse")
#install.packages("tidycensus")
#install.packages("paletteer")
#install.packages("gridExtra")
#install.packages("collapse")
#install.packages("lmtest")
#install.packages("marginaleffects")

library(tidyverse)
library(tidycensus)
library(tidyverse)
library(paletteer)
library(gridExtra)
library(readxl)
library(collapse)
library(lmtest)
library(marginaleffects)

# Get Census Data ####

# census_api_key("288a7aed7ac29b4739f8b2870535a07a1f6cf3c6")

# Here are the variable names and descriptions we are collecting:
# S1701_C02_001E  Estimate!!Percent below poverty level!!Population for whom poverty status is determined"
# S2301_C04_001E  Estimate!!Unemployment rate!!Population 16 years and over"
# B03002_001E     Estimate!!Total:
# B03002_003E     Estimate!!Total:!!Not Hispanic or Latino:!!White alone
# B03002_004E     Estimate!!Total:!!Not Hispanic or Latino:!!Black or African American alone
# B03002_005E     Estimate!!Total:!!Not Hispanic or Latino:!!American Indian and Alaska Native alone
# B03002_006E     Estimate!!Total:!!Not Hispanic or Latino:!!Asian alone
# B03002_007E     Estimate!!Total:!!Not Hispanic or Latino:!!Native Hawaiian and Other Pacific Islander alone
# B03002_008E     Estimate!!Total:!!Not Hispanic or Latino:!!Some other race alone
# B03002_009E     Estimate!!Total:!!Not Hispanic or Latino:!!Two or more races:
# B03002_012E     Estimate!!Total:!!Hispanic or Latino:

## Poverty ####
poverty <- get_acs(geography = "county",
                   variables = "S1701_C03_001",
                   state = "texas",
                   year = 2023) %>%
  rename(poverty_rate = estimate,
         poverty_rate_moe = moe,
         county = GEOID) %>%
  select(county, poverty_rate, poverty_rate_moe)

## Unemployment ####
unemploy <- get_acs(geography = "county",
                    variables = "S2301_C04_001",
                    state = "texas",
                    year = 2023,) %>%
  rename(unemployment_rate = estimate,
         unemployment_rate_moe = moe,
         county = GEOID) %>%
  select(county, unemployment_rate, unemployment_rate_moe)

## Race/Ethnicity ####
total_pop <- get_acs(geography = "county",
                     variables = "B03002_001",
                     state = "texas",
                     year = 2023,) %>%
  rename(total_pop = estimate,
         total_pop_moe = moe,
         county = GEOID) %>%
  select(county, total_pop, total_pop_moe)

white_pop <- get_acs(geography = "county",
                     variables = "B03002_003",
                     state = "texas",
                     year = 2023,) %>%
  rename(white_pop = estimate,
         white_pop_moe = moe,
         county = GEOID) %>%
  select(county, white_pop, white_pop_moe)

black_pop <- get_acs(geography = "county",
                     variables = "B03002_004",
                     state = "texas",
                     year = 2023,) %>%
    rename(black_pop = estimate,
         black_pop_moe = moe,
         county = GEOID) %>%
  select(county, black_pop, black_pop_moe)

ai_pop <- get_acs(geography = "county",
                  variables = "B03002_005",
                  state = "texas",
                  year = 2023,) %>%
    rename(ai_pop = estimate,
           ai_pop_moe = moe,
           county = GEOID) %>%
    select(county, ai_pop, ai_pop_moe)

asian_pop <- get_acs(geography = "county",
                     variables = "B03002_006",
                     state = "texas",
                     year = 2023,) %>%
  rename(asian_pop = estimate,
         asian_pop_moe = moe,
         county = GEOID) %>%
  select(county, asian_pop, asian_pop_moe)

nhpi_pop <- get_acs(geography = "county",
                    variables = "B03002_007",
                    state = "texas",
                    year = 2023,) %>%
    rename(nhpi_pop = estimate,
           nhpi_pop_moe = moe,
           county = GEOID) %>%
    select(county, nhpi_pop, nhpi_pop_moe)

other_pop <- get_acs(geography = "county",
                     variables = "B03002_008",
                     state = "texas",
                     year = 2023,) %>%
  rename(other_pop = estimate,
         other_pop_moe = moe,
         county = GEOID) %>%
    select(county, other_pop, other_pop_moe)

twoplus_pop <- get_acs(geography = "county",
                       variables = "B03002_009",
                       state = "texas",
                       year = 2023,) %>%
  rename(twoplus_pop = estimate,
         twoplus_pop_moe = moe,
         county = GEOID) %>%
    select(county, twoplus_pop, twoplus_pop_moe)

hisp_pop <- get_acs(geography = "county",
                    variables = "B03002_012",
                    state = "texas",
                    year = 2023,) %>%
  rename(hisp_pop = estimate,
         hisp_pop_moe = moe,
         county = GEOID) %>%
  select(county, hisp_pop, hisp_pop_moe)

#Join race/Ethnicity frames together
race_eth <- total_pop %>%
  left_join(white_pop) %>%
  left_join(black_pop) %>%
  left_join(ai_pop) %>%
  left_join(asian_pop) %>%
  left_join(nhpi_pop) %>%
  left_join(other_pop) %>%
  left_join(twoplus_pop) %>%
  left_join(hisp_pop)

# Create percentages of each race/ethnicity
race_eth <- race_eth %>%
  mutate(perc_white = round((white_pop/total_pop)*100, digits = 1),
         perc_black = round((black_pop/total_pop)*100, digits = 1),
         perc_ai = round((ai_pop/total_pop)*100, digits = 1),
         perc_asian = round((asian_pop/total_pop)*100, digits = 1),
         perc_nhpi = round((nhpi_pop/total_pop)*100, digits = 1),
         perc_other = round((other_pop/total_pop)*100, digits = 1),
         perc_twoplus = round((twoplus_pop/total_pop)*100, digits = 1),
         perc_hisp = round((hisp_pop/total_pop)*100, digits = 1)) %>%
  select(county, total_pop, perc_white, perc_black, perc_ai, perc_asian, perc_nhpi, perc_other, perc_twoplus, perc_hisp)

## Join to create census_demo ####
census_demo <- race_eth %>%
  left_join(poverty) %>%
  left_join(unemploy)

## Read in FIPS codes so we can join frames
fips <- read_delim("/home/hilary/Documents/RExample/RExample/countyfips.txt", col_names = TRUE, delim = "|")
fips <- fips %>% 
  mutate(county=paste(STATEFP, COUNTYFP, sep = "")) %>% 
  rename(county_name = COUNTYNAME) %>% 
  select(county, county_name) %>% 
  mutate(county_name = str_to_upper(county_name))

##Join census demo with FIPS codes
census_demo <- census_demo %>%
  left_join(fips)

## Urban and Rural status ####
## Read in urban/rural county (Downloaded from https://www.ers.usda.gov/data-products/rural-urban-continuum-codes)
urban <- read_xlsx("/home/hilary/Documents/RExample/RExample/Ruralurbancontinuumcodes2023.xlsx") 
urban <- urban %>% 
  filter(State == "TX") %>% 
  mutate(urban_rural = case_when(RUCC_2023 < 4 ~ "Urban",
                                 RUCC_2023 >= 4 ~ "Rural")) %>% 
  rename(county = FIPS) %>% 
  select(county, urban_rural)

## Join census demo with urban/rural info
census_demo <- census_demo %>% 
  left_join(urban)

# Child Care Data ####
## Open Child Care Data #### 
  ## Downloaded from https://data.texas.gov/See-Category-Tile/HHSC-CCL-Daycare-and-Residential-Operations-Data/bc5r-88dy/about_data
cc <- read_csv("/home/hilary/Documents/RExample/RExample/HHSC_CCL_Daycare_and_Residential_Operations_Data_20250406.csv") 

## Prepare for joining with census_demo. Requires County name to be uppercase without COUNTY word
cc <- cc %>% 
  mutate(county = "COUNTY") %>% 
  mutate(county_name=paste(COUNTY, county, sep = " ")) %>% 
  select(!county)
  
##Join census_demo with child care data
df <- census_demo %>% 
 full_join(cc)

## Clean up Child Care data ####
  ## Remove non-traditional child care programs, and combine registered and licensed child care homes as "Home"s
df <- df %>% 
  filter(OPERATION_TYPE != "Child Placing Agency" & OPERATION_TYPE != "General Residential Operation" & OPERATION_TYPE != "Listed Family Home")  %>% 
  filter( is.na(CARE_TYPE) | (CARE_TYPE != "Before/After School Program" & CARE_TYPE != "School Age Program")) %>% 
  filter(TEMPORARILY_CLOSED != "YES") %>% 
  mutate(operation_type = OPERATION_TYPE) %>% 
  mutate(prov_type = case_when(operation_type == "Licensed Center" ~ "Center",
                               operation_type == "Registered Child-Care Home" ~ "Home",
                               operation_type == "Licensed Child-Care Home" ~ "Home")
         )

## More cleaning. 
ccset <- df %>% 
  select(county, perc_white, perc_black, perc_hisp, poverty_rate, unemployment_rate, county_name, OPERATION_ID, OPERATION_NAME, LOCATION_ADDRESS, ACCEPTS_CHILD_CARE_SUBSIDIES, HOURS_OF_OPERATION, DAYS_OF_OPERATION, TOTAL_CAPACITY, prov_type, total_pop, urban_rural) %>% ##Trim dataset to variables we want
  rename_with(tolower, .cols = everything()) %>% ## Rename to lowercase for easier typing 
  mutate(prov_type = as.factor(prov_type), ## Convert to factor
         county_name = as.factor(county_name), ## Convert to factor
         days_of_operation = as.factor(days_of_operation) ## Convert to factor
  ) %>% 
  mutate(hours_open = str_split_i(hours_of_operation, "-", 1), ## Isolate Opening Time
         hours_close = str_split_i(hours_of_operation, "-", 2)) %>% ## Isolate Closing time
  mutate(hours_close_hr = as.numeric(str_split_i(hours_close, ":", 1)), ## Isolate closing hour
         hours_close_min = str_split_i(hours_close, ":", 2), ## Isolate closing minute = AM PM
         hours_close_aft = str_split_i(hours_close, " ", 2)) %>% ## Isolate AM vs. PM
  mutate(hours_close_min = as.numeric(str_split_i(hours_close_min, " ", 1))) %>% ## Isolate closing minute
  mutate(hours_close_hrplus12 = hours_close_hr + 12) %>% ## Convert closing hour to 24-hour time (PM)
  mutate(hours_close_hr24 = case_when(hours_close_aft == "PM" ~ hours_close_hrplus12, ## Use 24-hour PM time if PM
                                      hours_close_aft == "AM" ~ hours_close_hr) ## Use 24-hour AM time if AM
         ) %>% 
  mutate(hours_close_hr24 = (hours_close_hr24*100)+hours_close_min) %>% ## Multiply by 100 so 12 hours becomes 1200
  select(!c(hours_close_hr, hours_close_min, hours_close_hrplus12, hours_close_aft)) %>% 
  mutate(open_aft_6 = case_when(hours_close_hr24 > 1800 ~ 1, ## Open_aft_6 = 1 if open past 6 pm
                                          TRUE ~ 0)) %>% ## Otherwise 0
  mutate(open_wknd = case_when(str_detect(days_of_operation, "Sat") ~ 1, ## Open weekend = 1 if open Saturday
                               str_detect(days_of_operation, "Sun") ~ 1, ## or Sunday
                               TRUE ~ 0)) %>% 
  mutate(urban_rural = as.factor(urban_rural)) %>% ## Convert to factor
  mutate(total_cap_home = case_when(prov_type == "Home" ~ total_capacity, ## Total capacity just for Homes
                                    TRUE ~ 0),
         total_cap_center =  case_when(prov_type == "Center" ~ total_capacity, ## Total capacity just for Centers
                                       TRUE ~ 0))
 
# Collapse to county-level values ####
sum <- ccset %>% 
  group_by(county_name) %>% 
  summarize(county_cap_sum = sum(total_capacity, na.rm = TRUE),
            county_cap_mean = mean(total_capacity, na.rm = TRUE),
            county_cap_n = sum(!is.na((total_capacity))), 
            county_home_n = sum(prov_type == "Home"),
            county_center_n = sum(prov_type == "Center"),
            perc_home = mean(prov_type == "Home", na.rm = TRUE), 
            perc_white = mean(perc_white, na.rm = TRUE), 
            perc_black = mean(perc_black, na.rm = TRUE), 
            perc_hisp = mean(perc_hisp, na.rm = TRUE),
            total_pop = mean(total_pop, na.rm = TRUE), 
            unemployment_rate = mean(unemployment_rate, na.rm = TRUE), 
            poverty_rate = mean(poverty_rate, na.rm = TRUE), 
            total_pop = mean(total_pop, na.rm = TRUE),
            open_wknd = sum(open_wknd, na.rm = TRUE),
            open_aft_6 = sum(open_aft_6, na.rm = TRUE),
            urban_rural = fmode(urban_rural, na.rm = TRUE), ## Keeps most common value... which is same across county
            county_cap_home_sum = sum(total_cap_home, na.rm = TRUE),
            county_cap_center_sum = sum(total_cap_center, na.rm = TRUE),
  ) %>% 
  mutate(perc_cap_home = (county_cap_home_sum/county_cap_sum)*100,
         pop_rank = rank(total_pop),
         perc_pop_cap = (county_cap_sum/total_pop)*100,
         total_pop1000 = total_pop/1000,
         perc_home = perc_home*100)


sum_50 <- sum %>% ## 50 most populated counties
  filter(pop_rank > 189)

sum_urban <- sum %>% ## Urban Counties only
  filter(urban_rural == "Urban")

sum_rural<- sum %>% ## Rural Counties only
  filter(urban_rural == "Rural")

# Analysis ####

# Barplot- Total Population in 50 Most-Populated Texas Counties
ggplot(sum_50, aes(x=reorder(county_name, total_pop), y=total_pop,total_pop, fill = poverty_rate)) + 
  geom_bar(stat = "identity") +
  labs(title = "Total Population in 50 Most-Populated Texas Counties",
       x = "County",
       y = "Population")+
  coord_flip()

# Barplot - Licensed Capacity in 50 Most-Populated Texas Counties
ggplot(sum_50, aes(x=reorder(county_name, total_pop), y=county_cap_sum, county_cap_sum, fill = poverty_rate)) + 
  geom_bar(stat = "identity") +
  labs(title = "Total Licensed Capacity in 50 Most-Populated Texas Counties, sorted by total population descending",
       x = "County",
       y = "Total Licensed Capacity")+
  coord_flip()

## Which county characteristics, if any, predict percent of child care providers that are home-based providers? ####

ggplot(sum, aes(x=urban_rural, y=perc_home)) + 
  geom_boxplot(fill="slateblue") 


ggplot(sum, aes(x=poverty_rate, y=perc_home)) + 
  geom_point() +
  geom_smooth(method = lm)

perc_home1 <- lm(perc_home ~ unemployment_rate, data = sum)
  summary(perc_home1)

perc_home2 <- lm(perc_home ~ unemployment_rate + urban_rural, data = sum)
  summary(perc_home2)
  lrtest(perc_home1, perc_home2)
  
perc_home3 <- lm(perc_home ~ unemployment_rate + urban_rural + perc_white, data = sum)
  summary(perc_home3)
  lrtest(perc_home2, perc_home3)
  
perc_home4 <- lm(perc_home ~ unemployment_rate + urban_rural + perc_black, data = sum)
  summary(perc_home4)
  lrtest(perc_home2, perc_home4)
  
perc_home5 <- lm(perc_home ~ unemployment_rate + urban_rural + perc_hisp, data = sum)
  summary(perc_home5)
  lrtest(perc_home2, perc_home5)

perc_home6 <- lm(perc_home ~ unemployment_rate + urban_rural + unemployment_rate:urban_rural, data = sum)
  summary(perc_home6)
  lrtest(perc_home2, perc_home6)
  
perc_home_final <- lm(perc_home ~ unemployment_rate + urban_rural, data = sum)
  summary(perc_home_final)
  
  
            ggplot(sum, aes(x=urban_rural, y=perc_pop_cap)) + 
  geom_boxplot(fill="slateblue") 

ggplot(sum, aes(x=total_pop, y=perc_pop_cap, color=urban_rural)) +
  geom_point()

model1 <- lm(perc_pop_cap ~ total_pop1000, data = sum)
summary(model1)


model2 <- lm(perc_pop_cap ~ total_pop1000 + urban_rural, data = sum)
summary(model2)
lrtest(model1, model2)


model3 <- lm(perc_pop_cap ~ total_pop1000 + urban_rural +  perc_black, data = sum)
summary(model3)
lrtest(model2, model3)

model4<- lm(perc_pop_cap ~ total_pop1000 + urban_rural +  poverty_rate, data = sum)
summary(model4)
lrtest(model2, model4)


model5<- lm(perc_pop_cap ~ total_pop1000 + urban_rural +  unemployment_rate, data = sum)
summary(model5)
lrtest(model2, model5)


model6<- lm(perc_pop_cap ~ total_pop1000 + urban_rural +  perc_white, data = sum)
summary(model6)
lrtest(model2, model6)


model7<- lm(perc_pop_cap ~ total_pop1000 + urban_rural +  perc_home, data = sum)
summary(model7)
lrtest(model2, model7)


model7<- lm(perc_pop_cap ~ total_pop1000 + urban_rural +  perc_home + perc_white, data = sum)
summary(model7)
lrtest(model2, model7)

model1 <- lm(perc_home ~ urban_rural, data= sum)
summary(model1)

model1 <- lm(perc_cap_home ~ urban_rural, data= sum)
summary(model1)

model2 <- lm(perc_home ~ poverty_rate + urban_rural, data= sum)
summary(model2)
lrtest(model1, model2)

ggplot(sum, aes(x=urban_rural, y=perc_home)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) 


ggplot(sum, aes(x=urban_rural, y=perc_black)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) 


ggplot(sum, aes(x=perc_pop04_cap, y=perc_black, color=perc_pop04_cap)) +
  geom_point()

ggplot(sum, aes(x=poverty_rate, y=perc_home, color=poverty_rate)) +
  geom_point()

#grid.arrange(plot1, plot2, ncol=2)
ggplot(sum_rural, aes(x=perc_pop_cap, y=perc_home, color=poverty_rate)) +
  geom_point()+
  geom_smooth(method = lm)

ggplot(sum, aes(x=total_pop, y=county_cap_sum, color=unemployment_rate)) +
  geom_point() 

ggplot(sum, aes(x=perc_black, y=perc_home, color=poverty_rate)) + geom_point()

ggplot(sum, aes(x=perc_black, y=county_cap_sum, color=poverty_rate)) + geom_point()

ggplot(sum, aes(x=total_pop, y=county_cap_mean, color=poverty_rate)) + geom_point()
ggplot(sum, aes(x=total_pop, y=county_cap_mean, color=poverty_rate)) + geom_point()
plot1 <- ggplot(sum_rural, aes(x=total_pop, y=county_cap_sum, color=poverty_rate)) + geom_point()
plot2 <- ggplot(sum_urban, aes(x=total_pop, y=county_cap_sum, color=poverty_rate)) + geom_point()
grid.arrange(plot1, plot2, ncol=2)

ggplot(sum, aes(x=total_pop, y=county_cap_sum, color=urban_rural)) + geom_point()



ggplot(sum, aes(x=poverty_rate, y=open_aft_6, color=poverty_rate)) + geom_point()
ggplot(sum, aes(x=perc_black, y=poverty_rate, color=poverty_rate)) + geom_point()
ggplot(sum, aes(x=perc_black, y=unemployment_rate, color=poverty_rate)) + geom_point()

ggplot(sum, aes(x=perc_black, y=unemployment_rate, color=urban_rural)) + geom_point() +geom_smooth(method = "lm") 
ggplot(sum, aes(x=total_pop, y=county_cap_sum, color=urban_rural)) + geom_point() +geom_smooth(method = "lm") 
ggplot(sum_rural, aes(x=total_pop, y=county_cap_sum, color=urban_rural)) + geom_point() +geom_smooth(method = "lm") 
ggplot(sum, aes(x=perc_black, y=perc_home, color=urban_rural)) + geom_point() +geom_smooth(method = "lm") 

##"cat_tab_lab" is for traditional categorical variables, it places the variable label for the variable being tabbed as the first cell in the table.By default it does not show NAs. To show NAs (true missings set to . in stata), include na = TRUE, By default it shows levels that are empty (0%), but to set as not showing them, do em = FALSE
cat_tab_lab1 <- function(x, y, na, em){
  step1 <- x %>%
    tabyl(all_of(y), show_na = na, show_missing_levels = em) %>%
    adorn_totals("row") %>%
    adorn_pct_formatting() %>%
    as_tibble() %>% 
    rename (Number = n) %>% 
    rename (Percent = percent)
  return(step1)
}
cat_tab_lab <- function(x, y, na = FALSE, em = TRUE){
  step1 <- cat_tab_lab1(x, y, na, em)
  test <- var_lab(x[[y]])
  names(step1)[1] <- test
  return(step1)
}
cat_tab_lab(sum$urban_rural)


## Create function
regout <- function(data, formula, digits = 3){
  model <- lm(formula, data = data)
  tab <- kable(tidy(model), digits = digits, caption = "lm(formula, data = data)")
  print(tab)
}
test <- regout(data = sum, formula = "perc_pop_cap ~ poverty_rate + urban_rural" )
test

## Create function for simple Regression output
regout <- function(data, formula, digits = 3){
  mod <<- lm(formula, data = data)
  tab <- kable(tidy(mod), digits = digits, caption = {{formula}})
  print(tab)
} 

regout(data = sum, formula = "perc_pop_cap ~ poverty_rate + urban_rural" )
regout(data = sum, formula = "perc_pop_cap ~ poverty_rate + urban_rural + total_pop")


lrtest(mod1, mod2)

