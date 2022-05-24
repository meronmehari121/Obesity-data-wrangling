# Data cleaning of WSIC data
# Replicate data frame as : "Comorbidity_ByYear_age_standardised_Age_at_Index_BMI_Category_for_WS4"
# i.e. a df with 35 rows representing comorbidities and 54 columns representing percentage of people in a given age band at index BMI

# libraries used

pacman::p_load(rio,tidyverse)

# Import original data 

path <- '....file.csv'
dat0 <- rio::import(file = path)  


# First, replace all "<5" & "<25" values in the dataframe to 0

dat1 <-  dat0 %>% mutate_all(~ str_replace(., "< 5", "0")) %>% mutate_all(~ str_replace(., "< 25", "0")) 
  
# select all relevant variables & change character but count columns to numeric values

dat2 <-  dat1 %>% select("Year", "Strata_Category", "Strata", starts_with("Age standardised"), -ends_with("CI")) %>% 
                  mutate_at(vars(matches("Age standardised")), as.numeric)

# check variable types are correct
 sapply(dat2, class)

# Then, clean variable names, and create new column with "year" and "strata" variables  to later use it create data frame by disease

dat3 <- dat2 %>%
  rename_with(function(x){gsub("Age standardised","",x)}) %>% 
  unite(year_strata, Year, Strata, sep = "_" ,remove = FALSE) 





############## Comorbidity df ###############


# To create comorbitity status data frame, filter  people with comorbidity & 
# create year by co morbidity column for column values

mcomorbidity_yes <-  dat3 %>%  filter(Strata == "Yes") %>% 
  unite(year_comorbidity,Year, Strata_Category, sep = "_", remove = FALSE) %>% 
  select(everything(), -year_strata, -Year, -Strata, -Strata_Category) 
  

# Transpose df to get year_comorbidity on column & comorbidity in rows 

mcomorbidity_yes <- mcomorbidity_yes %>%  
  gather(key= disease_name, value =  value, - "year_comorbidity") %>% 
  spread(key =  "year_comorbidity", value =  value) %>% slice(-36)
 





############## No Comorbidity df ###############


# To create comorbitity status data frame, filter  people with comorbidity & 
# create year by co morbidity column for column values

mcomorbidity_no <-  dat3 %>%  filter(Strata == "No") %>% 
  unite(year_comorbidity,Year, Strata_Category, sep = "_", remove = FALSE) %>% 
  select(everything(), -year_strata, -Year, -Strata, -Strata_Category) 


# Transpose df to get year_comorbidity on column & comorbidity in rows 

mcomorbidity_no <- mcomorbidity_no %>%  
  gather(key= disease_name, value =  value, - "year_comorbidity") %>% 
  spread(key =  "year_comorbidity", value =  value) %>% slice(-36)
 


