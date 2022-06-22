# Data cleaning of WSIC data
# Replicate data frame as : "Comorbidity_ByYear_age_standardised_Age_at_Index_BMI_Category_for_WS4"
# i.e. a df with 35 rows representing comorbidities and 54 columns representing percentage of people in a given age band at index BMI

# libraries used

pacman::p_load(rio,tidyverse)
select <-  dplyr::select

# Import original data 
path <- '....file.csv'

dat0 <- rio::import(file = path)  


# First replace all "<5" and "<25" values to 0 & change character but count columns to numeric values

dat1 <- dat0 %>% 
        mutate_all(~ str_replace(., "< 5", "0")) %>% 
        mutate_all(~ str_replace(., "< 25", "0"))


# Then, select correct variables, clean variable names, and create new column with "year" and "strata" variables  

dat2 <- dat1 %>% select("Year", "Strata_Category", "Strata", starts_with("Age standardised"), -ends_with("CI")) %>% 
        mutate_at(vars(matches("Age standardised")), as.numeric)%>%
        unite(year_strata, Year, Strata, sep = "_" ,remove = F) %>% 
        filter(Strata_Category == "Age_in_Year_Category") %>% 
        select(everything(), - (2:4)) 

# check variable types are correct

sapply(dat2, class)         

# transposing column names to row names to get desired df format

dat3 <-  dat2   %>% 
         rownames_to_column() %>%
         gather(variable, value, - "year_strata") %>% 
         spread("year_strata", value) %>% slice(-36) %>% 
         mutate(variable = str_replace(variable, "Age standardised ", "")) 


write.csv(dat3, file = "dat3.csv", rownames = F)
