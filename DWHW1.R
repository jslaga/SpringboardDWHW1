#install and load packages
install.packages( "tidyr")

library(dplyr)
library(tidyr)

##load file into dataframe
refine_df <- data.frame(read.csv("~/Documents/School/Springboard/DataWranglingHW1/HW1/refine_original.csv"))

##standardize brand names: philips, akzo, van houten and unilever 
refine_df$company <- gsub(glob2rx("^*lips*"), replacement = "philips", x = refine_df$company, ignore.case = TRUE) 
refine_df$company <- gsub(glob2rx("*ak*$"), replacement = "akzo", x = refine_df$company, ignore.case = TRUE) 
refine_df$company <- gsub(glob2rx("*van*$"), replacement = "van houten", x = refine_df$company, ignore.case = TRUE) 
refine_df$company <- gsub(glob2rx("*uni*$"), replacement = "unilever", x = refine_df$company, ignore.case = TRUE) 

## Separate Product Code and Number
refine_df <- refine_df %>% separate("Product.code...number", c("product_code", "product_number"), sep = "-")

## Add product categories: p = Smartphone, v = TV, x = Laptop, q = Tablet
refine_df <- refine_df %>% mutate("product_category" = ifelse(product_code == "p", "Smartphone", " ")) %>% 
  mutate("product_category" = ifelse(product_code == "v", "TV", product_category)) %>%
  mutate("product_category" = ifelse(product_code == "x", "Laptop", product_category)) %>%
  mutate("product_category" = ifelse(product_code == "q", "Tablet", product_category))

## Add full address for geocoding
refine_df <- refine_df %>% unite(full_address, address:country, sep = ", ", remove = FALSE)

## Create dummy varibles for company and product category
refine_df <- refine_df %>% mutate(company_philips = ifelse(company == "philips", 1, 0)) %>%
  mutate(company_akzo = ifelse(company == "akzo", 1, 0)) %>%
  mutate(company_van_houten = ifelse(company == "van houten", 1, 0)) %>%
  mutate(company_unilever = ifelse(company == "unilever", 1, 0))

refine_df <- refine_df %>% mutate(product_smartphone = ifelse(product_category == "Smartphone", 1, 0)) %>%
  mutate(product_tv = ifelse(product_category == "TV", 1, 0)) %>%
  mutate(product_laptop = ifelse(product_category == "Laptop", 1, 0)) %>%
  mutate(product_tablet = ifelse(product_category == "Tablet", 1, 0))

write.csv(refine_df, "~/Documents/School/Springboard/DataWranglingHW1/HW1/refine_clean.csv")

