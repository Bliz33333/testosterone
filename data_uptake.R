library(pacman)
p_load(tidyverse, readr, readxl, writexl)

data_organized <- read_excel("data/data_organized.xlsx")

m_census_2010 <- read_csv("data/SC-EST2020-AGESEX-CIV.csv")
m_census_2020 <- read_csv("data/sc-est2022-agesex-civ.csv")

#bad practice to just stick the sheets together instead of joining by the first 7 rows, but whatever
(m_census_2010[,1:7] == m_census_2020[,1:7])  %>%  sum()
7*nrow(m_census_2010)
census_data <- cbind(m_census_2010[,-c(8,19)],m_census_2020[,-(1:8)])
rm(m_census_2010)
rm(m_census_2020)

census_data <- tibble(census_data)

census_data <-
  census_data %>% 
  filter(SEX == 0) %>% 
  select(-c(SUMLEV:STATE,SEX))

colnames(census_data)[-(1:2)] <- (2010:2022)

census_data <-
  census_data %>% 
  select(-c("2010":"2017","2022"))

census_data <-
  census_data %>%
  mutate(NAME = str_replace(NAME, " ", "_")) %>% 
  mutate(NAME = str_to_lower(NAME))

regions <- unique(census_data$NAME)
pop_data_template <- matrix(nrow = 7,ncol = 7)
pop_data_template <- as_tibble(data.frame(pop_data_template, stringsAsFactors = F))
# pop_data_template <- 
#   pop_data_template %>% 
#   mutate(age = as.numeric(age))
colnames(pop_data_template) <- colnames(data_organized)
pop_data_template$age <- (data_organized$age[1:7])
pop_data_template$type <- "population"
i <- 1
for (i in 1:length(regions)) {
  
  temp_dat <- 
    census_data %>% 
    filter(NAME == regions[i])
  
  fill_temp <- pop_data_template
  fill_temp$state <- regions[i]
  fill_temp[1,-(1:3)] <- t(colSums(temp_dat[(temp_dat$AGE >= 0) & (temp_dat$AGE <= 24),-(1:2)]))
  fill_temp[2,-(1:3)] <- t(colSums(temp_dat[(temp_dat$AGE >= 25) & (temp_dat$AGE <= 34),-(1:2)]))
  fill_temp[3,-(1:3)] <- t(colSums(temp_dat[(temp_dat$AGE >= 35) & (temp_dat$AGE <= 44),-(1:2)]))
  fill_temp[4,-(1:3)] <- t(colSums(temp_dat[(temp_dat$AGE >= 45) & (temp_dat$AGE <= 54),-(1:2)]))
  fill_temp[5,-(1:3)] <- t(colSums(temp_dat[(temp_dat$AGE >= 55) & (temp_dat$AGE <= 64),-(1:2)]))
  fill_temp[6,-(1:3)] <- t(colSums(temp_dat[(temp_dat$AGE >= 65) & (temp_dat$AGE <= 100),-(1:2)]))
  fill_temp[7,-(1:3)] <- t(colSums(temp_dat[(temp_dat$AGE == 999),-(1:2)]))
  
  if(i == 1)
  {
    pop_data <- fill_temp
  } else {
    pop_data <- rbind(pop_data, fill_temp)
  }
  
}

colnames(pop_data)[-(1:3)] <-2018:2021 

pop_data %>% 
  select(!type) %>% 
  write_xlsx(path = "data/pop_data_full.xlsx")

pop_data <-
  pop_data %>% 
  filter(state %in% data_organized$state)

data_organized <-
  rbind(data_organized, pop_data)

data_organized <-
  data_organized %>% 
  arrange(state, type, age)

save(data_organized, file = "data/data_organized")

#----



