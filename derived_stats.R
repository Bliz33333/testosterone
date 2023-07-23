library(pacman)
p_load(tidyverse, reshape2)

load(file = "data/data_organized")
colnames(data_organized)[-(1:3)] <- as.character(2018:2021)
age_groups <- unique(data_organized$age)
years <- as.character(2018:2021)

#helper functions----
slicer_func <- function(dat1, dat2, func, name ,cutoff = 3, change_col = 2){
  
  same_cols <- (1:cutoff)[-change_col]
  
  stopifnot(((dat1[,same_cols] == dat1[,same_cols]) %>% mean()) == 1)
  
  temp1 <- dat1[,-(1:cutoff)]
  temp2 <- dat2[,-(1:cutoff)]
  
  temp3 <- Reduce(func,list(temp1,temp2))
  
  output <- dat1
  output$type <- name
  output[,-(1:cutoff)] <- temp3
  
  return(output)
}
melt_dat <- function(dat, var_name){
  melt(data_organized,id.vars = 1:3, variable.name = var_name) %>% as.tibble() %>% return()
}
unmelt_year <- function(melted){
  dcast(melted, state + type + age~year) %>% relocate(age,type,state) %>% as.tibble() %>% return()
}
unmelt_age <- function(melted){
  dcast(melted, state + type + year~age) %>% relocate(year,type,state) %>% as.tibble() %>% return()
}
to_age <- function(dat){
  if(colnames(dat)[1] == "year")
  {
    return(dat)
  }
  dat %>% 
    melt_dat("year") %>% 
    unmelt_age() %>% 
    return()
}
to_year <- function(dat){
  if(colnames(dat)[1] == "age")
  {
    return(dat)
  }
  dat %>% 
    melt_dat("age") %>% 
    unmelt_year() %>% 
    return()
}
divide <- function(x1,x2) {
  return(x1/x2)
}
#-----

data_organized <-
  data_organized %>% 
  rbind(
    slicer_func(
      filter(data_organized, type == "patients"),
      filter(data_organized, type == "population"),
      func = "/",
      name = "ptpercap"
      )
    ) %>% 
  arrange(state, type, age)

data_organized <-
  data_organized %>% 
  to_age()

# data_organized <-
#   data_organized %>% 
#   rbind(
#     data_organized %>% 
#       filter(type == "ptpercap") %>%
#       mutate(across("<24":total, .fns = ~ .x/(.data[["<24"]] + .data[["65+"]]+ .data[["25-34"]]+ .data[["35-44"]]+ .data[["45-54"]]+ .data[["55-64"]]))) %>% 
#       mutate(type = "relptpercap") #%>% View()
#   ) %>% 
#   arrange(state, type, year) # %>% View()

data_organized <-
  data_organized %>% 
  rbind(
    data_organized %>% 
      filter(type == "ptpercap") %>%
      mutate(across("<24":total, .fns = ~ .x/(total))) %>% 
      mutate(type = "relptpercap") # %>% View()
  ) %>% 
  arrange(state, type, year) # %>% View()

data_organized <-
  data_organized %>% 
  to_age()

data_organized <-
  data_organized %>% 
  rbind(
    data_organized %>% 
      filter(type == "ptpercap") %>%
      mutate(across("<24":total, .fns = ~ .x/.data[["<24"]])) %>% 
      mutate(type = "youngratioptpercap")
  ) %>% 
  arrange(state, type, year) # %>% View()

data_organized <-
  data_organized %>% 
  to_year()

data_organized <-
  data_organized %>% 
  rbind(
    data_organized %>% 
      filter(type == "youngratioptpercap") %>%
      mutate(`2021` = `2021` - `2020`) %>% 
      mutate(`2020` = `2020` - `2019`) %>% 
      mutate(`2019` = `2019` - `2018`) %>% 
      mutate(`2018` = NA) %>% 
      mutate(type = "youngratioyeardiff") #%>% View()
  ) %>% 
  arrange(state, type, age) # %>% View()

#old debug-----------------------
# func <- "/"
# name <- "test"
# dat1 <- filter(data_organized, type == "patients")
# dat2 <- filter(data_organized, type == "population")
# cutoff <- 3
# change_col <- 2

