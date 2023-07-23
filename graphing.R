data_organized <-
  data_organized %>% 
  to_age()

data_organized <-
  data_organized %>% 
  melt_dat("age")

data_organized %>% 
  filter(type == "relptpercap") %>% 
  filter(age == "<24") %>% # View()
  ggplot(mapping = aes(x=year, y = value, fill = age)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ state)

data_organized %>% 
  filter(type == "relptpercap") %>% 
  filter(age != "total") %>% 
  ggplot(mapping = aes(x=year, y = value, fill = age)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ state)

data_organized <-
  data_organized %>% 
  unmelt_age()


#todo: 6 maps of us, each showing change of per capita prescription growth relative to 2018
#also write methods section