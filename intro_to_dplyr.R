library(dplyr)

# Data: starwars

# tbl of characters
dim(starwars)
starwars

# single table verbs

# filter
starwars %>%
  filter(skin_color == "light", eye_color=="brown")

# arrange
starwars %>% 
  arrange(height, mass)

starwars %>% 
  arrange(desc(height))

# choose rows
starwars %>%
  slice(5:10)

starwars %>%
  slice_tail(n=5)

starwars %>%
  slice_sample(n=5)
starwars %>%
  slice_sample(prop=0.1)
starwars %>%
  filter(!is.na(height)) %>%
  slice_max(height, n=3)

# select
starwars %>%
  select(hair_color)
starwars %>%
  select(hair_color:eye_color)
starwars %>%
  select(!(hair_color:eye_color))
starwars %>%
  select(ends_with("color"))

# mutate
starwars %>%
  mutate(height_m = height/100) %>%
  relocate(height_m)

starwars %>%
  mutate(height_m = height/100) %>%
  select(height_m, height, everything())

starwars %>%
  mutate(
    height_m = height/ 100,
    BMI = mass/ (height_m^2)
    
  ) %>%
  select(BMI, everything())

starwars %>%
  transmute(
    height_m = height/ 100,
    BMI = mass/ (height_m^2)
    
  ) 

# change col order
starwars %>%
  relocate(sex:homeworld, .before = height)

# summarize
starwars %>%
  summarise(height = mean(height, na.rm=T))

# group
group_by(starwars, sex) %>%
  summarize(height_mean = mean(height, na.rm=T))

group_by(starwars, height_binned = cut(height, 3)) %>%
  relocate(height_binned, everything())

starwars %>%
  select(height) %>%
  arrange(height)
