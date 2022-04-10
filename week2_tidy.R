# 12 Tidy data

# 12.1 intro

library(tidyverse)

# 12.2 tidy data
table1

table2

table3

table4a

table4b

table1 %>%
  count(year, wt = cases)

library(ggplot2)
ggplot(table1, aes(x=year, y=cases)) +
  geom_line(aes(group=country), color="grey50") +
  geom_point(aes(color=country))

# 12.2.1.2
table2

tb2 = as_tibble(cbind(
  table2[table2$type=="cases",c("country", "year", "count")],
  table2[table2$type=="population","count"]
), .name_repair = "minimal")
colnames(tb2) = c("country", "year", "cases", "population")
tb2$rate = tb2$cases / tb2$population * 10000
tb2

table4a
table4b

tb4=as_tibble(
  cbind(
  gather(table4a, key = "year", value = "cases", -country),
  gather(table4b, key = "year", value = "population", -country)[,"population"]
  )
)
tb4 = tb4[order(tb4$country, tb4$year),]
tb4$rate = tb4$cases / tb4$population * 10000
tb4

# 12.3 pivoting

# 12.3.1 longer

td4a = table4a %>%
  pivot_longer(c(`1999`, `2000`), names_to ="year", values_to = "cases")

td4b = table4b %>%
  pivot_longer(c(`1999`, `2000`), names_to ="year", values_to = "population")

left_join(td4a, td4b)

# 12.3.2 wider

table2 %>%
  pivot_wider(id_cols = c(country, year), names_from = type, values_from = count)


stocks <- tibble(
  year   = c(2015, 2015, 2016, 2016),
  half  = c(   1,    2,     1,    2),
  return = c(1.88, 0.59, 0.92, 0.17)
)


stocks %>% 
  pivot_wider(names_from = year, values_from = return) %>% 
  pivot_longer(`2015`:`2016`, names_to = "year", values_to = "return")

# 12.4 separating and uniting

# 12.4.1 separate
table3
table3 %>%
  separate(rate, into=c("cases", "population"), sep = "/", convert = T)

table3 %>%
  separate(year, into = c("century", "year"), sep=2)

# 12.4.2 unite
table3 %>%
  separate(year, into = c("century", "year"), sep=2) %>%
  unite(col = "year", century:year, sep='')

# 12.5 missing values

stocks <- tibble(
  year   = c(2015, 2015, 2015, 2015, 2016, 2016, 2016),
  qtr    = c(   1,    2,    3,    4,    2,    3,    4),
  return = c(1.88, 0.59, 0.35,   NA, 0.92, 0.17, 2.66)
)
stocks %>% 
  pivot_wider(names_from = year, values_from = return)

stocks %>%
  complete(year, qtr)

treatment <- tribble(
  ~ person,           ~ treatment, ~response,
  "Derrick Whitmore", 1,           7,
  NA,                 2,           10,
  NA,                 3,           9,
  "Katherine Burke",  1,           4
)

treatment %>%
  fill(person)
