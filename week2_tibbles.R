# 10.1 intro

# tibble package included 
library(tidyverse)

# 10.2 creating tibbles

# coerce
as_tibble(iris)

# create new
tibble(x = 1:5,
       y = 1,
       z = x^2 + y
       )

tb = tibble(
  `:)` = "smile",
  ` ` = "space",
  `2000` = "number"
)
tb

tribble(
  ~x, ~y, ~z,
  #-|---|---|
  "a", 2, 3.6
)

# 10.3 tibbles vs df

# 10.3.1 printing
tibble(
  a = lubridate::now() + runif(1e3) * 86400,
  b = lubridate::today() + runif(1e3) * 30,
  c = 1:1e3,
  d = runif(1e3),
  e = sample(letters, 1e3, replace = TRUE)
)

nycflights13::flights %>% 
  print(n = 10, width = Inf)

nycflights13::flights %>% 
  View()

# 10.3.2 subsetting

df <- tibble(
  x = runif(5),
  y = rnorm(5)
)

# Extract by name
df$x

df[["x"]]

# Extract by position
df[[1]]

df %>% .$x

df %>% .[["x"]]

# 10.4 interacting w/ older code

# use as.data.frame() to revert tibble to df
class(as.data.frame(tb))

# 10.5 exercises

df <- data.frame(abc = 1, xyz = "a")
df$x # partial matching
df[, "xyz"] # converts to factor
df[, c("abc", "xyz")] 


df_ <- tibble(
  abc = 1, 
  xyz = "a")
df_$xyz
df_[["xyz"]]
df_[c("abc", "xyz")] # diff syntax for single col vs mult col

df_tb <- tibble(
  x = runif(5),
  y = rnorm(5)
)

# 3 
var = "mpg"
mt_tb = as_tibble(mtcars)
mt_tb[var] # returns tibble
mt_tb[[var]] # returns vector

# 4
annoying <- tibble(
  `1` = 1:10,
  `2` = `1` * 2 + rnorm(length(`1`))
)

annoying$`1`
annoying[["1"]]

