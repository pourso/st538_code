library(sparklyr)

# connect to local session
sc = spark_connect(master="local")

# https://spark.rstudio.com/get-started/read-data.html
# copy from R env to Spark
tbl_mtcars = copy_to(sc, mtcars, "spark_mtcars")

# disconnect
spark_disconnect(sc)

# working w/ files
# Do not run the next following command. It is for example purposes only.
#spark_read_csv(sc, name = "test_table",  path = "/test/path/test_file.csv")


#https://spark.rstudio.com/guides/dplyr.html

#flights data
library(dplyr)
library(ggplot2)

# connect to local session
sc = spark_connect(master="local")

flights_tbl = copy_to(sc, nycflights13::flights, "flights")

airlines_tbl = copy_to(sc, nycflights13::airlines, "airlines")

# when connected to Spark DF, dplyr converts verbs to Spark SQL
# select() ~ SELECT
# filter() ~ WHERE
# arrange() ~ ORDER
# summarise() ~ aggregators: sum, min, sd, etc.
# mutate() ~ operators: +, *, log, etc.

select(flights_tbl, year:day, arr_delay, dep_delay)

filter(flights_tbl, dep_delay>1000)

arrange(flights_tbl, desc(dep_delay))

summarise(
  flights_tbl,
  mean_dep_delay = mean(dep_delay, na.rm=T)
)

mutate(flights_tbl, speed = distance / air_time * 60)

# laziness: 
#
# It never pulls data into R unless you explicitly ask for it.
# 
# It delays doing any work until the last possible moment: it collects together everything you want to do and then sends it to the database in one step.

c1 <- filter(
  flights_tbl, 
  day == 17, month == 5, carrier %in% c('UA', 'WN', 'AA', 'DL')
)
c2 <- select(c1, year, month, day, carrier, dep_delay, air_time, distance)
c3 <- mutate(c2, air_time_hours = air_time / 60)
c4 <- arrange(c3, year, month, day, carrier)
c4

# piping
c4 <- flights_tbl %>%
  filter(month == 5, day == 17, carrier %in% c('UA', 'WN', 'AA', 'DL')) %>%
  select(carrier, dep_delay, air_time, distance) %>%
  mutate(air_time_hours = air_time / 60) %>% 
  arrange(carrier) 
c4

# grouping
flights_tbl %>% 
  group_by(carrier) %>%
  summarize(
    count = n(), 
    mean_dep_delay = mean(dep_delay, na.rm = FALSE)
  )

# collecting to R
carrierhours = collect(c4)
with(carrierhours, pairwise.t.test(air_time, carrier))

carrierhours %>% 
  ggplot() + 
  geom_boxplot(aes(carrier, air_time_hours))

# SQL translation
# Rank each flight within a daily
ranked <- flights_tbl %>%
  group_by(year, month, day) %>%
  select(dep_delay) %>% 
  mutate(rank = rank(desc(dep_delay)))

ranked

dplyr::show_query(ranked)

# performing joins: 3 families of verbs
#
# Mutating joins, which add new variables to one table from matching rows in another.
# 
# Filtering joins, which filter observations from one table based on whether or not they match an observation in the other table.
# 
# Set operations, which combine the observations in the data sets as if they were set elements.

flights_tbl %>% 
  left_join(airlines_tbl, by = "carrier") %>% 
  select(name, flight, dep_time)

# sampling
sample_n(flights_tbl, 10) %>% 
  select(1:4)

sample_frac(flights_tbl, 0.01) %>% 
  count()

# hive functions
flights_tbl %>% 
  mutate(
    flight_date = paste(year,month,day,sep="-"),
    days_since = datediff(current_date(), flight_date)
  ) %>%
  group_by(flight_date,days_since) %>%
  count() %>%
  arrange(-days_since)

spark_disconnect(sc)

#https://spark.rstudio.com/guides/mlib.html

# can access routines in spark.ml package

# 3 families of functions
#
# Machine learning algorithms for analyzing data (ml_*)
# Feature transformers for manipulating individual features (ft_*)
# Functions for manipulating Spark DataFrames (sdf_*)

# analytic workflow could be composed as:
#
# Perform SQL queries through the sparklyr dplyr interface
# Use the sdf_* and ft_* family of functions to generate new columns, or partition your data set
# Choose an appropriate machine learning algorithm from the ml_* family of functions to model your data
# Inspect the quality of your model fit, and use it to make predictions with new data.
# Collect the results for visualization and further analysis in R

sc = spark_connect(master="local")
iris_tbl = copy_to(sc, iris, "iris", overwrite = T)
iris_tbl

# linear regression
lm_model = iris_tbl %>%
  ml_linear_regression(Petal_Length ~ Petal_Width)

spark_slope = coef(lm_model)[["Petal_Width"]]
spark_intercept = coef(lm_model)[["(Intercept)"]]

iris_tbl %>%
  select(Petal_Width, Petal_Length) %>%
  collect() %>%
  ggplot(aes(Petal_Length, Petal_Width)) +
  geom_point(aes(Petal_Width, Petal_Length), size = 2, alpha = 0.5) +
  geom_abline(aes(
    slope = spark_slope,
    intercept = spark_intercept
  ),
  color = "red"
  ) +
  labs(
    x = "Petal Width",
    y = "Petal Length",
    title = "Linear Regression: Petal Length ~ Petal Width",
    subtitle = "Use Spark.ML linear regression to predict petal length as a function of petal width."
  )

# logistic regression
glm_model <- iris_tbl %>% 
  mutate(is_setosa = ifelse(Species == "setosa", 1, 0)) %>% 
  select_if(is.numeric) %>% 
  ml_logistic_regression(is_setosa ~.)

summary(glm_model)

ml_predict(glm_model, iris_tbl) %>% 
  count(Species, prediction) 

spark_disconnect(sc)

# https://spark.rstudio.com/guides/distributed-r
# spark_apply() applies R functio to Spark object (df)
# objects are partitioned by default, but partitions can be specified
# applied function must return another Spark DF

# apply an R function to spark obj
library(sparklyr)

sc <- spark_connect(master = "local")

sdf_len(sc, 5, repartition = 1) %>%
  spark_apply(function(e) I(e))

sdf_len(sc, 10, repartition = 1) %>%
  spark_apply(function(e) class(e))

trees_tbl <- sdf_copy_to(sc, trees, repartition = 2)

spark_apply(
  trees_tbl,
  function(e) nrow(e), names = "n"
)

spark_apply(trees_tbl, function(e) head(e, 1))

spark_apply(trees_tbl, function(e) scale(e))

spark_apply(trees_tbl, function(e) lapply(e, jitter))

spark_apply(
  trees_tbl,
  function(e) data.frame(2.54 * e$Girth, e), names = c("Girth(cm)", colnames(trees))
)

# group by
iris_tbl <- sdf_copy_to(sc, iris)

spark_apply(iris_tbl, nrow, group_by = "Species")

iris_tbl %>%
  spark_apply(
    function(e) summary(lm(Petal_Length ~ Petal_Width, e))$r.squared,
    names = "r.squared",
    group_by = "Species"
  )

# distributing packages
spark_apply(
  iris_tbl,
  function(e) broom::tidy(lm(Petal_Length ~ Petal_Width, e)),
  names = c("term", "estimate", "std.error", "statistic", "p.value"),
  group_by = "Species"
)

spark_disconnect(sc)
