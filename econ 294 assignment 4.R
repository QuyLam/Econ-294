

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 0 

print("Quy Lam")
print(1505123)
print("qlam@ucsc.edu")


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 1 

library(foreign)

## Load files from github

flights <- read.csv("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/flights.csv",
  stringsAsFactors = FALSE,
  header = T
)

planes <- read.csv(
  "https://github.com/EconomiCurtis/econ294_2015/raw/master/data/planes.csv",
  stringsAsFactors = FALSE,
  header = T
)

airports <- read.csv(
  "https://github.com/EconomiCurtis/econ294_2015/raw/master/data/airports.csv",
  stringsAsFactors = FALSE,
  header = T
)

weather <- read.csv(
  "https://github.com/EconomiCurtis/econ294_2015/raw/master/data/weather.csv",
  stringsAsFactors = FALSE,
  header = T
)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 2
require(dplyr)

#convert date column from type char to type date

weather <- weather %>%
  mutate(date = as.Date(date))

flights <- flights %>%
  mutate(date = as.Date(date))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 3

#all flights went to city of San Francisco & Oakland CA 

flights.2a <- subset(flights, dest == "SFO" | dest == "OAK")
print(nrow(flights.2a))

#all flights delayed by an hour or more

flights.2b <- subset(flights, dep_delay + arr_delay >= 60)
print(nrow(flights.2b))

#all flights which arrival delay was more than twice departure delay
flights.2c <- subset(flights, arr_delay > ((2)*dep_delay))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 4

flights.4a <- flights %>%
  dplyr::select(ends_with("delay"))

flights.4b <- flights %>%
  dplyr::select(starts_with("arr_"),starts_with ("dep_"))

flights.4c <- flights %>%
  dplyr::select(dep_delay:arr_delay)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 5
flights.5a <- flights %>%
  dplyr::arrange(desc(dep_delay))
print(head(flights.5a,5))


flights.5b <- flights %>%
  dplyr::mutate(
    caughtup_lost = dep_delay - arr_delay)

flights.5b <- flights.5b %>%
  dplyr::arrange(desc(caughtup_lost))
print(head(flights.5b,5))



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 6

flights.6 <-mutate(flights,
    speed = dist /(hour + minute/60),
    delta = dep_delay-arr_delay
  )

##6a##
print(flights.6 %>%
  dplyr::arrange(desc(speed)) %>%
  head(5)
)

##6b##
print(flights.6 %>%
  dplyr::arrange(desc(delta)) %>%
  head(5)
)

##6c##
print(flights.6 %>%
  dplyr::arrange(delta) %>%
  head(5)
)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 7

#Grouping by carrier

flights.7a <- flights.6 %>%
  dplyr::group_by(carrier) %>%
  dplyr::summarise(
    n_canc = sum(cancelled),
    p_canc = mean(cancelled),
    delta.min = min(delta, na.rm = T),
    delta.1stq = quantile(delta, 0.25, na.rm = T),
    delta.mean = mean(delta, na.rm = T),
    delta.median = median(delta, na.rm = T),
    delta.3rdq = quantile(delta, 0.75, na.rm = T),
    delta.90th = quantile(delta, 0.90, na.rm = T),
    delta.max = max(delta, na.rm = T),
    count = n()
  )
print(flights.7a %>%
  dplyr::ungroup() %>%
  dplyr::arrange(desc(p_canc)
  )
)

flights.7b <- flights.6 %>%
  dplyr::group_by(date,hour) 
  
day_delay <- flights %>%
  filter(!is.na(dep_delay)) %>%
  group_by(date) %>%
  summarize(delay = mean(dep_delay),n = n()) %>%
  filter(n > 10)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 8

day_delay <- day_delay %>%
  mutate(diff = delay - lag(delay))

print(day_delay %>%
        arrange(desc(diff)) %>%
        head(5)
)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 9

dest_delay <- flights %>%
  group_by(dest) %>%
  summarise(
  average_arrde = mean(arr_delay, na.rm = T),
  count = n()
  )

airports <- airports %>%
  select(city,state,lat,long, dest = iata , name = airport)

df.9a <- dest_delay %>%
  left_join(airports,
            by = "dest")
#print out 5 city & state
df.9a1 <- df.9a %>%
        arrange(desc(average_arrde)) %>% 
        head(5)

print(data.frame(df.9a1$city,df.9a1$state))


df.9b <- dest_delay %>% inner_join(airports)
print("2 data have same number of observations")
df.9c <- dest_delay %>% right_join(airports)
print(nrow(df.9c))
print("there are 3376 observations in data set +there are NAs in df.9c because right_join include all observations in airport which are match or not match in dest_delay.NAs is observation available in airports but dest_delay doesn't have")

df.9d <- dest_delay %>% full_join(airports) 
print(nrow(df.9d))
print("there are 3378 observations in dataset +there are NAs in df.9d,2 dataset join whether observation matching perfectly or not")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 11

install.packages("tidyr")
require(tidyr)
df <- data.frame(treatment = c("a", "b"), subject1 = c(3, 4), subject2 = c(5, 6))
df


data.frame(
  subject = c(1,1,2,2),
  treatment = c("a","b","a","b"),
  value = c(3,4,5,6)
)

df <- df %>%
  gather(subject,value,2:3) %>%
  mutate(
    subject = c(1,1,2,2)) %>%
  select(subject,treatment,value)

  df <- df %>% 
  spread(subject,value) %>%
  rename(subject1 = `1`, subject2 = `2`)

###11C
 df <- data.frame(
      subject = c(1,2,3,4),
      demo = c("f_15_CA","f_50_NY","m_45_HI","m_18_DC"),
      value = c(3,4,5,6)
  )
  
  df1 <- df %>%
    separate(
      demo, #column to separate
      c("sex","age","state"), #name of new column(s)
      sep = "_" # read "sep" argument, if numeric: index position to split at.
    )
View(df1)
####11d
  df <- data.frame(
    subject = c(1,2,3,4),
    sex = c("f","f","m",NA),
    age = c(11,55,65,NA),
    city = c("DC","NY","WA",NA),
    value = c(3,4,5,6)
  )

df2 <- df %>%
    unite(
      col = demo, 
      ... = sex, age, city, 
      sep = "." 
    ) 
View(df2)
