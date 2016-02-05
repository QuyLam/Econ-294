#' Quy Lam
#' Winter 2016
#' Assignment 3
#' 


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 0 
print("Quy Lam")
print(1505123)
print("qlam@ucsc.edu")


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 1
library(foreign)
df.ex <- read.dta(
  "https://github.com/EconomiCurtis/econ294_2015/raw/master/data/org_example.dta"
)
#loaded this way, it's a data frame
class(df.ex)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 2 Filter
require(dplyr)
df.ex.2 <- df.ex %>%
  dplyr::filter(
    year == 2013 & month == 12
  )
print(nrow(df.ex.2))

df.ex.2 <- df.ex %>%
  dplyr::filter(
    year == 2013 & (month == 7 | month == 8 | month == 9)
  )
print(nrow(df.ex.2))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 3.Arrange
df.ex.3a <- df.ex.2 %>% arrange(year,month)

# 4.Select
df.ex.4a <- df.ex.3a %>% select(year:age)
df.ex.4b <- df.ex.3a %>% select(year,month,starts_with("i"))
print(distinct(select(df.ex,state)))

# 5.Mutate
# 1st function stndz & returns the standard scores 

stndz <- function(x){
  (x - mean(x, na.rm = T))/ sd(x, na.rm = T)
}

#2nd function nrmlz & returns the feature scaled value

nrmlz <- function(x){
  (x - min(x, na.rm = T)) / (max(x, na.rm = t) - min(x, na.rm = T))
}
#nrmlz = function(x) (x - min(x,na.rm = T))/diff(range(x, na.rm = T))

df.ex.5a <- df.ex %>% 
  mutate(
    rw.stndz = stndz(rw),
    rw_nrmlz = nrmlz(rw)
  ) 


df.ex.5b <- df.ex.5a %>% 
  group_by(year,month) %>%   
  mutate(
    rw.stndz = stndz(rw),
    rw_nrmlz = nrmlz(rw),
    count       = n()
  ) %>%
  select(year,month,rw,rw.stndz,rw_nrmlz,count)

print(head(df.ex.5b)) #for double check

# 6.Summarize
df.ex.6 <- df.ex %>% 
  group_by(year,month,state) %>%               
  summarise(
    rw_min    = min(rw, na.rm = T),
    rw_1stQnt = quantile(rw, 0.25,na.rm = T),
    rw_avg    = mean(rw, na.rm = T),
    rw_median = median(rw, na.rm = T),
    rw_3rdQnt = quantile(rw, 0.75,na.rm = T),
    rw_max    = max(rw, na.rm = T),
    rw_count  = n()
  )

print(nrow(df.ex.6)) #double check

#find the year, month, state combination with the highest mean real wage
print(df.ex.6 %>%
  select(year,month,state,rw_avg) %>%
  arrange(rw_avg) %>% 
    tail(1)
)
print(
    paste(
        "year 2013,",
        "month 12,",
        "state DC",
        "has the highest mean real wage"
    )
)

# 7.
df <-df.ex %>% arrange(year,month,desc(as.character(df.ex$state)))










