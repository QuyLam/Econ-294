# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 0 
print("Quy Lam")
print(1505123)
print("qlam@ucsc.edu")


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 1

library(ggplot2)
#####1a###

ggplot(diamonds, aes(x*y*z, price, color=clarity, size=carat)) + 
  geom_point() + 
  scale_x_log10() + 
  scale_y_log10()

#####1b###

ggplot(diamonds, aes(carat,fill = clarity)) +
  geom_histogram(aes(y = ..density..), bins = 25) + ylab("Density") +
  facet_grid(cut ~ .) 

#####1c##

ggplot(diamonds, aes(cut,price)) +
  geom_violin() +
  geom_jitter(alpha = 0.02)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 3
#####3a##
library(foreign)
library(dplyr)

org_example <- read.dta(
  file = "https://github.com/EconomiCurtis/econ294_2015/raw/master/data/org_example.dta"
)

###
ggplot(data = (
  org_example %>%
    group_by(month,year) %>%
    summarize(
      Median.RW = median(rw,na.rm = TRUE),
      rw.1stq = quantile(rw, 0.25, na.rm = T),
      rw.3rdq = quantile(rw, 0.75, na.rm = T),
      rw.1stdec = quantile(rw, 0.1, na.rm = TRUE),
      rw.9thdec = quantile(rw, 0.9, na.rm = TRUE)) %>%
    mutate(
      date = paste(year, month, "01", sep = "-"),
      date = as.Date(date, format = "%Y-%m-%d"))
), 
  aes(date,Median.RW)) +
  geom_ribbon(aes(ymin = rw.1stdec, ymax = rw.9thdec), alpha = 0.2) +
  geom_ribbon(aes(ymin = rw.1stq, ymax = rw.3rdq), alpha = 0.2) +
  geom_line() +
  ylim(0,50)

######3b##

ggplot( data = (
   org_example %>%
      group_by(month, year,educ) %>%
      summarize(
        Median.RW = median(rw, na.rm = TRUE)) %>%
     mutate(
       date = paste(year, month, "01", sep = "-"),
       date = as.Date(date, format = "%Y-%m-%d"))
 ),aes(x = date, y = Median.RW, color = educ)) +
  geom_line()

 

