library("dplyr")  #for dplyr
library("RSQLite") #for sqllite
library("nycflights13") 
library("ggplot2")

my_db <- nycflights13_sqlite()

# creates `flights_sqlite` db table
flights <- left_join(
  tbl(my_db, "flights"),
  tbl(my_db, "weather"),
  c("year", "month", "day","hour","origin")) %>%
  collect() %>%
  mutate(canceled = is.na(arr_time))
 
a_data<- flights%>%
  select(
    arr_delay, temp:visib
  )

#create correlation matrix : correlation between weather and arr_delay
corr_matrix<-cor(a,use = "complete")

#find the maximum correlation 
sort(abs(cor(corr_matrix)[, 1]), decreasing = TRUE)[2:3]

#plot pressure and arr_delay
plot_1<-ggplot(flights,aes(pressure,arr_delay,color=visib))
plot_1+geom_point()+
labs(title="Figure 1: plot pressure vs arrival delay(color by visib", x="Pressure", y="Arrive Delay") 

#plot pressure and canceled
plot_2<-ggplot(flights,aes(x=canceled,y=pressure,color=visib))
plot_2+geom_point()+
labs(title="Figure 2: plot pressure vs canceled", x="Canceled", y="Pressure") 
  

ggplot(flights, aes(arr_delay)) +
  geom_histogram(aes(y = ..density..), bins = 25) + ylab("Density") 


head(weather)
avgdelay <- flights %>%
  group_by(month, day) %>%
  filter(month < 13) %>%
  summarise(avgdelay = mean(arr_delay, na.rm=TRUE)) 
precip <- weather %>%
  group_by(month, day) %>%
  filter(month < 13) %>%
  summarise(totprecip = sum(precip), maxwind = max(wind_speed))
precip <- mutate(precip, anyprecip = ifelse(totprecip==0, "No", "Yes"))
merged <- left_join(avgdelay, precip, by=c("day", "month"))
merged<-na.omit(merged)
plot_3<-ggplot(merged,aes(x=maxwind,y=avgdelay))
plot_3+geom_point()+
labs(title="Figure 3: plot maxwind vs avgdelay", x="Maxwind", y="Avgdelay") 


##Part B
b_data<- flights%>%
  select(
    month,dep_time:arr_delay
  )
corr_matrix_2<-cor(b_data,use = "complete")


sort(abs(cor(corr_matrix_2)[, 1]), decreasing = TRUE)[2:3]
plot_4<-ggplot(flights,aes(month,dep_delay,color=dep_time))
plot_4+geom_point()+
labs(title="Figure 4: plot Departure Delay vs Departure Time", x="Departure Delay", y="Departure Time") 


summary(lm(flights$dep_delay ~ flights$month+flights$day))

###PART c

model_origin <- aov(arr_delay~origin,flights)
anova(model_origin)

summary(lm(arr_delay~origin,flights))

model_interaction <- aov(arr_delay~origin*dest,flights)
anova(model_interaction)

#plot relationship between origin and arr_delay
plot_5<-ggplot(flights,aes(origin,arr_delay))
plot_5+geom_point()+
labs(title="Figure 5: plot Origin vs Arrival Delay", x="Origin", y="Arrival Delay") 


#regress arr_delay on distance and airtime
lm<- lm(flights$arr_delay~flights$distance+flights$air_time)
summary(lm)

# create a small size
data_clean<-na.omit(flights)
flight.index <- sample(1:nrow(data_clean),261,replace=FALSE)
flights_sample<-data_clean[flight.index,]

#plot relationship between arr_delay and distance
plot(y = flights_sample$arr_delay,x = flights_sample$distance, pch=21, bg="darkviolet", main="Figure 6:Distance Traveled by Flight vs. Arrival Delay [in minutes]", ylab = "Arrival Delay [in minutes]", xlab = "Distance Traveled by Flight (in miles)")

###part D
d_data<-left_join(
    tbl(my_db, "flights"),
    tbl(my_db, "planes"),
    by = "tailnum")%>% 
  collect() %>%
  mutate(canceled = is.na(arr_time))

##plot relationship between arr_delay and carrier
ggplot(d_data,aes(x = d_data$carrier,y = d_data$arr_delay)) +geom_point() +
labs(title="Figure 7: plot Arrival Delay vs Carrier", x="Carrier", y="Arrival Delay") 


##regress arr_delay to engine
lm(d_data$arr_delay~d_data$engine)
summary(lm(d_data$canceled~d_data$engine))


