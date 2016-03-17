library("dplyr")  #for dplyr
library("RSQLite") #for sqllite
library("nycflights13") 
library("ggplot2")

#load nycflights13_sqlite data
my_db <- nycflights13_sqlite()

# creates `flights_sqlite` db table
flights <- left_join(
  tbl(my_db, "flights"),
  tbl(my_db, "weather"),
  c("year", "month", "day","hour","origin")) %>%
  collect() %>%
  mutate(canceled = is.na(arr_time))

##PartA :Weather

#create a subset of flights
a_data<- flights%>%
  select(
    dep_delay, temp:visib
  )

#create correlation matrix : correlation between weather and arr_delay
corr_matrix<-cor(a_data,use = "complete")

#find the maximum correlation 
sort(abs(cor(corr_matrix)[, 1]), decreasing = TRUE)[2:3]
cat("We can see that pressure and visib have strongest correlation with arr_delay, so we can plot pressure and arr_delay to see their relationship with color determined by color")

#plot pressure and arr_delay
plot_1<-ggplot(flights,aes(pressure,dep_delay,color=visib))
plot_1+geom_point()+
labs(title="Figure 1: Plot Pressure vs Dep_Delay (color by visib)", x="Pressure", y="Departure Delay") 

print("Through figure 1, we can say there are relationship between Pressure and Departure Delay.")

#plot pressure and canceled
plot_2<-ggplot(flights,aes(x=canceled,y=pressure,color=visib))
plot_2+geom_point()+
labs(title="Figure 2: Plot Pressure vs Canceled", x="Canceled", y="Pressure") 
print("Based on figure 2, we can say the effect of pressure on number of canceled flights is not significant") 

# run regression to test weather effect on dep_delay

summary(lm(dep_delay~temp+humid+wind_dir+wind_speed+precip+
              pressure+visib,data = a_data))
cat("Based on result we can see that temperature, wind_dir, pressure and visib have negative effect on departure delay and there are all statistically significant at 99% level.")

##Part B

plot_3<-ggplot(flights,aes(month,dep_delay,color=dep_time))
plot_3+geom_point()+
labs(title="Figure 3: plot Departure Delay vs Departure Time", x="Departure Delay", y="Departure Time") 
cat("plot to have a look on relationship between dep_delay and dep_time.")

#run regression of dep_delay on variables time
summary(lm(flights$dep_delay ~ flights$month+flights$day+flights$hour+flights$minute))
cat("Based on regression result, month, hour and minute all have affect on dep_dely and all statistically significant at the 99% confidence level. Day isn't statistically significant.It seems like month and minute have negative effect on deplay while hour has positive effect.")

#run regression of canceled on variables time
summary(glm(canceled~month+day+hour+minute, data = flights))
cat("in all varianles, only hour has significant statistically effect on canceled.")

#plot deplay and hour
ggplot(data = flights, aes(x = dep_delay, y = hour)) + 
scale_x_log10()  + geom_point( alpha = 1) + 
scale_size(range = c(1,10)) +
labs(title="Figure 4: plot Departure Delay vs Hour", x="Departure Delay in minutes", y="Hour of day") 
cat("Based on the graph we can see between midnight and 5 A.M has least change delay occured,the delay time begin to increase after 5 am. This may because more number of flights in daylight than in midnight to earlier morning.")
###PART c

model_origin <- aov(arr_delay~origin,flights)
anova(model_origin)

summary(lm(arr_delay~origin,flights))

model_interaction <- aov(arr_delay~origin*dest,flights)
anova(model_interaction)

#plot relationship between origin and dep_delay
plot_5<-ggplot(flights,aes(origin,dep_delay))
plot_5+geom_point()+
labs(title="Figure 5: plot Origin vs Departure Delay", x="Origin", y="Departure Delay") 


#regress dep_delay on distance and airtime
lm<- lm(flights$dep_delay~flights$distance+flights$air_time)
summary(lm)
cat("Airtime has negative effect on dep_delay and its statistically significant at 99%. Distance have smaller in magniture positive effect on dep_delay.")

# create a small size
data_clean<-na.omit(flights)
flight.index <- sample(1:nrow(data_clean),261,replace=FALSE)
flights_sample<-data_clean[flight.index,]

#plot relationship between dep_delay and distance
plot(y = flights_sample$dep_delay,x = flights_sample$distance, pch=21, bg="darkviolet", main="Figure 6:Distance Traveled by Flight vs. Departure Delay [in minutes]", ylab = "Departure Delay [in minutes]", xlab = "Distance Traveled by Flight (in miles)")

###part D
d_data<-left_join(
    tbl(my_db, "flights"),
    tbl(my_db, "planes"),
    by = "tailnum")%>% 
  collect() %>%
  mutate(canceled = is.na(arr_time))

##plot relationship between dep_delay and carrier
ggplot(d_data,aes(x = d_data$carrier,y = d_data$dep_delay)) +geom_point() +
labs(title="Figure 6: plot Departure Delay vs Carrier", x="Carrier", y="Departure Delay") 


##regress dep_delay to engine
lm(d_data$dep_delay~d_data$engine)
summary(lm(d_data$canceled~d_data$engine))


