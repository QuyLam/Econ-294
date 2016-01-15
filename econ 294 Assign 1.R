
firstName <- "Quy"
lastName <- "Lam"

print(
  paste(
    firstName,
    lastName
  )
)
studentID <- "1505123"
print(studentID)

#Question 1

library("foreign", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
df.dta <- read.dta (
  file = "https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_dta.dta"
)

df.csv <- read.csv(
  file ="https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_CSV.csv"
)

df.td <- read.table(
  file = "https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_TSV.txt"
)

load(url("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_RData.RData"
))
# The name os this RData file is NHIS_2007_RData

#Question 2
object.size(df.dta) # 463168 bytes
object.size(df.csv) #193048 bytes
object.size(df.td) #518544 bytes
object.size(NHIS_2007_RData) #193048 bytes
# The df.csv and NHIS_2007_RData have the smaller size. 


#Question 2

#Question 3
typeof(df.dta) #
class(df.dta) #data.frame
length(df.dta) #9
#print(length(df.dta))
dim(df.dta) #4785 9
#print(dim(df.dta)) #4785 9


nrow(df.dta) #4785
ncol(df.dta) #9
summary(df.dta)

#Question 4
df <- read.dta(
  file = "https://github.com/EconomiCurtis/econ294_2015/raw/master/data/org_example.dta"
)
str(df) #'data.frame':	1119754 obs. of  30 variables

summary(df$rw) # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
              # 1.8    10.7    15.9    19.8    24.4   354.8  521279 

#Question 5
v <- c(1,2,3,4,5,6,7,4,NULL,NA)
# dont get the NULL

length(v) #9

mean(v,na.rm = TRUE)

#Question 6
x <- matrix(c(1,2,3, 4,5,6, 7,8,9), nrow = 3, ncol = 3, byrow = TRUE)
t(x) #transpose
eigen(x)

y <- matrix(c(1,2,3,3,2,1,2,3,0),nrow = 3, ncol = 3, byrow = TRUE)
solve(y) #square matrix
solve(y,y) #identity matrix
#Question 7



carat <- c(5,2,0.5,1.5,5,NA,3)
cut <- c(c("“fair”", "“good”","“very good”","“good”","“fair”","“Ideal”","“fair”"))
clarity <- I(c("“SI1”", "“I1”", "“VI1”", "“VS1”","“IF”","“VVS2”","NA"))
#price <- c(850, 450, 450, "NULL", 750, 980, 420)
price <- as.factor(c("850", "450", "450", "NULL", "750", "980", "420"))
price <- as.numeric(as.character(price))
price[is.na(price)] <- 0
diamonds <- data.frame(carat, cut, clarity, price)
View(diamonds)
str(diamonds)
mean(price)

aggregate(price ~ cut,diamonds,mean) # the mean price of cut "fair"
#The mean price of cut "good" is 225, of cut "very good" is 450, "Ideal" is 980













