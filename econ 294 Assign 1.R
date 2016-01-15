
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
#load data
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
print("The name of this RData file is NHIS_2007_RData")

#Question 2
object.size(df.dta) 
print("size of df.dta is 463168 bytes or 463.168KB")
object.size(df.csv) 
print("size of df.csv is 193048 bytes or 193.048KB")
object.size(df.td) 
print("size of df.td is 518544 bytes or 518.544KB")
object.size(NHIS_2007_RData) 
print("size of NHIS_2007_RData is 193048 bytes or 193.048KB")
print("The df.csv and NHIS_2007_RData file have the smallest size. Besides the .dta file,the format of data inside these files are different so these files have different sizes.")

#Question 3
df.rdata <- (NHIS_2007_RData)
typeof(df.rdata) 
print("The typeof df.rdata is list")
class(df.rdata) 
print("The class of df.rdata is data.frame")

length(df.rdata) 
print("The length of the data is 9")
dim(df.rdata)
print("Report dimension of data, df.rdata has 4785 observations and 9 variables")
nrow(df.rdata)
print("Report number of rows in data, there are 4785 rows in df.rdata")
ncol(df.rdata)
print("Report number of columns in data, there are 9 columns in df.rdata ")
summary(df.rdata)


#Question 4
#load org_example.dta
df <- read.dta(
  file = "https://github.com/EconomiCurtis/econ294_2015/raw/master/data/org_example.dta"
)
str(df) # report the str
print("There are 1119754 observations of  30 variables ")
summary(df$rw)
print("The min of variable (column) rw is 1.8, the mean of variable rw is 19.8,the median of variable rw is 15.9,the max is 354.8, the first quartile value is 10.7, and the third quartile value is 24.4")
print("There are 521279 NAs ")

#Question 5
v <- c(1,2,3,4,5,6,7,4,NULL,NA)

length(v)
print("The length of vector v is 9.The number of values in vector don't match the number reported in length because R doesn't see NULL as a value in vector")
mean(v,na.rm = TRUE)
print("mean of v when ignoring the NA value is 4")

#Question 6
x <- matrix(c(1,2,3, 4,5,6, 7,8,9), nrow = 3, ncol = 3, byrow = TRUE)
print(x)

T <- t(x) #transpose
print(T)

eigen(x) # find the eigenvalues and the eigenvectors of x
#create matrix y
y <- matrix(c(1,2,3,3,2,1,2,3,0),nrow = 3, ncol = 3, byrow = TRUE)
print(y) 
#find inverse of y
y_inverse <- solve(y) 
#muliple y and y_inverse
I <- y %*%y_inverse
print(I)
print("the new matrix I is the identity matrix")

#Question 7
# create a data frame called diamonds
carat <- c(5,2,0.5,1.5,5,NA,3)
cut <- c(c("“fair”", "“good”","“very good”","“good”","“fair”","“Ideal”","“fair”"))
clarity <- I(c("“SI1”", "“I1”", "“VI1”", "“VS1”","“IF”","“VVS2”","NA"))
price <- c(850, 450, 450, NA, 750, 980, 420)
diamonds <- data.frame(carat, cut, clarity, price)
View(diamonds) # view the diamonds data frame
mean(price,na.rm=TRUE)
print("the mean price is 650")
#aggregate(price ~ cut,diamonds,mean) # the mean price of cut "fair"
#The mean price of cut "good" is 225, of cut "very good" is 450, "Ideal" is 980
subs <- subset(diamonds,cut=="“fair”") #create subset of data frame have cut "fair"
mean(subs$price,na.rm=TRUE)
print("the mean price of cut fair is 673.3333")
subs1 <- subset(diamonds,cut!="“fair”") #create subset contains cut "good", "very good", "Ideal"
mean(subs1$price,na.rm=TRUE)
print("the mean price of cut: good, very good, Ideal is 626.6667")
subs2 <- subset(diamonds,carat>2 & cut=="“good”"||cut=="“Ideal”") 
mean(subs2$price,na.rm=TRUE)
print("first, creating the subset named subs2 with diamonds greater than 2 carats, and cut Ideal or very good. Then find the mean.R can't find the mean because there is no observation in subs2 or no diamonds satisfied those conditions.")









