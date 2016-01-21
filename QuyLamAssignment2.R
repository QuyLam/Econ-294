#Question 0
QuyLamAssignment2 <-list(
  firstName = "Quy",
  lastName = "Lam",
  email = "qlam@ucsc.edu",
  studentID = 1505123
)
########################################################################
#Question 1

library(foreign)
diamonds <- get(  
  load(
    file = url("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/diamonds.RData")
  )
)
QuyLamAssignment2$s1a <- nrow(diamonds)
QuyLamAssignment2$s1b <- ncol(diamonds)
QuyLamAssignment2$s1c <- names(diamonds)
QuyLamAssignment2$s1d <- summary(diamonds$price)

#Question 2
df.td <- read.table(
  file = "https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_TSV.txt",
  sep = "\t",
  header = T
)
QuyLamAssignment2$s2a <- print(4785)
QuyLamAssignment2$s2b <- ncol(df.td)
QuyLamAssignment2$s2c <- names(df.td)
QuyLamAssignment2$s2d <- mean(df.td$weight, na.rm = FALSE)
QuyLamAssignment2$s2e <- median(df.td$weight, na.rm = FALSE)
df.td$weight2 <- ifelse(df.td$weight < 996 | df.td$weight > 999,
                       yes = df.td$weight,
                       no = NA)
#hist(df.td$weight2)
#hist(df.td$weight)
#table(df.td$weight)
QuyLamAssignment2$s2f <- mean(df.td$weight2, na.rm = TRUE)
QuyLamAssignment2$s2g <- median(df.td$weight2, na.rm = TRUE)
df_m <- subset(df.td,SEX == 1)
df_f <- subset(df.td,SEX ==2)
QuyLamAssignment2$s2h <- summary(df_f$weight2)
QuyLamAssignment2$s2i <- summary(df_m$weight2)
#Question 3
vec <-c(letters,LETTERS)
QuyLamAssignment2$s3a <- vec[seq(2, length(vec),2)]
QuyLamAssignment2$s3b <-paste(vec[c(43,21,25)],collapse = "")

arr <- array(c(letters,LETTERS),dim = c(3,3,3))
QuyLamAssignment2$s3c <- paste(arr[1,1,2],arr[2,1,2],arr[3,1,2], sep = "")
QuyLamAssignment2$s3d <- paste(arr[2,2,1],arr[2,2,2],arr[2,2,3], sep = "")
QuyLamAssignment2$s3e <- paste(arr[2,3,2],arr[3,1,3],arr[1,3,3], sep = "")


#Question 4
org_example <- read.dta(
  file = "http://people.ucsc.edu/~aspearot/Econ_217_Data/org_example.dta"
)
sort(unique(org_example$year))
sort(unique(org_example$month))
sort(unique(org_example$educ))
month <- aggregate(rw ~ month,org_example,mean)
year <- aggregate(rw ~ year,org_example,mean)
educ <- aggregate(rw ~ educ,org_example,mean)

