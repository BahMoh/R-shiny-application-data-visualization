

# Business Analytics  with R -------------------------------------------------
# Mobile Project ---------------------

# Load required libraries
library(dplyr)
library(ggplot2)

# Generate sample sales data
set.seed(123)  # for reproducibility
start_date <- as.Date("2023-01-01")
end_date <- as.Date("2024-12-31")
num_days <- as.integer(end_date - start_date)

sales_data <- data.frame(
  Date = seq(start_date, by = "day", length.out = num_days),
  Product_ID = sample(1:5, num_days, replace = TRUE),
  Quantity = sample(1:10, num_days, replace = TRUE),
  Revenue = runif(num_days, min = 10, max = 100)
)

# Summary statistics of sales data
summary(sales_data)

# Visualize sales trends over time
ggplot(sales_data, aes(x = Date, y = Revenue)) +
  geom_line() +
  labs(title = "Sales Trends Over Time",
       x = "Date",
       y = "Revenue")

# Calculate monthly revenue
monthly_revenue <- sales_data %>%
  mutate(Month = format(Date, "%Y-%m")) %>%
  group_by(Month) %>%
  summarise(Total_Revenue = sum(Revenue))

# Visualize monthly revenue
ggplot(monthly_revenue, aes(x = Month, y = Total_Revenue)) +
  geom_bar(stat = "identity") +
  labs(title = "Monthly Revenue",
       x = "Month",
       y = "Total Revenue")


##Set work directory-----------------------------
setwd("C:/Users/gsstech/Desktop")
##Required libraries-----------------------------
install.packages("cluster")
library("cluster")                      #clustering
install.packages("e1071")
library("e1071")                        #classification
library("ggplot2")                      #visualization
install.packages("factoextra")
library("factoextra")                   #clustering visualization

##Read data from file----------------------------
# Segmentation

data<-read.csv(file.choose())
# data <- read.csv("segmentation.csv", header = TRUE)
names(data)
dim(data)
head(data)
tail(data)

# Unservise Learning delete label aand no columns
df1 <- data[,-c(1,8)]  
head(df1)

summary(df1)
str(df1)


# Show different  plots in one row and 3 columns
# par(mfrow = c(1,3))
par(mfrow = c(1,3))  # 1 rows and 3 column

for (i in c(1,3,4)) { 
  hist(df1[,i], xlab = "", main = paste("Histogram of", names(df1)[i]))
}

# Back to normal
par(mfrow = c(1,1))

#correlation table
cor_table <- round(cor(df1[,c(1,3,4)]),2) 
cor_table


#Categorical variable
table(df1$gender)
table(df1$Home_Owner)
table(df1$CRM)
summary(df1)

# TODO:Turn Gender into factor

# dist = distance between observations (Euclidean)
# distance between obs i(rows) and j(column)
d <- dist(df1[,c("age", "Salary", "No_childs")])

d
# For better understandin  put them in a matrix
as.matrix(d)[1:298, 1:298]
# as can be seen the distances between  most of the obs are very large
str(df1)

# Scale
# different data type

# Gowers distance(different datatypes aand different distances)
# Because we have gender, Home_Owner and CRM as char ,daisy throws error so we convert them into factor
d <- daisy(df1)
names(df1)
df1$gender <- as.factor(df1$gender)
df1$Home_Owner <- as.factor(df1$Home_Owner)
df1$CRM <- as.factor(df1$CRM)
library(cluster)
d <- daisy(df1)
sum(is.na(d))
d<-na.omit(d)
seg_hc <- hclust(d, method="complete")
plot(seg_hc)

plot(cut(as.dendrogram(seg_hc), h=0.5)$lower[[1]])
# examine the similarieties between similar samples
df1[c(128, 137),]

# choose one on the right  and one on the left
df1[c(141, 171),]
# Sex made all of the difference, salary and no child!!!!

# cophenetic correlation coefficient (CPCC)
# correlation between distance and seg_hc
cor(cophenetic(seg_hc), d)
rect.hclust(seg_hc, k=4, border="red")


install.packages('dendextend', dependencies = TRUE)
library(dendextend)


dend_seg<- as.dendrogram(seg_hc)
col_dend <- color_branches(dend_seg, k = 4)
plot(col_dend)

# cutree
seg_hc_segment <- cutree(seg_hc, k = 4) 
seg_hc_segment
table(seg_hc_segment)


# add segemnt to the dataframe 
df1$segment <- seg_hc_segment


# tapply
tapply(df1$Salary, df1$segment, mean)

# Cant use tapply on factor
table(df1$gender, df1$segment)
table(df1$Home_Owner, df1$segment)

table(df1$CRM, df1$segment)


# M and F people which of them were not registed in CRM?
plot(jitter(as.numeric(df1$gender)), 
     jitter(as.numeric(df1$CRM)),
     col = df1$segment)

# Kmeans clustering
setwd("F:\\dars\\Data Science Tehran\\Data Science\\6.R\\Work for GitHub\\3rd")
data <- read.csv(file.choose())
data <- data[,-c(1,8)]

set.seed(123)
segement_data <- read.csv(file.choose())
segement_data <- segement_data[, -c(1,8)]
segement_data




head(segement_data)
tail(segement_data)

is.na(segement_data)
sum(is.na(segement_data))

segement_data$Home_Owner <- as.factor(segement_data$Home_Owner)
segement_data$gender <- as.factor(segement_data$gender)



summary(segement_data)
table(segement_data$Home_Owner)

# Have abug in the dataset for kmeans so we try to debug
# Find invalid datatypes:
any(is.nan(segement_data$age))
any(is.nan(segement_data$gender))
any(is.nan(segement_data$Salary))
any(is.nan(segement_data$No_childs))
any(is.nan(segement_data$Home_Owner))
any(is.nan(segement_data$CRM))

any(is.na(segement_data$age))
any(is.na(segement_data$gender))
any(is.na(segement_data$Salary))
any(is.na(segement_data$No_childs))
any(is.na(segement_data$Home_Owner))
any(is.na(segement_data$CRM))

any(is.infinite(segement_data$age))
any(is.infinite(segement_data$gender))
any(is.infinite(segement_data$Salary))
any(is.infinite(segement_data$No_childs))
any(is.infinite(segement_data$Home_Owner))
any(is.infinite(segement_data$CRM))

# alternatively:
sum(sapply(segement_data, is.na))
sum(sapply(segement_data, is.nan))
sum(sapply(segement_data, is.infinite))

summary(segement_data)
str(segement_data)
segement_data$gender <- ifelse(segement_data$gender == "M", 1.0, 0.0)
segement_data$Home_Owner <- ifelse(segement_data$Home_Owner == "NO", 0.0, 1.0)
segement_data$age <- as.numeric(segement_data$age)
segement_data$Salary<-as.numeric(segement_data$Salary)
segement_data$No_childs<-as.numeric(segement_data$No_childs)
segement_data$CRM <-ifelse(segement_data$CRM == "NO", 0.0, 1.0)
set.seed(234)
seg_km <- kmeans(segement_data, centers=4)
table(seg_km$cluster)

# optimized K (cluster centers) with Elbow method

segement_data$segment <- seg_km$cluster
wss <- function(k){
  kmeans(segement_data, k, nstart = 10)$tot.withinss
    }
k_values <- 1:10
wss_values <- data.frame(k = k_values)
for (i in k_values) {
  wss_values$wss[i] <- wss(wss_values$k[i])
}

plot(wss_values$k, wss_values$wss,
     type = "b", pch = 20, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Total within-clusters sum of squares")

segement_data
tapply(segement_data$age, segement_data$segment, mean) 
table(segement_data$gender, segement_data$segment) 
tapply(segement_data$Salary, segement_data$segment, mean) 
tapply(segement_data$No_child, segement_data$segment, mean) 
table(segement_data$Home_Owner, segement_data$segment) 
table(segement_data$CRM, segement_data$segment) #CRM 

# To see if there is a differnece beween clusters
# boxplot different fields
boxplot(segement_data$Salary ~ segement_data$segment, ylab= "Salary", xlab="Cluster")


# PCA-like dimensionality reduction
install.packages("factoextra")

library(ggplot2)
library(factoextra)
data = segement_data[,-c(6)]
data
fviz_cluster(seg_km, geom = "point", data=data)+
  ggtitle("Number  of clusters k =4")
summary(segement_data)

ggplot(data = segement_data, aes(Salary, age, color = factor(segment))) +
  geom_point()


install.packages("RWeka")

# Heart Data
setwd("F:/dars/Data Science Tehran/Data Science/6.R/Work for GitHub/3rd") 
data1<-read.csv(file.choose())


data1 <- na.omit(data1)
Normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}


data1 <- lapply(data1, Normalize)
data1 <- Normalize( c ~ . , data1)
data1 <- data1[,c(4,5)]

# Hyper K : 

install.packages("clusterCrit")
library(clusterCrit)

# as.numeri
#as.factor
for(i in 1:ncol(data1)){
  data1[,i] <- as.numeric(data1[,i])
}

data1 <- as.matrix(data1)

sil1 <- intCriteria(traj = data1, km1$cluster, "Silhouette")
