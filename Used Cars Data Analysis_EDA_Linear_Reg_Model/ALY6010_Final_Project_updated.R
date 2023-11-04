#ALY6010 FINAL PROJECT#


#Importing the required libraries

install.packages(c("FSA","FSAdata","magrittr","dplyr","plotrix","ggplot2","moments")) 
lapply(c("FSA","FSAdata","magrittr","dplyr","plotrix","ggplot2","moments"),
       require, character.only = TRUE)
install.packages (c("FSA","FSAdata", "magrittr","dplyr", "tidyr", "plyr","tidyverse"))
lapply(c("FSA","FSAdata", "magrittr","dplyr", "tidyr", "plyr","tidyverse"), require, character.only = TRUE)
install.packages("dplyr")
install.packages("sqldf")
install.packages("rcompanion")
install.packages("ggcorrplot")
install.packages("corrplot")
install.packages("plotly")
install.packages("hrbrthemes")
install.packages("rstatix")
install.packages("psych",dependencies=TRUE)
library(plotly)
library(hrbrthemes)
library(ggcorrplot)
library(corrplot)
library(sqldf)
library(dplyr)
library(psych)
library(rcompanion)
library(ggplot2)
library(ggpubr)
library(rstatix)
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)

#Importing the data using read.csv()
f1 <- read.csv("Car details v3.csv", header = TRUE)
str(f1)


#Removing kmpl from Mileage Column and converting to Numeric attribute#
f1$mileage <-  gsub("[a-zA-Z/ ]", "", f1$mileage)
data.class(f1$mileage)
f1$mileage <- as.numeric (f1$mileage)

#Removing CC from Engine Column and converting to Numeric attribute#
f1$engine <-  gsub("[a-zA-Z/ ]", "", f1$engine)
data.class(f1$engine)
f1$engine <- as.numeric (f1$engine)

#Removing bhp from Maxpower#
f1$max_power <-  gsub("[a-zA-Z/ ]", "", f1$max_power)
data.class(f1$max_power)
f1$max_power <- as.numeric (f1$max_power)

#Extracting the torque value and Rpm value#
f1$torque_val <- as.numeric(str_sub(f1$torque, rep(1, nrow(f1)), 
                                           str_locate(f1$torque, "\\D+")[,1]-1))
num_length <- str_length(gsub("\\D+", "", f1$torque))
f1$rpm <- as.numeric(str_sub(as.numeric(gsub("\\D+", "", f1$torque)),num_length-3, num_length))

#Extracting the Manufacturer name from the Name Column
f1$manufacturer <- str_extract(f1$name,"(\\w+)") 

#Extracting the Model Name from the Name Column
f1$modelname<-word(f1$name, 2,-1)


#Checking data is clean?
colSums(is.na(f1)) # check & Returns the number of missing values in each column

sum(is.na(f1)) # Counts missing values in entire dataframe

colSums(f1==0) #Using colSums function to find the total number of Zero records in each column


#Replacing records with missing values with their respective mean
f1$mileage[is.na(f1$mileage)]<-round (mean(f1$mileage,na.rm=TRUE),2) #mileage#
f1$engine[is.na(f1$engine)]<-round (mean(f1$engine,na.rm=TRUE),2)#engine#
f1$max_power[is.na(f1$max_power)]<-round(mean(f1$max_power,na.rm=TRUE),2) #max_power#
f1$seats[is.na(f1$seats)] <- round(mean(f1$seats, na.rm = TRUE),2)#seats#
f1$torque_val[is.na(f1$torque_val)]<- round(mean(f1$torque_val,na.rm=TRUE),2)  #torque value#
f1$rpm[is.na(f1$rpm)]<- round(mean(f1$rpm,na.rm=TRUE),2)  #rpm#
  
  
#Removing records with 0 values from Mileage and Max power
f1_new <-subset(f1,f1$mileage != "0" & f1$max_power != "0")
colSums(f1_new==0)
sum(is.na(f1_new)) 

#Dropping Columns Torque & Name as it is not needed.
f1_final <-select(f1_new,-c(name,torque))
colSums(is.na(f1_final))
colSums(f1_final==0)
sum(is.na(f1_final)) 

#Now ,Creating a new Column - 'Age' of the car based on the 'Year' column 
f1_final$age <- as.numeric(format(Sys.Date(), "%Y")) - f1_final$year
str(f1_final)


#Now since we have a clean dataset , lets check the Correlation
data_corr <- f1_final [,c("selling_price" , "km_driven"  ,"mileage" , "engine" ,"max_power" ,"seats" , "torque_val" , "rpm" ,"age")]
corr <- round(cor(data_corr), 2)

ggcorrplot(
  corr,
  hc.order = TRUE,
  outline.color = "white",
  ggtheme = ggplot2::theme_gray,
  colors = c("#6D9EC1", "white", "#E46726")
)

ggcorrplot(corr, hc.order = TRUE, type = "lower",
           lab = TRUE)


corrplot(cor(corr), type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, title = "Correlation of all Numeric Attributes ",
         mar=c(0,0,1,0))


#EXPLORATORY DATA ANALYSIS#
f1_final %>%describe()
describeBy(f1_final, group=f1_final$transmission)


#Univariate Analysis#
#Frequency Tables & Graphs#

#For Fuel Type#
freq_fuel<-data.frame(sqldf("select fuel,count(fuel) as Frequency , round(avg(selling_price),2) as AveragePrice
       from f1_final group by fuel order by count(fuel) " ))
freq_fuel

par(mar=c(8,4,5,5))

p1<- ggplot(freq_fuel, aes(fuel,Frequency)) + 
  ggtitle("Fuel Type - Distribution")+
  labs(x='Fuel', y='Frequency count') +
  geom_bar(position = "dodge",
           stat = "summary",
           fun = "mean", fill="Orange")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
p1

#For Seller Type#
freq_seller_type <- data.frame(sqldf("select seller_type,count(seller_type) as Frequency
       from f1_final group by seller_type order by count(seller_type) " ))
freq_seller_type

p2 <-ggplot(freq_seller_type, aes(seller_type,Frequency)) + 
  ggtitle("Seller Type - Distribution")+
  labs(x='Seller Type', y='Frequency count') +
  geom_bar(position = "dodge",
           stat = "summary",
           fun = "mean", fill="Orange")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
p2

#For Transmission Type#
freq_transmission <- data.frame(sqldf("select transmission,count(transmission) as Frequency, 
round(avg(selling_price),2) as AveragePrice
from f1_final group by transmission order by count(transmission) " ))
freq_transmission

p3<-ggplot(freq_transmission, aes(transmission,Frequency)) + 
  ggtitle("Transmission Type - Distribution")+
  labs(x='Transmission Type', y='Frequency count') +
  geom_bar(position = "dodge",
           stat = "summary",
           fun = "mean", fill="Orange")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
p3

#For Owner Type#
freq_owner <- data.frame(sqldf("select owner,count(owner) as Frequency
       from f1_final group by owner order by count(owner) " ))
freq_owner

p4 <- ggplot(freq_owner, aes(owner,Frequency)) + 
  ggtitle("Owner Type - Distribution")+
  labs(x='Owner Type', y='Frequency count') +
  geom_bar(position = "dodge",
           stat = "summary",
           fun = "mean", fill="Orange")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
p4

#Arrange for the frequency graphs in a Grid 
grid.arrange(p1, p2, p3, p4)

#Top 10 Manufacturers#

f1_final$selling_price<-round((f1_final$selling_price/1000),2)

top10 <-data.frame(sqldf("select manufacturer,count(manufacturer) as Freq,round (avg(selling_price),2) as AveragePrice
       from f1_final group by manufacturer order by count(manufacturer) desc limit 10" ))
top10

top10_plot <- ggplot(top10, aes(manufacturer,Freq)) + 
  ggtitle("Top 10 Car Manufacturers as per Sales")+
  labs(x='Manufacturer/Company', y='Count') +
  geom_bar(position = "dodge",
           stat = "summary",
           fun = "mean", fill="orange")+
  geom_text(aes(label = Freq), vjust = -0.3) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
top10_plot


top10_avg_price <- ggplot(top10, aes(manufacturer,AveragePrice)) + 
  ggtitle("Top 10 Manufacturers Average Selling Price")+
  labs(x='Manufacturer/Company', y='Average Selling Price') +
  geom_bar(position = "dodge",
           stat = "summary",
           fun = "mean", fill="orange")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
top10_avg_price


#Box Plot for Numeric Variables#
par(mar=c(8,4,6,6))

#For Km Driven#
b1 <-ggplot(f1_final, aes(x=transmission, y=km_driven , color =transmission)) + 
  geom_boxplot()

#For Selling Price#
b2 <-ggplot(f1_final, aes(x=transmission, y=selling_price, color =transmission)) + 
  geom_boxplot()

#For Mileage#
b3 <-ggplot(f1_final, aes(x=transmission, y=mileage,color =transmission)) + 
  geom_boxplot()

#For Engine#
b4 <-ggplot(f1_final, aes(x=transmission, y=engine,color =transmission)) + 
  geom_boxplot()

#For Max Power#
b5 <-ggplot(f1_final, aes(x=transmission, y=max_power,color =transmission)) + 
  geom_boxplot()

#For Torque Value#
b6 <-ggplot(f1_final, aes(x=transmission, y=torque_val,color =transmission)) + 
  geom_boxplot()

#For Rpm#
b7 <-ggplot(f1_final, aes(x=transmission, y=rpm,color =transmission)) + 
  geom_boxplot()

#For Age#
b8 <-ggplot(f1_final, aes(x=transmission, y=age,color =transmission)) + 
  geom_boxplot()

grid.arrange(b1, b2, b3, b4)
grid.arrange(b5,b6,b7,b8)


#Histograms#
#Price Distribution of Used Cars#

price_hist<-ggplot(f1_final, aes(x=selling_price)) + 
  geom_histogram(color="black", fill="orange", position="identity")+
  labs(title="Selling Price Distribution",x="Price in Thousands(Rs.)", y = "Count")
price_hist

#Kms Driven Distribution
f1_final$km_driven<-round((f1_final$km_driven/1000),2)

kms_driven_hist<-ggplot(f1_final, aes(x=km_driven)) + 
  geom_histogram(color="black", fill="orange", position="identity")+
  labs(title="Kms Driven Distribution",x="kms Driven", y = "Count")
kms_driven_hist

#Mileage  Distribution
mileage_hist<-ggplot(f1_final, aes(x=mileage)) + 
  geom_histogram(color="black", fill="orange", position="identity")+
  labs(title="Mileage Distribution",x="Mileage", y = "Count")
mileage_hist

#Engine  Distribution
engine_hist<-ggplot(f1_final, aes(x=engine)) + 
  geom_histogram(color="black", fill="orange", position="identity")+
  labs(title="Engine Distribution",x="Engine (cc)", y = "Count")
engine_hist


#Max power  Distribution
max_power_hist<-ggplot(f1_final, aes(x=max_power)) + 
  geom_histogram(color="black", fill="orange", position="identity")+
  labs(title="Max power Distribution",x="Max power(bhp)", y = "Count")
max_power_hist

#Torque Distribution
torque_val_hist<-ggplot(f1_final, aes(x=torque_val)) + 
  geom_histogram(color="black", fill="orange", position="identity")+
  labs(title="Torque value Distribution",x="Torque value", y = "Count")
torque_val_hist


#Rpm Distribution
rpm_hist<-ggplot(f1_final, aes(x=rpm)) + 
  geom_histogram(color="black", fill="orange", position="identity")+
  labs(title="Rpm Distribution",x="Rpm", y = "Count")
rpm_hist

#Age Distribution
age_hist<-ggplot(f1_final, aes(x=age)) + 
  geom_histogram(color="black", fill="orange", position="identity")+
  labs(title="Age Distribution",x="Age of the Car in Years", y = "Count")
age_hist

#Arrange the histograms in a Grid 
grid.arrange(price_hist, kms_driven_hist, mileage_hist,engine_hist)
grid.arrange(max_power_hist, torque_val_hist, rpm_hist,age_hist)



#BIVARIATE ANALYSIS#


#For Max power vs Selling Price#
sp1 <- ggplot(f1_final, aes(x = max_power, y = selling_price)) +
  geom_point(aes(color = transmission) , shape=18)+
  labs(
    x = "Max Power in bhp",
    y = "Selling Price",
    title = "Relation between Max Power and Price of the Car")+
  geom_smooth(method = 'lm', color = "Blue")
sp1
#Extra code for max power vs selling#
ggplot(f1_final, aes(x=max_power, y=selling_price)) + 
  geom_point(col=I("orange"), shape=18) +
  geom_smooth(method = 'lm', color = "Blue")


#For Torque value vs Selling Price#
sp2 <- ggplot(f1_final, aes(x = torque_val, y = selling_price)) +
  geom_point(aes(color = transmission) , shape=18)+
  labs(
    x = "Torque Value",
    y = "Selling Price",
    title = "Relation between Torque Value and Price of the Car")+
  geom_smooth(method = 'lm', color = "Blue")
sp2

#For Engine vs Selling Price#
sp3 <- ggplot(f1_final, aes(x = engine, y = selling_price)) +
  geom_point(aes(color = transmission) , shape=18)+
  labs(
    x = "Engine in CC",
    y = "Selling Price",
    title = "Relation between Engine and Price of the Car")+
  geom_smooth(method = 'lm', color = "Blue")
sp3


grid.arrange(sp1, sp2, sp3)


#For km_driven Vs Selling Price#
sp4 <-ggplot(f1_final, aes(x = km_driven, y = selling_price)) +
  xlim(1,750)+
   geom_point(aes(color = transmission) , shape=18)+
  labs(
    x = "Kilometers Driven",
    y = "Selling Price",
    title = "Relation between KmsDriven and Price of the Car")+
geom_smooth(method = 'lm', color = "Blue")
sp4

#For Mileage vs Selling Price#
sp5 <-ggplot(f1_final, aes(x = mileage, y = selling_price)) +
  geom_point(aes(color = transmission) , shape=18)+
  labs(
    x = "Mileage",
    y = "Selling Price",
    title = "Relation between Cars' Mileage and Price of the Car")+
  geom_smooth(method = 'lm', color = "Blue")
sp5

#For Age vs Selling Price#
sp6 <-ggplot(f1_final, aes(x = age, y = selling_price)) +
  geom_point(aes(color = transmission) , shape=18)+
  labs(
    x = "Age of the Car in Years",
    y = "Selling Price",
    title = "Relation between Cars' Age and Price of the Car")+
  geom_smooth(method = 'lm', color = "Blue")
sp6

grid.arrange(sp4, sp5, sp6)



#For Fuel Type vs Selling Price#
sp7 <-ggplot(f1_final, aes(x = fuel, y = selling_price)) +
  geom_jitter(aes(color = transmission) , shape=18)+
  labs(
    x = "Fuel Type",
    y = "Selling Price",
    title = "Relation between Fuel Type and Price of the Car")
sp7

#For Owner Type vs Selling Price#
sp8 <-ggplot(f1_final, aes(x = owner, y = selling_price)) +
  geom_jitter(aes(color = transmission) , shape=18)+
  labs(
    x = "Owner Type",
    y = "Selling Price",
    title = "Relation between Owner Type and Price of the Car")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
sp8

#For Owner Type vs Selling Price#
sp9<-ggplot(f1_final, aes(x = seller_type, y = selling_price)) +
  geom_jitter(aes(color = transmission) , shape=18)+
  labs(
    x = "Seller Type",
    y = "Selling Price",
    title = "Relation between Seller Type and Price of the Car")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
sp9

grid.arrange(sp7, sp8, sp9)


#Selling Price Over the Years with Line Graph#
f1_final %>%
  ggplot( aes(x=year, y=selling_price, group=transmission, color=transmission)) +
  geom_line() +
  ggtitle("Selling Price from 1983 - 2020") +
  theme_ipsum() +
  ylab("Selling Price")+
  xlab("Year")


#######################********HYPOTHESIS TESTING*********##############################

#ONE-SAMPLE T-TEST#

#Checking normality for selling price#

#Density Plot#
n_selling_price <- ggdensity(f1_final$selling_price, main = "Density plot of Selling Price", 
                             xlab = "Selling Price", fill = "#ffa514")
n_selling_price

#qq-plot#
ggqqplot(f1_final$selling_price) #Extra code#
qqnorm(f1_final$selling_price, pch = 1, frame = FALSE)
qqline(f1_final$selling_price, col = "steelblue", lwd = 2)



##########COMPUTE ONE SAMPLE T-TEST###########

#Selling price of the car sold by the First Owner#
m1 <-round(mean(f1_final$selling_price),2)

t1 <- t.test(f1_final$selling_price[f1_final$owner == "First Owner"], mu=m1,
             alt ="greater")
t1
format(t1$p.value, scientific = FALSE)


#Selling price of the car sold by Seller Type - Dealer#
t2 <- t.test(f1_final$selling_price[f1_final$seller_type == "Dealer"], mu=m1 ,
             alt="greater")
t2
format(t2$p.value, scientific = FALSE)

#Kms driven by First Owner#

m2 <-round(mean(f1_final$km_driven),2)

t3 <- t.test(f1_final$km_driven[f1_final$owner == "First Owner"], mu=m2 ,
             alt="less")
t3
format(t3$p.value, scientific = FALSE)


#Kms driven by Seller Type - Individual#

t4 <- t.test(f1_final$km_driven[f1_final$seller_type == "Individual"], mu=m2 , alt="greater")
t4
format(t4$p.value, scientific = FALSE)


############### TWO-SAMPLE T-TEST ###################

#Selling Price of the car  for Dealer & Trustmark Dealer
t5 <- t.test(f1_final$selling_price[f1_final$seller_type == "Dealer"], 
             f1_final$selling_price[f1_final$seller_type == "Trustmark Dealer"] , alt="two.sided")
t5
format(t5$p.value, scientific = FALSE)


#Mileage of the Car for Manual & Automatic Car 
t6 <- t.test(f1_final$mileage[f1_final$transmission == "Manual"], 
             f1_final$mileage[f1_final$transmission == "Automatic"], alt = "greater" )
t6
format(t6$p.value, scientific = FALSE)


#Selling Price of the car for Second owner vs Third Owner
t7 <- t.test(f1_final$selling_price[f1_final$owner == "Second Owner"], 
             f1_final$selling_price[f1_final$owner == "Third Owner"] , alt="greater")
t7
format(t7$p.value, scientific = FALSE)

#Max power value for Manual Vs Automatic 
t8 <- t.test(f1_final$max_power[f1_final$transmission == "Manual"], 
             f1_final$max_power[f1_final$transmission == "Automatic"], alt = "less" )
t8
format(t8$p.value, scientific = FALSE)



#LINEAR REGRESSION#

# -Model1-Numerical attributes
Model1 <-summary(lm(formula = selling_price ~ max_power+torque_val+engine,
           data = f1_final))
Model1

#Model2- numerical + categorical attributes
Model2 <- summary(lm(formula = selling_price ~ max_power+torque_val+rpm+km_driven+age+mileage+
                       transmission+seller_type+manufacturer,
                        data = f1_final))
Model2

#Model3 - numerical + categorical attributes (Extra)
Model3 <-  summary(lm(formula = selling_price ~ max_power+torque_val+rpm+km_driven+age+mileage+
                        transmission+seller_type+manufacturer,
           data = f1_final))
Model3 




