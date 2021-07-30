#Jesus is the way the truth and the life

#Libraries
library(RColorBrewer)
library(ggplot2)
library(gridExtra)
library(psych)
library(corrplot)
library(rpart)
library(rpart.plot)
library(randomForest)
library(tree)

#Loading the data
mdata = read.csv("C:/Users/tonyk/Documents/DSP 34/Task/4 Random Forest/Dataset/train.csv",
                stringsAsFactors = TRUE)

dim(mdata)
'2000   21'

#Checking is there any missing values
sapply(mdata, function(x) sum(is.na(x)))
sapply(mdata, function(x) sum(is.null(x)))
'No missing values'

#Subsetting data as per price range
mdata_p0 = subset(mdata, price_range==0)
mdata_p1 = subset(mdata, price_range==1)
mdata_p2 = subset(mdata, price_range==2)
mdata_p3 = subset(mdata, price_range==3)

#___________________________Target variable - price_range
'The value of 0(low cost), 1(medium cost), 2(high cost) and 3(very high cost)'

str(mdata$price_range)
'int [1:2000] 1 2 2 2 1 1 3 0 0 0 ...'

#Converting data into categories
mdata$price_range = as.factor(mdata$price_range)

str(mdata$price_range)
'Factor w/ 4 levels "0","1","2","3": 2 3 3 3 2 2 4 1 1 1 ...'

table(mdata$price_range)
'  0   1   2   3 
 500 500 500 500 '

#Barplot  
barplot(table(mdata$price_range),
        names.arg = c('Low', 'Medium', 'High', 'Very High'),
        col = brewer.pal(4, 'Accent'),
        main = 'Prices of Mobiles',
        xlab = 'Mobile Cost',
        ylab = 'Counts of Mobiles')

#___________________________Battery Power
'Total energy a battery can store in one time measured in mAh'

str(mdata$battery_power)
'int [1:2000] 842 1021 563 615 1821 1859 1821 1954 1445 509 ...'

summary(mdata$battery_power)
'   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  501.0   851.8  1226.0  1238.5  1615.2  1998.0'

#Histogram
hist(mdata$battery_power,
     col = brewer.pal(9,'Dark2'),
     main = 'Histogram of Battery Power of Mobiles',
     xlab = 'Battery Power of Mobiles')

#Histogram comparision of battery_power with Price range
p0 = qplot(battery_power, data = mdata_p0,bins=30, main = 'Low Cost Mobiles')
p1 = qplot(battery_power, data = mdata_p1,bins=30, main = 'Medium Cost Mobiles')
p2 = qplot(battery_power, data = mdata_p2,bins=30, main = 'High Cost Mobiles')
p3 = qplot(battery_power, data = mdata_p3,bins=30, main = 'Very High Cost Mobiles')
grid.arrange(p0,p1,p2,p3, ncol=2,top="Battery Power Comparision")

#Average Battery_Power of Mobiles at different prices
with(mdata, tapply(battery_power, price_range, mean))
'       0        1        2        3 
 1116.902 1228.868 1228.320 1379.984 '

#Barplot of Average Battery_power
barplot(with(mdata, tapply(battery_power, price_range, mean)),
        names.arg = c('Low', 'Medium', 'High', 'Very High'),
        col = brewer.pal(4,'RdBu'),
        main = 'Barplot of Average Battery_power',
        xlab = 'Price of Mobiles')

#Boxplot
boxplot(mdata$battery_power,
        col = 'lightcoral',horizontal = TRUE,
        main = 'Boxplot of Battery Power of Mobiles',
        xlab = 'Battery Power of Mobiles')

#___________________________Blue
str(mdata$blue)
'int [1:2000] 0 1 1 1 1 0 0 0 1 1 ...'

table(mdata$blue)
'   0    1 
 1010  990'

#Converting data into categories
mdata$blue = as.factor(mdata$blue)

str(mdata$blue)
'Factor w/ 2 levels "0","1": 1 2 2 2 2 1 1 1 2 2 ...'

#Barplot
table(mdata$blue)
'   0    1 
 1010  990'

barplot(table(mdata$blue),
        names.arg = c('No', 'Yes'),
        col = c('turquoise4','tan4'),
        main = 'Barplot of Whether Mobile is Blue',
        xlab = 'Blue')

#Blue vs Price Range
table(mdata$price_range,mdata$blue)
'   0   1
0 257 243
1 255 245
2 257 243
3 241 259'

#Blue Mobiles comparision with price
p0 = qplot(blue, data = mdata_p0, main = 'Low Cost Mobiles', geom = 'bar')
p1 = qplot(blue, data = mdata_p1, main = 'Medium Cost Mobiles',geom = 'bar')
p2 = qplot(blue, data = mdata_p2, main = 'High Cost Mobiles',geom = 'bar')
p3 = qplot(blue, data = mdata_p3, main = 'Very High Cost Mobiles',geom = 'bar')
grid.arrange(p0,p1,p2,p3, ncol=2, top="Blue Mobile Comparision")

#___________________________clock speed
'The speed at which microprocessor executes instructions'

str(mdata$clock_speed)
'num [1:2000] 2.2 0.5 0.5 2.5 1.2 0.5 1.7 0.5 0.5 0.6 ...'

#Histogram
hist(mdata$clock_speed,
     col = brewer.pal(9,'Set1'),
     main = 'Histogram of Clock Speed of Mobiles',
     xlab = 'Clock Speed of Mobiles')

#Histogram comparision of Clock Speed with Price range
p0 = qplot(clock_speed, data = mdata_p0,bins=30, main = 'Low Cost Mobiles')
p1 = qplot(clock_speed, data = mdata_p1,bins=30, main = 'Medium Cost Mobiles')
p2 = qplot(clock_speed, data = mdata_p2,bins=30, main = 'High Cost Mobiles')
p3 = qplot(clock_speed, data = mdata_p3,bins=30, main = 'Very High Cost Mobiles')
grid.arrange(p0,p1,p2,p3, ncol=2, top="Clock Speed Comparision")

#Average Clock Speed of Mobiles of different price range
with(mdata, tapply(clock_speed,price_range, mean))
'     0      1      2      3 
 1.5502 1.4886 1.5298 1.5204 '

#Barplot of Average Clock Speed of Mobiles
barplot(with(mdata, tapply(clock_speed,price_range, mean)),
        names.arg = c('Low', 'Medium', 'High', 'Very High'),
        col = brewer.pal(4,'BrBG'),
        main = 'Barplot of Avg Clock Speed of Mobiles',
        xlab = 'Price of Mobiles')

#Boxplot
boxplot(mdata$clock_speed,
        col = 'firebrick3',horizontal = TRUE,
        main = 'Boxplot of Mobiles Clock Speed',
        ylab = 'Mobiles Clock Speed')

#___________________________Dual Sim
str(mdata$dual_sim)
'int [1:2000] 0 1 1 0 0 1 0 1 0 1 ..'
table(mdata$dual_sim)
'   0    1 
  981 1019'

#Converting data into categories
mdata$dual_sim = as.factor(mdata$dual_sim)

str(mdata$dual_sim)
'Factor w/ 2 levels "0","1": 1 2 2 1 1 2 1 2 1 2 ...'

#Barplot of Dual sim
table(mdata$dual_sim)
'   0    1 
  981 1019 '

barplot(table(mdata$dual_sim),
        names.arg = c('No', 'Yes'),
        col = c('royalblue','firebrick3'),
        main = 'Barplot of Mobile having Dual sim',
        xlab = 'Dual sim')

#Dual sim with Price Range
table(mdata$price_range,mdata$dual_sim)
'      0   1
    0 250 250
    1 245 255
    2 251 249
    3 235 265'

#Dual Sim Mobiles comparison with price
p0 = qplot(dual_sim, data = mdata_p0, main = 'Low Cost Mobiles', geom = 'bar')
p1 = qplot(dual_sim, data = mdata_p1, main = 'Medium Cost Mobiles',geom = 'bar')
p2 = qplot(dual_sim, data = mdata_p2, main = 'High Cost Mobiles',geom = 'bar')
p3 = qplot(dual_sim, data = mdata_p3, main = 'Very High Cost Mobiles',geom = 'bar')
grid.arrange(p0,p1,p2,p3, ncol=2, top="Dual Sim Mobile Comparision")

#___________________________FC - Front Camera megapixels
str(mdata$fc)
'int [1:2000] 1 0 2 0 13 3 4 0 0 2 ...'

summary(mdata$fc)
'   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   0.000   1.000   3.000   4.309   7.000  19.000 '

table(mdata$fc)
'  0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19 
 474 245 189 170 133 139 112 100  77  78  62  51  45  40  20  23  24   6  11   1 '

#Histogram
hist(mdata$fc,
     col = brewer.pal(9,'Spectral'),
     main = 'Histogram of Mobile Front Camera',
     xlab = 'Mobile Front Camera Megapixels')

#Histogram FC Megapixel of Mobiles with Price range
p0 = qplot(fc, data = mdata_p0,bins=30, main = 'Low Cost Mobiles')
p1 = qplot(fc, data = mdata_p1,bins=30, main = 'Medium Cost Mobiles')
p2 = qplot(fc, data = mdata_p2,bins=30, main = 'High Cost Mobiles')
p3 = qplot(fc, data = mdata_p3,bins=30, main = 'Very High Cost Mobiles')
grid.arrange(p0,p1,p2,p3, ncol=2, top="Mobiles Front Camera Comparision")

#Average FC Megapixel of Mobiles of different price range
with(mdata, tapply(fc,price_range, mean))
'    0     1     2     3 
 4.084 4.340 4.498 4.316 '

#Barplot of Average FC Megapixel of Mobiles
barplot(with(mdata, tapply(fc,price_range, mean)), 
        names.arg = c('Low', 'Medium', 'High', 'Very High'),
        col = brewer.pal(4,'PiYG'),
        main = 'Barplot of Avg FC Megapixel of Mobiles',
        xlab = 'Price of Mobiles')

#Boxplot
boxplot(mdata$fc,
     col = 'darkslateblue', horizontal = TRUE,
     main = 'Boxplot of Mobile Front Camera',
     xlab = 'Mobile Front Camera Megapixels')

#Upper Boundary
fc_ub = quantile(mdata$fc, 0.75)+1.5*IQR(mdata$fc)
length(mdata$fc[mdata$fc>fc_ub])
'18'
'Outliers are less in number and it will not have effect in Random forest, Ignore'

#___________________________Four G
str(mdata$four_g)
'int [1:2000] 0 1 1 0 1 0 1 0 0 1 ...'

table(mdata$four_g)
'   0    1 
  957 1043'

#Converting data into categories
mdata$four_g = as.factor(mdata$four_g)

str(mdata$four_g)
'Factor w/ 2 levels "0","1": 1 2 2 1 2 1 2 1 1 2 ...'

#Barplot
table(mdata$four_g)
'   0    1 
  957 1043'

barplot(table(mdata$four_g),
        names.arg = c('No', 'Yes'),
        col = c('darkgreen', 'brown4'),
        main = 'Barplot of Mobile having 4G')

#Four G Mobiles comparison with price
p0 = qplot(four_g, data = mdata_p0, main = 'Low Cost Mobiles', geom = 'bar')
p1 = qplot(four_g, data = mdata_p1, main = 'Medium Cost Mobiles',geom = 'bar')
p2 = qplot(four_g, data = mdata_p2, main = 'High Cost Mobiles',geom = 'bar')
p3 = qplot(four_g, data = mdata_p3, main = 'Very High Cost Mobiles',geom = 'bar')
grid.arrange(p0,p1,p2,p3, ncol=2, top="Four G Mobile Comparision")

#___________________________Internal Memory
str(mdata$int_memory)
'int [1:2000] 7 53 41 10 44 22 10 24 53 9 ...'

summary(mdata$int_memory)
'   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   2.00   16.00   32.00   32.05   48.00   64.00 '

#Histogram
hist(mdata$int_memory,
     col = brewer.pal(9, 'Paired'),
     main = 'Histogram of Mobiles Internal Memory',
     xlab = 'Internal Memory',
     ylim = c(0,200))

#Histogram of Mobiles Internal Memory with Price range
p0 = qplot(int_memory, data = mdata_p0,bins=30, main = 'Low Cost Mobiles')
p1 = qplot(int_memory, data = mdata_p1,bins=30, main = 'Medium Cost Mobiles')
p2 = qplot(int_memory, data = mdata_p2,bins=30, main = 'High Cost Mobiles')
p3 = qplot(int_memory, data = mdata_p3,bins=30, main = 'Very High Cost Mobiles')
grid.arrange(p0,p1,p2,p3, ncol=2, top="Mobiles Internal Memory Comparision")

#Average Internal Memory of Mobiles of different price range
with(mdata, tapply(int_memory,price_range, mean))
'     0      1      2      3 
 31.174 32.116 30.920 33.976'

#Barplot of Average Internal Memory of Mobiles
barplot(with(mdata, tapply(int_memory,price_range, mean)),
        names.arg = c('Low', 'Medium', 'High', 'Very High'),
        col = brewer.pal(4,'PRGn'),
        main = 'Barplot of Avg Internal Memory of Mobiles',
        xlab = 'Price of Mobiles')

#Boxplot
boxplot(mdata$int_memory,
        col = 'purple4', horizontal = TRUE,
        main = 'Boxplot of Mobiles Internal Memory',
        xlab = 'Internal Memory')

#___________________________Mobile Depth in cm
str(mdata$m_dep)
'num [1:2000] 0.6 0.7 0.9 0.8 0.6 0.7 0.8 0.8 0.7 0.1 ...'

summary(mdata$m_dep)
'   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.1000  0.2000  0.5000  0.5018  0.8000  1.0000 '

#Histogram
hist(mdata$m_dep,
     col = brewer.pal(9,'RdYlGn'),
     main = 'Histogram of Mobile Depth',
     xlab = 'Mobile Depth in cm')

#Histogram Mobiles Depth with Price range
p0 = qplot(m_dep, data = mdata_p0,geom = 'bar', main = 'Low Cost Mobiles')
p1 = qplot(m_dep, data = mdata_p1,geom = 'bar', main = 'Medium Cost Mobiles')
p2 = qplot(m_dep, data = mdata_p2,geom = 'bar', main = 'High Cost Mobiles')
p3 = qplot(m_dep, data = mdata_p3,geom = 'bar', main = 'Very High Cost Mobiles')
grid.arrange(p0,p1,p2,p3, ncol=2, top="Mobiles Depth Comparision")

#Average Depth of Mobiles with different price range
with(mdata, tapply(m_dep,price_range, mean))
'     0      1      2      3 
 0.4902 0.5240 0.4908 0.5020'

#Barplot of Average Mobile Depth of Mobiles
barplot(with(mdata, tapply(m_dep,price_range, mean)),
        names.arg = c('Low', 'Medium', 'High', 'Very High'),
        col = brewer.pal(4,'PuOr'),
        main = 'Barplot of Avg Depth of Mobiles',
        xlab = 'Price of Mobiles')

#Boxplot
boxplot(mdata$m_dep,
        col = 'Orange3', horizontal = TRUE,
        main = 'Boxplot of Mobiles Depth',
        xlab = 'Mobile Depth in cm')

#___________________________Weight of the mobile phone
str(mdata$mobile_wt)
'int [1:2000] 188 136 145 131 141 164 139 187 174 93 ...'

summary(mdata$mobile_wt)
'   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   80.0   109.0   141.0   140.2   170.0   200.0 '

#Histogram
hist(mdata$mobile_wt,
     col = brewer.pal(8,'Dark2'),
     main = 'Histogram of Mobile Weights',
     xlab = 'Weight of the mobile phone')

#Histogram comparison of Mobile Weight with Price range
p0 = qplot(mobile_wt, data = mdata_p0,bins=30, main = 'Low Cost Mobiles')
p1 = qplot(mobile_wt, data = mdata_p1,bins=30, main = 'Medium Cost Mobiles')
p2 = qplot(mobile_wt, data = mdata_p2,bins=30, main = 'High Cost Mobiles')
p3 = qplot(mobile_wt, data = mdata_p3,bins=30, main = 'Very High Cost Mobiles')
grid.arrange(p0,p1,p2,p3, ncol=2, top="Mobiles Weight Comparision")

#Average Mobile Weight of different price range
with(mdata, tapply(mobile_wt,price_range, mean))
'      0       1       2       3 
 140.552 140.510 143.614 136.320'

#Barplot of Average Mobile Weight
barplot(with(mdata, tapply(mobile_wt,price_range, mean)),
        names.arg = c('Low', 'Medium', 'High', 'Very High'),
        col = brewer.pal(4,'RdBu'),
        main = 'Barplot of Avg weight of Mobiles',
        xlab = 'Price of Mobiles')

#Boxplot
boxplot(mdata$mobile_wt,
        col = 'Red', horizontal = TRUE,
        main = 'Boxplot of Mobiles Weight',
        xlab = 'Mobile Weight')

#___________________________Number of cores of a processor
str(mdata$n_cores)
'int [1:2000] 2 3 5 6 2 1 8 4 7 5 ...'

summary(mdata$n_cores)
'   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   4.000   4.521   7.000   8.000 '

#Barplot
barplot(table(mdata$n_cores),
     col = brewer.pal(8,'BrBG'),
     main = 'Histogram of Mobile processor cores',
     xlab = 'Number of cores of a processor')

#Histogram comparison of Mobile Processor cores with Price range
p0 = qplot(n_cores, data = mdata_p0,geom = 'bar', main = 'Low Cost Mobiles')
p1 = qplot(n_cores, data = mdata_p1,geom = 'bar', main = 'Medium Cost Mobiles')
p2 = qplot(n_cores, data = mdata_p2,geom = 'bar', main = 'High Cost Mobiles')
p3 = qplot(n_cores, data = mdata_p3,geom = 'bar', main = 'Very High Cost Mobiles')
grid.arrange(p0,p1,p2,p3, ncol=2, top="Mobiles Processor Cores Comparision")

#Average Mobile Processor cores of different price range
with(mdata, tapply(n_cores,price_range, mean))
'    0     1     2     3 
 4.600 4.298 4.682 4.502'

#Barplot of Average Mobile Processor cores
barplot(with(mdata, tapply(n_cores,price_range, mean)),
        names.arg = c('Low', 'Medium', 'High', 'Very High'),
        col = brewer.pal(4,'RdGy'),
        main = 'Barplot of Avg Number of Processor cores of Mobiles',
        xlab = 'Price of Mobiles')

#Boxplot
boxplot(mdata$n_cores,
        col = 'firebrick3', horizontal = TRUE,
        main = 'Boxplot of Processor cores of Mobiles',
        xlab = 'Mobile Weight')

#___________________________Primary Camera megapixels
str(mdata$pc)
'int [1:2000] 2 6 6 9 14 7 10 0 14 15 ...'

summary(mdata$pc)
'   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   0.000   5.000  10.000   9.916  15.000  20.000 '

#Barplot
barplot(table(mdata$pc),
        col = brewer.pal(8,'RdYlBu'),
        main = 'Histogram of Mobile Primary Camera megapixels',
        xlab = 'Primary Camera megapixels')

#Histogram comparison of Mobile Primary Camera megapixels with Price range
p0 = qplot(pc, data = mdata_p0,geom = 'bar', main = 'Low Cost Mobiles')
p1 = qplot(pc, data = mdata_p1,geom = 'bar', main = 'Medium Cost Mobiles')
p2 = qplot(pc, data = mdata_p2,geom = 'bar', main = 'High Cost Mobiles')
p3 = qplot(pc, data = mdata_p3,geom = 'bar', main = 'Very High Cost Mobiles')
grid.arrange(p0,p1,p2,p3, ncol=2, top="Mobiles Primary Camera megapixels Comparision")

#Average Mobile Primary Camera megapixels of different price range
with(mdata, tapply(pc,price_range, mean))
'     0      1      2      3 
  9.574  9.924 10.018 10.150 '

#Barplot of Average Mobile Primary Camera megapixels
barplot(with(mdata, tapply(pc,price_range, mean)),
        names.arg = c('Low', 'Medium', 'High', 'Very High'),
        col = brewer.pal(4,'Dark2'),
        main = 'Barplot of Mobiles Primary Camera megapixels',
        xlab = 'Price of Mobiles')

#Boxplot
boxplot(mdata$pc,
        col = 'seagreen4', horizontal = TRUE,
        main = 'Boxplot of Mobiles Primary Camera megapixels',
        xlab = 'Mobiles Primary Camera megapixels')

#___________________________Pixel Resolution Height
str(mdata$px_height)
' int [1:2000] 20 905 1263 1216 1208 1004 381 512 386 1137 ...'

summary(mdata$px_height)
'   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    0.0   282.8   564.0   645.1   947.2  1960.0 '

#Histogram
hist(mdata$px_height,
    col = brewer.pal(8,'Spectral'),
    main = 'Histogram of Mobile Pixel Resolution Height',
    xlab = 'Mobile Pixel Resolution Height')

#Histogram comparison of Mobile Pixel Resolution Height with Price range
p0 = qplot(px_height, data = mdata_p0,bins=30, main = 'Low Cost Mobiles')
p1 = qplot(px_height, data = mdata_p1,bins=30, main = 'Medium Cost Mobiles')
p2 = qplot(px_height, data = mdata_p2,bins=30, main = 'High Cost Mobiles')
p3 = qplot(px_height, data = mdata_p3,bins=30, main = 'Very High Cost Mobiles')
grid.arrange(p0,p1,p2,p3, ncol=2, top="Mobiles Pixel Resolution Height Comparision")

#Average Mobile Pixel Resolution Height with different price range
with(mdata, tapply(px_height,price_range, mean))
'      0       1       2       3 
 536.408 666.892 632.284 744.848 '

#Barplot of Average Mobile Pixel Resolution Height
barplot(with(mdata, tapply(px_height,price_range, mean)),
        names.arg = c('Low', 'Medium', 'High', 'Very High'),
        col = brewer.pal(4,'Spectral'),
        main = 'Barplot of Mobiles Pixel Resolution Height',
        xlab = 'Price of Mobiles')

#Boxplot
boxplot(mdata$px_height,
        col = 'seagreen4', horizontal = TRUE,
        main = 'Boxplot of Mobiles Pixel Resolution Height',
        xlab = 'Mobiles Pixel Resolution Height')

#Upper Boundary
pxh_ub = quantile(mdata$px_height, 0.75)+1.5*IQR(mdata$px_height)
length(mdata$px_height[mdata$px_height>pxh_ub])
'2'
'Outliers are less in number and it will not have effect in Random forest, Ignore'

#___________________________Pixel Resolution Width
str(mdata$px_width)
'int [1:2000] 756 1988 1716 1786 1212 1654 1018 1149 836 1224 ...'

summary(mdata$px_width)
'   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   500.0   874.8  1247.0  1251.5  1633.0  1998.0'

#Histogram
hist(mdata$px_width,
     col = brewer.pal(8,'YlOrRd'),
     main = 'Histogram of Mobile Pixel Resolution Width',
     xlab = 'Mobile Pixel Resolution Width')

#Histogram comparison of Mobile Pixel Resolution Width with Price range
p0 = qplot(px_width, data = mdata_p0,bins=30, main = 'Low Cost Mobiles')
p1 = qplot(px_width, data = mdata_p1,bins=30, main = 'Medium Cost Mobiles')
p2 = qplot(px_width, data = mdata_p2,bins=30, main = 'High Cost Mobiles')
p3 = qplot(px_width, data = mdata_p3,bins=30, main = 'Very High Cost Mobiles')
grid.arrange(p0,p1,p2,p3, ncol=2, top="Mobiles Pixel Resolution Width Comparision")

#Average Mobile Pixel Resolution Width with different price range
with(mdata, tapply(px_width,price_range, mean))
'       0        1        2        3 
 1150.270 1251.908 1234.046 1369.838 '

#Barplot of Average Mobile Pixel Resolution Width
barplot(with(mdata, tapply(px_width,price_range, mean)),
        names.arg = c('Low', 'Medium', 'High', 'Very High'),
        col = brewer.pal(4,'YlOrRd'),
        main = 'Barplot of Mobiles Pixel Resolution Width',
        xlab = 'Price of Mobiles')

#Boxplot
boxplot(mdata$px_width,
        col = 'firebrick3', horizontal = TRUE,
        main = 'Boxplot of Mobiles Pixel Resolution Width',
        xlab = 'Mobiles Pixel Resolution Width')

#___________________________Random Access Memory in MegaBytes
str(mdata$ram)
'int [1:2000] 2549 2631 2603 2769 1411 1067 3220 700 1099 513 ...'

summary(mdata$ram)
'   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    256    1208    2146    2124    3064    3998 '

#Histogram
hist(mdata$ram,
     col = brewer.pal(8,'PuOr'),
     main = 'Histogram of Mobile RAM in MegaBytes',
     xlab = 'Mobile RAM')

#Histogram comparison of Mobile RAM with Price range
p0 = qplot(ram, data = mdata_p0,bins=30, main = 'Low Cost Mobiles')
p1 = qplot(ram, data = mdata_p1,bins=30, main = 'Medium Cost Mobiles')
p2 = qplot(ram, data = mdata_p2,bins=30, main = 'High Cost Mobiles')
p3 = qplot(ram, data = mdata_p3,bins=30, main = 'Very High Cost Mobiles')
grid.arrange(p0,p1,p2,p3, ncol=2, top="Mobiles RAM Comparision")

#Average Mobile RAM with different price range
with(mdata, tapply(ram,price_range, mean))
'       0        1        2        3 
  785.314 1679.490 2582.816 3449.232 '

#Barplot of Average Mobile RAM
barplot(with(mdata, tapply(ram,price_range, mean)),
        names.arg = c('Low', 'Medium', 'High', 'Very High'),
        col = brewer.pal(4,'PuOr'),
        main = 'Barplot of Mobiles RAM in MegaBytes',
        xlab = 'Price of Mobiles')

#Boxplot
boxplot(mdata$ram,
        col = 'blueviolet', horizontal = TRUE,
        main = 'Boxplot of Mobiles RAM in MegaBytes',
        xlab = 'Mobiles RAM')

#___________________________Screen Height of mobile in cm
str(mdata$sc_h)
' int [1:2000] 9 17 11 16 8 17 13 16 17 19 ...'

summary(mdata$sc_h)
'   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    5.00    9.00   12.00   12.31   16.00   19.00 '

#Histogram
hist(mdata$sc_h,
     col = brewer.pal(8,'Paired'),
     main = 'Histogram of Mobile Screen Height',
     xlab = 'Mobile Screen Height')

#Histogram comparison of Mobile Screen Height with Price range
p0 = qplot(sc_h, data = mdata_p0,geom = 'bar', main = 'Low Cost Mobiles')
p1 = qplot(sc_h, data = mdata_p1,geom = 'bar', main = 'Medium Cost Mobiles')
p2 = qplot(sc_h, data = mdata_p2,geom = 'bar', main = 'High Cost Mobiles')
p3 = qplot(sc_h, data = mdata_p3,geom = 'bar', main = 'Very High Cost Mobiles')
grid.arrange(p0,p1,p2,p3, ncol=2, top="Mobiles Screen Height Comparision")

#Average Mobile Screen Height with different price range
with(mdata, tapply(sc_h,price_range, mean))
'     0      1      2      3 
 12.324 12.212 12.010 12.680 '

#Barplot of Average Mobile Screen Height
barplot(with(mdata, tapply(sc_h,price_range, mean)),
        names.arg = c('Low', 'Medium', 'High', 'Very High'),
        col = brewer.pal(4,'Paired'),
        main = 'Barplot of Mobiles Screen Height',
        xlab = 'Price of Mobiles')

#Boxplot
boxplot(mdata$sc_h,
        col = 'cyan4', horizontal = TRUE,
        main = 'Boxplot of Mobiles Screen Height',
        xlab = 'Mobiles Screen Height')

#___________________________Screen Width of mobile in cm
str(mdata$sc_w)
'int [1:2000] 7 3 2 8 2 1 8 3 1 10 ...'

summary(mdata$sc_w)
'   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   0.000   2.000   5.000   5.767   9.000  18.000 '

#Histogram
hist(mdata$sc_w,
     col = brewer.pal(8,'RdYlBu'),
     main = 'Histogram of Mobile Screen width',
     xlab = 'Mobile Screen width')

#Histogram comparison of Mobile Screen Width with Price range
p0 = qplot(sc_w, data = mdata_p0,geom = 'bar', main = 'Low Cost Mobiles')
p1 = qplot(sc_w, data = mdata_p1,geom = 'bar', main = 'Medium Cost Mobiles')
p2 = qplot(sc_w, data = mdata_p2,geom = 'bar', main = 'High Cost Mobiles')
p3 = qplot(sc_w, data = mdata_p3,geom = 'bar', main = 'Very High Cost Mobiles')
grid.arrange(p0,p1,p2,p3, ncol=2, top="Mobiles Screen width Comparision")

#Average Mobile Screen Width of different price range
with(mdata, tapply(sc_w,price_range, mean))
'    0     1     2     3 
 5.682 5.544 5.714 6.128'

#Barplot of Average Mobile Screen Width
barplot(with(mdata, tapply(sc_w,price_range, mean)),
        names.arg = c('Low', 'Medium', 'High', 'Very High'),
        col = brewer.pal(4,'RdYlBu'),
        main = 'Barplot of Mobiles Screen width',
        xlab = 'Price of Mobiles')

#Boxplot
boxplot(mdata$sc_w,
        col = 'darkred', horizontal = TRUE,
        main = 'Boxplot of Mobiles Screen width',
        xlab = 'Mobiles Screen width')

#___________________________Talk Time
'The longest time that a single battery charge will last when you are'

str(mdata$talk_time)
' int [1:2000] 19 7 9 11 15 10 18 5 20 12 ...'

summary(mdata$talk_time)
'   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   2.00    6.00   11.00   11.01   16.00   20.00 '

#Histogram
hist(mdata$talk_time,
     col = brewer.pal(8,'Spectral'),
     main = 'Histogram of Mobile Talk Time',
     xlab = 'Mobile Talk Time')

#Histogram comparison of Mobile Talk Time with Price range
p0 = qplot(talk_time, data = mdata_p0,geom = 'bar', main = 'Low Cost Mobiles')
p1 = qplot(talk_time, data = mdata_p1,geom = 'bar', main = 'Medium Cost Mobiles')
p2 = qplot(talk_time, data = mdata_p2,geom = 'bar', main = 'High Cost Mobiles')
p3 = qplot(talk_time, data = mdata_p3,geom = 'bar', main = 'Very High Cost Mobiles')
grid.arrange(p0,p1,p2,p3, ncol=2, top="Mobiles Talk Time Comparision")

#Average Mobile Talk Time with different price range
with(mdata, tapply(talk_time,price_range, mean))
'     0      1      2      3 
 10.612 11.362 10.972 11.098 '

#Barplot of Average Mobile Talk Time
barplot(with(mdata, tapply(talk_time,price_range, mean)),
        names.arg = c('Low', 'Medium', 'High', 'Very High'),
        col = brewer.pal(4,'Spectral'),
        main = 'Barplot of Mobiles Talk Time',
        xlab = 'Price of Mobiles')

#Boxplot
boxplot(mdata$talk_time,
        col = 'royalblue4', horizontal = TRUE,
        main = 'Boxplot of Mobiles Talk Time',
        xlab = 'Mobiles  Talk Time')

#___________________________Three G

str(mdata$three_g)
'int [1:2000] 0 1 1 1 1 1 1 1 1 1 ...'

#Converting into categories
mdata$three_g = as.factor(mdata$three_g)

str(mdata$three_g)
'Factor w/ 2 levels "0","1": 1 2 2 2 2 2 2 2 2 2 ...'

#Barplot
barplot(table(mdata$three_g),
        names.arg = c('No', 'Yes'),
        col = c('steelblue4', 'tomato4'),
        main = 'Histogram of Mobile Three G',
        xlab = 'Mobile Three G')

#Histogram comparison of Mobile Touch screen with Price range
p0 = qplot(three_g, data = mdata_p0,geom = 'bar', main = 'Low Cost Mobiles')
p1 = qplot(three_g, data = mdata_p1,geom = 'bar', main = 'Medium Cost Mobiles')
p2 = qplot(three_g, data = mdata_p2,geom = 'bar', main = 'High Cost Mobiles')
p3 = qplot(three_g, data = mdata_p3,geom = 'bar', main = 'Very High Cost Mobiles')
grid.arrange(p0,p1,p2,p3, ncol=2, top="Mobiles having Three G Comparision")

#___________________________Touch Screen
str(mdata$touch_screen)
'int [1:2000] 0 1 1 0 1 0 0 1 0 0 ...'

#Converting into categories
mdata$touch_screen = as.factor(mdata$touch_screen)

str(mdata$touch_screen)
'Factor w/ 2 levels "0","1": 1 2 2 1 2 1 1 2 1 1 ...'

#Barplot
barplot(table(mdata$touch_screen),,
        names.arg = c('No', 'Yes'),
        col = c('orchid4', 'palegreen4'),
        main = 'Histogram of Mobile Touch screen',
        xlab = 'Mobile Touch screen')

#Histogram comparison of Mobile Touch screen with Price range
p0 = qplot(touch_screen, data = mdata_p0,geom = 'bar', main = 'Low Cost Mobiles')
p1 = qplot(touch_screen, data = mdata_p1,geom = 'bar', main = 'Medium Cost Mobiles')
p2 = qplot(touch_screen, data = mdata_p2,geom = 'bar', main = 'High Cost Mobiles')
p3 = qplot(touch_screen, data = mdata_p3,geom = 'bar', main = 'Very High Cost Mobiles')
grid.arrange(p0,p1,p2,p3, ncol=2, top="Mobiles Touch screen Comparision")

#___________________________Wifi
str(mdata$wifi)
'int [1:2000] 1 0 0 0 0 0 1 1 0 0 ...'

#Converting into categories
mdata$wifi = as.factor(mdata$wifi)

str(mdata$wifi)
'Factor w/ 2 levels "0","1": 2 1 1 1 1 1 2 2 1 1 ...'

#Barplot
barplot(table(mdata$wifi),
        names.arg = c('No', 'Yes'),
        col = c('turquoise4', 'violetred4'),
        main = 'Histogram of Mobile Wifi',
        xlab = 'Mobile Wifi')

#Histogram comparison of Mobile Wifi with Price range
p0 = qplot(wifi, data = mdata_p0,geom = 'bar', main = 'Low Cost Mobiles')
p1 = qplot(wifi, data = mdata_p1,geom = 'bar', main = 'Medium Cost Mobiles')
p2 = qplot(wifi, data = mdata_p2,geom = 'bar', main = 'High Cost Mobiles')
p3 = qplot(wifi, data = mdata_p3,geom = 'bar', main = 'Very High Cost Mobiles')
grid.arrange(p0,p1,p2,p3, ncol=2, top="Mobiles Wifi Comparision")


#Selecting only numeric columns
num_col = unlist(lapply(mdata, is.numeric))

mdata_num = mdata[,num_col]
str(mdata_num)

#Correlation plot for numeric data
corrplot(round(cor(mdata_num),1), method = 'number')

corrplot(round(cor(mdata_num),1), method = 'color',
         col = brewer.pal(8, 'RdYlGn'))

colnames(mdata)
#Pairs Panels
pairs.panels(mdata[c(1:4,21)])

pairs.panels(mdata[c(5:8,21)])

pairs.panels(mdata[c(9:11,21)])

pairs.panels(mdata[c(12:16,21)])
'
Pixel Height & Width correlated .51
Screen Height & Width correlated .51
Ram & Price Range correlated .92'

pairs.panels(mdata[c(17:20,21)])

#Splitting the data into train & test
set.seed(121)
select_rows_80 = sample(1:nrow(mdata), round(0.8*nrow(mdata)),replace = FALSE)         
mdata_train = mdata[select_rows_80,]
mdata_test = mdata[-select_rows_80,]

#Building a Tree Model
model_tree = tree(price_range~.-price_range, data = mdata, method = 'class')

#Plotting the Treee
plot(model_tree, type='uniform')
text(model_tree, cex=0.8)

#Summary of the Tree
summary(model_tree)

#Prediction
tree_pred  = predict(model_tree, mdata_test, type = 'class')

tab = table(tree_pred, mdata_test$price_range)

#Accuracy
sum(diag(tab))/sum(tab)
'80.25'

#Pruning
set.seed(121)
cv_mdata = cv.tree(model_tree, FUN = prune.misclass) 

cv_mdata

#Plotting the error
par(mfrow = c(1,2))
plot(cv_mdata$size, cv_mdata$dev, type = 'b', col='navy', lwd=2)
plot(cv_mdata$k, cv_mdata$dev, type = 'b', col='darkred', lwd=2)
par(mfrow = c(1,1))

#Prune the tree 4 classes
prune_mdata = prune.misclass(model_tree, best = 8)
plot(prune_mdata, type='uniform')
text(prune_mdata, cex=0.8)

#Prediction
prune_pred  = predict(prune_mdata, mdata_test, type = 'class')

tab1 = table(prune_pred, mdata_test$price_range)

#Accuracy
sum(diag(tab1))/sum(tab1)
'0.75'

#Bagging
set.seed(25)
bag_mdata = randomForest(price_range~.-price_range, mdata_train, 
                         mtry=10, importance = TRUE)
'mtry - how many variables we should select at a node split'
#Getting Important variables
importance(bag_mdata)

#Plotting the important variables
varImpPlot(bag_mdata, col='darkred',pch=20, cex=0.7)

#Output of bagging
bag_mdata

#Accuracy
sum(diag(bag_mdata$confusion))/sum(bag_mdata$confusion)
'0.8947638'

#Random Forest with mtry 4
rf_mdata = randomForest(price_range~.-price_range, mdata_train, 
                        mtry=4, importance = TRUE)

#Getting Important variables
importance(rf_mdata)

#Plotting the important variables
varImpPlot(rf_mdata, col='darkgreen',pch=10, cex=0.7)

#Output of bagging
rf_mdata

#Accuracy
sum(diag(rf_mdata$confusion))/sum(rf_mdata$confusion)
'0.8803601'




################################################################################

#install.packages("superml")

library(superml)

gscv = GridSearchCV$new(trainer = RFTrainer$new(),
                 parameters = list(n_estimators = c(100,200,300),
                                   max_depth = c(3,6,9),
                                   min_node_size = c(6,12,18)))

gscv$fit(mdata_train,"price_range")

?GridSearchCV
is.null(mdata_train)


###















model_tree = rpart(price_range~.-price_range, data = mdata, method = 'class')

#Plotting the tree
rpart.plot(model_tree, cex = 0.65,fallen.leaves = FALSE)

#Summary of the Model
summary(model_tree)

#Prediction
tree_pred = predict(model_tree, mdata_test, type = 'class')

confusionMatrix(tree_pred, mdata_test$price_range)
"Confusion Matrix and Statistics

          Reference
Prediction  0  1  2  3
         0 80 23  0  0
         1  7 71  9  0
         2  0 21 65 14
         3  0  0 19 91

Overall Statistics
                                        
               Accuracy : 0.7675        
                 95% CI : (0.723, 0.808)
    No Information Rate : 0.2875        
    P-Value [Acc > NIR] : < 2.2e-16     
                                        
                  Kappa : 0.6905        
 Mcnemar's Test P-Value : NA            

Statistics by Class:
  
                       Class: 0 Class: 1 Class: 2 Class: 3
Sensitivity            0.9195   0.6174   0.6989   0.8667
Specificity            0.9265   0.9439   0.8860   0.9356
Pos Pred Value         0.7767   0.8161   0.6500   0.8273
Neg Pred Value         0.9764   0.8594   0.9067   0.9517
Prevalence             0.2175   0.2875   0.2325   0.2625
Detection Rate         0.2000   0.1775   0.1625   0.2275
Detection Prevalence   0.2575   0.2175   0.2500   0.2750
Balanced Accuracy      0.9230   0.7806   0.7925   0.9011"

