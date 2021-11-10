# R基本數學操作
sqrt(5)
abs(-94)
5%%2
5%/%2


# 安裝及匯入套件
install.packages("installr")
require(installr)
library(installr)
install.packages("lattice")
require(lattice)
install.packages("pastecs") 
require(pastecs)


#更新R語言(需考量版本問題)
updateR()

x1=c(1,2,3,NA,5)

data1 <- data.frame(x1=c(1,2,3,NA,5),
                    y1=c(4,5,3,NA,NA))

data1[,"x1"]
data1$x1
data1[,"y1"]
data1$y1

data1[1,]
data1[3,]


# 遺漏值的地方，標註為TRUE (TRUE/FALSE矩陣的型態)
is.na(data1)


# 移除有遺漏值資料的列
data1
na.omit(data1)


# 用平均數填補遺漏值
data1
data1[is.na(data1$y1), "y1"] <- mean(data1$y1, na.rm=T)
data1[is.na(data1$x1), "x1"] <- mean(data1$x1, na.rm=T)
data1


# 一些統計名詞
HW=read.csv(file.choose()) # 身高體重.csv
h=HW$身高
h

mean(h)     #平均數
median(h)   #中位數
var(h)
sd(h)
max(h)
min(h)
range(h)
# 全距=226-110=116
quantile(h)
# IQR=Q3-Q1=181-163.75=17.25


#眾數
table(h) # 出現次數
max(table(h)) # 出現最大次數
names(table(h)) # 出現次數的名稱
names(table(h))[which(table(h) == max(table(h)))] # 出現最大次數的名稱(眾數)
as.integer(names(table(h))[which(table(h)==max(table(h)))]) # 轉成int


Mode<-function(x) # 建立眾數的function
{
  ux<-sort(unique(x))
  tab<-tabulate(match(x,ux))
  ux[tab==max(tab)]
}

Mode(h)



#各式統計圖表

#圓餅圖-Tires
#col圖例中出現的點或線的顏色
#round將 x 四捨五入到第 n 位
#paste()函数主要是用於字符串連接
#prop.table函數：頻率統計函數
#（1）prop.table(data)：將data轉換爲百分比
#（2）prop.table(data,1)：將data按行求百分比
#（3）prop.table(data,2)：將data按列求百分比

Tires = read.csv(file.choose()) # 導入Tires.csv

percent <- round(prop.table(Tires$Counts)*100, 2)
percent

label <- paste(Tires$CausesA," ", percent, "%", sep="")
label

pie(Tires$Counts,labels = label, col= c("skyblue","lightgreen","red","blue","lightyellow"), main = "Tires")



#長條圖-Tires
barplot(Tires$Counts, names.arg=Tires$CausesA, main="Tires", ylim = c(0,500), col=rainbow(7))



#直方圖1-重量
HW=read.csv(file.choose()) #身高體重
hist(HW$體重, main="體重", xlab = "體重", ylab="次數")
hist(HW$身高, main="身高", xlab = "身高", ylab="次數")


# 常態/偏態檢定
shapiro.test(W$重量)
# 若p值<0.05，代表資料是偏態的(不是常態)
# 若p值>0.05，代表資料是常態的(不是偏態)


#直方圖2-尺寸
S=read.csv(file.choose()) # 尺寸
hist(S$尺寸, main="尺寸", xlab = "尺寸", ylab="次數",
     col = "orange",border = "blue", breaks = 5)

# 常態/偏態檢定
shapiro.test(S$尺寸)
# 若p值>0.05，則代表常態分配



#箱形圖-尺寸
boxplot(S$尺寸, main="尺寸")


#箱形圖-身高體重
HW=read.csv(file.choose()) #身高體重
boxplot(HW, main="身高體重", border="black", col=c("orange","green"))


#折線圖-Storage
Storage = read.csv(file.choose())
plot(Storage[,2],xlab="Month",ylab="Storage", main="數量",lwd=4) # lwd為寬度
plot(Storage[,2],xlab="Month",ylab="Storage", main="數量",lwd=4, type = "l")



#散佈圖
require(datasets)
airquality_data = airquality
plot(x=airquality_data$Ozone,      # X軸的值
     y=airquality_data$Wind,       # Y軸的值
     main="airquality_data: Ozone to Wind",    # 圖片名稱
     xlab="Ozone(ppb)",       # X軸的名稱
     ylab="Wind(mph)")        # Y軸的名稱


#其他補充參考
#https://rpubs.com/skydome20/R-Note4-Plotting_System



#相關性(Correlation)-廣告費
AS=read.csv(file.choose())
cor.test(AS$Ad,AS$Sales)
plot(AS)




airquality_data = airquality
# 相關性
cor.test(airquality_data$Ozone,airquality_data$Wind)
# 迴歸模型
lm_model <- lm(Wind~Ozone, airquality_data)    # 建立一個Y~X的線性回歸
lm_model ### Wind = -0.06519*Ozone + 12.60843
# ANOVA
anova(lm_model)
# 散佈圖
plot(x=airquality_data$Ozone,      # X軸的值
     y=airquality_data$Wind,       # Y軸的值
     main="airquality_data: Ozone to Wind",    # 圖片名稱
     xlab="Ozone(ppb)",       # X軸的名稱
     ylab="Wind(mph)")        # Y軸的名稱
# 原本的圖畫上回歸線
abline(lm_model, lwd=4)     # lwd代表線的粗細


# 額外補充參考
install.packages("lm.beta")
require(lm.beta)
lm_model_beta = lm.beta(lm_model)
summary(lm_model_beta)




# Lemonade迴歸
L=read.csv(file.choose())
cor.test(L$Temperature,L$Sales)
plot(L$Temperature,L$Sales)

L_lm<-lm(Sales~Temperature,data=L)
L_lm

abline(L_lm)
anova(L_lm) #檢定Y與X是否有迴歸關聯

L_lm_beta = lm.beta(L_lm)
summary(L_lm_beta)
#Ho:天氣溫度對檸檬水銷售額的影響無影響/無差異(天氣溫度非影響檸檬水銷售額的真因)
#Ha:天氣溫度對檸檬水銷售額的影響有影響/有差異(天氣溫度是影響檸檬水銷售額的真因)
#因為p<0.05，所以接受Ha、拒絕Ho
#天氣溫度會顯著影響檸檬水銷售額




# Lemonade複回歸
L_lm<-lm(Sales~Temperature+Rainfall+Price, data=L)
L_lm
anova(L_lm)

L_lm_beta = lm.beta(L_lm)
summary(L_lm_beta)





# 複回歸
L_lm<-lm(Sales~Temperature+Rainfall+Flyers+Price, data=L)
L_lm
anova(L_lm)

L_lm_beta = lm.beta(L_lm)
summary(L_lm_beta)

#預測
new <- data.frame(Temperature=27.0, Rainfall=2.0, Flyers=15.0, Price=0.3)
new
predict(L_lm,new)





# Hierarchical Clustering
#25個歐洲國家（n = 25個單位）及其來自9個主要食物來源的蛋白質攝入量（百分比）（p = 9）。數據如下所示。
url = 'http://www.biz.uiowa.edu/faculty/jledolter/DataMining/protein.csv'
food <- read.csv(url)
food

food=read.csv(file.choose()) # protein.csv


#在階層式分群中，主要是以資料之間的「距離」遠近，來決定兩筆資料是否接近。
#R的話，我們可以使用dist()，來建立資料之間的「距離矩陣」(Distance Matrix)，判斷資料之間的遠與近：
E_dist <- dist(food, method="euclidean") # 歐式距離
M_dist <- dist(food, method="manhattan") # 曼哈頓距離

par(mfrow=c(1,2)) # 讓圖片以1x2的方式呈現

# 使用歐式距離進行分群
h_E_cluster <- hclust(E_dist)
plot(h_E_cluster, xlab="歐式距離")

# 使用曼哈頓距離進行分群
h_M_cluster <- hclust(M_dist) 
plot(h_M_cluster, xlab="曼哈頓距離")





# K-means Clustering
# 由於分群屬於「非監督式學習」的演算法，因此我們先把iris內的Species欄位拿掉，以剩下的資料進行分群：
iris_data = read.csv(file.choose()) # iris

iris_data_without_species <- iris_data[, 1:4] # 因為Species是第五欄位，故只取第1~4欄
head(iris_data_without_species)          # iris只剩下前四個欄位的資料

# 分成三群
kmeans.cluster <- kmeans(iris_data_without_species,centers=3)

# 群內的變異數
kmeans.cluster$withinss

# 分群結果和實際結果比較(其實沒有意義)
table(kmeans.cluster$cluster, iris_data$species)


# 視覺化k-means分群結果
install.packages("factoextra")
require(factoextra)
fviz_cluster(kmeans.cluster,   # 分群結果
             data = iris_data_without_species,   # 資料
             geom = c("point","text"), # 點和標籤(point & label)
             frame.type = "norm")      # 框架型態