#####
# R基本數學操作

sqrt(5)
abs(-94)
5%%2
5%/%2


# 安裝及匯入套件
install.packages("installr")
require(installr)

#更新R語言(需考量版本問題)
updateR()

x=c(1,2,3,NA,5)

data1 <- data.frame(x=c(1,2,3,NA,5),
                    y=c(4,5,3,NA,NA))

data1[,"x"]
data1[,"y"]
data1[1,]
data1[3,]


# 遺漏值的地方，標註為TRUE (TRUE/FALSE矩陣的型態)
is.na(data1)


# 移除有遺漏值資料的列

na.omit(data1)


# 用平均數填補遺漏值

data1[is.na(data1[,"y"]), "y"] <- mean(data1[,"y"], na.rm=T)
data1[is.na(data1[,"x"]), "x"] <- mean(data1[,"x"], na.rm=T)
data1


#####
#各式統計圖表

#圓餅圖-Tires-作法1
#Puncture刺孔
#Valve Stem Leak氣門桿泄漏
#Damaged Sidewall輪胎壁損害
#Valve Core Leak氣門芯泄漏
#Damaged Liner內襯損害
#Leak From Seating底座泄漏
#cex字符大小
#col圖例中出現的點或線的顏色
#rainbow
#http://iccm.cc/colors-and-palettes-in-r-language/
    #round將 x 四捨五入到第 n 位
    #在R中，paste() 函数主要是用於字符串連接
    #legend:Add Legends to Plots | 圖例
    #https://walkonnet.com/archives/29986
Counts <- c(414,397,209,184,132,132)
CausesA <- c(" Puncture "," Valve Stem Leak "," Damaged Sidewall "," Valve Core Leak", " Damaged Liner" ," Leak From Seating ")
color = c("skyblue","lightgreen","red","blue","lightyellow","yellow")

pie(Counts, labels= CausesA, col= color)


per.Counts <- paste(round(100 * Counts / sum(Counts),2), "%")
slice.col <- rainbow(10)
pie(Counts,labels = per.Counts, col= slice.col, main = "輪胎漏氣各原因相對所佔的百分比")
legend("topright", CausesA, cex=0.55, fill=slice.col) 


#圓餅圖-Tires-作法2
#prop.table函數：頻率統計函數
#（1）prop.table(data)：將data轉換爲百分比
#（2）prop.table(data,1)：將data按行求百分比
#（3）prop.table(data,2)：將data按列求百分比
#其中需要注意的是data的數據類型爲矩陣（as.matrix(data)）

A=read.csv(file.choose())
percent<-round(prop.table(A$Counts)*100,2)
label<-paste(A$CausesA,percent,"%",sep="")
pie(A$Counts,labels =label,col= c("skyblue","lightgreen","red","blue","lightyellow"),main = "輪胎漏氣各原因相對所佔的百分比")


#長條圖-圓餅圖輪胎漏氣練習題-作法2

Tires=read.csv(file.choose())
Tires
percent<-round(prop.table(Tires$Counts)*100,2)
label<-paste(Tires$CausesA, percent, "%", sep="")
pie(Tires$Counts, labels =label,
    col= c("skyblue","lightgreen","red","blue","lightyellow"),
    main = "不良項目所佔的百分比")

#長條圖-Exh_qc-作法2
A=read.csv(file.choose())
A
barplot(A$次數,names.arg=A$Flaws,border="green",
        main="不良",col=c("red","orange","lightblue","yellow"))
barplot(A$次數,names.arg=A$Flaws,border="green",
        main="不良")
barplot(A$次數,names.arg=A$Flaws,
        main="不良")


#Practice1（Exh_qc）
#Exh-qc圓餅圖
A=read.csv(file.choose())
A
percent<-round(prop.table(A$次數)*100,2)
label<-paste(A$Flaws,percent,"%",sep="")
pie(A$次數,labels =label,col= c("skyblue","lightgreen","red","blue","lightyellow"),main = "退貨數量")


#長條圖-圓餅圖輪胎漏氣練習題-作法2
A=read.csv(file.choose()) # 圓餅圖長條圖練習題
A
barplot(A$件數,names.arg=A$不良項目,border="green",main="不良",
        col=c("red","orange","lightblue","yellow","lightgreen"))


#直方圖-重量範例-作法2
A=read.csv(file.choose()) #重量
A
view(A)
summary(A)
hist(A$重量, main="直方圖示例", xlab = "重量", ylab="高度",
     col = "green", border = "red", 
     xlim = c(50,85), ylim = c(0,10),breaks = 8)
shapiro.test(A$重量) # 常態檢定 # 若p值>0.05，則代表常態分配


#直方圖-尺寸練習題-作法2
A=read.csv(file.choose()) # 尺寸
A
view(A)
summary(A)
hist(A$尺寸, main="直方圖演練", xlab = "尺寸", ylab="次數",
     col = "orange",border = "blue", 
     xlim = c(120,155), ylim = c(0,30),breaks = 5)
hist(A$尺寸, main="直方圖演練", xlab = "尺寸", ylab="次數",
     col = "orange",border = "blue", 
     breaks = 5)
shapiro.test(A$尺寸) # 常態檢定 # 若p值>0.05，則代表常態分配



#集中與離散趨勢-example
A=read.csv(file.choose()) # 身高體重
A
summary(A) # R語言中無奇數的計算，是偶數相加/2
h<-A$身高
h=A$身高
mean(h)     #平均數
median(h)   #中位數
sd(h)
var(h)
max(h)
min(h)
range(h)
#全距=226-110=116
quantile(h)
# IQR=Q3-Q1=181-163.75=17.25


#眾數
table(h) # 出現次數
max(table(h)) # 出現最大次數
names(table(h)) # 出現次數的名稱
names(table(h))[which(table(h)==max(table(h)))] # 出現最大次數的名稱(眾數)
as.integer(names(table(h))[which(table(h)==max(table(h)))]) # 轉成int

#集中離散趨勢-身高體重練習題
A=read.csv(file.choose())
install.packages(c("pastecs")) 
library(pastecs) 
A
summary(A)
round(stat.desc(A$身高),2) #小數點2位數
round(stat.desc(A$身高),1) #小數點1位數
round(stat.desc(A$身高))   #整數
round(stat.desc(A$體重),2)

Mode<-function(x) # 建立重數的function
{
    ux<-sort(unique(x))
    tab<-tabulate(match(x,ux))
    ux[tab==max(tab)]
}
Mode(A$身高)
Mode(A$體重)


#箱形圖-產出量作法1
x <- c(660, 957, 355, 378, 425, 477, 183, 345, 354, 536)
f <- factor(rep(c("產出量(台)_白","產出量(台)_夜"), each=5)) #定義分組因子
f
data<- data.frame(x,f) #生成數據框
boxplot(x~f,data)

#箱形圖-產出量作法2
A=read.csv(file.choose()) #產出量
產出量.台._白<- c(A$產出量.台._白)
產出量.台._夜<- c(A$產出量.台._夜)
data <- data.frame(產出量.台._白, 產出量.台._夜)
data
boxplot(data, main="產出量", border="black",
        col=c("orange","green","blue")) # blue用不到


#時間數列圖-Storge作法2
Storage=read.csv(file.choose())
Storage<-ts(Storage, start=1) #從1月開始
plot(Storage[,2],xlab="Month",ylab="Storage",
     main="盤點記錄產品的庫存數量",lwd=4) # lwd為線寬

#時間數列圖table2_3作法
table2_3=read.csv(file.choose())
table2_3<-ts(table2_3,start=1950) #從1950年開始
plot(table2_3[,2],xlab="學年度",ylab="人數/萬",main="歷年小學生數",lwd=7)

#補充
#學習網站
#R筆記–(4)繪圖-資料視覺化
#https://rpubs.com/skydome20/R-Note4-Plotting_System

require(datasets) # R語言的內建datasets
head(airquality) # airquality的dataset

hist(x=airquality$Month, 
     main="Histogram of Month",         # 圖片的名稱
     xlab="Month",                      # X軸的名稱
     ylab="Frequency")                  # Y軸的名稱

boxplot(formula = Ozone ~ Month, # Y ~ X (代表X和Y軸要放的數值) 
        data = airquality,       # 資料
        xlab = "Month",          # X軸名稱
        ylab = "Ozone (ppb)",    # Y軸名稱
        col ="gray")             # 顏色

plot(x=airquality$Month,            # X軸的值
     y=airquality$Temp,             # Y軸的值
     main="Month to Temperature",   # 圖片名稱
     xlab="Month(1~12)",            # X軸名稱
     ylab="Temperature(degrees F)") # Y軸名稱

plot(x=airquality$Ozone,      # X軸的值
     y=airquality$Wind,       # Y軸的值
     main="Ozone to Wind",    # 圖片名稱
     xlab="Ozone(ppb)",       # X軸的名稱
     ylab="Wind(mph)")        # Y軸的名稱



# 建立一個畫布，上面已經有一張散布圖(Ozone to Wind)
plot(x=airquality$Ozone,
     y=airquality$Wind,
     main="Ozone to Wind",
     xlab="Ozone(ppb)",
     ylab="Wind(mph)",
     pch=16                  # 點的圖形
) 
# 現在我們要在這張圖片中，把5月的資料點用藍色標註上去
May_data <- airquality[airquality$Month==5, ]   # 找出5月的資料
# 標上藍色的點
points(x=May_data$Ozone,                       
       y=May_data$Wind, 
       pch=16,                  # 點的圖形
       col="blue")              # 顏色
# 同理，也可以把8月的資料點用紅色標註上去
Aug_data <- airquality[airquality$Month==8, ]   # 找出8月的資料
# 標上紅色的點
points(x=Aug_data$Ozone, 
       y=Aug_data$Wind, 
       pch=16,               # 點的圖形
       col="red")            # 顏色
# 在右上角做出標示
legend("topright",                                # 表示在右上角
       pch = 1,                                   # pch代表點的圖案
       col = c("blue", "red", "black"),           # col代表顏色 
       legend = c("May", "August", "Other Month") # 顏色所對應的名稱
)
# 我們也可以畫出回歸趨勢線
lm.model <- lm(Wind~Ozone, airquality)    # 建立一個線性回歸
# 畫上回歸的趨勢線
abline(lm.model, lwd=2)     # lwd代表線的粗細











#分月份機率密度圖
densityplot( ~ Ozone| Month ,      
             data=airquality)    
#全部機率密度圖
densityplot( ~ Ozone ,      
             data=airquality)


#目的:我們想要在散布圖中，畫出線性回歸的趨勢線 #
# 卡溝斷練習
A=read.csv(file.choose())
A
View(A)
xyplot(x=A$壓力~A$厚度 ,         # 壓力放在Y軸，厚度放在X軸
       data=A,     
       
       # 在這裡，我們要使用panel function，畫出線性回歸的趨勢線
       panel=function(x,y){  
           # function的寫法，會用大括號包起來，裡面表示要進行的動作：
           # 在這個panel function裡面，我們進行了三個動作
           
           panel.xyplot(x, y)             # 2.繪製x-y的散布圖
           panel.lmline(x, y, col="blue")  # 3.畫出線性回歸的趨勢線
       }
       
)


# 壓力放在Y軸，厚度放在X軸
A=read.csv(file.choose())
qplot(x=A$厚度,                               
      y=A$壓力,                              
      data=A,                      
      geom="point",                         # 圖形=scatter plot
      main = "Scatter Plot ",  
      xlab="厚度",                          
      ylab="壓力")


#月份分群
qplot(x=Temp,                               
      y=Ozone,                              
      data=airquality,                      
      geom="point",                         # 圖形=scatter plot
      main = "Scatter Plot of Ozone-Temp",  
      xlab="Temp",                          
      ylab="Ozone(ppb)",                    
      color=factor(Month)                           # 以顏色標註月份，複合式的散布圖
)
#月份分群
qplot(x=Temp,                             
      data=airquality,                     
      geom="density",        # 圖形=density
      xlab="Temp",                         
      color=factor(Month)            # 以顏色標註月份，複合式的機率密度圖
)
#月份分群
qplot(x=Month,                               
      y=Ozone,
      data=airquality,                     
      geom="boxplot",       # 圖形=boxplot
      xlab="Temp",                          
      color=factor(Month)              # 以顏色標註月份，複合式的合鬚圖
)




###################################
#相關性(Correlation)-廣告費
A=read.csv(file.choose())
cor.test(A[,1],A[,2])
plot(A)
#	Pearson's product-moment correlation
#data:  A[, 1] and A[, 2]
#t = 19.977, df = 10, p-value = 2.17e-09
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.9553160 0.9966557
#sample estimates:
#      cor 
#0.9877024 
#P值小於<0.05，也就是我們認為廣告費與銷售量有相關
#r=0.988強相關



#迴歸分析與散佈圖-Hospital
A=read.csv(file.choose())
cor.test(A[,1],A[,2])

#	Pearson's product-moment correlation
#data:  A[, 1] and A[, 2]
#t = 14.051, df = 8, p-value = 6.39e-07
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
#    0.9162712 0.9954963
#sample estimates:
#    cor 
#0.9803343 


#P值小於<0.05，也就是我們認為住院天數(x)與開銷(y)有相關
#r=0.980強相關
plot(A)
fit<-lm(開銷~住院天數,data=A)
abline(fit)
anova(fit)

#Analysis of Variance Table
#Response: 開銷
#Df   Sum Sq  Mean Sq F value   Pr(>F)    
#住院天數   1 20768648 20768648  197.42 6.39e-07 ***
#    Residuals  8   841602   105200                     
#---
#    Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#P值小於<0.05，也就是我們認為住院天數(x)與開銷(y)有相關



install.packages("lm.beta")
library(lm.beta)
fit.beta<-lm.beta(fit)
summary(fit.beta)

#Call:
#    lm(formula = 開銷 ~ 住院天數, data = A)
#
#Residuals:
#    Min      1Q  Median      3Q     Max 
#-438.58 -135.85  -40.33   89.15  668.63 
#
#Coefficients:
#    Estimate Standardized Std. Error t value Pr(>|t|)    
#(Intercept)  63.4701       0.0000   185.4881   0.342    0.741    
#住院天數    339.3016       0.9803    24.1485  14.051 6.39e-07 ***
#    ---
#    Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 324.3 on 8 degrees of freedom
#Multiple R-squared:  0.9611,	Adjusted R-squared:  0.9562 
#F-statistic: 197.4 on 1 and 8 DF,  p-value: 6.39e-07
#1.回歸方程式
#開銷=63.47+339.30*天數
#2.住院天數是顯著真因
#住院多1天,開銷多339元
#3.標準化的迴歸係數為0.9803


#請以Lemonade為例, Sales為y, Temperature(x),完成迴歸分析與預測
A=read.csv(file.choose())
A
View(A)
cor.test(A$Temperature,A$Sales)
plot(A$Temperature,A$Sales)
fit<-lm(A$Sales~A$Temperature,data=A)
abline(fit)
anova(fit)
install.packages("lm.beta")
library(lm.beta)
fit.beta<-lm.beta(fit)
summary(fit.beta)







#多元回歸分析-卡勾斷
A=read.csv(file.choose())
fit<-lm(壓力~肉厚+厚度+高度+肉厚,data=A)
anova(fit)
#Analysis of Variance Table
#
#Response: 壓力
#Df   Sum Sq  Mean Sq  F value    Pr(>F)    
#肉厚       1 0.219769 0.219769 256.5866 < 2.2e-16 ***
#    厚度       1 0.020834 0.020834  24.3245 4.582e-06 ***
#    高度       1 0.000017 0.000017   0.0198    0.8883    
#Residuals 77 0.065951 0.000857                       
#---
#    Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#肉厚,厚度P<0.05為顯著重要因子,高度P>0.05,不顯著
#Ho:肉厚對壓力的影響是無影響無差異(肉厚非影響壓力的真因)
#Ha:肉厚對壓力的影響是有影響有差異(肉厚是影響壓力的真因)
#因為肉厚P=<0.05=>接受Ha
#肉厚是影響壓力的真因

#Ho:高度對壓力的影響是無影響無差異(高度非影響壓力的真因)
#Ha:高度對壓力的影響是有影響有差異(高度是影響壓力的真因)
#因為高度P>0.05=>接受Ho
#高度非影響壓力的真因

#肉厚,厚度P<0.05為顯著重要因子
#P值：
#(1)用來判斷允許錯誤的最大機率值
#(2)樣本能代表母體的可能性機率
#錯誤：
#(1) Type 1 Error(誤殺)：把好品檢驗成壞品的風險(alpha=0.05)(加班再生產)
#(2) Type 2 Error(露放)：把壞品檢驗成好品的風險(beta=0.20)(客訴的來源)

# 刪除不顯著的原因(高度)
fit<-lm(壓力~肉厚+厚度+肉厚,data=A)
anova(fit)



fit<-lm(壓力~肉厚+厚度,data=A)
anova(fit)
#Analysis of Variance Table
#
#Response: 壓力
#Df   Sum Sq  Mean Sq F value    Pr(>F)    
#肉厚       1 0.219769 0.219769 259.852 < 2.2e-16 ***
#    厚度       1 0.020834 0.020834  24.634  3.98e-06 ***
#    Residuals 78 0.065968 0.000846                      
#---
#    Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



anova(update(fit,~1),fit)
#Analysis of Variance Table
#
#Model 1: 壓力 ~ 1
#Model 2: 壓力 ~ 肉厚 + 厚度
#Res.Df      RSS Df Sum of Sq      F    Pr(>F)    
#1     80 0.306572                                  
#2     78 0.065968  2    0.2406 142.24 < 2.2e-16 ***
#    ---
#    Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#影響卡勾強度的顯著因子有卡勾肉厚,卡勾厚度
#SST=0.306572


install.packages("lm.beta")
library(lm.beta)
fit<-lm(壓力~肉厚+肉厚+厚度,data=A)
fit.beta<-lm.beta(fit)
summary(fit.beta)
#Call:
#    lm(formula = 壓力 ~ 肉厚 + 厚度, data = A)
#
#Residuals:
#    Min        1Q    Median        3Q       Max 
#-0.078676 -0.015205  0.003216  0.021075  0.050593 
#
#Coefficients:
#    Estimate Standardized Std. Error t value Pr(>|t|)    
#(Intercept) -0.16740      0.00000    0.05483  -3.053   0.0031 ** 
#    肉厚         0.35381      0.81899    0.02282  15.506  < 2e-16 ***
#    厚度         0.20406      0.26215    0.04111   4.963 3.98e-06 ***
#    ---
#    Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 0.02908 on 78 degrees of freedom
#Multiple R-squared:  0.7848,	Adjusted R-squared:  0.7793 
#F-statistic: 142.2 on 2 and 78 DF,  p-value: < 2.2e-16
#壓力 = - 0.167 + 0.354 肉厚 + 0.204 厚度

# Iris：sepal_length~sepal_width petal_length petal_width
A=read.csv(file.choose())
fit<-lm(sepal_length~+sepal_width+petal_length+petal_width,data=A)
anova(fit)
fit.beta<-lm.beta(fit)
summary(fit.beta)
# sepal_length=
# 1.84506+0.65486*sepal_width+0.71106*petal_length-0.56257*petal_width



#多元迴歸分析練習Lemonade：Sales~Temperature Rainfall Flyers Price
A=read.csv(file.choose())
fit<-lm(Sales~+Temperature+Rainfall+Flyers+Price,data=A)
anova(fit)
fit.beta<-lm.beta(fit)
summary(fit.beta)
# Sales=3.191959+0.369225*Temperature-2.246034*Rainfall
#       +0.018819*Flyers+2.414258*Price
#結論：
#根據p值，4個自變數對Sales都標示顯著
#Adj R squared=0.9819>0.8表示模型預測能力不錯(真因找乾淨)
#Residual SE=0.9284>0.8建議換模型來訓練
shapiro.test(A$Rainfall) # 常態檢定
# p<0.05代表非常態分配




#2-Sample t 檢定-Stress

# 前提1：要做平均值
#常態分配檢定
A=read.csv(file.choose())
mean(A$Stress.New)
mean(A$Stress.Old)
shapiro.test(A$Stress.New) #p-value = 0.7408符合常態分配
shapiro.test(A[,2]) #p-value = 0.555符合常態分配
#Ho:該New數值近似常態分配
#Ha:該New數值非常態分配
#P > 0.05=>接受Ho
#所以該New數值近似常態分配


#前提2：要做變異數檢定
var.test(A[,1],A[,2],alternative = "two.sided")
#F test to compare two variances
#data:  A[, 1] and A[, 2]
#F = 2.1549, num df = 9, denom df = 9, p-value = 0.2682
#alternative hypothesis: true ratio of variances is not equal to 1
#95 percent confidence interval:
#    0.535253 8.675717
#sample estimates:
#    ratio of variances 
#2.154925
#Ho:Var(New) = Var(Old)
#Ha:Var(New) not= Var(Old)
#P > 0.05=>接受Ho
#所以該New數值近似常態分配
#P-value 值>0.05,接受Ho；亦即：變異數相等


# 當完成變異數檢定,才來做2-sample t
#Ho：新材料=舊材料
#Ha：新材料>舊材料
t.test(A[,1],A[,2],alternative="greater",var.equal = TRUE)
#Two Sample t-test
#data:  A[, 1] and A[, 2]
#t = 0.98756, df = 18, p-value = 0.1682
#alternative hypothesis: true difference in means is greater than 0
#95 percent confidence interval:
#    -0.02347813         Inf
#sample estimates:
#    mean of x mean of y 
#12.41744  12.38638 
#結論:
# P-value爲0.168＞0.05，所以接受Ho，因此我們相信新材料的結構強度不會比舊材料好





#變異數分析-ANOVA
A=read.csv(file.choose())
install.packages("car") 
library(car)

#常態: Bartlett
#不常態:Levene
shapiro.test(A$A廠商)
shapiro.test(A$B廠商)
shapiro.test(A$C廠商)
shapiro.test(A$D廠商)

#變異數檢定
bartlett.test(PAD拉力~廠商別,data=A)
#Bartlett test of homogeneity of variances
#data:  PAD拉力 by 廠商別
#Bartlett's K-squared = 4.3576, df = 3, p-value = 0.2254>0.05
#接受Ho，因為p-值>0.05
#表示四家廠商之變異數確實沒有顯著差異

#平均數檢定
#做法1
model<-aov(PAD拉力~廠商別,data=A)
anova(model)
#做法2
oneway.test(PAD拉力~廠商別,data=A,var.equal=TRUE)
#拒絕Ho，因為p值<=0.05






#變異數分析-Switch Vendor ANOVA
A=read.csv(file.choose()) # SWICH VENDOR ANOVA
install.packages("car") 
library(car)

# 個別廠商看
shapiro.test(A$PP)
shapiro.test(A$HCP)
shapiro.test(A$DIA)

#變異數檢定
bartlett.test(高度~廠商別, data=A)
#Bartlett test of homogeneity of variances
#data:  高度 by 廠商別
#Bartlett's K-squared = 12.323, df = 2, p-value = 0.00211
#拒絕Ho，因為p-值<0.05
#表示三家廠商之變異數確實有顯著差異

#平均數檢定1
# Ho：u(PP)=u(HCP)=u(DIA)
# Ha：至少有一個u不等
model<-aov(高度~廠商別,data=A)
anova(model)
#Analysis of Variance Table
#
#Response: 高度
#           Df   Sum Sq   Mean Sq F value    Pr(>F)    
#廠商別      2 0.058576 0.0292879  114.75 < 2.2e-16 ***
#Residuals 102 0.026033 0.0002552                      
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#平均數檢定2
oneway.test(高度~廠商別,data=A,var.equal=TRUE)
#	One-way analysis of means
#
#data:  高度 and 廠商別
#F = 114.75, num df = 2, denom df = 102, p-value < 2.2e-16
#拒絕Ho，因為p-值 <<0.05
#表示三家廠商之Switch高度確實有顯著差異



#常態曲線
pnorm(6)-pnorm(-6)
pnorm(3)-pnorm(-3)
pnorm(2)-pnorm(-2)
pnorm(1)-pnorm(-1)
#> pnorm(6)-pnorm(-6)
#[1] 1
#> pnorm(3)-pnorm(-3)
#[1] 0.9973002
#> pnorm(2)-pnorm(-2)
#[1] 0.9544997
#> pnorm(1)-pnorm(-1)
#[1] 0.6826895

#常態分配

pnorm(6.2,mean=5,sd=10)-pnorm(5,mean=5,sd=10)
#[1] 0.04775843

#常態分配練習
pnorm(179,mean=172,sd=7)-pnorm(165,mean=172,sd=7)
#[1] 0.6826895


#二項式分配範例

dbinom(0,10,0.01)
dbinom(1,10,0.01)
#> dbinom(0,10,0.01)
#[1] 0.9043821
#> dbinom(1,10,0.01)
#[1] 0.09135172
#P(X≦1) ＝ P(X＝0)＋P(X＝1)＝0.904382＋0.091352＝0.995734


#二項式分配練習
#R語言
dbinom(0,10,0.05)
dbinom(1,10,0.05)
dbinom(2,10,0.05)
#> dbinom(0,10,0.05)
#[1] 0.5987369
#> dbinom(1,10,0.05)
#[1] 0.3151247
#> dbinom(2,10,0.05)
#[1] 0.0746348
#P(X=1) =0.315125
#P(X≧3) =1-P(X=0)- P(X=1)- P(X=2)=0.011503



#卜瓦松 (Poisson) 分配
dpois(0,0.01)
dpois(1,0.01)
#> dpois(0,0.01)
#[1] 0.9900498
#> dpois(1,0.01)
#[1] 0.009900498
#P(X=0) = 0.9900,   P(X=1) = 0.0099 所以
#P( X  ≦ 1 ) =  P( X = 0 ) + P( X = 1 )
#               =0.9900 + 0.0099 = 0.9999


#卜瓦松 (Poisson) 分配練習
#練習1
dpois(4,2.4)
#[1] 0.1254085
dpois(0,2.4)
dpois(1,2.4)
dpois(2,2.4)
dpois(3,2.4)
dpois(5,2.4)
dpois(6,2.4)


#練習2
dpois(0,0.5)
dpois(1,0.5)
#> dpois(0,0.5)
#[1] 0.6065307
#> dpois(1,0.5)
#[1] 0.3032653
1-dpois(0,0.5)-dpois(1,0.5)


#F 檢定( F Testing )雷射電擊鋼板P143作法1
雷射電擊鋼板<-read.csv("C:/Users/user/Desktop/大數據之必備統計分析技巧實務實戰/正式講義/大數據必備統計分析上課用檔/雷射電擊鋼板.csv")
save(雷射電擊鋼板,file="C:/Users/user/Desktop/大數據之必備統計分析技巧實務實戰/正式講義/大數據必備統計分析上課用檔/雷射電擊鋼板.RData")
load("C:/Users/user/Desktop/大數據之必備統計分析技巧實務實戰/正式講義/大數據必備統計分析上課用檔/雷射電擊鋼板.RData")
雷射電擊鋼板
var.test(雷射電擊鋼板[,1],雷射電擊鋼板[,2],alternative = "two.sided")
#	F test to compare two variances
#
#data:  雷射電擊鋼板[, 1] and 雷射電擊鋼板[, 2]
#F = 0.7365, num df = 1499, denom df = 1499, p-value = 3.456e-09
#alternative hypothesis: true ratio of variances is not equal to 1
#95 percent confidence interval:
# 0.6655538 0.8150001
#sample estimates:
#ratio of variances 
#         0.7364961 
#P值小於α=0.05因此拒絕虛無假設，也就是我們認為不同鋼板對錫膏平均厚度的變異有顯著影響
#



#F 檢定( F Testing )雷射電擊鋼板P143作法2
A=read.csv(file.choose())
var.test(A[,1],A[,2],alternative = "two.sided")
#	F test to compare two variances
#data:  A[, 1] and A[, 2]
#F = 0.7365, num df = 1499, denom df = 1499, p-value = 3.456e-09
#alternative hypothesis: true ratio of variances is not equal to 1
#95 percent confidence interval:
# 0.6655538 0.8150001
#sample estimates:
#ratio of variances 
#         0.7364961 
#P值小於α=0.05因此拒絕虛無假設，也就是我們認為不同鋼板對錫膏平均厚度的變異有顯著影響













#K-means Clustering
#我們考慮了25個歐洲國家（n = 25個單位）及其來自9個主要食物來源的蛋白質攝入量（百分比）（p = 9）。數據如下所示。
url = 'http://www.biz.uiowa.edu/faculty/jledolter/DataMining/protein.csv'
food <- read.csv(url)
View(food)
head(food)
food

#在階層式分群中，主要是以資料之間的「距離」遠近，來決定兩筆資料是否接近。
#R的話，我們可以使用dist()，來建立資料之間的「距離矩陣」(Distance Matrix)，判斷資料之間的遠與近：
E.dist <- dist(food, method="euclidean") # 歐式距離
M.dist <- dist(food, method="manhattan") # 曼哈頓距離
par(mfrow=c(1,2)) # 讓圖片以1x2的方式呈現
# 使用歐式距離進行分群
h.E.cluster <- hclust(E.dist) # Hierarchical Clustering
plot(h.E.cluster, xlab="歐式距離")
# 使用曼哈頓距離進行分群
h.M.cluster <- hclust(M.dist) 
plot(h.M.cluster, xlab="曼哈頓距離")

#由於分群屬於「非監督式學習」的演算法，因此我們先把iris內的品種(Species)欄位拿掉，以剩下的資料進行分群：
iris
data <- iris[, -5] # 因為Species是第五欄位，故移除掉
head(data)         # 現在data只剩下前四個欄位的資料
# 分成三群
kmeans.cluster <- kmeans(data,centers=3) # K-Means Clustering
# 群內的變異數
kmeans.cluster$withinss
# 分群結果和實際結果比較
table(kmeans.cluster$cluster, iris$Species) 
# 視覺化 k-means 分群結果
#(基於ggplot2的語法)
#install.packages("factoextra")
require(factoextra)
fviz_cluster(kmeans.cluster,   # 分群結果
             data = data,              # 資料
             geom = c("point","text"), # 點和標籤(point & label)
             frame.type = "norm")      # 框架型態


####################


A=read.csv(file.choose())
View(A)
require(datasets)  # source package
str(A)          # check structure of A
A
head(A, n=6)
list(A)
summary(A)


#*** 附上三種繪圖系統的程式碼，以ggplot2輸出 ***#

### Base Plotting System 
#  plot(x=iris$Sepal.Length, y=iris$Sepal.Width,pch=2)

### Lattice 
#  require(lattice)
#  xyplot(Sepal.Width~Sepal.Length, data=iris)

### ggplot2 
install.packages("factoextra")
require(factoextra)
install.packages("ggplot2")
require(ggplot2)
ggplot(data=A) +
        geom_point(aes(x=A$Temperature,
                       y=A$Sales)) +
        theme_bw() 
ggplot(data=A) +
        geom_point(aes(x=A$Rainfall,
                       y=A$Sales)) +
        theme_bw() 
### ggplot2 
require(ggplot2)
qplot(x=A$Temperature,      
      y=A$Sales, 
      data=A, 
      geom="boxplot")    # graph type is boxplot



data[is.na(data[,"x"]), "x"] <- mean(data[,"x"], na.rm=T)
data

#迴歸分析
model <- lm(formula= A$Sales ~ A$Temperature + A$Temperature + A$Rainfall+ A$Flyers + A$Price,
            data=A)
summary(model)

#預測
new.A <- data.frame(Temperature=27, Rainfall=2, Flyers=15, Price=0.3)
new.A

predict(model,new.A)

