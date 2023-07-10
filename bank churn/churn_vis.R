setwd('C:/Users/hajin/Downloads/Sample_Data')
x<-read.csv(file='Churn_Modelling.csv')
View(x)
x<-subset(x,Geography=='Germany')#German data
attach(x) 
plot(x)

summary(x)
x1<-subset(x,select=-c(RowNumber,CustomerId,Surname, Geography))
x1
attach(x1)                     

#Acitve member: A member of a securities exchange who makes trades on a regular basis.
#Tenure: 신용도

#continous:"NumOfProducts","Tenure","Age","EstimatedSalary","Balance","CreditScore"   
#factor:"Gender", "IsActiveMember" ,"HasCrCard" 

#1. active memver-exited
#install.packages('vcd')
library(vcd)

prop.table(table(IsActiveMember))*100
#증권 거래를 하는회원은 전체의 49.7%
prop.table(table(Exited))*100
#탈퇴 회원은 전체의 32.4%
cross=prop.table(table(Exited,IsActiveMember))*100
cross
#전체 중 규칙적인 증권거래가 없는 탈퇴 회원 20.6%

f1<-mosaicplot(cross,
           las=1, #[position 정렬]
           border=F,
           xlab="Exited",
           ylab="IsActiveMember",
           main='Why acitive memebr do exited?') 
f1
#no mean

#2. credit card-exited
cross1=prop.table(table(Exited,HasCrCard))*100
cross1
mosaicplot(cross1,
           border=F,
           xlab="Exited",
           ylab="HasCrCard",
           main='s') 
#has credit card, lower exit

#3. produce-eixted(밀도함수수)
library(ggplot2)
f4<-ggplot(x, aes(x = NumOfProducts, fill = factor(Exited))) +
  geom_density(alpha = 0.5) +
  labs(title = "Histogram of Num Of Products",
       x = "Num Of Products",
       y = "Count") +
  scale_fill_discrete(name = "1")+
  guides(fill = guide_legend(title = "Exited", labels = c("no",'yes')))

f4

#4.연속함수 간의 관계 확인인

x3=data.frame(Tenure,Age,EstimatedSalary,Balance,CreditScore)
plot(x3)

#5. credit score - exited
library(ggplot2)
f5<-ggplot(x, aes(x = CreditScore, fill = factor(Exited))) +
  geom_histogram(binwidth = 10, color = "black", aes(y = ..density..),alpha=0.7) +
  geom_density(alpha = 0.5) +
  labs(title = "Histogram of CreditScore",
       x = "CreditScore",
       y = "Count") +
  scale_fill_discrete(name = "1")+
  guides(fill = guide_legend(title = "Exited", labels = c("no",'yes')))
f5
#600후반-700초반에 탈퇴자가 좀 있는 듯듯

#Age-exited
f6<-ggplot(x, aes(x = Age, fill = factor(Exited))) +
  geom_histogram(binwidth = 10, color = "black", aes(y = ..density..),alpha=0.7) +
  geom_density(alpha = 0.5) +
  labs(title = "Histogram of Age",
       x = "Age",
       y = "Count") +
  scale_fill_discrete(name = "1")+
  guides(fill = guide_legend(title = "Exited", labels = c("no",'yes')))
f6
#50대에 탈퇴를 더 많이 함. 

#주로 50대의 신용점수가 600후반 700초반에 위치하게 되지 않을까?  
plot(x1$Age,x1$CreditScore,
     xlab='Age',
     ylab='CreditScore',
     main='50s credit score') #scatter plot

#contour plot사용해서 등고선 처럼 그래프 밀도 확인하고 싶어
library(corrplot)
xr<-x[c('Age','CreditScore')]

corrplot(cor(xr),
         method='color',
         diag=F,
         type='upper',
         order='hclust',#FPC :아이겐 벡터
         addCoef.col='black',
         number.cex=.5,
         tl.col='black')     
    
ggplot(data=x, aes(x = Age, y = CreditScore, fill=factor(Exited))) +
  geom_density_2d(geom = "CreditScore", contour = TRUE,
                  aes(fill = factor(Exited)), colour = "black",
                  bins = 7) +
  #scale_fill_manual(values = c("1" = "red", "0" = "blue"))+
  theme_minimal()
factor(Exited)

#fill=factor(Exited)
# scale_fill_manual(values = c("1" = "red", "0" = "blue")) +