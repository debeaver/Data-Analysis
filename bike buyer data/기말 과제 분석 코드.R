#불러올 패키지-------------------------
library(data.table)

library(psych)

library(Hmisc)

library(skimr)

library('dplyr')

library(car)

library(ggplot2)

library(plotly)

library(magrittr)

library(sm)

library(fBasics)

library(RColorBrewer)

library(gridExtra)
#2.데이터 탐색
#2-1)데이터셋을 로딩 -----------------------------------
tr<-read.csv(file='bike_buyers_clean.csv',header=T,sep=',',
             stringsAsFactors=F,strip.white=T,na.strings=c(',','?','NA'))

str(tr) 

#데이터 전처리--------------------------
tr=rename(tr,'customer ID'='癤풦D') #칼럼 이름 바꾸기기

unique(tr$Marital.Status)#
tr$Marital.Status=recode(tr$Marital.Status,'"Married"=2; "Single"=1' )

unique(tr$Gender)
tr$Gender=recode(tr$Gender,'"Female"=2; "Male"=1' )

unique(tr$Education)
tr$Education=recode(tr$Education,'"Bachelors"=4;"Partial College"=3;"High School"=2;      
                "Partial High School"=1; "Graduate Degree"=5'  )

unique(tr$Occupation)
tr$Occupation=recode(tr$Occupation,'"Skilled Manual"=2 ; "Clerical" =4 ;  
                "Professional"=3 ;"Manual" = 1 ; "Management"  =5')

unique(tr$Home.Owner)
tr$Home.Owner=recode(tr$Home.Owner,'"Yes"=1;"No"=0') 

unique(tr$Commute.Distance)
tr$Commute.Distance=recode(tr$Commute.Distance,'"0-1 Miles"=1; "2-5 Miles"=2;
                           "5-10 Miles"=3; "1-2 Miles"=4;  "10+ Miles"=5')

unique(tr$Region)
tr$Region=recode(tr$Region,'"Europe"=1; "Pacific"=2; "North America"=3')

unique(tr$Purchased.Bike)
tr$Purchased.Bike<- recode (tr$Purchased.Bike, '"Yes"=1; "No"=0')

#2-2) 데이터셋 내용 및 구조 탐색-----------------------------------


class(tr)#객체 파악 
str(tr)
headTail(tr)
length(unique(tr$`customer ID`))#중복되는 ID없음

row.names(tr)#데이터 구조(변수 종류, 열 이름) 파악
names(tr)


View(tr)#객체별 데이터 보기
tr_tb
tr_dt


dim(tr)#데이터 규모 파악
tr_dim<-dim(tr)
sprintf('데이터 셋의 규모-> 레코드 갯수: %d개, 변수 컬럼 갯수: %d개', 
        tr_dim[1],tr_dim[2])

#데이터 셋 기본 요약 : 기본 요약-utils::str()/ 기술통계분석-psych::describe(), Hmisc::describe(), skimr::skim()

summary(tr)

psych::describe(tr)

Hmisc::describe(tr)

skimr::skim(tr)

# 2-3) 데이터셋 시각화-----------------------------------

#plot(tr)#산점도 매트릭스: 전체 변수

temp<-cor(tr,use='complete.obs') #결측값 제거한 상관 분석
temp_r<-round(temp,3) #소수점 둘째 자리까지
View(temp_r)

#3.개별 변수 요약과 집계----------------

#범주형 변수 컬럼 분석-----------
ctg_names=c('Marital.Status','Gender','Occupation','Home.Owner',
            'Commute.Distance','Region','Education','Purchased.Bike',
            'customer ID')
ctg<-tr[ctg_names]
str(ctg)
head(ctg)
summary(ctg)

#연속형 변수 컬럼 분석-------
cnt_names<-c('Income','Children','Cars','Age')
cnt<-tr[cnt_names]
head(cnt)
str(cnt)           
summary(cnt)

sprintf('전체변수갯수: %d개 = 범주형변수갯수(%d개) + 연속형변수갯수(%d개)',
        length(names(tr)), length(ctg_names), length(cnt_names))

names(tr)

#3-1) 범주형 변수를 2개 선정-----
# 범주형 변수1 : 자전거 보유------------------------
str(tr$Purchased.Bike)

#요약집계1-----
ctg$Purchased.Bike_f<-factor(tr$Purchased.Bike,
                             levels=c(0,1),labels=c('no','yes'))
str(ctg)
head(ctg)


psych::describe(tr$Purchased.Bike)
Hmisc::describe(ctg$Purchased.Bike)
#자전거 보유변수는 왜도가 0.08, 첨도가 -2인 것으로 보아 비교적 대칭적인 그래프이며 
#정규분포보다는 더 납작한 모양의 분포를 보이는 것을 알 수 있다.


pur.bike_f_freq <- table(ctg$Purchased.Bike_f)  
pur.bike_f_freq

pur.bike_f_prop <-pur.bike_f_freq%>%prop.table()
pur.bike_f_pect<-round(pur.bike_f_prop *100,3)
pur.bike_f_pect

#시각화1----
par(mfrow=c(2, 2)) # 멀티 캔버스 프레임 구성

barplot(pur.bike_f_freq,
        main="자전거 구매 고객 지표 ",
        xlab="구매 여부", ylab="고객 수",col=c("sky blue", "plum"),
        xlim=c(0,5),width=c(2,2))

barplot(pur.bike_f_freq,
        main="자전거 구매 여부 지표: Horizontal Bar Plot",
        xlab="고객 수", ylab="구매 여부", horiz=TRUE,col=c("sky blue", "plum")
        , ylim=c(0,19),width=c(6,6))

barplot(pur.bike_f_prop,
        main="자전거 구매 여부 지표: Simple Bar Plot",
        xlab="구매 여부", ylab="구매 비율", density=c(60, 60),
        legend=rownames(pur.bike_f_freq),col=c("sky blue", "plum"),
        xlim=c(0,19),width=c(4,4))

barplot(pur.bike_f_prop,
        main="자전거 구매 여부 지표: Horizontal Bar Plot",
        xlab="구매 비율", ylab="구매 여부",
        horiz=TRUE, col=c("sky blue", "plum"), 
        beside=TRUE, legend=rownames(pur.bike_f_freq),
        ylim=c(0,19),width=c(4,4))

par(mfrow=c(1, 1)) # 멀티 캔버스 프레임 리셋

#집계 결과 자전거를 구매하지 않는 사람이 구매한 사람에 비해 2.8%정도로 미세하게 
#더 많은 것을 알 수 있다. 자전거를 사는 고객은 고객 1000명중 481명으로 
#약 전체 고객의 48.1%만이 자전거를 구매했다고 볼 수 있다. 이는 고객 2명중에 1명은
#자전거를 사는 꼴이므로 생각보다 높은 수치를 보유한다고 볼수 있다.


#범주형 변수2: 통근 거리----------
str(tr$Commute.Distance)

#요약집계2----
psych::describe(tr$Commute.Distance)
Hmisc::describe(ctg$Commute.Distance)

ctg$Commute.Distance_f <- factor(ctg$Commute.Distance, levels = c( 1, 2, 3,4,5)
                                 ,labels = c('0-1 Miles', '1-2 Miles', '2-5 Miles', '5-10 Miles', '10+ Miles '))
head(ctg)                    
str(ctg)

commte_f_freq <- table(ctg$Commute.Distance_f)
commte_f_freq

commte_f_prop <- prop.table(commte_f_freq)
commte_f_prop

commte_f_pect<-round(commte_f_prop*100,3)
commte_f_pect
 
#시각화2----
par(mfrow=c(2, 2)) 

barplot(commte_f_freq,
        main="통근 거리 분포비교",
        xlab="통근 거리", ylab="고객 수",col=c('light cyan','powder blue'
                                        ,'light sky blue','Dodger blue','steel blue')
        ,cex.names=0.8,xlim=c(0,6),width=c(1),las=1.5)

barplot(commte_f_freq,
        main="통근 거리 분포비교",
        xlab="고객 수", horiz=TRUE,
        col=c('light cyan','powder blue','light sky blue','Dodger blue','steel blue')
        ,ylim=c(0,5),width=c(0.8),cex.names=1,las=1)

barplot(commte_f_prop,
        main="통근 거리 비율 분포 비교",
        xlab="통근 거리", ylab="고객 비율", density=c(40, 40),
        legend=rownames(commte_f_freq),col=c('light cyan','powder blue','light sky blue','Dodger blue','steel blue')
        ,xlim=c(0,8),width=c(0.8),cex.names=1)

barplot(commte_f_prop,
        main="통근 거리  비율 분포 비교",
        xlab="고객 비율",las = 1,
        horiz=TRUE,legend=rownames(commte_f_freq) ,col=c('light cyan','powder blue','light sky blue','Dodger blue','steel blue')
        ,ylim=c(0,4.5),width=c(0.8),cex.names=1)

par(mfrow=c(1, 1))
# 고객 데이터의 통근 거리는 1mile이내, 2-5mile, 5-10mile, 1-2mile, 10mile 이상
#의 순으로 비율을 차지 했고 가장 많은 비율을 1mile이내는 6.6%으로 2번째로 높은
#2-5mile과도 17%로 월등한 차이를 보여주었다. 해당 데이터의 주 고객들은 직장과
#가까운 곳에 살고 있음을 알 수 있다. 

#3-2) 연속형 변수를 2개 선정-----------------

#연속형 변수1: 소득------------

str(cnt$Income)

#특성요약1-----------------------------------
psych::describe(cnt$Income)#최대, 최소 왜도, 첨도확인 가능
Hmisc::describe(cnt$Income)
skim(cnt$Income)
#왜도 0.75 첨도0.5

mean(cnt$Income, na.rm = TRUE, trim = 0.3)#평균
income_freq<-  table(cnt$Income)
income_freq%>%sort(., decreasing = TRUE)%>%{names(which.max(.))}#최빈값
var(cnt$Income, na.rm = TRUE)#분산
sd(cnt$Income, na.rm = TRUE)#표준 편차
sd(cnt$Income, na.rm = TRUE)#범위

#------------------------------------시각화------------------------------------
my_blue<-c('light cyan','powder blue' ,'light sky blue','Dodger blue',
           'steel blue','royal blue','medium blue','dark blue','navy blue')#색상


plot(cnt$Income, type = "p", pch = 21, bg = "sky blue")

abline(h= seq(from = 1, to = 10, by = 1), col = "gray", lty = 2)   # 가이드라인(안내선) 그리기

abline(v = seq(from = 1000, 
               to = 14000, 
               by = 1000), 
       col = "gray", lty = 2)

par(mfrow=c(2, 2))
hist(cnt$Income, main="hist(), Frequency 옵션", col=c(my_blue), xlab="소득액($)")
hist(cnt$Income, probability=TRUE, main="hist(), Probabilty 옵션", xlab="소득액($)",col=c(my_blue))
plot(density(cnt$Income), main="density() 확률밀도 옵션", xlab="소득액($)")
hist(cnt$Income, probability=TRUE, xlab="소득액($)",
     main="hist() 히스토그램과 density() 확률밀도함수 통합",col=c(my_blue))
lines(density(cnt$Income),col=c('fire brick'))
par(mfrow=c(1, 1))

boxplot(cnt$Income,
        main="박스플롯",
        ylab="소득액($)",col=c(my_blue))
#고객의 소득액 추이를 봤을때 평균은 56140$로 위치해있고 최대값은 170000 
#최소값은 10000이다. 그래프로 이 추이를 알아보았을때 중위 소득 구간에 유독 밀집되어있고
# 80000$이상의 소득액에서는 눈에 띄게 감소되는 걸로 보아
#중류층 소비자가 대부분을 차지함을 알수 있다. 

#연속형 변수2: 보유 자동차 수량------------------------------------
str(cnt$Cars)

#요약집계2------------------------------------
psych::describe(cnt$Cars)#최대, 최소 왜도, 첨도확인 가능
Hmisc::describe(cnt$Cars)
skim(cnt$Cars)


mean(cnt$Cars, na.rm = TRUE, trim = 0.3)#평균
price_freq <- table(cnt$Cars)
price_freq%>%sort(., decreasing = TRUE)%>%{names(which.max(.))}#최빈값
var(cnt$Cars, na.rm = TRUE)#분산
sd(cnt$Cars, na.rm = TRUE)#표준 편차
sd(cnt$Cars, na.rm = TRUE)#범위

#시각화2------------------------------------
plot(cnt$Cars, type = "p", 
     pch = 21, bg = "sky blue")

# 가이드라인(안내선) 그리기
abline(h = seq(from = 1, 
               to = 400, 
               by = 25), 
       col = "gray", lty = 2)
abline(v = seq(from = 1000, 
               to = 14000, 
               by = 1000), 
       col = "gray", lty = 2)

par(mfrow=c(2, 2))
hist(cnt$Cars, main="hist(), Frequency 옵션",xlab="자동차 보유 수",col=c(my_blue))
hist(cnt$Cars, probability=TRUE, main="hist(), Probabilty 옵션",xlab="자동차 보유 수",col=c(my_blue))

plot(density(cnt$Cars), main="density() 확률밀도 옵션",xlab="자동차 보유 수")
hist(cnt$Cars, probability=TRUE,
     main="hist() 히스토그램과 density() 확률밀도함수 통합",xlab="자동차 보유 수",col=c(my_blue))
lines(density(cnt$Cars),col=c('fire brick'))
par(mfrow=c(1, 1))

boxplot(cnt$Cars,
        main="박스플롯",
        ylab="자동차 보유 수")

# 시각화 그래프로 미루어보아 해당 고객층은 2대의 자동차를 보유하고 있는 고객
#가장 높은 밀도를 보이고 아예 자동차를 가지고 있지 않거나 1대만 보유하고 있는 경우가 
#대부분이다. 고객의 대두분은 자동차를 보유하고 있는 경우가 더 많다. 

#4.다차원변수 요약과 집계----

#4-1) 범주형 변수 2개간 특성요약과 시각화------------------------------------------------------------------------

table(ctg$Purchased.Bike_f,ctg$Commute.Distance_f, useNA = 'ifany')
bik_dis_freq <-table(ctg$Purchased.Bike_f,ctg$Commute.Distance_f)
bik_dis_freq # 제품 중요도에 따른 정시 배송 성공 교차분석 

table(ctg$Purchased.Bike_f,ctg$Commute.Distance_f, useNA = 'ifany')

dis_bik_freq <-table(ctg$Commute.Distance_f,ctg$Purchased.Bike_f)
dis_bik_freq# 정시 배송에 따른 제품 중요도성공 교차분석

#특성요약1------------------------------------
# 교차빈도분석 부분합(margin) 계산하기
addmargins(bik_dis_freq)
addmargins(bik_dis_freq, 1)
addmargins(bik_dis_freq, 2)

bik_dis_freq_sum <- addmargins(bik_dis_freq, 2)
bik_dis_freq_sum

# 교차빈도분석을 비율분석으로 변환
prop.table(bik_dis_freq, 1) # 각 중요도에서 정시 성공 분포비율 비교
prop.table(bik_dis_freq, 2) # 각 정시성공에서 중요도 분포비율 비교

bik_dis_prop <- prop.table(bik_dis_freq, 1) 
bik_dis_prop 

bik_dis_prop%>%round(.,3)%>%addmargins(.,2)

dis_bik_prop <- prop.table(dis_bik_freq, 1) 

# 교차비율분석을 백분율분석으로 변환
bik_dis_result <- round(bik_dis_prop, 3) * 100
bik_dis_result

addmargins(bik_dis_result, 2)

#시각화1-----------------------------------
par(mfrow=c(2, 2))

barplot(bik_dis_freq,
        main="통근거리에 따른\n 자전거 구매 결정 분포비교: Stacked",
        xlab="통근거리(단위:miles)", ylab="구매 여부 건수",
        col=c("sky blue","plum"), legend=rownames(bik_dis_freq))

barplot(bik_dis_prop,
        main="통근거리에 따른\n 자전거 구매 결정 분포비교: Grouped",
        xlab="통근거리(단위:miles)", ylab="구매 여부 건수", beside=TRUE, 
        col=c("sky blue","plum"), legend=rownames(bik_dis_freq))

barplot(dis_bik_freq,
        main="통근거리에 따른\n 자전거 구매 결정 분포비교: Stacked",
        xlab="통근거리(단위:miles)",ylab="Frequency",
        col=c(my_blue), legend=rownames(dis_bik_freq),xlim=c(0,5))

barplot(dis_bik_prop,
        main="통근거리에 따른\n 자전거 구매 결정 분포비교: Grouped",
        xlab="Treatment", ylab="비율",
        col=c(my_blue[0:5]), legend=rownames(dis_bik_freq),xlim=c(0,25),beside=TRUE)

par(mfrow=c(1, 1))

par(mfrow=c(2, 2))

plot(Purchased.Bike_f ~Commute.Distance_f , data = ctg,
     main="통근 거리에 따른\n 자전거 구매 여부 분포비교",
     xlab="통근 거리", ylab="제품 구매 여부",
     col =c("sky blue","plum") )

plot(Commute.Distance_f~ Purchased.Bike_f, data = ctg,
     main="자전거 구매 여부에 따른\n 통근 거리 분포비교",
     xlab="구매 여부", ylab="통근 거리",col=c(my_blue[0:5]))

mosaicplot( Commute.Distance_f~Purchased.Bike_f, data = ctg,
            main="통근 거리에 따른\n 자전거 구매 여부 분포비교",
            xlab="통근 거리", ylab="제품 구매 여부",
            col = c("sky blue","plum"))

mosaicplot(Purchased.Bike_f~Commute.Distance_f, data = ctg,
           main="자전거 구매 여부에 따른\n 통근 거리 분포비교",
           xlab="구매 여부", ylab="통근 거리",,
           col=c(my_blue[0:5]))

par(mfrow=c(1, 1)) # 멀티 캔버스 프레임 리셋
levels(ctg$Commute.Distance_f)

# 위 시각화 자료로 미루어 보아 통근거리가 가장 짧은 소비자군이 가장 높은
# 자전거 구매율을 보이며 통근 거리가 가까울 수록 자전거를 구매하려는 경향을 보였다.
#이러한 결과로 미루어 보아 일터와 보다 가까운 곳에 위치한 소비자가 자전거를 더 많이 
#이용하려는 욕구를 가짐을 알수 있다.

#4-2)연속형 변수 2개간 특성요약과 시각화---------------------

# 특성 요약2: 소득액에 따른 자동차 보유 개수에 대한 변수간 상관관계 파악------------------------------

# 공분산(covariance) 분석
var(cnt$Income, cnt$Cars)

# 상관성(correlation) 분석
cor(cnt$Income, cnt$Cars, method = 'spearman')
cor(cnt$Income, cnt$Cars, method = 'pearson')
#상관 계수가 양수임을 보아 제품 가격과 고객 문의 전화 수는 같은 방향으로 움직인다는 것을 알수 있다.

#시각화2------------------------------

# 기본 graphics::plot() 함수이용: 직선과 곡선 최적합화선 추가
plot( Income~Cars , data = cnt, pch=19,
      main = '소득액과 자동차 보유 수간 관련성',
      xlab = '자동차 보유 수', 
      ylab = '소득액')

# 최적의 추세직선추가
abline(lm(Income~Cars, data = cnt), 
       col="red", lwd=2, lty=1)

# 최적의 추세곡선추가
lines(lowess(cnt$Income ~ cnt$Cars), 
      col="blue", lwd=2, lty=2) 

# ggplot2::ggplot() 함수이용: 최적합선 추가


p <- ggplot(data = cnt, aes(x =Income  , y =Cars)) +
        geom_point() + labs(title = "자동차 보유 수와 소득액간 관련성", 
                            y = "자동차 보유 수", 
                            x = "소득액($)")
p

p <- p + geom_smooth() # 최적합선과 오차범위를 표현
p


ggplotly(p) 

#자동보차 유 수 와 소득액 간 관련성은 위의 시각화 그래프로 미루어 보아
#자동차 보유수가 증가할 수록 소득의 액수도 증가함을 알수있다. 
#소득액점이 점점 커지다가 120000$구간 즘에서 변곡점이 나타나는데,
#이는 일정 소득이 구간이 넘어서면 소득의 증가 폭에 비해 느리게 증가한다는 
#사실을 유추할 수 있다. 이와는 반대로 46500$이하의 소득액을 가지고 있다면 
#소득액에 비해 자동차를 1대 이상으로 유지하지 않으려는 경향이 있지만 오히려 
#소득액이 46500$이상을 돌파하면 1250000$구간이 오기 전까지는 자동차 보유 수량을
#2~3대까지 늘리려는 경향을 보여준다. 

#4-3)범주형변수와 연속형변수 중 1개 관계를 선정해 특성요약과 시각화------------

#특성요약3-----------------------------------

tr$Purchased.Bike_f<-ctg$Purchased.Bike_f
str(tr$Purchased.Bike_f)

# stats::aggregate()함수이용 요약집계
aggregate(formula = Income ~ Purchased.Bike, data = tr, FUN = mean, na.rm = TRUE)
aggregate(Income  ~ Purchased.Bike_f, tr, mean, na.rm = TRUE, trim = 0.05)
aggregate(Income  ~ Purchased.Bike_f, tr, sd, na.rm = TRUE)

# magrittr::파이프연산자 %>%, dplyr::데이터가공함수 이용 요약집계  

tr %>% group_by(Purchased.Bike_f) %>%  
        dplyr::summarize(Avg = mean(Income), SD = sd(Income)) %>% 
        arrange(desc(Avg))

#시각화3-----------------------------------
par(mfrow=c(2, 2))

# sm::sm.density.compare()를 사용한 밀도그래프

sm.density.compare(x=tr$Income, group=tr$Purchased.Bike_f,
                   xlab="자전거 구매 여부", ylab="밀도", 
                   col=c(2, 3), lty=c(2, 3))
title(main="소득액에 따른 자전거 구매 여부 분포비교")
legend(x=100000, y=0.01, legend=levels(tr$Purchased.Bike_f), 
       col=c(2, 3, 4), lty=c(2, 3, 4), bty="n")

# boxplot()를 사용한 그래프
boxplot(Income ~ Purchased.Bike_f, data = tr,
        main="자전거 구매 여부에 따른 소득액 분포비교",
        xlab="소득액", ylab="자전거 구매 여부",
        col=c(2, 3), varwidth=T, notch=T)

par(mfrow=c(1, 1))

# 멀티프레임에 배치할 개별 그래프 준비
# 단순 플롯팅 그래프
p1 <- ggplot(tr, aes(Purchased.Bike_f,Income)) + 
        geom_point(color = "red", shape = 20, size = 2)

# 모든 데이터를 플로팅하는 형태로 분포모양을 나타냄
p2 <- ggplot(tr, aes(Purchased.Bike_f, Income)) + 
        geom_jitter(color = "blue", shape = 8, size = 0.8)

# 박스플롯 형태로 분포모양을 나타냄
p3 <- ggplot(tr, aes(Purchased.Bike_f, Income)) + 
        geom_boxplot(fill = "lightblue", 
                     outlier.color ="orange", outlier.shape = 17,
                     outlier.size = 2, notch = TRUE)

# 바이올린 플롯형태로 분포모양을 나타냄
p4 <- ggplot(tr, aes(Purchased.Bike_f, Income)) + 
        geom_violin(fill = "lightpink")

# gridExtra::grid.arrange() 함수이용 
# ggplot2용 멀티프레임 생성
grid.arrange(p1, p2, p3, p4, nrow = 2, ncol = 2) 
# 소득액에 따른 자전거 구매 여부는 주로 25000-75000사이의 중위 소득을 가진 
#소비자들이 주로 구매하는 추세이다.두 그래프를 비교해 보면 소득 격차에 따라 
#눈에 띄게 자전거를 이용하는 소득 분위가 존재한다기 보단 오히려 특정 소득액에서 
# 상위 중위 하위 분위에 골고루 자전거를 구매함을 알수있다. 비교적 소득액에 위치한 
#고객 밀도가 높아 60000$~750000$, 20000$~30000$구간의 고객 측이 조금더 자전거 구매를
#선호하는 것으로 보이나, 밀도를 감안하면 특정 소득액 구간에서 차이가 발생하는 것이지 
#소득 분위에 따라 선호가 결정되는 추세는 아닌것으로 볼수 있다. 
#5. 데이터 가공----------
        
#5-1)리코딩 작업 2개 실시--------------------------------------------

# 리코딩 변수1: 성별---------------------------------------------

str(ctg$Gender)
tr$Gender_l1[tr$Gender==1] <-'남자'
tr$Gender_l1[tr$Gender ==2]<-'여자'

# 리코딩결과 변수컬럼명으로 확인
head(tr)

# 정규표현식으로 성별 관련변수들만 인덱싱
gen_idx <- grep(names(tr), pattern ='^Gender_[:alpha:]{0,}')
gen_idx
gen_names <- grep(names(tr), pattern = '^Gender_[:alpha:]{0,}', value = TRUE)
gen_names

head(tr[c('Gender', ontime_names)])

# 리코딩 결과 변수특성으로 확인
sapply(tr[c(gen_names)], table) # gender관련 변수들 빈도분석
sapply(tr[c(gen_names)], unique) # gender관련 변수들 고유값 확인


#리코딩 변수2:직업------------------------------------

tr$Occupation_c1[tr$Occupation == 1] <-'육체 노동자' 
tr$Occupation_c1[tr$Occupation == 2] <-'숙련공' 
tr$Occupation_c1[tr$Occupation == 3]='전문직'
tr$Occupation_c1[tr$Occupation == 4]='사무직'
tr$Occupation_c1[tr$Occupation == 5]='경영직'

head(tr)

# 정규표현식으로 성별 관련변수들만 인덱싱
ocp_idx <- grep(names(tr), pattern = '^Occupation_[:alpha:]{0,}')
ocp_idx
ocp_names <- grep(names(tr), pattern = '^Occupation_[:alpha:]{0,}', value = TRUE)
ocp_names
head(tr[c(ocp_names)])

# 리코딩 결과 변수특성으로 확인
sapply(tr[c( ocp_names)], table) # gender관련 변수들 빈도분석
sapply(tr[c( ocp_names)], unique) # gender관련 변수들 고유값 확인


#5-2)요약변수 2개 만들기------------------------------
#요약변수1: 나이--------------
#library(skimr) 변동성-채널) 카드사용 채널종류의 집중도·다양성
psych::describe(cnt$Age)#최대, 최소 왜도, 첨도확인 가능
Hmisc::describe(cnt$Age)

Age_freq <- table(cnt$Age)

# 왜도와 첨도 계산용 패키지:fBasics 

#평균, 분산,표준편차,범위, 최빈값, 최대, 최소
a<-c(mean(cnt$Age, na.rm = TRUE, trim = 0.3),var(cnt$Age, na.rm = TRUE),
     sd(cnt$Age, na.rm = TRUE),names(which.max(sort(Age_freq, decreasing = TRUE))),max(cnt$Age),min(cnt$Age))

n<-c('평균','분산','표준편차','최빈값','최대','최소')
Age_m<-matrix(a,ncol=6)
Age_m
colnames(Age_m)<-n
Age_m

#요약변수2: 소득----------

inc_freq <- table(cnt$Income)
b<-c(mean(cnt$Income, na.rm = TRUE, trim = 0.3),var(cnt$Income, na.rm = TRUE),
     sd(cnt$Income, na.rm = TRUE),names(which.max(sort(inc_freq, decreasing = TRUE)))
     , max(cnt$Income),min(cnt$Income))
b
n<-c('평균','분산','표준편차','최빈값','최대','최소')
cus_ra<-matrix(a,ncol=6)
cus_ra
colnames(cus_ra)<-n
colnames(cus_ra)
cus_ra

#5-3) 파생변수 2개 만들기------------------
#파생변수1: 통근 거리----------------------------------------

psych::describe(tr$Commute.Distance)
Hmisc::describe(tr$Commute.Distance)
# base::cut() 함수이용
tr$Commute.Distance_r[tr$Commute.Distance %in%c(1,2)]='3km 이내'
tr$Commute.Distance_r[tr$Commute.Distance %in% c(3)]='8km 이내'
tr$Commute.Distance_r[tr$Commute.Distance %in%c(4,5)]='8km+'

# 구간화를 통한 파생변수 생성결과 변수컬럼명으로 확인
head(tr[c('Commute.Distance', 'Commute.Distance_r')])
View(tr$Commute.Distance_r)
str(tr)

#파생변수2: 교육 수준---------------------------------------

#구매자의 학력 구간 설정

psych::describe(tr$Education
Hmisc::describe(tr$Education)
tr_b$Education_r<-factor(tr_b$Education,levels=c(1,2,3,4,5),labels=c('저학력자',
                                          '저학력자','고학력자','고학력자','고학력자'))


# 구간화를 통한 파생변수 생성결과 변수컬럼명으로 확인
head(tr[c('Education', 'Education_r')])
str(tr)

#5-4)5장에서 만든 범주형 변수 2개간 특성요약과 시각화-------------

#특성 요약1: 통근 거리-직업군-------
tr_b=tr[tr$Purchased.Bike==1,] #자전거 구매자만 따로 보기로 함
str(tr_b)

ocp_r_f<-factor(tr_b$Occupation_c1)
dis_r_f<-factor(tr_b$Commute.Distance_r)

table(ocp_r_f, dis_r_f, useNA = 'ifany')
ocp_dis_freq <-table(ocp_r_f,dis_r_f)
ocp_dis_freq # 배송 방식에 따른 정시 배송 교차분석 

table(tr_b$Commute.Distance_r,tr_b$Occupation_c1, useNA = 'ifany')
dis_ocp_freq <-table(tr_b$Commute.Distance_r,tr_b$Occupation_c1)
dis_ocp_freq# 정시 배송에 따른 배송방식 교차분석


# 교차빈도분석 부분합(margin) 계산하기
addmargins(ocp_dis_freq )
addmargins(ocp_dis_freq , 1)
addmargins(ocp_dis_freq , 2)

ocp_dis_freq_sum <-addmargins(ocp_dis_freq , 2)
ocp_dis_freq_sum

# 교차빈도분석을 비율분석으로 변환
prop.table(ocp_dis_freq , 1) # 각 배송 방식에서 정시 배달분포비율 비교
prop.table(ocp_dis_freq , 2) # 각 정시 배달에서 배송 방식 분포 비율 비교

ocp_dis_prop <- prop.table(ocp_dis_freq , 1) 
ocp_dis_prop 

dis_ocp_prop=prop.table(dis_ocp_freq , 1) 

ocp_dis_prop%>%round(.,3)%>%addmargins(.,2)

# 교차비율분석을 백분율분석으로 변환
ocp_dis_result <- round(ocp_dis_prop, 3) * 100
ocp_dis_result

dis_ocp_result <- round(dis_ocp_prop, 3) * 100

addmargins(ocp_dis_result, 2)

my_yellow=c('lemon chiffon','light goldenrod','gold','goldenrod','dark goldenrod')

#시각화1: 통근거리-직업군-----------------------------------
par(mfrow=c(2, 2))

barplot(ocp_dis_freq,
        main="통근 거리에 따른\n 직업군 빈도수 분포비교: Stacked",
        xlab="통근 거리", ylab="구매자 수",
        col=c(my_blue[1:5]), legend=rownames(ocp_dis_freq))

barplot(ocp_dis_result,
        main="통근 거리에 따른\n 직업군 비율 분포비교: Grouped",
        xlab="통근 거리", ylab="Percent(%)", beside=TRUE, 
        col=c(my_blue[1:5]), legend=rownames(ocp_dis_freq))

barplot(dis_ocp_freq,
        main="직업군에 따른\n 통근 거리 빈도수 분포 비교: Stacked",
        xlab="구매자 수",
        col=c(my_yellow[c(1,3,5)]), legend=rownames(dis_ocp_freq),ylim=c(0,10),
       ,horiz=TRUE,cex.names=0.8,las=1.5)

barplot(dis_ocp_result,
        main="직업군에 따른\n 통근 거리 비율 분포비교: Grouped",
        xlab="Treatment", ylab="Percent(%)",
        col=c(my_yellow[c(1,3,5)]), legend=rownames(dis_ocp_result), beside=TRUE,xlim=c(0,17.5),
        ,cex.names=0.7)



par(mfrow=c(1, 1))

par(mfrow=c(2, 2))#-------------------------


plot(ocp_r_f~dis_r_f,
     main="통근 거리에 따른\n직업군 분포비교",
     xlab="통근 거리에 따른", ylab="직업군",
     col = c(my_blue[1:5]))

plot(ocp_r_f~dis_r_f,
     main="통근 거리에 따른\n직업군 분포비교",
     xlab="통근 거리에 따른", ylab="직업군",
     col = c(my_blue[1:5]))

mosaicplot( dis_r_f~ocp_r_f,
            main="통근 거리에 따른\n직업군 분포비교",
            xlab="통근 거리에 따른", ylab="직업군",
            col = c(my_blue[1:5]))

mosaicplot(ocp_r_f~dis_r_f,
           main="통근 거리에 따른\n직업군 분포비교",
           xlab="통근 거리에 따른", ylab="직업군",
           col = c(my_yellow[c(1,3,5)]))

par(mfrow=c(1, 1)) # 멀티 캔버스 프레임 리셋

#위 시각화 그래프로 미루어보아 자전거 구매자 중 대부분의 소비자가 직장과 3km 이내에
#살고 있는, 직장과 가까운 소비자가 주로 자전거를 압도적으로 많이 구매하는 것을 알수 있다.
# 또한 3km이내에 위치한 구매자의 직업의 가장 큰 파이는 전문직, 사무직 숙련공, 육체노동자, 경영직
#순으로 가장 많다. 즉, 자전거의 주 소비자 층은 집과 회사의 거리가 3km이내에 
#위치한 사무직 직종에 종사하는 구매자로 특정 지을 수 있다. 따라서 자전거를 이용하고자 하는 욕구가 가장
#큰 소비자 층인 회사 근처에 살고 있는 사무직 직장인을 주요 타겟층으로 잡을 수 있을 것이다. 
#만약 향후 따릉이와 같은 자전거 공유 서비스를 설치할 장소는 사무직 직장인이 많은 회사가 모여있는 도시에서 3km 
#이내에 존재하는 주거단지 근처에 설치하는 것이 가장 높은 이용도를 가질 것으로 예측해 볼 수 있다. 


#5-5) 5장에서 만든 리코딩변수/요약변수/파생변수 중 연속형 변수 2개간 특성요약과 시각화 --------------------------------------------------------
#특성 요약: 나이-소득
str(tr_b$Age)
str(tr_b$Income)

# 공분산(covariance) 분석
var(tr_b$Age,tr_b$Income)

# 상관성(correlation) 분석
cor(tr_b$Age,tr_b$Income, method = 'spearman')
cor(tr_b$Age,tr_b$Income, method = 'pearson')
#상관 계수가 양수임을 보아 제품 가격과 고객 문의 전화 수는 같은 방향으로 움직인다는 것을 알수 있다.

#시각화2: 나이-소득------------------------------

# 기본 graphics::plot() 함수이용: 직선과 곡선 최적합화선 추가
plot(tr_b$Age, tr_b$Income, pch=19,
     main = '나이와 소득간 관련성',
     xlab = '나이', 
     ylab = '소득')

# 최적의 추세직선추가
abline(lm(tr_b$Income~Age,data=tr_b), 
       col="red", lwd=2, lty=1)

# 최적의 추세곡선추가
lines(lowess(tr_b$Age,tr_b$Income), 
      col="blue", lwd=2, lty=2) 

# ggplot2::ggplot() 함수이용: 최적합선 추가
library(ggplot2)

p <- ggplot(data = tr, aes(x = tr_b$Income , y =tr_b$Age)) +
        geom_point() + labs(title = "나이와 소득간 관련성", 
                            y = "나이", 
                            x = "소득")
p

p <- p + geom_smooth() # 최적합선과 오차범위를 표현
p

# plotly::plotly() 함수이용: 최적합선 추가
# install.packages('plotly')
library(plotly)

ggplotly(p) 

#위 그래프로 유추해 보면 자전거 구매자들은 30대에 소득이 나이가 들수록 증가하지만
#40대에는 정체하여 일정 소득 이상을 잘 넘지 못하며 40대 후반쯤 소득의 정점을 찍고 
#이후로는 점점 내려가는 그래프를 보여준다. 그럼에도 전반적으로 나이가 들면 
#소득이 늘어나는 형태를 보여준다. 

#5-6) 5장에서 만든 변수 중 1개 관계를 선정해 특성요약과 시각화-------------

#특성요약3: 교육 수준-소득-----------------------

str(tr$Education_r)

# stats::aggregate()함수이용 요약집계
aggregate(formula = Income ~Education_r , data = tr, FUN = mean, na.rm = TRUE)
aggregate(Income  ~ Education_r, tr, mean, na.rm = TRUE, trim = 0.05)
aggregate(Income  ~ Education_r, tr, sd, na.rm = TRUE)

# magrittr::파이프연산자 %>%, dplyr::데이터가공함수 이용 요약집계  

tr %>% group_by(Education_r) %>%  
  dplyr::summarize(Avg = mean(Income), SD = sd(Income)) %>% 
  arrange(desc(Avg))

#시각화3-----------------------------------
par(mfrow=c(2, 2))

# sm::sm.density.compare()를 사용한 밀도그래프

sm.density.compare(x=tr_b$Income, group=tr_b$Education_r,
                   xlab="교육수준", ylab="밀도", 
                   col=c(2, 3), lty=c(2, 3))

title(main="소득액에 따른 구매자 교육 수준 분포비교")
legend(x=100000, y=0.01, legend = levels(tr_b$Education_r),
       col=c(2, 3, 4), lty=c(2, 3, 4), bty="n")

# boxplot()를 사용한 그래프
boxplot(Income ~ Education_r, data = tr_b,
        main="구매자 교육 수준에 따른 소득액 분포비교",
        xlab="소득액", ylab="소득액($)",
        col=c(2, 3), varwidth=T, notch=T)

par(mfrow=c(1, 1))

# 멀티프레임에 배치할 개별 그래프 준비
# 단순 플롯팅 그래프
p1 <- ggplot(tr, aes(Education_r,Income)) + 
  geom_point(color = "red", shape = 20, size = 2)

# 모든 데이터를 플로팅하는 형태로 분포모양을 나타냄
p2 <- ggplot(tr, aes(Education_r, Income)) + 
  geom_jitter(color = "blue", shape = 8, size = 0.8)

# 박스플롯 형태로 분포모양을 나타냄
p3 <- ggplot(tr, aes(Education_r, Income)) + 
  geom_boxplot(fill = "lightblue", 
               outlier.color ="orange", outlier.shape = 17,
               outlier.size = 2, notch = TRUE)

# 바이올린 플롯형태로 분포모양을 나타냄
p4 <- ggplot(tr, aes(Education_r, Income)) + 
  geom_violin(fill = "lightpink")
#위의 그래프로 살펴보자면 구매자의 밀도는 저학력자에서는 구매자가 주로 
#25000$의 낮은 소득수준에 위치하고 고학력사에서는 구매자가 주로 60000$정도의 
#비교적 높은 소득액에 위치한다. 즉 저학력자들은 낮은 소득액을 가졌을때 비교적 
#자전거 구매율이 높고 고학력자들은 그것보다 조금 위인 60000$수준의 소득액을 가졌을때
#가장 구매가 활발한 것으로 볼수 있다. 