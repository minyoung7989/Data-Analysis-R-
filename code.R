### 필요한 패키지 불러오기
install.packages(c("psych","GPArotation"))
install.packages("scatterplot3d")
install.packages("corrplot")
install.packages("car")
install.packages("ggplot2")
library(lm.beta)
library(car)
library(corrplot)
library(scatterplot3d)
library(psych)
library(GPArotation)
library(readxl)
library(dplyr)
library(ggplot2)
library(colorspace)
### 목차
# 1. 상관분석 및 시각화
# 2. 회귀분석 및 시각화
# 3. 요인분석 및 시각화
# 4. 비교분석 및 시각화

### 데이터 불러오기 및 핸들링
dat = read_xlsx("dong.xlsx")

dat$범죄발생 <- max(dat$범죄발생) - dat$범죄발생

#---------------------------------------------상관분석-----------------------------------------------------#

### 상관분석 및 시각화

cor_d = cor(dat[,-1], method = "pearson")
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(cor_d,
         method = "color",
         type = "lower",
         order = "hclust",
         addCoef.col = "black",
         tl.col = "black",
         tl.srt = 45,
         diag = F)

#---------------------------------------------회귀분석-----------------------------------------------------#

### 회귀분석 및 시각화

# 회귀분석
result = lm(평당가~도서관개수+역개수+경찰서수+병원개수+범죄발생+대형마트수+편의점수+보건소수+CCTV,
             data=dat)
summary(result)

# 표준화 회귀계수 산출
result2 = lm.beta(result)
summary(result2)
# 다중공선성 10 안넘는거 확인
vif(result)

# 시각화

plot(평당가~.,data = dat[,-1]) # 평당가에 대한 독립변수 분포 확인

par(mfrow=c(2,2))
plot(result)
## 1. Residual vs Fitted : 적합화된 모델에서 산출된 수치와 잔차를 분석.
## 잔차가 0을 중심으로 고르게 분포하고 있어야 적합한 모델이다. 10, 2, 22는 이상치

## 2. Nomal Q-Q는 잔차가 표준편차를 따라서 잘 분포하는지 보여준다.
## 이상치 2 개를 제외하고 잘 분포하고 있음을 확인

## 3. Scale-Location은 잔차를 표준화하여 제곱근을 적용한 것으로 특별한 Trend가 있으면 안됨.
## 대체로 그러한 분포임을 확인.

## 4. Residual vs Leverage는 각 Redisual의 영향을 보여줌.
## 점으로 된 곡선은 Cook's Distance를 나타내는데 각 점의 영향력을 평가하는 곡선이다.
## 표를 보면 한 개 빼고 Cook's distance 0.5 선 아래에 있으므로 적합한 것을 확인

# 변수별 상대적 중요도 산점도
relweights <- function(fit,...){
  R <- cor(fit$model)
  nvar <- ncol(R)
  rxx <- R[2:nvar, 2:nvar]
  rxy <- R[2:nvar, 1]
  svd <- eigen(rxx)
  evec <- svd$vectors
  ev <- svd$values
  delta <- diag(sqrt(ev))
  lambda <- evec %*% delta %*% t(evec)
  lambdasq <- lambda ^ 2
  beta <- solve(lambda) %*% rxy
  rsquare <- colSums(beta ^ 2)
  rawwgt <- lambdasq %*% beta ^ 2
  import <- (rawwgt / rsquare) * 100
  import <- as.data.frame(import)
  row.names(import) <- names(fit$model[2:nvar])
  names(import) <- "Weights"
  import <- import[order(import),1, drop=FALSE]
  dotchart(import$Weights, labels=row.names(import),
           xlab="% of R-Square", pch=19,
           main="Relative Importance of Predictor Variables",
           sub=paste("Total R-Square=", round(rsquare, digits=3)),
           ...)
  return(import)
}
relweights(result,col="blue")

# 변수별 상대적 중요도 barplot
plotRelWeights=function(fit){
  data<-relweights(fit)
  data$Predictors<-rownames(data)
  p<-ggplot(data=data,aes(x=reorder(Predictors,Weights),y=Weights,fill=Predictors))+
    geom_bar(stat="identity",width=0.5)+
    ggtitle("Relative Importance of Predictor Variables")+
    ylab(paste0("% of R-square \n(Total R-Square=",attr(data,"R-square"),")"))+
    geom_text(aes(y=Weights-0.1,label=paste(round(Weights,1),"%")),hjust=1)+
    guides(fill=FALSE)+
    coord_flip()
  p
}
plotRelWeights(result)

#------------------로지스틱틱
mp <- mean(dat$평당가)

dat$Log <- ifelse(dat$평당가>mp,1,0)
dat

result = glm(Log~도서관개수+역개수+경찰서수+병원개수+범죄발생+대형마트수+편의점수+보건소수+CCTV,
             data=dat, family = binomial)
summary(result)




#-----------------------------------------------요인분석--------------------------------------------------------#

### 요인분석 전 KMO검정 및 Bartlett 검정

# 요인분석 전 KMO검정 시행 -> # KMO 검정 결과 0.59이므로 공통성이 있다
KMO(dat[,-c(1,8,13)])

# 요인분석 전 구형성 검정(바틀렛 검정 이용) 시행 - 바틀렛 검정 결과 = 유의확률(p-value)가 2.2e - 16 < 0.05임으로 귀무가설을 기각한다.
bartlett.test(list(dat$CCTV,dat$도서관개수,dat$역개수,dat$공원개수,dat$경찰서수,
                   dat$경찰서수,dat$병원개수,dat$범죄발생,dat$대형마트수,
                   dat$편의점수,dat$보건소수))
# 즉 KMO 검정 결과 공통성이 있고,
# 바틀렛 검정으로 변수들간의 선형 상관성을 가지는 것으로 판단할 수 있다 -> 분산 차이가 없으므로 요인분석을 해도 된다.

### 요인 수 다양한 방법으로 정해보기

# 필요한 데이터만 추출하여 summary로 확인
dat_fac = dat[,-c(1,8,13)]
name = dat$자치구별
pc = prcomp(dat_fac)
summary(pc) # PC1 = 76%/ PC2 = 99% 요인 수 2개로 결정
plot(pc) #분산비율 시각화

# 고유값으로 요인 수 분석
en = eigen(cor(dat_fac))
en$values # 고유값 보기, 1이 넘는게 3개이므로 요인 수 3개로 결정
VSS.scree(dat_fac) # 스크리 도표로 결정하기-> 3개로 결정

### 요인회전법을 적용

# 주성분분석에 의해 정해진 2개의 요인으로 분석
result = factanal(dat_fac,factor=2,
                  rotation = "varimax",
                  scores = "regression")
result #P-value가 0.183으로 0.05보다 크기 때문에 모형 적합도는 양호하다

# 고유값에서 분석한 요인수 3개로 설정
result2 = factanal(dat_fac,factors = 3,
                   rotation = "varimax",
                   scores = "regression")
result2 # P-value가 0.276로 0.05보다 크기 때문에 모형 적합도는 양호하다
# Uniquenesses 0.5 이하면 유효함 -> 경찰처수, 병원개수, 범죄발생, 편의점수, CCTV
# Loadings에서 0.5가 넘는 것을 기준으로
# -> F1: 병원개수, 범죄발생, 편의점수 | F2: 도서관개수, 역개수, 경찰처수 | F3: CCTV
print(result2, digits = 2, cutoff=0.5) # 0.5가 넘는 것만 추출하여 쉽게 볼 수 있다.


### 요인 수가 3개로 정해졌기 때문에, 각 요인 별 시각화를 진행한다.

# 요인별 시각화
barplot(result2$loadings[,1], col="lightblue",cex.names = 0.7) #첫 번째 요인 - 병원,범죄,편의점 -> 인프라 범죄율
barplot(result2$loadings[,2], col="lightblue",cex.names = 0.7) #두 번째 요인 - 도서관,경찰처,역 -> 치안율
barplot(result2$loadings[,3], col="lightblue",cex.names = 0.7) #세 번째 요인 - CCTV -> 안전

# 요인지표 시각화
plot(result2$scores[, c(1:3)], main="Factor들의 요인점수 행렬")
text(result2$scores[, 1], result2$scores[,2],result2$scores[, 3],
     labels = name, cex = 0.7, pos = 3, col = "blue")
points(result2$loadings[, c(1:3)], pch=19, col = "red") # 요인 적재량 추가
text(result2$loadings[, 1], result2$loadings[,2],result2$loadings[,3],
     labels = rownames(result2$loadings),
     cex = 0.8, pos = 3, col = "red")

# 3D 시각화
Factor1 <- result2$scores[,1]
Factor2 <- result2$scores[,2]
Factor3 <- result2$scores[,3]
l1 = result2$loadings[,1]
l2 = result2$loadings[,2]
l3 = result2$loadings[,3]
D3 <- scatterplot3d(Factor1, Factor2, Factor3)
D3$points3d(l1,l2,l3, col='red',phc=20,cex=2,type = 'h')

### 요인별로 묶어서 데이터프레임 생성
cr = data.frame(dat_fac$병원개수,dat_fac$범죄발생,dat_fac$편의점수) #인프라 범죄율
aas = data.frame(dat_fac$도서관개수,dat_fac$역개수,dat_fac$경찰서수) # 치안율
saf = data.frame(dat_fac$CCTV) # 안전

# 산술평균 계산
cr_mean = round((dat_fac$병원개수+ dat_fac$범죄발생 + dat_fac$편의점수)/ncol(cr),2)
aas_mean = round((dat_fac$경찰서수+dat_fac$역개수+dat_fac$도서관개수)/ncol(aas),2)
saf_mean = round((dat_fac$CCTV)/ncol(saf),2)


# 상관관계 분석 -> cr-aas약한 상관성, saf-cr 적당한 상관성
dat_fac_df <- data.frame(cr_mean,aas_mean,saf_mean)
cor(dat_fac_df)

### 도서관 역 공원 경찰서 병원 범죄 평당가 대형마트 편의점 보건소 CCTV 중
### 병원 범죄 편의점 경찰서 역 도서관 CCTV 선택 -> 공원 평당가 대형마트 보건소 X


# --------------------------------------------비교분석----------------------------------------------#

# 요인분석으로 차원축소를 시킨 후 변수별 백분율로 나타낸다.
# 그 후에, ifelse를 사용하여 구 별 집값이 평균 집값보다 높으면 1, 아니면 0으로 나타내고,
# 요인별로 0과 1 사이에 차이가 있는지 비교하기 위해 시각화를 진행한다.

### 백분율로 만들기
a = (dat$병원개수/sum(dat$병원개수))*100
b = (dat$범죄발생/sum(dat$범죄발생))*100
c = (dat$편의점수/sum(dat$편의점수))*100
d = (dat$경찰서수/sum(dat$경찰서수))*100
e = (dat$역개수/sum(dat$역개수))*100
f = (dat$도서관개수/sum(dat$도서관개수))*100
g = (dat$CCTV/sum(dat$CCTV))*100
h = (dat$보건소수/sum(dat$보건소수))*100
i = (dat$공원개수/sum(dat$공원개수))*100
j = (dat$대형마트수/sum(dat$대형마트수))*100
# 새로운 데이터 프레임 생성
dat2 <- data.frame(cbind(a,b,c,d,e,f,g))

# 행과 열에 이름을 부여함
names(dat2) <- c("병원개수", "범죄발생", "편의점수", "경찰처수", "역개수", "도서관개수", "CCTV")
dat2 <- round(dat2, 3)

rownames(dat2) <- c("종로구", "중구", "용산구", "성동구", "광진구", "동대문구", "중랑구", "성북구",
                    "강북구", "도봉구", "노원구", "은평구", "서대문구", "마포구", "양천구", "강서구",
                    "구로구", "금천구", "영등포구", "동작구", "관악구", "서초구", "강남구","송파구", "강동구")

# 백분위로 나타낸 데이터를 요인별로 묶는다.
cr_p = data.frame(cbind(dat2[,1:3]))
ass_p = data.frame(cbind(dat2[,4:6]))
saf_p = data.frame(dat2[,7])
names(saf_p) = "CCTV"

cr_pf = rowSums(cr_p)/3
ass_pf=rowSums(ass_p)/3
saf_pf=rowSums(saf_p)
mp <- mean(dat$평당가)
fac_final = data.frame(cbind(cr_pf,ass_pf,saf_pf))
names(fac_final) = c("f1","f2","f3")
fac_final = round(fac_final,3)
fac_final$pri = ifelse(dat$평당가>mp,1,0)

low = fac_final[fac_final$pri==0,]
high = fac_final[fac_final$pri==1,]
tot = rbind(low,high)
tot$자치구별 = c("종로구","중구","동대문구","중랑구","성북구","강북구","도봉구","노원구","은평구","서대문구",
             "강서구","구로구","금천구","관악구","강동구","용산구","성동구","광진구",
             "마포구","양천구","영등포구","동작구","서초구","강남구","송파구")

### ggplot을 이용하여 barplot으로 시각화(1은 하늘색, 0은 남색)

p1 = ggplot(data = tot)+
  geom_bar(aes(자치구별,f1, fill = pri ),stat = "identity")+
  theme_classic()+
  ggtitle("Factor1")+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "darkblue"))+
  scale_y_continuous(limits = c(0, 15))+
  labs(y="Factor1")+
  theme(legend.title = element_blank())
p1
p2 = ggplot(data = tot)+
  geom_bar(aes(자치구별,f2, fill = pri ),stat = "identity")+
  theme_classic()+
  ggtitle("평균 집값에 따른 요인2")+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "darkblue"))+
  scale_y_continuous(limits = c(0, 10))+
  labs(y="Factor2")+
  theme(legend.title = element_blank())
p2
p3 = ggplot(data = tot)+
  geom_bar(aes(자치구별,f3, fill = pri ),stat = "identity")+
  theme_classic()+
  ggtitle("평균 집값에 따른 요인3")+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "darkblue"))+
  scale_y_continuous(limits = c(0, 10))+
  labs(y="Factor3")+
  theme(legend.title = element_blank())
p3

### ggplot을 이용하여 pie-chart로 시각화

v1 = c(mean(low$f1),mean(high$f1))
v2 = c(mean(low$f3),mean(high$f2))
v3 = c(mean(low$f2),mean(high$f3))
k = c('평균이하','평균이상')

df1 = data.frame(rank=k,value1=v1)
gg_pie1 = ggplot(df1,aes(x="",y=value1,fill=rank))+
  geom_bar(width=1,stat='identity')+
  coord_polar('y',start=0)+
  ggtitle("평균 집값을 기준으로 나눈 비율")+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20, color = "darkblue"))

df2 = data.frame(rank=k,value2=v2)
gg_pie2 = ggplot(df2,aes(x="",y=value2,fill=rank))+
  geom_bar(width=1,stat='identity')+
  coord_polar('y',start=0)+
  ggtitle("평균 집값을 기준으로 나눈 비율")+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20, color = "darkblue"))

df3 = data.frame(rank=k,value3=v3)
gg_pie3 = ggplot(df3,aes(x="",y=value3,fill=rank))+
  geom_bar(width=1,stat='identity')+
  coord_polar('y',start=0)+
  ggtitle("평균 집값을 기준으로 나눈 비율")+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20, color = "darkblue"))

# 한 번에 출력
gg_pie1 + gg_pie2 + gg_pie3

###각 요소별 시각화
dat3 <- data.frame(cbind(a,b,c,d,e,f,g,h,i,j))


dat3 <- round(dat3, 2)

dat3 <- data.frame(cbind(dat$자치구별,dat3))

names(dat3) <- c("자치구별","병원개수", "범죄발생", "편의점수", "경찰서수", "역개수", "도서관개수", "CCTV","보건소수",
                 "공원개수", "대형마트수")

rownames(dat3) <- c("종로구", "중구", "용산구", "성동구", "광진구", "동대문구", "중랑구", "성북구",
                    "강북구", "도봉구", "노원구", "은평구", "서대문구", "마포구", "양천구", "강서구",
                    "구로구", "금천구", "영등포구", "동작구", "관악구", "서초구", "강남구","송파구", "강동구")

#색만들기
mycol <- hsv(seq(0,1, length=10), s=0.5, v=1, alpha=1)

## 병원개수
a1 <- data.frame(dat3$자치구별, dat3$병원개수)
hospital <- head(a1[c(order(a1$dat3.병원개수, decreasing = T)),], n = 5)
bp <- barplot(hospital$dat3.병원개수, names = hospital$dat3.자치구별, main = "병원개수 상위5개 구",
              ylab = "병원개수", ylim = c(0,15), cex.main = 2.5, cex.lab = 2, col = mycol, border = "gray")
text(x = bp, y = hospital$dat3.병원개수, labels = hospital$dat3.병원개수, col = "blue", cex = 1.5)

## 범죄발생
b1 <- data.frame(dat3$자치구별, dat3$범죄발생)
crime <- head(b1[c(order(b1$dat3.범죄발생, decreasing = T)),], n = 5)
bp1 <- barplot(crime$dat3.범죄발생, names = crime$dat3.자치구별, main = "범죄발생 하위5개 구",
              ylab = "범죄발생", ylim = c(0,7), cex.main = 2.5, cex.lab = 2, col = mycol, border = "gray")
text(x = bp1, y = crime$dat3.범죄발생, labels = crime$dat3.범죄발생, col = "blue", cex = 1.5)

## 편의점수
c1 <- data.frame(dat3$자치구별, dat3$편의점수)
cstore <- head(c1[c(order(c1$dat3.편의점수, decreasing = T)),], n = 5)
bp2 <- barplot(cstore$dat3.편의점수, names = cstore$dat3.자치구별, main = "편의점수 상위5개 구",
               ylab = "편의점수", ylim = c(0,15), cex.main = 2.5, cex.lab = 2, col = mycol, border = "gray")
text(x = bp2, y = cstore$dat3.편의점수, labels = cstore$dat3.편의점수, col = "blue", cex = 1.5)

## 경찰서수
d1 <- data.frame(dat3$자치구별, dat3$경찰서수)
police <- head(d1[c(order(d1$dat3.경찰서수, decreasing = T)),], n = 5)
bp3 <- barplot(police$dat3.경찰서수, names = police$dat3.경찰서수, main = "경찰서수 상위5개 구",
               ylab = "경찰서수", ylim = c(0,7), cex.main = 2.5, cex.lab = 2, col = mycol, border = "gray")
text(x = bp3, y = police$dat3.경찰서수, labels = police$dat3.경찰서수, col = "blue", cex = 1.5)
## 역개수
e1 <- data.frame(dat3$자치구별, dat3$역개수)
subw <- head(e1[c(order(e1$dat3.역개수, decreasing = T)),], n = 5)
bp4 <- barplot(subw$dat3.역개수, names = subw$dat3.역개수, main = "역개수 상위5개 구",
               ylab = "역개수", ylim = c(0,11), cex.main = 2.5, cex.lab = 2, col = mycol, border = "gray")
text(x = bp4, y = subw$dat3.역개수, labels = subw$dat3.역개수, col = "blue", cex = 1.5)

##  도서관개수
f1 <- data.frame(dat3$자치구별, dat3$도서관개수)
libr <- head(f1[c(order(f1$dat3.도서관개수, decreasing = T)),], n = 5)
bp5 <- barplot(libr$dat3.도서관개수, names = libr$dat3.도서관개수, main = "도서관개수 상위5개 구",
               ylab = "도서관개수", ylim = c(0,12), cex.main = 2.5, cex.lab = 2, col = mycol, border = "gray")
text(x = bp5, y = libr$dat3.도서관개수, labels = libr$dat3.도서관개수, col = "blue", cex = 1.5)


## CCTV
g1 <- data.frame(dat3$자치구별, dat3$CCTV)
ctv <- head(g1[c(order(g1$dat3.CCTV, decreasing = T)),], n = 5)
bp6 <- barplot(ctv$dat3.CCTV, names = ctv$dat3.CCTV, main = "CCTV 상위5개 구",
               ylab = "CCTV", ylim = c(0,10), cex.main = 2.5, cex.lab = 2, col = mycol, border = "gray")
text(x = bp6, y = ctv$dat3.CCTV, labels = ctv$dat3.CCTV, col = "blue", cex = 1.5)

## 보건소수
h1 <- data.frame(dat3$자치구별, dat3$보건소수)
phost <- head(h1[c(order(h1$dat3.보건소수, decreasing = T)),], n = 5)
bp7 <- barplot(phost$dat3.보건소수, names = phost$dat3.보건소수, main = "보건소수 상위5개 구",
               ylab = "보건소수", ylim = c(0,7), cex.main = 2.5, cex.lab = 2, col = mycol, border = "gray")
text(x = bp7, y = phost$dat3.보건소수, labels = phost$dat3.보건소수, col = "blue", cex = 1.5)

## 공원개수
i1 <- data.frame(dat3$자치구별, dat3$공원개수)
park <- head(i1[c(order(i1$dat3.공원개수, decreasing = T)),], n = 5)
bp8 <- barplot(park$dat3.공원개수, names = park$dat3.공원개수, main = "공원개수 상위5개 구",
               ylab = "공원개수", ylim = c(0,7), cex.main = 2.5, cex.lab = 2, col = mycol, border = "gray")
text(x = bp8, y = park$dat3.공원개수, labels = park$dat3.공원개수, col = "blue", cex = 1.5)

## 대형마트수
j1 <- data.frame(dat3$자치구별, dat3$대형마트)
market <- head(j1[c(order(j1$dat3.대형마트, decreasing = T)),], n = 5)
bp9 <- barplot(market$dat3.대형마트, names = market$dat3.대형마트, main = "대형마트 상위5개 구",
               ylab = "대형마트", ylim = c(0,7), cex.main = 2.5, cex.lab = 2, col = mycol, border = "gray")
text(x = bp9, y = market$dat3.대형마트, labels = market$dat3.대형마트, col = "blue", cex = 1.5)


# 요인분석
install.packages(c("psych","GPArotation"))
install.packages("scatterplot3d")
library(scatterplot3d)
library(psych)
library(GPArotation)
library(readxl)
library(dplyr)
dat = read_xlsx("dong.xlsx")

### 요인분석 전 KMO검정과 구형섬 검정(바틀렛 검정 이용)
KMO(dat[,-1]) # KMO 검정 결과 0.52이므로 공통성이 있다
# 구형성 검정 - 바틀렛 검정 = 유의확률(p-value)가 2.2e - 16 < 0.05임으로 귀무가설을 기각한다.
# 즉 변수들간의 선형 상관성을 가지는 것으로 판단할 수 있다. -> 분산 차이가 없다 따라서 요인분석을 해도 된다.
bartlett.test(list(dat$CCTV,dat$도서관개수,dat$역개수,dat$공원개수,dat$경찰서수,
                   dat$경찰서수,dat$병원개수,dat$범죄발생,dat$평당가,dat$대형마트수,
                   dat$편의점수,dat$보건소수))

### 요인 수 다양한 방법으로 정해보기

#데이터 불러오기 및 전처리 후 summary로 확인
dat_fac = dat[,-1]
name = dat$자치구별
pc = prcomp(dat_fac)
summary(pc) # PC1 = 63%/ PC2 = 85%/ PC3 = 99%, 3 개
plot(pc) #분산비율 시각화

# 고유값으로 요인 수 분석
en = eigen(cor(dat_fac))
en$values # 고유값 보기, 1이 넘는게 4개이므로 요인 수 4개로 결정
# 스크리 도표로 결정하기-> 4개로 결정
VSS.scree(dat_fac)

##요인회전법 적용

# - 주성분분석에 의해 3개의 요인으로 분석
result = factanal(dat_fac,factor=3,
                  rotation = "varimax",
                  scores = "regression")
result #P-value가 0.06으로 0.05보다 크기 때문에 모형 적합도는 양호하다


## 고유값에서 분석한 요인수 4개로 설정 -> p-value 결과 3개보다 4개가 적합도가 더 좋기 때문에 4개로 설정한다.
result2 = factanal(dat_fac,factors = 4,
                   rotation = "varimax",
                   scores = "regression")
result2 # P-value가 0.139로 0.05보다 크기 때문에 모형 적합도는 양호하다
# Uniquenesses 0.5 이하면 유효함 -> 역,경찰서,병원,범죄,편의점,CCTV 유효
# Loadings에서 0.5가 넘는 것을 기준으로
# ->F1- 병원,범죄,편의점,CCTV | F2- 역,경찰서, F3- 도서관, F4- CCTV
print(result2, digits = 2, cutoff=0.5) #이걸로도 확인 가능



### 요인 수가 정해졌기 때문에, 각 요인 별 시각화를 진행한다.

# 요인별 시각화
barplot(result2$loadings[,1], col="red",cex.names = 0.7) #첫 번째 요인 - 병원,범죄,편의점 -> 병원 범죄율
barplot(result2$loadings[,2], col="red",cex.names = 0.7) #두 번째 요인 - 경찰처,역 -> 역세권
barplot(result2$loadings[,3], col="red",cex.names = 0.7) #세 번째 요인 - 도서관 -> 자기개발
barplot(result2$loadings[,4], col="red",cex.names = 0.7) #네 번째 요인 - CCTV -> 치안

# 요인지표 시각화
plot(result2$scores[, c(1:4)], main="Factor들의 요인점수 행렬")
text(result2$scores[, 1], result2$scores[,2],result2$scores[, 3],result2$scores[, 4],
     labels = name, cex = 0.7, pos = 3, col = "blue")
points(result2$loadings[, c(1:4)], pch=19, col = "red") # 요인 적재량 추가
text(result2$loadings[, 1], result2$loadings[,2],result2$loadings[,3],result2$loadings[,4],
     labels = rownames(result2$loadings),
     cex = 0.8, pos = 3, col = "red")

# 3D 시각화 -> 3개의 요인으로 해봄
Factor1 <- result2$scores[,1]
Factor2 <- result2$scores[,2]
Factor3 <- result2$scores[,3]
l1 = result2$loadings[,1]
l2 = result2$loadings[,2]
l3 = result2$loadings[,3]
D3 <- scatterplot3d(Factor1, Factor2, Factor3)
D3$points3d(l1,l2,l3, col='red',phc=20,cex=2,type = 'h')

### 요인별로 묶어서 데이터프레임 생성
cr = data.frame(dat_fac$병원개수,dat_fac$범죄발생,dat_fac$편의점수) #인프라 범죄율
aas = data.frame(dat_fac$경찰서수,dat_fac$역개수) # 역세권
sd = data.frame(dat_fac$도서관개수) #자기개발
pc = data.frame(dat_fac$CCTV) # 치안

# 산술평균 계산
cr_mean = round((dat_fac$병원개수+ dat_fac$범죄발생 + dat_fac$편의점수)/ncol(cr),2)
aas_mean = round((dat_fac$경찰서수+dat_fac$역개수)/ncol(aas),2)
sd_mean = round((dat_fac$도서관개수)/ncol(sd),2)
pc_mean = round((dat$CCTV)/ncol(pc),2)

# 상관관계 분석
dat_fac_df <- data.frame(cr_mean,aas_mean,sd_mean,pc_mean)
cor(dat_fac_df) #-> cr - pc|aas/ aas- sd/ pc-cr

### 도서관 역 공원 경찰서 병원 범죄 평당가 대형마트 편의점 보건소 CCTV 중
### 병원 범죄 편의점 경찰서 역 도서관 CCTV 선택 -> 공원 평당가 대형마트 보건소 X




#차트
aa <-function(x){
round(prop.table(x)*100,3)
  }

dat3 <- apply(dat2, 1, aa)
dat3 <- data.frame(dat3)

rownames(dat3) <- c("공원", "CCTV", "도서관", "역개수", "경찰서", "병원", "범죄발생", "대형마트",
                    "편의점", "보건소")
jong<-data.frame(state=c("범죄발생","CCTV","경찰서","편의점","병원","대형마트","도서관","보건소","공원","역"),
                 num=c(5.337,11.759,13.774,8.959,22.663,6.428,4.847,7.137,5.391,13.705),
                 종로구=c("치안","치안","치안","인프라","인프라","인프라","공공시설","공공시설","공공시설","교통"))
jung<-data.frame(state=state,num=c(dat3$중구),중구=op)
yong<-data.frame(state=state,num=c(dat3$용산구),용산구=op)
seong<-data.frame(state=state,num=c(dat3$성동구),성동구=op)
gwang<-data.frame(state=state,num=c(dat3$광진구),광진구=op)
dongdae<-data.frame(state=state,num=c(dat3$동대문구),동대문구=op)
jungnang<-data.frame(state=state,num=c(dat3$중랑구),중랑구=op)
seongbuk<-data.frame(state=state,num=c(dat3$성북구),성북구=op)
gangbook<-data.frame(state=state,num=c(dat3$강북구),강북구=op)
dobong<-data.frame(state=state,num=c(dat3$도봉구),도봉구=op)
nowon<-data.frame(state=state,num=c(dat3$노원구),노원구=op)
eunp<-data.frame(state=state,num=c(dat3$은평구),은평구=op)
seodae<-data.frame(state=state,num=c(dat3$서대문구),서대문구=op)
mapo<<-data.frame(state=state,num=c(dat3$마포구),마포구=op)
yang<-data.frame(state=state,num=c(dat3$양천구),양천구=op)
gangsoe<-data.frame(state=state,num=c(dat3$강서구),강서구=op)
guro<-data.frame(state=state,num=c(dat3$구로구),구로구=op)
geum<-data.frame(state=state,num=c(dat3$금천구),금천구=op)
yeongdeung<-data.frame(state=state,num=c(dat3$영등포구),영등포구=op)
dongjak<-data.frame(state=state,num=c(dat3$동작구),동작구=op)
gwanak<-data.frame(state=state,num=c(dat3$관악구),관악구=op)
seocho<-data.frame(state=state,num=c(dat3$서초구),서초구=op)
gangnam<-data.frame(state=state,num=c(dat3$강남구),강남구=op)
songpa<-data.frame(state=state,num=c(dat3$송파구),송파구=op)
gangdong<-data.frame(state=state,num=c(dat3$강동구),강동구=op)

#-------------------------------------------------파이차트트
library(ggplot2)
library(webr)
library(dplyr)

jongrogu<-PieDonut(jong,aes(종로구,state,count=num), title="종로구 요소",
                   titlesize =5)
junggu<-PieDonut(jung,aes(중구,state,count=num),
                 title="중구 요소별 비율",titlesize = 5)
yongsangu<-PieDonut(yong,aes(용산구,state,count=num),
                    title="용산구 요소별 비율",titlesize = 5)
seongdonggu<-PieDonut(seong,aes(성동구,state,count=num)
                      ,title="성동구 요소별 비율",titlesize = 5)
gwangjingu<-PieDonut(gwang,aes(광진구,state,count=num)
                     ,title="광진구 요소별 비율",titlesize = 5)
dongdaemoongu<-PieDonut(dongdae,aes(동대문구,state,count=num),
                        title="동대문구 요소별 비율",titlesize = 5)
jungnanggu<-PieDonut(jungnang,aes(중랑구,state,count=num)
                     ,title="중랑구 요소별 비율",titlesize = 5)
seongbukgu<-PieDonut(seongbuk,aes(성북구,state,count=num)
                     ,title="성북구 요소별 비율",titlesize = 5)
gangbookgu<-PieDonut(gangbook,aes(강북구,state,count=num)
                     ,title="강북구 요소별 비율",titlesize = 5)
dobonggu<-PieDonut(dobong,aes(도봉구,state,count=num)
                   ,title="도봉구 요소별 비율",titlesize = 5)
nowongu<-PieDonut(nowon,aes(노원구,state,count=num)
                  ,title="노원구 요소별 비율",titlesize = 5)
eunpyeonggu<-PieDonut(eunp,aes(은평구,state,count=num)
                      ,title="은평구 요소별 비율",titlesize = 5)
seodaenoongu<-PieDonut(seodae,aes(서대문구,state,count=num)
                       ,title="서대문구 요소별 비율",titlesize = 5)
mapogu<-PieDonut(mapo,aes(마포구,state,count=num)
                 ,title="마포구 요소별 비율",titlesize = 5)
yangcheongu<-PieDonut(yang,aes(양천구,state,count=num)
                      ,title="양천구 요소별 비율",titlesize = 5)
gangsoe<-PieDonut(gangsoe,aes(강서구,state,count=num)
                  ,title="강서구 요소별 비율",titlesize = 5)
gurogu<-PieDonut(guro,aes(구로구,state,count=num)
                 ,title="구로구 요소별 비율",titlesize = 5)
geumcheongu<-PieDonut(geum,aes(금천구,state,count=num)
                      ,title="금천구 요소별 비율",titlesize = 5)
yeongdeungpogu<-PieDonut(yeongdeung,aes(영등포구,state,count=num)
                         ,title="영등포구 요소별 비율",titlesize = 5)
dongjakgu<-PieDonut(dongjak,aes(동작구,state,count=num)
                    ,title="노원구 요소별 비율",titlesize = 5)
gwanakgu<-PieDonut(gwanak,aes(관악구,state,count=num)
                   ,title="관악구 요소별 비율",titlesize = 5)
seochogu<-PieDonut(seocho,aes(서초구,state,count=num)
                   ,title="서초구 요소별 비율",titlesize = 5)
gangnamgu<-PieDonut(gangnam,aes(강남구,state,count=num)
                    ,title="강남구 요소별 비율",titlesize = 5)
songpagu<-PieDonut(songpa,aes(송파구,state,count=num)
                   ,title="송파구 요소별 비율",titlesize = 5)
gangdonggu<-PieDonut(gangdong,aes(강동구,state,count=num)
                     ,title="강동구 요소별 비율",titlesize = 5)
