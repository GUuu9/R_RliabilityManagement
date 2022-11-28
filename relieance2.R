install.packages("lpSolve")
library(lpSolve)

#lner 3-2 4p ex)1-3
R1 <- function(t) 1/(t+1)
R2 <- function(t) exp(-0.005 * t)
Rs <- function(t) R1(t)*R2(t)
integrate(Rs,0,Inf)$value


# 7p ex)2-2
R1 <- function(t) exp(-0.002 * t)
R2 <- function(t) exp(-0.004 * t)
Rs <- function(t) 1-(1-R1(t))*(1-R2(t))
integrate(Rs,0,Inf)$value
#11p ex)3
R <- function(t) exp(-0.0004 * t)
term <- function(y, t) choose(4,y)*R(t)^y*(1-R(t))^(4-y)
Rs <- function(t) term(2,t) + term(3,t) + term(4,t)
integrate(Rs,0,Inf)$value
#12p
lambda <- 1 # 가정
R <- function(t) exp(-lambda * t)
term <- function(y, t) choose(3,y)*R(t)^y*(1-R(t))^(3-y)
Rs <- function(t) term(2,t) + term(3,t)
integrate(Rs,0,Inf)$value

# 알아서 고쳐볼걸
lambda <- 0.0005 # 가정
R <- function(t) exp(-lambda * t)
n <- 100 #부품의 수
k <- 50 #살아있어야 하는 부품수
term <- function(n, y, t) choose(n,y)*R(t)^y*(1-R(t))^(n-y)
Rs <- function(t) sum(term(n, k+1:n,t))
integrate(Rs,0,Inf)$value
# 16p
lambda <- 0.006
R1 <- R2 <- R3 <- function(t) exp(-lambda * t)
Rs <- function(t) R1(t)*(R2(t)+R3(t)-R2(t)*R3(t))
integrate(Rs,0,Inf)$value
#18
lambda <- 0.006
R1 <- R2 <- R3 <- R4 <- function(t) exp(-lambda * t)
Rs <- function(t) R1(t)*(R2(t)*R3(t) + R2(t)*R4(t) + R3(t)*R4(t) - 2*R2(t)*R3(t)*R4(t))
Rs(100)
integrate(Rs,0,Inf)$value



#숙제 1
R1 <- function(t) 1/(t+1)
R2 <- function(t) 2/(t+2)
R3 <- function(t) 3/(t+3)
Rs <- function(t) R1(t)*R2(t)*R3(t)
integrate(Rs,0,Inf)$value

#숙제 2
R1 <- function(t) exp(-0.001 * t)
R2 <- function(t) exp(-0.222 * t)
R3 <- function(t) exp(-0.4 * t)
R4 <- function(t) exp(-0.00008 * t)
Rs <- function(t) 1-(1-R1(t))*(1-R2(t)*(1-R3(t)*(1-R4(t))))
integrate(Rs,0,Inf)$value

#숙제 3
install.packages("Deriv")
library(Deriv)
lambda <- 0.0001 # 가정
R <- function(t) exp(-lambda * t)
R(1500)
term <- function(y, t) choose(4,y)*R(t)^y*(1-R(t))^(4-y)
Rs <- function(t) term(2,t) + term(3,t) + term(4,t)
Rs(1500)
h <- Deriv(Rs, "t")
h(1500)
h(1500)/Rs(1500)
integrate(Rs,0,Inf)$value



Rs1 <- function(t) (t-4500)/sqrt(450000)
Rs1(2000)
dnorm(Rs1(2000),0,1)#확률밀도함수
Rs2 <- function(t) (t-5500)/sqrt(550000)
Rs2(2000)
dnorm(Rs2(2000),0,1)
Rs <- function(t) Rs1(t)*Rs2(t)
dnorm(Rs(2000),0,1)


#_----------------------------
install.packages("rootSolve")
CT <- function(n) exp(-n)+0.002*n+6000
library(rootSolve)
uniroot(CT,c(2,8))$root
optimize(CT,c(0,10), maximum = F)
CT(6)
CT(7)

#------------------
CT <- 0
for (n in 1:10) {
  if(n/.003>=1000)
    CT[n] <- 0.002*n+exp(-n)+6000
  else
    CT[n] <- Inf
}
which.min(CT)
CT[which.min(CT)]


# 동전던지기
rbinom(1,1,0.5) # 베르누이 난수
n <- 1000 # 총 실험 횟수
result = NULL
for (i in 1:n){
  result[i] <- rbinom(1,1,0.5) #n회차 실험 결과
}
result # 실험 결과 확인
table(result)

sum(result)/length(result) # 검증

# 윳놀이 시뮬레이션
rbinom(1,4,0.5) # 이항분포 난수
n <- 10000
result <- NULL
for (i in 1:n) {
  result[i] <- rbinom(1,4,0.5)
}

table(result)

simulation <- function(m) length(which(result==m))/length(result)
theory <- function(m) choose(4,m)*0.5^m*0.5^(4-m)

vsim <- c(simulation(1), simulation(2), simulation(3), simulation(4), simulation(5))
vthe <- c(theory(1), theory(2), theory(3), theory(4), theory(0))

compare <- as.matrix(cbind(vsim,vthe)) 
rownames(compare) <- c("도", "개", "걸", "윷", "모") 
colnames(compare) <- c("시뮬레이션", "이론값") 
View(compare)


# 복불복 게임
n <- 10 # 음료갯수 
kkanari <- sample(1:n,1) 
order <- sample(1:n,n) 
kkanari
order 
which(order==kkanari)
m <- 50000 # 시뮬레이션 시행횟수 
data <- rep(0,n)
for(i in 1:m) {
  kkanari <- sample(1:n,1) 
  order <- sample(1:n,n) 
  for(j in 1:n) {
    if(which(order==kkanari)==j) data[j] <- data[j]+1 
  }
}
data
vsim <- t(as.matrix(data/m)) 
rownames(vsim) <- "확률" 
colnames(vsim) <- c(1:n) 
View(vsim)

# 부품 수명 시뮬레이션 - 교재
ob <- 0
Ts <- NULL
n <- 1000 # 시행 횟수 수가 많아질 수록 구하고자 하는 값에 근접
for(i in 1:n) {
  T1 <- rexp(1,0.01)
  T2 <- rexp(1,0.02)
  T3 <- rexp(1,0.03)
  Ts[i] <- min(T1,max(T2,T3)) 
  if(Ts[i] > 20) ob <- ob+1
}
Ts
ob/n # 신뢰도
mean(Ts) # 평균 수명


ob <- 0
Ts <- NULL
n <- 10000 # 시행 횟수 수가 많아질 수록 구하고자 하는 값에 근접
for(i in 1:n) {
  T1 <- rexp(1,0.006)
  T2 <- rexp(1,0.006)
  T3 <- rexp(1,0.006)
  Ts[i] <- min(T1,(T2+T3)) 
  if(Ts[i] > 125) ob <- ob+1
}
ob/n # 신뢰도
mean(Ts) # 평균 수명

ob <- 0
Ts <- NULL
n <- 10000 # 시행 횟수 수가 많아질 수록 구하고자 하는 값에 근접
for (i in 1:n){
  T100 <- rexp(100,0.01)
  Ts <- min(sum(T100[1:45]), sum(T100[46:100]))
  if(Ts > 2000) ob <- ob+1
}
ob/n # 신뢰도
mean(Ts) # 평균 수명


ob <- 0
Ts <- NULL
n <- 100000 # 시행 횟수 수가 많아질 수록 구하고자 하는 값에 근접
for(i in 1:n) {
  T1 <- rexp(1,0.0001)
  T2 <- rweibull(1, shape = 0.7, scale = 1/0.0001)
  T3 <- 8000
  T4 <- rgamma(1, shape = 10, scale = 1/0.00001)
  T5 <- rnorm(1, mean = 50000, sd = 1215)
  Ts <- max(min(T1,T4), min(T2,T5), min(T1,T3,T5), min(T2,T3,T4))
  if(Ts > 1000) ob <- ob+1
}
ob/n
mean(Ts)

ob <- 0
TS <- NULL
n <- 100000 # 시행 횟수 수가 많아질 수록 구하고자 하는 값에 근접
for(i in 1:n) {
  NOTTM <- rweibull(34, shape = 0.64, scale = 1/0.00001)
  TM <- rweibull(4, shape = 1.17, scale = 1/0.000008)
  T1 <- min(NOTTM[1], NOTTM[2], max(NOTTM[3], NOTTM[4]), NOTTM[5])
  T2 <- min(NOTTM[6], NOTTM[7], max(NOTTM[8], NOTTM[9]), NOTTM[10])
  T3 <- min(NOTTM[11], NOTTM[12], max(NOTTM[13], NOTTM[14]), NOTTM[15])
  T4 <- min(NOTTM[16], NOTTM[17], max(NOTTM[18], NOTTM[19]), NOTTM[20])
  T5 <- min(NOTTM[21], NOTTM[22], max(NOTTM[23], NOTTM[24]), NOTTM[25])
  T6 <- min(NOTTM[26], NOTTM[27], max(NOTTM[28], NOTTM[29]), NOTTM[30])
  TT1 <- min(NOTTM[31], TM[1], T2)
  TT2 <- min(NOTTM[32], TM[2], T3)
  TT3 <- min(NOTTM[33], TM[3], T5)
  TT4 <- min(NOTTM[34], TM[4], T6)
  TS1 <- min(T1, max(TT1, TT2))
  TS2 <- min(T4, max(TT3, TT4))
  TS <- max(TS1, TS2)
  if(TS > 120000) ob <- ob+1
}
ob/n
mean(TS)

NTM <- function(t) exp(-(0.00001*t)^0.64)
TM <- function(t) exp(-(0.000008*t)^1.17)
R <- function(t) NTM(t)*NTM(t)*NTM(t)*NTM(t)*NTM(t)*NTM(t)*NTM(t)*NTM(t)*NTM(t)*TM(t)
integrate(R,0,120000)$value


#4chap


#10p 예제 4.5
R1 <- function(t) exp(-0.22*t)*(1-exp(-0.01*t))/(1-exp(-0.23*t))
R1(2000)

R2 <- function(t) exp(-0.01*t)*(1-exp(-0.22*t))/(1-exp(-0.23*t))
R2(2000)

#숙제 1

R1 <- R2 <- R3 <- function(t) exp(-0.01*t)
I1B <- function(t) R2(t)+R3(t)-R2(t)*R3(t)
I2B <- function(t) R1(t)-R1(t)*R3(t)
I3B <- function(t) R1(t)-R1(t)*R2(t)
gR <- function(t) R1(t)*(R2(t)+R3(t)-R2(t)*R3(t))

I1CR <- function(t) I1B(t)*(1-R1(t))/(1-gR(t))
I2CR <- function(t) I2B(t)*(1-R2(t))/(1-gR(t))
I3CR <- function(t) I2B(t)*(1-R3(t))/(1-gR(t))
I1CR(200);I2CR(200);I3CR(200)

curve(I1CR,0,300, col = 'blue', lwd = 1.5) 
curve(I2CR,0,300, col = 'red', lwd = 1.5, lty=6, add = T) 

#예제 4.6
R1 <- function(t) exp(-0.01*t)
R2 <- function(t) exp(-0.22*t)

I1B <- function(t) R2(t)
I2B <- function(t) R1(t)


I1P <- function(t) I1B(t)*(1-R1(t))
I2P <- function(t) I2B(t)*(1-R2(t))

I1P(20)
I2P(20)

#예제 4.7

R1 <- function(t) exp(-0.01*t)
R2 <- function(t) exp(-0.22*t)

I1B <- function(t) 1-R2(t)
I2B <- function(t) 1-R1(t)

I1P <- function(t) I1B(t)*(1-R1(t))
I2P <- function(t) I2B(t)*(1-R2(t))

I1P(20)
I2P(20)

#예제 4.8

R1 <- function(t) exp(-0.01*t)
R2 <- function(t) exp(-0.22*t)
R2R <- function(t) exp(-0.022*t)
I2B <- function(t) R1(t)

I2CIP <- function(t) I2B(t)*(R2R(t)-R2(t))
I2CIP(20)

#예제 4.9

R1 <- function(t) exp(-0.01*t)
R2 <- function(t) exp(-0.22*t)
R1R <- function(t) exp(-0.022*t)

I1B <- function(t) 1-R2(t)

I1CIP <- function(t) I1B(t)*(R1R(t)-R1(t))
I1CIP(20)

#숙제 2 / 16p

R1 <- R2 <- R3 <- function(t) exp(-0.01 * t)
RR <- function(t) exp(-0.001 * t)

I1B <- function(t) R2(t)+R3(t)-R2(t)*R3(t)
I2B <- function(t) R1(t)-R1(t)*R3(t)
I3B <- function(t) R1(t)-R1(t)*R2(t)

I1CIP <- function(t) I1B(t)*(RR(t)-R1(t))
I2CIP <- function(t) I2B(t)*(RR(t)-R2(t))
I3CIP <- function(t) I3B(t)*(RR(t)-R3(t))

I1CIP(20);I2CIP(20);I3CIP(20);



#LINRE4 숙제3

R1 <- R2 <- R3 <- function(t) exp(-0.01*t)



gR <- function(t) R1(t)(R2(t)+R3(t)-R2(t)*R3(t))



IRAW1 <- function(t) 1/(1-gR(t))

IRAW2 <- function(t) (1-R1(t)*R3(t))/1-gR(t)

IRAW3 <- function(t) (1-R1(t)*R2(t))/1-gR(t)



IRAW1(20)   

IRAW2(20)

IRAW3(20)



IRRW1 <- function(t) 1/(1-(R2(t)+R3(t)-R2(t)*R3(t)))

IRRW2 <- function(t) 1/(1-R1(t))

IRRW3 <- function(t) 1/(1-R1(t))



IRRW1(20)   

IRRW2(20)

IRRW3(20)



#기출

R1 <- function(t) 1/(t+1)

R2 <- function(t) 2/(t+2)

#구조함수

gR <- function(t) (R1(t)+R2(t)-R1(t)*R2(t)) 

#Birnbaum

I1B <- function(t) 1-R2(t)

I2B <- function(t) 1-R1(t)



I1B(2000)

I2B(2000)

#결정적중요도 병렬구조이기에 둘다 1



#개선잠재력

I1P <- function(t) I1B(t) * (1-R1(t))

I2P <- function(t) I2B(t) * (1-R2(t))

I1P(8)

I2P(8)

#부품1 1/t+3 교체 개선 잠재력

R3 <- function(t) 3/(t+3)

I3B <- function(t) 1-R3(t)

I3P <- function(t) I3B(t) * (1-R3(t))

I1P(20) - I3P(20)

#위험수용, 위험감소

RAW1 <- function(t) 1-R2(t)/1-gR(t)

RAW1(Inf)

#위험감소 = Inf



#종속 숙제 5

B <-0.2

R1 <- R2 <- function(t) exp(-(1-B)0.01*t)

R3 <- R4 <- function(t) exp(-0.01*t)

RC <- function(t) exp(-B0.01*t)

RR1 <- function(t) RC(t) *(R1(t) +R2(t) -R1(t)R2(t))

RR2 <- function(t) R3(t) +R4(t) -R3(t)R4(t)

RS <- function(t) RR1(t) +RR2(t) -RR1(t)*RR2(t)



integrate(RS,0,Inf)$value


#숙제 5 
lambda <- 0.01
beta <- 0.2
R1 <- R2 <- function(t) exp(-(1-beta)*lambda*t)
R3 <- R4 <- function(t) exp(-0.01*t)
Rc <- function(t) exp(-beta*lambda*t)

R12 <- function(t) (R1(t)+R2(t)-R1(t)*R2(t))*Rc(t)
R34 <- function(t) R3(t)+R4(t)-R3(t)*R4(t)
Rs <- function(t) R12(t)+R34(t)-R12(t)*R34(t)

integrate(Rs,0,Inf)$value



#6장기출문제

R1 <- function(t) exp(-0.0001*t)
R2 <- function(t) exp(-0.0001*t)
R3 <- 1
R4 <- function(t) exp(-0.0001*t)
R5 <- function(t) exp(-0.0001*t)
R6 <- function(t) exp(-0.0001*t)

Ra <- function(t) 1-(1-R1(t))*(1-R2(t))
Rz <- function(t) R5(t)*R6(t)
Rb <- function(t) 1-(1-R4(t))*(1-Rz(t))

Rs <- function(t) Ra(t)*Rb(t)*R3
Rs(4000) # 4천 시점에서의 신뢰도

integrate(Rs,0,8000)$value # 평균수명

v <- integrate(Rs,4000,8000)$value
MRL <- (1/Rs(4000))*v
MRL

n <- 20000
TS <- NULL
ob <- 0
TT <- 0
for(i in 1:n){
  T1 <- rexp(1, 0.0001)
  T2 <- rexp(1, 0.0001)
  T3 <- 8000
  T4 <- rexp(1, 0.0001)
  T5 <- rexp(1, 0.0001)
  T6 <- rexp(1, 0.0001)
  TS[i] <- min(max(T1,T2), T3, max(T4, min(T5, T6)))
  if(TS[i] > 4000) ob <- ob + 1
  if(TS[i] > 4000) TT <- TT + TS[i]
}
mean(TT/ob)-4000 # 평균 잔여 수명

