##################################
library(lpSolve)
##################################
#1 - 1
Q <- matrix(c(-0.006, 0.003, 0.002, 0.001, 0.000, 0.000, 0.000, 0.000,
              0.020, -0.023, 0.000, 0.000, 0.002, 0.001, 0.000, 0.000,
              0.020, 0.000, -0.024, 0.000, 0.003, 0.000, 0.001, 0.000,
              0.020, 0.000, 0.000, -0.025, 0.000, 0.003, 0.002, 0.000,
              0.000, 0.020, 0.020, 0.000, -0.041, 0.000, 0.000, 0.001,
              0.000, 0.020, 0.000, 0.020, 0.000, -0.042, 0.000, 0.002,
              0.000, 0.000, 0.020, 0.020, 0.000, 0.000, -0.043, 0.003,
              0.000, 0.000, 0.000, 0.000, 0.020, 0.020, 0.020, -0.060), byrow = T, ncol = 8)

A <- t(Q) # 행렬 전환 t()
A[8,] <- 1 # 1개의 행을 제외하고 전체 파이의 합은 1 식으로 변환

B <- c(0,0,0,0,0,0,0,1) # 0벡터의 값 마지막 1 은 위 전체 파이의 합 1인 결과값.
solve(A,B)
pis <- solve(A,B)
sum(pis[1:3]) # 시스템 가용도

#검산
rp <- 0.02
r1 <- 0.001
r2 <- 0.002
r3 <- 0.003

A1 <- rp/(r1+rp)
A2 <- rp/(r2+rp)
A3 <- rp/(r3+rp)

As <- A1*(A2+A3-A2*A3)
As
#1-2

AS <- function(u) (u/(r1+u))*(u/(r2+u)+u/(r3+u)-u/(r2+u)*u/(r3+u))

while(T){
  if (AS(rp) >= 0.962)
    break;
  rp <- rp + 0.00000001
}
AS(rp)

rp
##################################
#2 
f <- function(t) exp(-(0.007*t)^2.9)
g <- function(t) integrate(f,0,t)$value 
h <- function(t) 237+28*(1-exp(-(0.007*t)^2.9))
CA <- function(t) h(t)/g(t)
x <- seq(1,400,1)
y <- NULL
for(i in 1:400) {
  y[i] <- CA(i) 
}
matplot(x[0:400],y[0:400],type="l", xlab="교체주기", ylab="단위시간당
교체 비용", col=c("blue"))
optimize(CA, interval=c(0, 400), maximum = F)


##################################
#3

# 각 부품의 수명
R1 <- 800
R2 <- 50
R3 <- 900
R4 <- 1024
R5 <- function(t) exp(-(0.002 * t)^0.5)
R5 <- integrate(R5,0,Inf)$value # 평균수명

T = max(min(R1,R2), R3, min(R2,R4)) + R5
T

##################################
#4

# 4-2
simul.result <- NULL
Ts <- NULL
n <- 100000 # 시행 횟수 수가 많아질 수록 구하고자 하는 값에 근접
for(i in 1:n) {
  T1 <- T2 <- T4 <- T5 <- T6 <- 200
  T3 <- rweibull(1, shape = 3, scale = 1/0.005)
  Ts <- min(max(T1,T2),T3,max(T4,min(T5, T6))) 
  simul.result <- c(simul.result,Ts)
}
mean(simul.result[simul.result > 211.8])-211.8 #평균 잔여수명. 

simul.result
T3 <- function(t) exp(-(0.005*t)^3)
integrate(T3,0,Inf)$value # T3 평균수명
# 4-3

T1 <- T2 <- T4 <- T5 <- T6 <- 1
T3 <- function(t) exp(-(0.005*t)^3)

Rs <- function(t) T3(t)
Rs(190) # 190 시점에서의 신뢰도

integrate(Rs,0,Inf)$value # 평균수명

v <- integrate(Rs,190,200)$value
MRL <- (1/Rs(190))*v
MRL

simul.result <- NULL
Ts <- NULL
n <- 100000 # 시행 횟수 수가 많아질 수록 구하고자 하는 값에 근접
for(i in 1:n) {
  T1 <- T2 <- T4 <- T5 <- T6 <- 200
  T3 <- rweibull(1, shape = 3, scale = 1/0.005)
  Ts <- min(max(T1,T2),T3,max(T4,min(T5, T6))) 
  simul.result <- c(simul.result,Ts)
}
mean(simul.result[simul.result > 190])-190 #평균 잔여수명. 

##################################
#5 

R1 <- function(t) exp(-0.001 * t)
R2 <- function(t) exp(-(0.006*t)^2)
R3 <- 1

R1(100);R2(100);
gR <- function(t) R1(t)+(R2(t)*R3-R1(t)*R2(t)*R3)
gR(100)

# 5-1
R1(100)
I1B <- function(t) 1-R2(t)*R3
I1B(100)

# 5-2
I2B <- function(t) R1(t)+R3-R1(t)*R3-R1(t)
I2CR <- function(t) I2B(t)*(1-R2(t))/(1-gR(t))

I2CR(100)

# 5-3
I3B <- function(t) R1(t)+R2(t)-R1(t)*R2(t)-R1(t)
I3B(100)

I3IP <- function(t) I3B(t)*(1-R3)
I3IP(100)

# 5-4
I3RAW <- function(t) (1-R1(t))/(1-gR(t))
I3RAW(100)

