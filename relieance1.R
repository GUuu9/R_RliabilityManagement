obj <- c(1.5, 1)
const <- matrix(c(1,0,
                  0,1,
                  1,1,
                  1,0,
                  0,1),byrow = T, ncol=2)
direction <- c("<=","<=","<=",">=",">=")
rhs <- c(4,6,5,0,0)

install.packages("lpSolve")
library(lpSolve)

?lp
lp ("max", obj, const, direction, rhs)$solution



## 4-5p
obj <- c(0.4, 0.6)
const <- matrix(c(5,10,
                  1,1,
                  0,1,
                  1,0,
                  0,1),byrow = T, ncol=2)
direction <- c("<=","<=",">=",">=",">=")
rhs <- c(38,5,1,0,0)


library(lpSolve)
?lp
lp ("max", obj, const, direction, rhs, int.vec = 1:2)
lp ("max", obj, const, direction, rhs, int.vec = 1:2)$solution



#감마.
#Pr{S2 > 5000} = 5000 에서 부터 무한대 까지 S2의 확률 밀도 함수를 적분

S2 <- function(x) dgamma(x,2,1/1500)
integrate(S2, 5000, Inf)$value

##Pr{S2 > 5000} = Pr{N(5000) = 0}+Pr{N(5000) = 1}

#Pr{N(5000)=0} = dpois(0,1/1500*5000)
#Pr{N(5000)=1} = dpois(1,1/1500*5000)
dpois(0,1/1500*5000) + dpois(1,1/1500*5000)


#2chap ex4 와이블분포 평균 수명
R <- function(t) exp(-(1.15*10^-4*t)^2.25)
integrate(R,0,Inf)$value

lambda <- 1.15*10^-4
beta <- 2.25
R <- function(t) exp(-(lambda*t)^beta)
t <- 4380
1/R(t)*integrate(R,t,Inf)$value


