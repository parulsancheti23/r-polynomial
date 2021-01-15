require(plyr)
require(dplyr)
library(plyr)
library(dplyr)
x = c(1.06, 3.52, 3.90)
y = c(1.16, 2.66, 5.44)
MatX = matrix(cbind(1,x,x^2), ncol=3)
detx = det(MatX)
A = (matrix(cbind(1,x,y),ncol=3) %>% det())/detx
B = (matrix(cbind(1,y,x^2),ncol=3) %>% det())/detx
C = (matrix(cbind(y,x,x^2),ncol=3) %>% det())/detx

cat(paste0("y = ",A,"x^2 ",B,"x ",C,sep = ""))

solve(MatX %*% t(MatX)) %*% t(MatX) %*% y

fn = function(x) {sqrt(1 + (A*2*x+B)^2)}
integrate(fn, lower = x[1], upper = x[3])