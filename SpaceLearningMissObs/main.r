
K = (t(rbind(c(1,0,1,0),c(1,0,1,0),c(0,1,0,1),c(1,0,0,1))))
K
#K <- matrix(sample.int(15, size = 4*5, replace = TRUE), nrow = 4, ncol = 5)
n = dim(K)[1]
m = dim(K)[2]

k = 2
W <- matrix(rnorm(n*k,0,1),n,k)
H <- matrix(rnorm(k*m,0,1),k,m)
dim(W);dim(K);dim(H)

U = matrix(FALSE,n,m)
U[n,m]=TRUE
U[3,4]=TRUE
U
#U = matrix(sample(c(TRUE,FALSE),size=n*m, replace=TRUE, prob=c(0.5, 0.5) ),n,m)

E = function(W,H,K){0.5*sum(sum((W%*%H-K)^2))}
eta = 0.01
E(W,H,K)
for (i in 1:1000){

    gW = (W%*%H-K)%*%t(H)
    gH = t(W)%*%(W%*%H-K)
    gU = -(W%*%H-K)

    W = W -eta*gW
    H = H -eta*gH
    K[U] = K[U] -eta*gU[U]
    
   
}
E(W,H,K)
K
W%*%H