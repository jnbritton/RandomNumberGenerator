# generate normals, check correlations
X <- array(rnorm(25000), dim = c(2500, 10))
cor(X)

# correlation coefficient
a<-0.8

# generate 10x10 correlation matrix
M <- c(1.0, a, a, a, a, a, a, a, a, a, a, 1.0, a, a, a, a, a, a, a, a, a, a, 1.0, a, a, a, a, a, a, a, a, a, a, 1.0, a, a, a, a, a, a, a, a, a, a, 1.0, a, a, a, a, a, a, a, a, a, a, 1.0, a, a, a, a, a, a, a, a, a, a, 1.0, a, a, a, a, a, a, a, a, a, a, 1.0, a, a, a, a, a, a, a, a, a, a, 1.0, a, a, a, a, a, a, a, a, a, a, 1.0)
dim(M) <- c(10, 10)

# adjust correlations for uniform distribution
for (i in 1:10){
    for (j in max(i, 1):10){
        if (i != j){
            M[i, j] <- 2 * sin(pi * M[i, j] / 6)
            M[j, i] <- 2 * sin(pi * M[j, i] / 6)
        }
    }
}

# induce correlation by Cholesky decomposition, check correlations
C <- chol(M)
Y <- X %*% C
cor(Y)

# create uniforms, check correlations
Y[, 1:10] <- pnorm(Y[, 1:10])
cor(Y)

# plot results (marginals)
par(mfrow = c(2, 5))
for (i in 1:10){
    hist(Y[, i], main = paste("Y", i), xlab = "")
}

write.table(Y, file= "corr80percent.txt")

# GP coding
GP<-function(x){ 
b<-x

b<-ifelse (x<0.292, 40,b) 
b<-ifelse (x>=0.292 & x<0.424, 37,b) 
b<-ifelse (x>=0.424 & x<0.528, 33,b) 
b<-ifelse (x>=0.528 & x<0.654, 30,b) 
b<-ifelse (x>=0.654 & x<0.722, 27,b) 
b<-ifelse (x>=0.722 & x<0.776, 23,b) 
b<-ifelse (x>=0.776 & x<0.857, 20,b) 
b<-ifelse (x>=0.857 & x<0.895, 10,b) 
b<-ifelse (x>=0.895 & x<0.931, 0,b)  
b<-ifelse (x>=0.931 & x<=1, -9,b)
b}

Z<- apply(Y,2,GP)
# plot GPA histograms
par(mfrow = c(2, 5))
for (i in 1:10){
    hist(Z[, i], main = paste("Z", i), xlab = "")
}

write.table(Z, file= "GPcorr80percent.txt")
