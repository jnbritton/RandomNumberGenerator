### Random Number Generator

A Python script (designed to run in the R language) that generates a set of mathematically random numbers using Uniform Distribution Inversion. This tool is similar in design to the Gaussian random number generator created by Dr. Mads Haahr and his colleagues over at Random.org, but my design is different in that I use Cholesky decomposition to induce desired levels of correlation among the normally distributed random arrays. My program also builds on the function of the random number generator by allowing users to code the resulting random values according to a predetermined scheme. In the code below, I illustrate the case of one such scheme in the example of a simple grade point average (GPA) distribution. Thus, in its current form, this program will generate sets of 10 randomly-generated grade point scores for 2,500 "students" at an adjustable level of 80% correlation between the scores. I've posted the full code in plain text below with brief notes for each section:

### Code

The first part of the code generates an array of normally distributed pseudo-random variables, orders them into a 10-by-2500 matrix, and generates a corresponding 10-by-10 correlation matrix with an adjustable correlation coefficient (in this case 80% or 0.8):

<!-- -->

     # generate normals, check correlations
       X <- array(rnorm(25000), dim = c(2500, 10))
       cor(X)
     
     # correlation coefficient
       a <- 0.8
     
     # generate 10x10 correlation matrix
       M <- c(1.0, a, a, a, a, a, a, a, a, a, a, 1.0, a, a, a, a, a, a, a, a, a, a, 1.0, 
       a, a, a, a, a, a, a, a, a, a, 1.0, a, a, a, a, a, a, a, a, a, a, 1.0, a, a, a, a,
       a, a, a, a, a, a, 1.0, a, a, a, a, a, a, a, a, a, a, 1.0, a, a, a, a, a, a, a, a, 
       a, a, 1.0, a, a, a, a, a, a, a, a, a, a, 1.0, a, a, a, a, a, a, a, a, a, a, 1.0)
       dim(M) <- c(10, 10)

The next section of the code normalizes the correlation matrix to the uniform distribution on the open interval (0,1):

     # adjust correlations for uniform distribution
       for (i in 1:10){
        for (j in max(i, 1):10){
         if (i != j){
           M[i, j] <- 2 * sin(pi * M[i, j] / 6)
           M[j, i] <- 2 * sin(pi * M[j, i] / 6)
         }
       }
     }

This next step in the program induces correlation in the original array X of normally distributed random numbers, using Cholesky factorization to decompose the adjusted correlation matrix M into its corresponding lower-triangular matrix L and conjugate transpose Lt and taking the product of X * (LLt) = Y. The code then generates the inverse of the uniform cumulative density function (CDF) for Y and plots the marginal distribution (histogram) for the resulting array of inverted uniformly distributed random numbers. The random array is then written to a text file and saved.

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
 
Now that the random normally distributed numbers have been normalized to the open interval (0,1), we can apply any coding system we want (according to a predetermined distribution) to obtain a particular class of random numbers. For this example, I am using the case of a grade point (GP) distribution in which, of all student scores, 29.2% earned a score of 40 GPs, 0.424 - 0.292 = 13.2% earned 37 GPs, 0.528 - 0.424 = 10.4% earned 33 GPs, and so on until 100% of the grade distribution has been accounted for.

The great thing about the cumulative density function we created from our array of random numbers is that it is uniformly distributed over the interval (0,1), so we can safely assume that 29.2% of the CDF will contain 29.2% of the total set of random numbers. This allows us to set that 29.2% of GP scores equal to 40 to preserve the initial distribution we wanted.

     # GP coding
       GP <- function(x){ 
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
