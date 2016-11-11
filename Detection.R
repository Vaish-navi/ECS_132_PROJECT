set.seed(1237)

#Step 1
x <- rnorm(30,mean = 0,sd = 1)
y <- rnorm(30,mean = 0,sd = 1)
qqplot(x,y, plot.it = TRUE)


#Step 2
x <- rnorm(100,mean = 0,sd = 1)
y <- rnorm(100,mean = 0,sd = 1)
qqplot(x,y, plot.it = TRUE)
  
x <- rnorm(1000,mean = 0,sd = 1)
y <- rnorm(1000,mean = 0,sd = 1)
qqplot(x,y, plot.it = TRUE)

#It is clear that the shape of the qqplot becomes more linear as n increases.
#This makes sense as the two distributions are exactly the same.
#The points are dense on the graph between the 1st and 3rd quartiles; they are especially dense around the mean. 

#Stem 3
x <- rnorm(100,mean = 0,sd = 1)
y <- rnorm(100,mean = 5,sd = 3)
qqplot(x,y, plot.it = TRUE)
#The shape of this qqplot also appears to be linear despite the fact that the means and standard deviations are different.
#Obviously, this shows us that the qqplot only indicates the similarity of two distributions regardless of their individual parameters. 

#Step 4
x <- rexp(100,rate = 1)
y <- rexp(100,rate = 1)
qqplot(x,y, plot.it = TRUE)

x <- rexp(1000,rate = 1)
y <- rexp(1000,rate = 1)
qqplot(x,y, plot.it = TRUE)

#It is clear that the shape of the qqplot becomes more linear as n increases.
#This makes sense as the two distributions are exactly the same.
#The points are dense around 1 which makes sense as the mean for an exponencial distribution is (lamda)^-1. 

#Step 5
x <- rnorm(100,mean = 0,sd = 1)
y <- rexp(100,rate = 1)
qqplot(x,y, plot.it = TRUE)

x <- rnorm(500,mean = 0,sd = 1)
y <- rexp(500,rate = 1)
qqplot(x,y, plot.it = TRUE)


