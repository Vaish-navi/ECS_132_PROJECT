setwd("/Users/pouneh/Desktop/STATS_PROJECT")
overtData <- read.csv(file = "Traffic_data_orig.csv", header = TRUE,sep = ",")
secretMessage <- "this is a secret message"
messageLen = as.numeric(nchar(secretMessage))

CharToBinary = function(pchar) {
    lhex = charToRaw(pchar)
    lbits = rev(as.numeric(rawToBits(lhex)))
    return(lbits)
}#Converts a character to Binary equivalent

StringToBinary = function(pstr, pstrlen) {
    lbitstream = NULL
    ltemp = NULL
    for (i in 1:pstrlen) {
        ltemp = CharToBinary(substring(pstr,i,i))
        lbitstream <- c(lbitstream,ltemp)
    }
    return(lbitstream)
}#converts a string to Binary vector

#Q1. GENERATING MODIFIED PACKET STREAM
binaryMessage = StringToBinary(secretMessage, messageLen)
lenBinaryMessage = as.numeric(length(binaryMessage))
covertPackets = NULL
timeStream = 0
covertDataDelay = NULL

for (i in 1:lenBinaryMessage) {
    if(binaryMessage[i] == 0){
        timeStream <- timeStream + 0.25
        covertDataDelay <- c(covertDataDelay,0.25)
    }#if bit is 0, delay is 0.25
    else{
        timeStream <- timeStream + 0.75
        covertDataDelay <- c(covertDataDelay,0.75)
    }#if bit is 1, delay is 0.75
    covertPackets <- c(covertPackets,timeStream)
    
}#loop to generate packet stream from binary message

#Q2. Generating the Histogram
temp = 0 #holds the previous timestamp
overtDataDelay = numeric(dim(overtData)[1])

for( i in 1:dim(overtData)[1])
{
    overtDataDelay[i] = overtData[i,2] - temp
    temp = overtData[i,2]
}

hist(overtDataDelay)
hist(covertDataDelay)


#Eve will be suspicious of the 0.75 & 0.25 because it's exactly two unique differences, whereas for the "Overt" data, given to us in the CSV, the histogram looks more like a negative exponential distribution, which is what you would expect of this type of time dependent data



#Q3
set.seed(1237)
m = median(overtDataDelay)
min = min(overtDataDelay)
max = max(overtDataDelay)

semiRandomPacketsDelay = NULL
semiRandomPackets = NULL
timeStream = 0

for (i in 1:lenBinaryMessage) {
    if(binaryMessage[i] == 0){
        timeLapse = runif(1, min, m)
    }#if bit is 0, delay is 0.25
    else{
        timeLapse = runif(1, m, max)
    }#if bit is 1, delay is 0.75
    timeStream <- timeStream + timeLapse
    semiRandomPacketsDelay <- c(semiRandomPacketsDelay,timeLapse)
    semiRandomPackets <- c(semiRandomPackets,timeStream)
}#loop to generate packet stream from binary message



#Q4
hist(semiRandomPacketsDelay)
hist(overtDataDelay)


#Q5
set.seed(1237)
#m = median(overtDataDelay)
#m = quantile(overtDataDelay, probs = c(0.75) ,names= FALSE)
m = mean(overtDataDelay)
min = min(overtDataDelay)
max = max(overtDataDelay)

semiRandomPacketsDelayB = NULL
semiRandomPacketsB = NULL
timeStream = 0
Nexp <- rexp(10000000, m^-1)

for (i in 1:lenBinaryMessage) {
    if(binaryMessage[i] == 0){
        timelapse = Nexp[i] - 0.0025
        #timeLapse = rexp(1, m)
    }#if bit is 0, delay is 0.25
    else{
        timeLapse = Nexp[i] + 0.0025
        #timeLapse = runif(1, m)
    }#if bit is 1, delay is 0.75
    timeStream <- timeStream + timeLapse
    semiRandomPacketsDelayB <- c(semiRandomPacketsDelayB,timeLapse)
    semiRandomPacketsB <- c(semiRandomPacketsB,timeStream)
}#loop to generate packet stream from binary message
hist(semiRandomPacketsDelayB)

###################### PART #2 DETECTION ##########################
set.seed(1237)

#Step 1
x <- rnorm(30,mean = 0,sd = 1)
y <- rnorm(30,mean = 0,sd = 1)
qqplot(x,y, plot.it = TRUE)
#qqline(x)
abline(0,1)

#Step 2
x <- rnorm(100,mean = 0,sd = 1)
y <- rnorm(100,mean = 0,sd = 1)
qqplot(x,y, plot.it = TRUE)
#qqline(x)
abline(0,1)

x <- rnorm(1000,mean = 0,sd = 1)
y <- rnorm(1000,mean = 0,sd = 1)
qqplot(x,y, plot.it = TRUE)
#qqline(x)
abline(0,1)

#It is clear that the shape of the qqplot becomes more linear as n increases.
#The close appearance of the points to the slope=1 line makes sense as the two distributions are exactly the same.
#The points are dense on the graph between the 1st and 3rd quartiles; they are especially dense around the mean.

#Stem 3
x <- rnorm(100,mean = 0,sd = 1)
y <- rnorm(100,mean = 5,sd = 3)
qqplot(x,y, plot.it = TRUE)
#qqline(x)
abline(0,1)
#abline(5,3)

#The shape of this qqplot also appears to be linear, however the slope is much larger than 1 (which would have indicated perfect correlation).
#Also, the line itself is shifted up so that at x = 0, y =  5. This shows the relationship between the means: the mean of x is 0, and the mean of y is 5. 
#Using this information, we can apply similar rational to the difference in slope.  It is caused by the different standard deviations. 
#The slope for y vs x is 3 in our graph, which also describes the ratio of the standard deviations between the two data sets.
#Despite the fact that the means and standard deviations are different, and that the numerical values for slope and intercepts change because of it, if we normalize y with respect to x, we see that the it does indeed fall on the slope=1 line


#Step 4
x <- rexp(100,rate = 1)
y <- rexp(100,rate = 1)
qqplot(x,y, plot.it = TRUE)
#qqline(x)
abline(0,1)

x <- rexp(1000,rate = 1)
y <- rexp(1000,rate = 1)
qqplot(x,x, plot.it = TRUE)
# qqline(x)
abline(0,1)

#It is clear that the shape of the qqplot becomes more linear as n increases. 
#Despite the This makes sense as the two distributions are exactly the same.
#The points are dense around 1 which makes sense as the mean for an exponencial distribution is (lamda)^-1.

#Step 5
x <- rnorm(100,mean = 0,sd = 1)
y <- rexp(100,rate = 1)
qqplot(x,y, plot.it = TRUE)
#qqline(x)
abline(0,1)

x <- rnorm(500,mean = 0,sd = 1)
y <- rexp(500,rate = 1)
qqplot(x,y, plot.it = TRUE)
#qqline(x)
abline(0,1)

#Step 6
qqplot(overtDataDelay, covertDataDelay, plot.it = TRUE)
#qqline(overtDataDelay)
lines(overtDataDelay, overtDataDelay)

#Step 7
qqplot(overtDataDelay, semiRandomPacketsDelay, plot.it = TRUE)
#qqline(overtDataDelay)
lines(overtDataDelay,overtDataDelay)

#Step 8
qqplot(overtDataDelay, semiRandomPacketsDelayB, plot.it = TRUE)
#lines(overtDataDelay,overtDataDelay)

