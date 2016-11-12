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
Nexp <- rexp(1000000000000000, m^-1)

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

#Step 6
qqplot(overtDataDelay, covertDataDelay, plot.it = TRUE)

#Step 7
qqplot(overtDataDelay, semiRandomPacketsDelay, plot.it = TRUE)

#Step 8
qqplot(overtDataDelay, semiRandomPacketsDelayB, plot.it = TRUE)

