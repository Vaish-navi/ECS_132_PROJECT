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

for (i in 1:lenBinaryMessage) {
  if(binaryMessage[i] == 0){
    timeStream <- timeStream + 0.25
  }#if bit is 0, delay is 0.25
  else{
    timeStream <- timeStream + 0.75
  }#if bit is 1, delay is 0.75
  covertPackets <- c(covertPackets,timeStream)
}#loop to generate packet stream from binary message  
covertPackets

