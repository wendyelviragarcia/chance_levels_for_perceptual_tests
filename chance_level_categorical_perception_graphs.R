###########################
# Mark chance level in categorical perception graphs
#
#
###########################
library(readxl)
myData <- as.data.frame(read_excel("C:/Users/labfonub99/OneDrive - UNED/2020-CIFE/0paper_WAPA/grafico_11_valencia_datos_reconstruidos.xlsx"))

twoOptions <- 0.5
numAnswers <- 100
yourResult <- 50



# Parsing data
myData$Stimuli =as.factor(myData[,1])
myData$Answer =as.factor(myData[,2])
myData$nAnswers =as.numeric(myData[,3])

##############
# using Bernoulli trials, We can test if your success score is beyond chance level
#############
# pbinom in r - binomial probability r
# cumulative probability of an event

pbinom(yourResult,nAnswers,0.5)

#the number of trials is not taken into account, it can be with rbinom(nTrials = 20)
# Example: If we flip a coin 120 times (nOfListeners), what is the probability of getting 40% (yourResults) or less heads?



##############
# But we can also compute the upper and lower limits of the distribution in order to know the chance level limits
#############


  ##############  ##############
  # We can compute it the hard way
  #############  ##############
    
    #upperLimit
    #qbinom(0.75,numAnswers,.5)
    #lowerLimit
    #qbinom(0.25,numAnswers,.5)
    
    #compute upper limit
    numAnswers <- 100
    possResult <-numAnswers-1
    pValue = 0
    while ((possResult > 0) & (pValue< 0.05)) {
      pValue = dbinom(possResult, numAnswers, prob=twoOptions)
      possResult= possResult-1
    }
    
    upperLimit= possResult+1
    
    
    possResult <-0
    pValue = 0
    while ((possResult < numAnswers) & (pValue< 0.05)) {
      pValue = dbinom(possResult, numAnswers, prob=twoOptions)
      possResult= possResult+1
    }
    lowerLimit= possResult-1


    
    ##############  ##############
    # But there is a function for it
    #############  ##############
    # you can have in one line the "inverse" of pbinom. 
    # You tell the function the percentile you want (25% = 0.25) and the function generates the number of successes associated
    #to that cumulative distribution. For example, 
    #qbinom(p, size = , prob = , lower.tail = FALSE, log.p = FALSE)
    # 25% limit of the distribution, for 1000 trials, the outcome is dichotomic (yes/no "a" or "b"), so probability 0.5
    qbinom(0.25,1000,0.5)



##############  ##############
# VISUAL RESULTS
#############  ##############

# Load ggplot
require(ggplot2)
# We draw a classical categorical perception graph
p= ggplot(data=myData, aes(x=Stimuli, y=nAnswers, group=Answer)) +
  geom_line(aes(color=Answer))+
  geom_point(aes(color=Answer))
 

# we add a red/transparent red ribbon with the limits of choice levels
p + geom_ribbon(aes(ymin=qbinom(0.25,numAnswers,twoOptions), ymax=qbinom(0.75,numAnswers,twoOptions)), fill = "red", alpha=0.2)+ylab("Cases of identification per category")


