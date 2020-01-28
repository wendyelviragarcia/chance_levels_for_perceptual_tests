###########################
# Mark chance level in categorical perception graphs
#
#
###########################
library(readxl)
myData <- as.data.frame(read_excel("C:/Users/labfonub99/OneDrive - UNED/2020-CIFE/0paper_WAPA/input.xlsx"))

twoOptions <- 0.5
numAnswers <- 120
yourResult <- 50



# Parsing data
myData$Stimuli =as.factor(myData[,1])
myData$OriginalStimuli =as.factor(myData[,2])
myData$nAnswers =as.numeric(myData[,3])
oneLevel = myData[myData$Stimluli == levels(myData$Stimuli)[1], ]
totalAnswers= sum(oneLevel$value)
##############
# using Bernoulli trials, We can test if your success score is beyond chance level.
# returns the probability of your result
#############
# pbinom in r - binomial probability r
# cumulative probability of an event

pbinom(yourResult,nAnswers,0.5,lower.tail=FALSE)

#the number of trials is not taken into account, it can be with rbinom(nTrials = 20)
# Example: If we flip a coin 120 times (nOfListeners), what is the probability of getting 40% (yourResults) or less heads?



##############
# But we can also compute the upper and lower limits of the distribution in order to know the chance level limits
#############


  ##############  ##############
  # We can compute it the hard way
  #############  ##############
    # we stablish the probability value dependending on the number of choices

#pbinom(4, size=12, prob=0.2) 
#[1] 0.92744

#Answer

#The probability of four or less questions answered correctly by random in a twelve question multiple choice quiz is 92.7%. 
    #upperLimit
    #qbinom(0.75,numAnswers,.5)
    #lowerLimit
    #qbinom(0.25,numAnswers,.5)
    #we test when our data becomes different of a probability of 50% 
    #compute upper limit
    possResult <-numAnswers-1
    pValue = 0
    while ((possResult > 0) && (pValue<0.01)) {
      pValue = binom.test(possResult, numAnswers, twoOptions, alternative = "two.sided")
      possResult= possResult-1
    }
    
    upperLimitEstricto= possResult+1
    
    
    possResult <-0
    pValue = 0
    while ((possResult < numAnswers) && (pValue< 0.01)) {
      pValue = binom.test(possResult, numAnswers, twoOptions, alternative = "two.sided")
      possResult= possResult+1
      print(pValue)
    }
    lowerLimitEstricto= possResult-1


    
    ##############  ##############
    # 
    #############  ##############
    possResult <-numAnswers-1
    pValue = 0
    while ((possResult > 0) && (pValue<0.05)) {
      pValue = binom.test(possResult, numAnswers, twoOptions, alternative = "two.sided")
      possResult= possResult-1
    }
    
    upperLimit= possResult+1
    
    
    possResult <-0
    pValue = 0
    while ((possResult < numAnswers) && (pValue< 0.05)) {
      pValue = binom.test(possResult, numAnswers, twoOptions,alternative = "two.sided")
      possResult= possResult+1
      print(pValue)
    }
    lowerLimit= possResult-1
    # 
    # You tell the function the percentile you want (25% = 0.25) and the function generates the number of successes associated
    #to that cumulative distribution. For example, 
    #qbinom(p, size = , prob = , lower.tail = FALSE, log.p = FALSE)
    # 25% limit of the distribution, for 1000 trials, the outcome is dichotomic (yes/no "a" or "b"), so probability 0.5
    #qbinom(0.25,100,0.5)



##############  ##############
# VISUAL RESULTS
#############  ##############

# Load ggplot
require(ggplot2)
# We draw a classical categorical perception graph
p= ggplot(data=myData, aes(x=Stimuli, y=nAnswers, group=OriginalStimuli)) +
  geom_line(aes(color=OriginalStimuli,linetype =OriginalStimuli),size=2)+
  geom_point(aes(color=OriginalStimuli, shape = OriginalStimuli),size=4)
 

# we add a red/transparent red ribbon with the limits of choice levels
p + geom_ribbon(aes(ymin=lowerLimit, ymax=upperLimit), fill = "red", alpha=0.2)+ylab("Identification count (N)")+
  geom_ribbon(aes(ymin=lowerLimitEstricto, ymax=upperLimitEstricto), fill = "blue", alpha=0.1)
