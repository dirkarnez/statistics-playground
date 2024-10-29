#Chi square 

#Problem 1
#Five coins are tossed 256 times. The number of heads observed by binomial distribution is given below.
#Examine if the coins are unbiased by employing chi-square goodness of fit.
#No. of heads :  0   1   2   3   4   5
#Frequency:      5   35  75  84  45  12


#Problem 2
#From the following information state whether the condition of the child is associated with the
#condition of the house.
#Condition of the child    Condition of house clean    Condition of house dirty
#        Clean                       69                          51
#    Fairly Clean                    81                          20
#        Dirty                       35                          44


#Solution - Problem 1

#Goodness of fit

#No. of coins
n = 5

#LOS
alpha = 0.05

#Total number of tosses
N = 256

#Probability of getting head
P = 0.5

#Random Variable representing number of heads
x = c(0:n)

#Observed frequency
obf = c(5,35,75,84,45,12)

#Expected frequency
exf = dbinom(x,n,P)*256
exf

#Check if sum of obf and exf are same
sum(obf)
sum(exf)

#Output using Chisq-distribution (Formula Method)
cv = sum((obf-exf)^2/exf)
cv

#Critical value using chisquare table
tv = qchisq(1-alpha,n)
tv

#Hypothesis conclusion
if(cv <= tv){
  print("Accept H0/Fit is good")
}else{
  print("Reject H0/Fit is bad")
}

#Problem 2
#Independent of attributes
#Input the data

#Data stored in matrix form
data = matrix(c(69,51,81,20,35,44),ncol=2,byrow=T)

#Number of data
l = length(data)

#Output by chisq-distribution
cv = chisq.test(data)
cv

#P-value
pv = cv$p.value
pv

if(pv > alpha){
  print("Attributes are independent")
}else{
  print("Attributes are not independent")
}


