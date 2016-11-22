#Chapter 5 mostly covers the binomial and Poisson distributions, and briefly mentions hypergeometric, multinomial, negative binomial, and logarithmic.  

#Section 5.1
#This section talks about probability theory, unions, intersections, independence,
#conditional probability, and Bayes' theorem (which apparently will not be used in the rest of the book).

#Section 5.2: the binomial distribution
?Distributions #to see distributions available in 'stats'.
?dbinom #to see what you can do with binomial distribution in particular.

#Let's generate table 5.1 and figure 5.2 (bar graph side by side).
#The remaining tables in section 5.2 would use similar code.

#You can get the binomial coefficient equation 5.9 by hand using factorial() 
#For example on page 71 in table 5.1, they give an example of a sample of 5 containing 2 of the infected insects.
#to population a table with coefficients and powers of p and q (columns 3 and 4 in table)
factorial(5)/(factorial(2)*factorial(5-2))
#Yes, this gives us 10 just like in table 5.1, column 2, for 2 infected insects.

#You can do this more simply using the choose() function.
#Beware the different use of k in the R function versus the book.
choose(n=5, #n is the size of the sample.  "k" in book.
       k=2) #k is the place in the coefficient, starting at 0 (number infected, number whatever); "Y" in book.

#To get the values in column 3 and 4 of table 5.1, except for the power of 0, which is 1 and not calculated here.
poly(0.4, degree=5, raw=T)
poly(0.6, degree=5, raw=T)
#You will note taht the numbers for 0.6 (powers of q) are reversed in column 4.
#When you note that the number of uninfected insects per sample will be just opposite of infected insects (in column 1)
#this makes more sense.

#To get the relative expected frequencies (column 5), use dbinom and pbinom.
#http://www.r-tutor.com/elementary-statistics/probability-distributions/binomial-distribution

(rel.expected.freq<-dbinom(c(0,1,2,3,4,5), size=5, prob=0.4))
#density is book's relative frequencies.
(pbinom(c(0,1,2,3,4,5), size=5, prob=0.4))
#to contrast, this adds up the densities/relative frequencies.

#To get column 6, the absolute expected frequencies, multiply rel.expected.freq by the actual sample size.
(abs.expected.freq<-2423*rel.expected.freq)

#Now we'll use the abs.expected.freq plus the observed frequencies together to make a side-by-side barplot.
obs.freq<-c(202,
            643,
            817,
            535,
            197,
            29)

infected.freq<-rbind(obs.freq,
                 abs.expected.freq)
colnames(infected.freq)<-0:5
rownames(infected.freq)<-c("Observed frequencies",
                           "Expected frequencies")
#Adding colnames and rownames gives the proper legend and x axis labels in the plot.

#If you want to make a side-by-side barplot,
#you need adjacent columns, the barplot() function,
#and to specify beside=TRUE.
barplot(infected.freq,
        beside = TRUE,
        ylab="Frequency",
        xlab="Number of infected insects per sample",
        axes=TRUE,
        legend.text = TRUE,
        args.legend=c(bty="n"))

#Section 5.3: the Poisson distribution
?Distributions #Let's look in help again to find the Poisson distribution.
?dpois #We can see that R does similar calculations for Poisson in addition to the binomial distribution.

#To get column 3 (expected absolute frequencies) in table 5.5,
#there is the recursion formula given in equations 5.11 and 5.12.
#5.12 is for calculations with sample means to get the relative expected frequency,
#and in the text below it notes what term to add to get the absolute ones given in column 3 of table 5.5

abs.expected.freq.pois<-400*dpois(c(0:9),
                                  lambda=1.8,
                                  log=FALSE)

#I also want to do equation 5.12 manually for relative expected frequencies.
#This is not normally needed because R does it so nicely with the base stats function dpois().

#Some reading:
#http://stackoverflow.com/questions/5273857/are-recursive-functions-used-in-r
#this one has a simple one for factorials that makes it clearest to me:
#http://www.programiz.com/r-programming/recursion

rel.exp.freq.pois<-function(samplemean, i) {
  if (i==0) return (exp(-samplemean))
  else      return (rel.exp.freq.pois(samplemean,
                                      i-1)*(samplemean/i))
}

#To get absolute frequencies, multiply by 400.
#For one example:
400*rel.exp.freq.pois(1.8, 1)

#To get all the frequencies at once, use lapply.
rel.freq.pois<-unlist(lapply(c(0:9),
                                     rel.exp.freq.pois,
                                     samplemean=1.8))
(abs.freq.pois<-400*rel.freq.pois)

#On page 82, they show how to calculate the coefficient of dispersion.  Here is a function that will do it.
coefficient.of.dispersion<-function(data) {
  var(data)/mean(data)
}
#You can input any set of data here and get the coefficient of dispersion.

#Figure 5.3 shows Poisson distributions with different means.
#We can make this with the dpois() function generating y data.
#Oddly, to add the extra lines, it is easiest to use points() with 
#type="l" (for lines).
plot(dpois(c(0:18),
      lambda=0.1,
      log=FALSE),
     type="l",
     ylab="Relative expected frequency",
     xlab="Number of rare events per sample")
points(dpois(c(0:18),
           lambda=1,
           log=FALSE),
       type="l")
points(dpois(c(0:18),
             lambda=2,
             log=FALSE),
       type="l")
points(dpois(c(0:18),
             lambda=3,
             log=FALSE),
       type="l")
points(dpois(c(0:18),
             lambda=10,
             log=FALSE),
       type="l")

#The remaining tables and examples in this section do not add anything new to code.

#Section 5.4: Other Discrete Probability Distributions
?dhyper #hypergeometric
?dmultinom #multinomial
?dnbinom #negative binomial
#There are also options in various packages later for regression to use different distributions,
#although of these I've only ever used negative binomial.

#The logarithmic distribution requires an extra package.
#https://cran.r-project.org/web/views/Distributions.html
#extraDistr, VGAM, or gamlss.dst
#I installed extraDistr to test it out.
library(extraDistr)
?LogSeries
#and
?dlgser #The distributions in this package follow the same us of d, p, and q prefixes.


#Exercises 5
#I'm doing the exercises that require new coding beyond what we have done already.

#Exercise 5.5
#The organism is present or absent in any given slide, assuming the person is diseased
#(if the person has the disease, their samples contain the organism but it's not very common).
#We want a false negative <1% of the time.  The organism is visible in 20% of the slides.
#At first I thought this was a Bayesian problem but I don't see how to do it that way.
#p=0.2 (organism visible), thus q=0.8 (organism present but not visible).
#We need to find the power of q=0.8 that equals 0.01 or smaller
#(1% false negative, which would be present but not visible).
#So, I made a while loop that cycled through to see how many times we need to raise 0.8
#to get to 0.01.  This helped: http://www2.warwick.ac.uk/fac/sci/moac/degrees/moac/ch923/r_introduction/r_programming/

n<-0
i<-1
print(n)
while (i > 0.01) {
  print(i)
  print(n)
     i<-0.8^n
     n=n+1
}
#the last number printed is the number of slides needed.
