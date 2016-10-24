Chapter 5 mostly covers the binomial and Poisson distributions, and briefly mentions hypergeometric, multinomial, negative binomial, and logarithmic.  

#Section 5.1
#This section talks about probability theory, unions, intersections, independence,
#conditional probability, and Bayes' theorem (which apparently will not be used in the rest of the book).

#Section 5.2: the binomial distribution
?Distributions #to see distributions available in 'stats'.
?dbinom


#Let's generate table 5.1, figure 5.2 (bar graph side by side).
#equation 5.9 by hand using factorial() 
#to population a table with coefficients and powers of p and q (3 and 4 in table)

#Then use 
choose(n=, #n is the size of the sample.  "k"
       k=) #k is the place in the coefficient, starting at 0 (number infected, number whatever) "Y"

#get relative expected frequencies (5) with pbinom
http://www.r-tutor.com/elementary-statistics/probability-distributions/binomial-distribution

dbinom(c(0,1,2,3,4,5), size=5, prob=0.4)
#density is book's relative frequencies.
pbinom(c(0,1,2,3,4,5), size=5, prob=0.4)
#to contrast, this adds up the densities/relative frequencies.
