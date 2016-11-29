#Chapter 6 moves on from discrete distributions to the normal distribution, 
#a continuous distribution that will be the basis of most statistics in the book.

#Let's check where it is.
?Distributions
?dnorm

#Section 6.2 Properties of the Norman Distribution
#We can draw a function as a plot using curve().

curve((1/(1*sqrt(2*3.14159))*exp(-0.5*((x-1)/1)^2)), 0, 1, n = 101, add = FALSE, type = "l")

#Following box 6.2 to manually make a Q-Q plot to understand how they are built.

#Then how to do them in R automatically.

#The rankits in box 6.2 are done this way.

#Look up Q-Q calculation in R to see how it deals with small sample sizes.