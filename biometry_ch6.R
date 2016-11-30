#Chapter 6 moves on from discrete distributions to the normal distribution, 
#a continuous distribution that will be the basis of most statistics in the book.

#Let's check where it is.
?Distributions
?dnorm

#Section 6.2 Properties of the Norman Distribution
#We can draw a function as a plot using curve().
normal.manual<-function(mean, sd){
curve((1/(sd*sqrt(2*pi)))*exp((-((x-mean)/sd)^2)/2),
      -4, 4, #go from -4 to +4 standard deviations.
      add = FALSE,
      ylab="freq",
      xlab="Y",
      type = "l")
}
normal.manual(0,1)

#You can get this automatically in R with dnorm.
curve(dnorm(x, mean=0, sd=1),
      -4, 4,
      col="white",
      lty="dashed",
      add=TRUE)
#I added a white dashed line over the original black line so you can see that they follow the same path.
#Following box 6.2 to manually make a Q-Q plot to understand how they are built.

#Then how to do them in R automatically.

#The rankits in box 6.2 are done this way.

#Look up Q-Q calculation in R to see how it deals with small sample sizes.