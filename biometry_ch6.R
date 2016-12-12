#Chapter 6 moves on from discrete distributions to the normal distribution, 
#a continuous distribution that will be the basis of most statistics in the book.

#Section 6.1 discusses frequency distributions of continuous variables briefly.

#Section 6.2 Properties of the Normal Distribution

#Let's check where it is.
?Distributions
?dnorm

#We can draw the normal distribution in a custom function as a plot using curve()
#and equation 6.1 on page 95.
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

#You can draw any of the distributions in Figure 6.2 using these functions 
#(either the R one or the one we just made).

#Figure 6.3 shows the cumulative normal distribution function.

#Let's do that and then plot the normal probability density function on it.
#Cumulative first since that plot will go from 0 to 1.
curve(pnorm(x, mean=0, sd=1),
      -4, 4,
      lty="solid")

#Then add the normal probability density function (like the one we made earlier, just with the default line color).
curve(dnorm(x, mean=0, sd=1),
      -4, 4,
      lty="dashed",
      add=TRUE)

#To get out some of the values  shown in page 96, we use pnorm().
#http://stackoverflow.com/questions/34236648/r-function-to-calculate-area-under-the-normal-curve-between-adjacent-standard-de

pnorm(0, mean=0, sd=1)
#Just adding one value gives you a point estimate.
#50% will be found at zero.

#The other values shown in Figure 6.3
pnorm(-1)
pnorm(-2)
#0 and 1 are the default values, so you can leave them out if you want in this case.
#If you add more than one, it will tell you the values.
(pnorm(0:1))
#Use diff as suggested here to get the area under the curve.
diff(pnorm(0:1))
#This works because it uses the cumulative function to calculate it.
#If you go from -1 to 1 you can add them up to get the 68.27% shown under Figure 6.2 and on page 96.
sum(diff(pnorm(-1:1)))

#To go the other way, i.e. to see where 50% of the items fall, you have to use qnorm.
#Let's see what qnorm looks like.
curve(qnorm(x))
#It is essentially tipping Figure 6.3 on its side.
#The frequency is now x on the x-axis, and the standard deviations are the y axis.
#You can get the point values in Figure 6.3 this way:
qnorm(0.5)
qnorm(0.1587)
qnorm(0.0228)
#The last two are close with rounding since those values in Figure 6.3 were rounded.
#How to get the values on page 96, of where 50%, 95%, etc of items are found?
qnorm(0.5+0.5*0.5)
qnorm(0.95+0.5*0.05)
qnorm(0.99+0.5*0.01)
#The values that you input are not the percent values given on page 96.
#You put them in proportions (the quantile you want to get) PLUS
#half of the remaining value.
#If you put in 0.95, it will go from 0 to 0.95.  That isn't centered at the mean of 0,
#where the 50% quantile is.  There is 0.05 left at the end.  So we divide it by two
#to put it on either end.
#You would get the negative number for the standard deviation if you put in the opposite value.
qnorm(0.5*0.01)
#This also explains it nicely down where it starts talking about qnorm.
#https://cran.r-project.org/web/packages/tigerstats/vignettes/qnorm.html

#On pg. 98, they show calculation of standard deviates.
#This is described in such a way that they seem to be z scores.
#You can easily calculate this manually.
z.score<-function(x){
  (x-mean(x))/sd(x)
}

x<-c(1,2,3)
z.score(x)
#You can also use the scale() function.
#https://www.r-bloggers.com/r-tutorial-series-centering-variables-and-generating-z-scores-with-the-scale-function/

scale(x,
      center=TRUE,
      scale=TRUE)
#help notes that scale=TRUE divides by the standard deviation when
#center=TRUE, and center=TRUE substracts each number by the column mean.
#This is the same thing we did in the z-score function.

#Table 6.2 has expected frequencies for the normal distribution in column 2 for a sample of 1000 individuals.
#We can generate this with pnorm() and thinking about what the class marks mean.
#Because the class marks are separated by 0.5, we need to go 0.5 around each class mark and start at -5.25.
boundaries<-seq(from=-5.25,
                to=5.25,
                by=0.5)

pnorm.results<-pnorm(boundaries)
expected.freqs<-abs(diff(pnorm.results))
cbind(boundaries[-length(boundaries)]+0.25,
      #this takes the last entry off because we only need the lower bound for each class,
      #and adds 0.25 to get the class mark.
      round(expected.freqs*1000, 1))

#Section 6.3
#Following box 6.2 to manually make a Q-Q plot to understand how they are built.

#Then how to do them in R automatically.

#The rankits in box 6.2 are done this way.

#Look up Q-Q calculation in R to see how it deals with small sample sizes.