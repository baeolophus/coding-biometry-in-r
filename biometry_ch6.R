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
#50% will be found up to zero (the mean).

#The other values shown in Figure 6.3
pnorm(-1)
pnorm(-2)
#0 and 1 are the default values for mean and standard deviation, so you can leave them out if you want in this case.
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
#This is described in such a way that they seem to be z scores although they are never named as such in the book.
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

#Table 6.1 has expected frequencies for the normal distribution in column 2 for a sample of 1000 individuals.
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
#Then you can have a look at the table and see that it gives the same results as column 2 in Table 6.1.

#Section 6.3 A Model for the Normal Distribution
#This section describes what produces a normal distribution and
#a heuristic showing how it is related to the binomial distribution.

#Section 6.4 Applications of the Normal Distribution
#This section describes how to do what we did to get Table 6.1,
#but with a different set of data back from Section 4.2. [http://www.cmcurry.com/2014/07/using-r-to-work-through-sokal-and.html]

mean.bw<-109.9
sd.bw<-13.593

(sdeviate.bw<-(151-mean.bw)/sd.bw)

pnorm(sdeviate.bw) #This value is more precise
pnorm(3.02) #But the book uses this rounded value so we will too here.
#We can do some of the calculations the book does on pp. 103-104.
#Only a very few individuals are higher than 151 oz.
1-pnorm(3.02)
#If we want to look both directions we can double this number since the distribution is symmetrical.
2*(1-pnorm(3.02))

#By default, lower.tail=TRUE in pnorm.
pnorm(3.02,
      lower.tail=FALSE)
#It is the same as our earlier value of
1-pnorm(3.02)
#Here is the default setting for comparison.
pnorm(3.02,
      lower.tail=TRUE)
#This simply tells use which direction we want to look at,
#the upper or lower tail of the distribution from our value of standard deviate.  

#Section 6.5 Fitting a Normal Distribution to Observed Data

#These data are again from Section 4.2 [http://www.cmcurry.com/2014/07/using-r-to-work-through-sokal-and.html].
classmark<-seq(from=59.5, to=171.5, by=8)
frequencies<-c(2,6,39,385,888,1729,2240,2007,1233,641,201,74,14,5,1)
samplesize<-sum(frequencies) #This confirms that we entered the data correctly, and gets our sample size.
#Multiply classmark and frequencies to get the sums for each class.
classsums<-classmark*frequencies

#To look at all this stuff together, combine it into a dataset.
birthweights<-data.frame(cbind(classmark, frequencies, classsums))

#Add on a row of the next class up which contains 0 individuals.
(birthweights<-rbind(birthweights, c(179.5, 0, 0)))


#On page 104, equation 6.2 is like equation 6.1 above but with sample size (n) and i (class intervals).
normal.manual.applied<-function(mean, sd, n, i){
  curve(((1/(sd*sqrt(2*pi)))*exp((-((x-mean)/sd)^2)/2))*n*i,
        -4, 4, #go from -4 to +4 standard deviations.
        add = FALSE,
        ylab="freq",
        xlab="Y",
        type = "l")
}

normal.manual.applied(mean=0, sd=1, n=1000, i=0.5)
#This gives the curve that Table 6.1 also has (same class intervals of 0.5, 1000 samples, and mean=0 with sd=1).
#Let's do this with the birthweights data.

birthweights.mean<-109.8996
birthweights.sd<-13.5942

#Like above we need the lower boundaries of the class marks.
birthweights$boundaries<-birthweights$classmark-4

#Get the expected frequencies for the class boundaries with the mean and sd of our dataset.
birthweights$pnorm.results<-pnorm(birthweights$boundaries,
                     mean=birthweights.mean,
                     sd=birthweights.sd)

#Then, take the difference of the first row minus the next row.
#The last row will not have anything, which is why we needed to add the lower boundary of
#the next class mark, which has a frequency of zero.  Thus, this calculation generates a vector of length 14. 
#We need 15, so we just add a zero on for the last difference as they do in Table 6.2
birthweights$expected.freqs<-c(abs(diff(birthweights$pnorm.results)),0) #add a zero on for the last difference

#Multipy the frequencies by the sample size to get the expected frequencies for a sample of this size.
#Round as in the table.
birthweights$expected.freqs.values<-round(birthweights$expected.freqs*samplesize, 1)

#We can even add the plus and minus signs using ifelse and sign() to see in which direction the differences are.
birthweights$departure.signs<-ifelse(sign(birthweights$frequencies-birthweights$expected.freqs.values)==-1,
                                     "-", #if -1, then write "-"
                                     "+") #else if not -1, write "+"

#View the table to confirm it has the same data as table 6.2
birthweights


#Section 6.6 Skewness and Kurtosis

#Box 6.1 shows how to compute g1 (skewness) and g2 (kurtosis) from a frequency distribution.
#This is unlike to be how one would do it with your own table of data, 
#but it is a helpful exercise in understanding how these moment statistics work and coding.
#This section assumes you have loaded the birthweights data from the last post. [link]
mean.bw<-sum(birthweights[-16, "frequencies"]*(birthweights[-16, "classmark"]))/samplesize
yfreq<-(
    birthweights[-16, "classmark"]-mean.bw #This is deviation from the mean (see pg 51, section 4.7)
  )

(g1<-(
        samplesize*
        sum(birthweights[-16, "frequencies"]*yfreq^3)
      )/
    (
      (samplesize-1)*(samplesize-2)*birthweights.sd^3
      )
)


(g2<-(
  (
  (samplesize+1)*samplesize*sum(birthweights[-16, "frequencies"]*yfreq^4)
   )/
  (
    (samplesize-1)*(samplesize-2)*(samplesize-3)*(birthweights.sd^4)
    )
  )-
  (
    (
      3*(samplesize-1)^2
      )/
      (
        (samplesize-2)*(samplesize-3)
        )
    )
)

#As an interesting side note, if you use the value of the mean given in the book,
birthweights.mean
#which is rounded to four decimal places, the calculation for cubing 
#(and raising to the power of 4) both were off.  The power of three one was off by 118!


##blog up to here current


#Section 6.7 Graphic Methods

#Figure 6.5 shows the cumulative normal distribution again (as in Fig. 6.3),
#with lines drawn down to equivalent deviates.
curve(pnorm(x, mean=0, sd=1),
      -4, 4,
      lty="solid")
#This is the same code as earlier in section 6.3.
#You can draw lines, we'll just do the one for 0.7 cumulative.
#Remember that qnorm gets us from the cumulative distribution to the value.
#Here are some examples of qnorm again.
qnorm(0.7)
qnorm(0.3)
qnorm(1) #As the text says, this is Inf(infinity)

#We'll use segments() to add arbitrary lines to the plot we made above.
#This code will not draw anything if you don't have a plot open.

segments(x0 = qnorm(0.7),
         y0 = 0,
         x1 = qnorm(0.7),
         y1 = 0.7)

segments(x0 = qnorm(0.7),
         y0 = 0.7,
         x1 = -4,
         y1 = 0.7)

#You can generalize this by making a function.

#function here.

#On page 111, they begin discussing quantile-quantile plots (or QQ plots)

#Make one manually.
#Following box 6.2 to manually make a Q-Q plot to understand how they are built.

#Then how to do them in R automatically.

#The rankits in box 6.2 are done this way.

#Look up Q-Q calculation in R to see how it deals with small sample sizes.