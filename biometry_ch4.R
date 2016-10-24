Chapter 4 covers descriptive statistics, such as statistics of location
(such as mean and median that show the central tendency of your population sample)
and statistics of dispersion (such as range and standard deviation).

The book notes that they will use "ln" for the natural logarithm, and "log"
for common base 10 logarithms.  Recall that R uses log for the natural logaritm.
log10 will give you the common base 10 logarithm.

Section 4.1: the arithmetic mean

#The arithmetic mean is the sum of all the sample values
#divided by the sample size (number of items in the sample).

#We'll test it using the aphid femur data from Chapter 2.

aphid.femur<-c(3.8,3.6,4.3,3.5,4.3,
               3.3,4.3,3.9,4.3,3.8,
               3.9,4.4,3.8,4.7,3.6,
               4.1,4.4,4.5,3.6,3.8,
               4.4,4.1,3.6,4.2,3.9)

aphid.average<-sum(aphid.femur)/length(aphid.femur)
#sum() adds up everything in the vector.  Length tells you how many values are in it
#(i.e., the sample size).

#Now that we know the mechanics, let's use R's function for it.

(aphid.Raverage<-mean(aphid.femur))

#Same value!  Yay!  Box 2.1 does additional things with these data beyond mean,
#but we won't worry about this until later in the chapter.

#Next in the text they refer to Box 4.2, 
#where we can calculate means from a frequency distribution.
#They give two options for these calculations: non-coded and coded.

#For non-coded, sum the class marks by frequency.
#The easiest way to do this seemed to be to make a vector of both and multiply and sum.

classmark<-seq(from=59.5, to=171.5, by=8)
frequencies<-c(2,6,39,385,888,1729,2240,2007,1233,641,201,74,14,5,1)
samplesize<-sum(frequencies) #This confirms that we entered the data correctly, and gets our sample size.
#Multiply classmark and frequencies to get the sums for each class.
classsums<-classmark*frequencies

#To look at all this stuff together, combine it into a dataset.
birthweights<-data.frame(cbind(classmark, frequencies, classsums))

summing<-sum(classmark*frequencies)

#Confusingly to me, it displays the answer as 1040200, but R apparently just rounds for display.
#In RStudio's global environment, it does show that R has correctly calculated the answer as 1040199.5.
#That is the value it uses for further calculations.
(mean.birthweights<-summing/samplesize)
#This is confirmed when our average birthweight calculation is 109.8996, as the book shows.


#Weighted averages are up next.  You use these if means or other stats are
#based on different sample sizes or for "other reasons".
#I'd assume something like different values should be weighted for study design
#or reliability reasons that are beyond the scope of the simple discussion of 
#how to calculate the weighted average.
#We can do it by hand using equation 4.2 as given on pg. 43.
values<-c(3.85,5.21,4.70)
weights<-c(12,25,8)
(weighted.mean<-sum(weights*values)/sum(weights))
#Yep, right answer.  R has a function for this, though.
r.weighted.mean<-weighted.mean(values, weights)

#Section 4.2: other means
#This section covers the geometric and harmonic means.

#The geometric mean is the nth root of the multiplication of all the values.
#This would be a huge number usually, so logarithms are used instead.
#R does not have a function for this in base R.

geometric.mean.myfunction<-function (dataset) {
  exp(mean(log(dataset)))
}

#Test it with the aphid femur data; answers are on pg. 45.
geometric.mean.myfunction(aphid.femur)

#It works!  This website http://stackoverflow.com/questions/2602583/geometric-mean-is-there-a-built-in points out that 0 values will give -Inf,
#but it gives code to fix it if you need that.
#The book indicates that geometric means are useful for growth rates measured at equal time intervals.
#Harmonic means are used for rates given as reciprocals such as meters per second.

harmonic.mean.myfunction<-function (dataset) {
  1/(
    sum(1/dataset)/length(dataset)
    )
}

harmonic.mean.myfunction(aphid.femur)
#Yep, works too!

#The R package psych has both built in.
library(psych)
geometric.mean(aphid.femur)
harmonic.mean(aphid.femur)

#Section 4.3: the median

#The median is simple to get (using their two examples on pg. 45):
median(c(14,15,16,19,23))
median(c(14,15,16,19))

#The example on pg. 46 shows how to find the median if your data are shown
#in a frequency table (in classes) already.  If we counted to the middle class,
#we'd get 4.0 as the median.  When you use their cumulative frequency, however,
#you correctly get 3.9 as median as we do when using R's function.
median(aphid.femur)

#This section also discusses other ways to divide the data.
#Quartiles cut the data into 25% sections.  The second quartile is the median.
#You can get this with the summary function (along with minimum, maxiumum, and mean).
summary(aphid.femur)

#R has an additional function called quantile().
quantile(aphid.femur, probs=c(0.25,.5,0.75))
#It can get surprisingly complicated.  If you'll look at the help file
?quantile
#there are nine types of quantiles to calculate.
#Type 7 is the default.  The help file indicates that type 3 gives you results like you'd get from SAS.
#This website compares results for each from a sample small dataset: http://tolstoy.newcastle.edu.au/R/e17/help/att-1067/Quartiles_in_R.pdf
#Interestingly, boxplot, which graphs quartiles for R, doesn't ALWAYS return quite the same results as summary
#or any of the quantiles.
boxplot(aphid.femur)

#If you read the help file, you'll see that these different quantiles result from different estimates of
#the frequency distribution of the data.
#You can read a copy of the paper they cite here: http://www.bios.unc.edu/~mhudgens/bios/662/2008fall/hyndman1996.pdf
#Help says that type 8 is the one recommended in the paper
#(you'll also note that Hyndman is one of the authors of this function).
#The default is type 7, however.  Just to see what we get:
quantile(aphid.femur, probs=c(0.25,0.5,0.75), type=8)
#It's slightly different than our original result.
#Biometry doesn't go into any of these different estimators, and I am not currently qualified
#to suggest any, so I'd either go with one of the defaults or the one suggested by Hyndman and Fan in their paper.

#Section 4.4: the mode
#I love that the mode is named because it is the "most 'fashionable' value".
#In other words, the number that shows up the most wins here.
#The mode function in R doesn't give us the statistical mode, so we must resort to other methods.
#I found the suggestions here: http://stackoverflow.com/questions/2547402/standard-library-function-in-r-for-finding-the-mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
Mode(e2.7)

#Figure 4.1 shows how the mean, median, and mode can differ using the Canadian cattle butterfat example
#that we first used in Chapter 2, exercise 7.
e2.7<-c(4.32, 4.25, 4.82, 4.17, 4.24, 4.28, 3.91, 3.97, 4.29, 4.03, 4.71, 4.20,
        4.00, 4.42, 3.96, 4.51, 3.96, 4.09, 3.66, 3.86, 4.48, 4.15, 4.10, 4.36,
        3.89, 4.29, 4.38, 4.18, 4.02, 4.27, 4.16, 4.24, 3.74, 4.38, 3.77, 4.05,
        4.42, 4.49, 4.40, 4.05, 4.20, 4.05, 4.06, 3.56, 3.87, 3.97, 4.08, 3.94,
        4.10, 4.32, 3.66, 3.89, 4.00, 4.67, 4.70, 4.58, 4.33, 4.11, 3.97, 3.99,
        3.81, 4.24, 3.97, 4.17, 4.33, 5.00, 4.20, 3.82, 4.16, 4.60, 4.41, 3.70,
        3.88, 4.38, 4.31, 4.33, 4.81, 3.72, 3.70, 4.06, 4.23, 3.99, 3.83, 3.89,
        4.67, 4.00, 4.24, 4.07, 3.74, 4.46, 4.30, 3.58, 3.93, 4.88, 4.20, 4.28,
        3.89, 3.98, 4.60, 3.86, 4.38, 4.58, 4.14, 4.66, 3.97, 4.22, 3.47, 3.92,
        4.91, 3.95, 4.38, 4.12, 4.52, 4.35, 3.91, 4.10, 4.09, 4.09, 4.34, 4.09)

hist(e2.7,
     breaks=seq(from=3.45, to=5.05, length.out=17),
     #To match the figure in the book, we see the classes go around 3.5 and 5 at each end
     #hence the need to start classes at 3.45-3.55 and end at 4.95-5.05.
     #There are 16 classes (17 breaks), or "by=0.1" will do the same in place of length.out.
     main="",
     xlab="Percent butterfat",
     ylab="Frequency",
     ylim=c(0,20))

abline(v=mean(e2.7), lty="longdash")
abline(v=median(e2.7), lty="dashed")
abline(v=Mode(e2.7), lty="dotdash")

#Section 4.5: sample statistics and parameters
#This section is here to remind you that we are calculating sample statistics
#as estimates of population parameters.  The number we get for mean, 
#for example, is not the actual population mean.  It is not the actual population
#mean unless we measured every single individual in the population.

#published above here

#Section 4.6: the range
#This section begins our study of dispersion and spread.
#The range is the difference between the largest and smallest items in a sample,
#so it should be pretty easy to calculate.
max(aphid.femur)-min(aphid.femur) #bottom of pg. 49
max(birthweights$classmark)-min(birthweights$classmark) #pg. 50

#R has a function for this too which gives the minimum and maximum values (not the difference between them).
#If your data contain any NA ("not available") or NaN ("not a number"), and na.rm=FALSE,
#then NA values will be returned, so go ahead and set na.rm=TRUE if you need want to see min and max
#AFTER ignoring your NA/NaN values.

range(aphid.femur)


#Section 4.7: the standard deviation
#The standard deviation measures a distance from the center of the distribution.
#Table 4.1 calculates the basics using our aphid data.

start work here

#On pg. 54 the authors suggest using the midrange to estimate where your mean should be.
#You can use this to see if your mean calculations look correct ("detect gross errors in computation").
#Just average the largest and smallest values.

mean(range(aphid.femur))

#To estimate the standard deviation, you should be able to use their little table on pg. 54.
#There are 25 aphid.femur samples.  This is closest to 30, so divide the range by 4.
(max(aphid.femur)-min(aphid.femur))/4
#They also mention Statistical Table I as a source of mean ranges for various sample sizes with a normal distribution.
#The statistical tables with Biometry come as a separate book.
#I don't have it as of this post, but I've ordered it and should get it in a week or three.
#http://www.amazon.com/Statistical-Tables-F-James-Rohlf/dp/1429240318/ref=sr_1_1?ie=UTF8&qid=1404660488&sr=8-1&keywords=biometry+statistical+tables


#Section 4.8: coding data before computation
#I can't really think of many applications for why you'd use coded averaging with a computer,
#but let's try it anyway.  This box is also on pg. 43 with the non-coding application of averaging.

#The original coding to 0-14 iS made by subtracting the lowest class mark
#and then dividing (by 8, in this case) so that the series is 0-n (pg. 55).

coded.classmarks<-seq(from=0, to=14, by=1)
#We still use the frequencies and sample size as before.
coded.classsums<-coded.classmarks*frequencies
(coded.summing<-sum(coded.classsums))
#Sure enough, we get 59629.
#Divide by sample size and get the coded average of 6.300.
(coded.mean.birthweights<-coded.summing/samplesize)
#(It's 6.299947 but apparently the book was the one to round.)

#Box 4.2 showed how to calculate mean and dispersion from frequency distributions,
#both coded and uncoded.
#Continue on to coded standard deviation and variance.
(SS.coded<-sum(frequencies*(coded.classmarks-(coded.summing/samplesize))^2)) #sum of squares
(variance.box4.2<-SS.coded/(sum(frequencies)-1)) #variance coded
(coded.sd<-sqrt(variance.box4.2)) #standard deviation coded

#To decode, refer to Appendix A.2 which discusses multiplicative,
#additive, and combination properties of codes for means and dispersion.
#In the box 4.2 example, to decode the mean, you just reverse the coding
#using algebra.
(decoded.mean<-8*coded.mean.birthweights+59.5)
#The sum of squares, standard deviation, and variance follow slightly different rules, 
#per the proofs (pg. 871).  Simple additive coding would not require any changes.
#Here we need to divide by the factor we coded with (1/8) (remember, additive is ignored).
#(Dividing by 1/8 equals multiplying by 8.)
(decoded.sd<-8*coded.sd)

#Section 4.9: the coefficient of variation
#The coefficient of variation is the standard deviation as a percentage of the mean.
#This allows comparision of variation in different populations
#which might have larger or smaller absolute values of measurements.

(coefficient.variation.aphid<-sd.aphid*100/mean(aphid.femur))

#As for variance and standard deviation, there is a bias correction to use with small samples.

(corrected.cv.aphid<-coefficient.variation.aphid*(1+(1/(4*(length(aphid.femur))))))
#If you use Statistical Table II and its correction factor to calculate a standard deviation,
#as discussed on the lower part of pg. 53,
#then do not use the corrected coefficient of variation.
#If you look at Statistical table II, you'll see that the correction factor for n>30 
#is similar to the correction factor in equation 4.10.

#Exercises 4

#Exercise 4.3
#Use the data to get mean, standard deviation, and coefficient of variation,
#then repeat using groupings.
e2.7<-c(4.32, 4.25, 4.82, 4.17, 4.24, 4.28, 3.91, 3.97, 4.29, 4.03, 4.71, 4.20,
        4.00, 4.42, 3.96, 4.51, 3.96, 4.09, 3.66, 3.86, 4.48, 4.15, 4.10, 4.36,
        3.89, 4.29, 4.38, 4.18, 4.02, 4.27, 4.16, 4.24, 3.74, 4.38, 3.77, 4.05,
        4.42, 4.49, 4.40, 4.05, 4.20, 4.05, 4.06, 3.56, 3.87, 3.97, 4.08, 3.94,
        4.10, 4.32, 3.66, 3.89, 4.00, 4.67, 4.70, 4.58, 4.33, 4.11, 3.97, 3.99,
        3.81, 4.24, 3.97, 4.17, 4.33, 5.00, 4.20, 3.82, 4.16, 4.60, 4.41, 3.70,
        3.88, 4.38, 4.31, 4.33, 4.81, 3.72, 3.70, 4.06, 4.23, 3.99, 3.83, 3.89,
        4.67, 4.00, 4.24, 4.07, 3.74, 4.46, 4.30, 3.58, 3.93, 4.88, 4.20, 4.28,
        3.89, 3.98, 4.60, 3.86, 4.38, 4.58, 4.14, 4.66, 3.97, 4.22, 3.47, 3.92,
        4.91, 3.95, 4.38, 4.12, 4.52, 4.35, 3.91, 4.10, 4.09, 4.09, 4.34, 4.09)

#a. without groupings
(e2.7.mean<-mean(e2.7))
(e2.7.sd<-sd(e2.7))
(cv.e2.7<-(e2.7.sd*100/e2.7.mean))

#b. with groupings.
#To create groupings, we use hist() but with plot=FALSE, which we did not use before.
(e2.7.hist<-hist(e2.7,
                 breaks=seq(min(e2.7), 
                            max(e2.7), 
                            length.out=11), #use 11 to get 10 groups (number of groups you want + 1)
                            #R will chose binning automatically if you do not use the breaks argument.
                 plot=FALSE))
#It comes out as a list so we need the midpoints (or "class marks" in the books' terminology)
#and the counts as a data frame. 
#We also multiply the two to get class sums as in section 4.1 (and box 4.2)
(e2.7.grouped<-data.frame("frequencies"=e2.7.hist[[2]],
                         "classmark"=e2.7.hist[[4]],
                          "classsums"=e2.7.hist[[2]]*e2.7.hist[[4]]))

(e2.7.samplesize<-sum(e2.7.grouped$frequencies))
(e.2.7.summing<-sum(e2.7.grouped$classmark*e2.7.grouped$frequencies))
(mean.moo<-e.2.7.summing/e2.7.samplesize)
#I did not use the coded class marks but you can see how to do that in a previous post [link].


#Exercise 4.4
#Test out effects of addition and multiplication.

(e2.7.mean<-mean(e2.7+5.2))
(e2.7.sd<-sd(e2.7+5.2))
(cv.e2.7<-(sd(e2.7+5.2)*100)/(mean(e2.7+5.2)))
median(e2.7+5.2)-5.2
median(e2.7)
Mode(e2.7+5.2)-5.2
