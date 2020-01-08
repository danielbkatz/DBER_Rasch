### The (Unidimensional) Rasch Model####

## set your working directory to the folder where you 
##downloaded the CSV file##

# 1.Go to Session -> Set Working Directory -> Choose Directory

# 2. install TAM and the WrightMap package


#install.packages("TAM")
#install.packages("WrightMap")

#Now we call the TAM library you installed in a prior step. 
#This tells R to use the set of functions in `TAM`
library(TAM)

#also load the WrightMap package
library(WrightMap)


### Read in the Data ###
# Take a the CSV from outside of R and read it in. 
# This means that it is something you can now work with in R. 
# The .csv file will be read in as a data frame or (dataframe). 
# This is a type of object in R, that's essentially a spreadsheet that 
# your're used to working with. 

setwd("~/DBER_Rasch")
hls <- read.csv("data/hls_dic_scale.csv")


## See the first few rows and columns
head(hls)

#If you want to see view the data frame:

View(hls)


### Run the Rasch Model ####

# This command runs a Rasch model on the selected data frame. 
# `mod1` is an object in R that holds the data from our Rasch model 
# (along with a lot of other information). 
# This is the main computation step.
# Now we just ask TAM questions about this model.

#Note that the object `hls` has to contain only items and no other 
#information. 

mod1 <- tam(hls)
summary(mod1)




### Item Difficulties #####

#So how difficult were those items? let’s ask TAM.
#We'll extract difficulties (`xsi`) from the `mod1` object
# We'll access this via `indexing`. 

#The `$` sign means, access `mod1` and extract the object `xsi` 
#The command `mod1$xsi$xsi` accesses just the column `xsi`

#Assign those values to a column in the environment called `ItemDiff` 
#using `<-`

mod1$xsi
ItemDiff <- mod1$xsi$xsi 
ItemDiff

## Visualize
#We may want to visualize or describe the distribution of item difficulties 
#(if you want to play with binwidth, you can).

#Get Item Characteristic Curves (ICC)
#Unfortunately TAM exports objects.
# We'll turn this to false because it doesn't work with Rstudio cloud/server. 
plot(mod1, export = F)


hist(ItemDiff)
mean(ItemDiff)
sd(ItemDiff)

### Exercise 1: #### 
# 1. Which item is the hardest? The easiest? The closest to the mean? 
# **Hint**: try to use the commands such as `max()`, `min()`.


# Person Abilities ####

# Person abilities are also of interest. We can look at the person side of the model by computing person abilities. Compute person abilities using the `tam.wle` function and assign to an object called `Abil`. Extract person abilities ($\theta_p$) from `Abil` and create an object in the `environment` called `PersonAbility` which will essentially be a column vector. **Note**: You may want more information than this at times (such as standard errors) so you may not always want to subset this way.


#generates a data frame
Abil <- tam.wle(mod1)

#See the first few rows of Abil. Notice you get:
#    
#  1. `pid`: person id assigned by TAM.
#  2. `N.items`: Number of items the person was given (this becomes interesting when you have linked test forms where students may not all see the same number of items)
#  3. `PersonScores`: Number of items the student got right/selected an option for in the survey case. 
#  4. `PersonMax`: Max that person could have gotten right/selected an option for
#  5. `theta`: estimated person ability
#  6. `error`: estimated measurement error
#  7. `WLE.rel`: estimated person seperation reliability.

head(Abil)

# or

View(Abil)




# The column in the `Abil` data.frame 
# corresponding to person estimates is the `theta` column. 
# Pull out the ability estimates, theta, column if you would like, though, 
# this creates a list. This makes it a little easier for a few basic tasks 
# below.

PersonAbility <- Abil$theta

head(PersonAbility)

# Only the first 20 shown
PersonAbility

#You can export those estimated abilites to a .csv to save 
#(you can also save directly in R, if you need to).

write.csv(Abil,"HLSmod1_thetas.csv")


#You can find the CSV file in your `Working Directory`. 
#If you need help finding where your `working directory` is:

getwd()

#Descriptives for person ability

hist(PersonAbility)
mean(PersonAbility)
sd(PersonAbility)

## Wright Map ####

#To visualize the relationship between item difficulty and person ability 
#distributions, call the WrightMap package installed previously. 
#We'll generate a simple WrightMap. We'll clean it up a little bit by 
#removing some elements


IRT.WrightMap(mod1)
IRT.WrightMap(mod1, show.thr.lab=FALSE)


### Exercise: 
#  1. Are the items appropriately targeted to the ability level of the population? 
#  2. Why do you think?

## Item Fit ##
## Let's find out if the data fit the model. Use the `tam.fit` function to compute fit statistics, then display.

fit <- tam.fit(mod1)

View(fit$itemfit)


### Exercise: 
#1. Look at the `Wright Map` and the histograms of person abilities and item difficulties. 
#Do you think this instrument is well-targeted for this sample? 

#2. How might it be optimized? 

#3. Relative to other items, which item fit our model the worst?


### This concludes the planned instruction in the Rasch model for our workshop. However, we've provided working code for a few other concepts in which you might be interested, including using the Rasch model with polytomous items and with multidimensional models.



# Polytomous Items ####

## Polytymous item types (anything with a rating Scale)
#We can use the Rasch Partial Credit Model (PCM) to look at polytomous data too. We’ll start by bringing in the polytomous items from the survey. Note that TAM needs the bottom category to be coded as 0, so you may need to recode.

hls2 <- read.csv("data/hls_poly_scale.csv")

head(hls2)

View(hls2)

#TAM will automatically run the PCM when our data is polytomous. There are other model-types for polytomous data such as the rating scale model. This may be more appropriate for Likert-type items. For more information, read TAM documentation or see the reference list (Bond & Fox, 2007)


mod2 <- tam(hls2)

summary(mod2)



## Item Difficulties
# Now we'll get item and person characteristics just like before

mod2$xsi
ItemDiff2 <- mod2$xsi$xsi 
View(ItemDiff2)

#note, if you want to see this in your viewer, you can also use View().


## Person ability (theta) estimates

person.ability <- tam.wle(mod2)
head(person.ability)



## Item fit statistics
Fit.poly <- tam.fit(mod2)

Fit.poly$itemfit


## Item characteristic curves (but now as thresholds). 
# There are item characteristic curves (ICCs) for each item choice

WLEestimates.poly <- tam.mml.wle(mod2)
tthresh.poly <- tam.threshold(mod2)
# plot(mod2, type = "items")

## Wright Map
# Here’s a polytomous Wright Map

wrightMap(WLEestimates.poly, tthresh.poly)
wrightMap()

## Exercises:
# 1. Find an item for which Cat 3 is actually easier than the Cat 2 of another item. 
# 2. Find an item that has two categories that are extremely close in severity.
# 3. Look at the ICC for item 14. Describe what is happening with Cat 3.

## Model Comparison

# Say we want to compare the two models we just ran 
# (note, these aren't really comparable since it's a completely different model - 
# not nested data)

logLik(mod1)
logLik(mod2)
anova(mod1, mod2)


# Log likelihood is the foundation of both AIC and BIC. 
# AIC and BIC allow you to compare non-nested models while penalizing 
#for model complexity (BIC penalizes more). 
# In general, the model with a smaller AIC/BIC is the one that the data fit
# better. The two criteria sometimes disagree.


### Multidimensional Rasch Model ###### 

# What if we envision something that's multidimensional? 
# We can model that with TAM. In fact, this is one of TAM's great strengths. 
# Do read package documentation

## we start by assigning the items to a dimension using a 
# Q-matrix

## If we want to have two dimensions, 
## we'll create a matrix with two columns. 
# A 1 or 0 denotes whether that item belongs to dimension 1 or 2 
#(or both!) 

Q <-
  matrix(c(1,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,0,1,1,1,1,1,1,0,
           0,0,0,0,0,0,0
           ,0),
         ncol=2)
View(Q)

# or
Q



## Run the multidimensional Rasch model
multi <- TAM::tam.mml(resp=hls, Q=Q)



# person and item estimates 

persons.multi <- tam.wle(multi)
WLEestimates.multi <- persons.multi$theta
thresholds.multi <- tam.threshold(multi)

#Fit and reliabilities

Fit.multi <- tam.fit(multi)
Fit.multi$itemfit
multi$EAP.rel 

#EAP reliabilities



### Multidimensional Wright Map ####

MDthetas.multi <-
  cbind(persons.multi$theta.Dim01,persons.multi$theta.Dim02) #one line
wrightMap(MDthetas.multi, thresholds.multi) #second line


#Compare the first unidimensional model to the multidimensional one

logLik(mod1)
logLik(multi)
anova(mod1, multi)

# Alternatively, you can use `IRT.compareModels`

compare <- CDM::IRT.compareModels(mod1, multi)
compare
summary(compare)

# We see that model `multi` fits slightly better. 
# However, the log likelihood difference test shows 
# the difference is statististically significant.


compare$LRtest

## Exercises
# 1. what evidence points towards multidimensionality?
# 2. compare the multidimensional model to the PCM model
