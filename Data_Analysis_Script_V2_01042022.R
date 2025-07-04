#Script to analyze dissertation data post committee review
#01/02/22

#Dr. Kochel believes the model is misspecified because it is broken into four different
#regression models.  She believes all the institutional theory dimensions should be 
#included in one model as independent variables.  She also believes that the control variables
#should have more research support as justification for using them.  

#Dr. Kimball added that similar comments and said to document the scales used to create the 
#independent variables and how they correlate.  

#libraries
library(tidyverse)
library(datapasta)
library(corrplot)
library(car)
library(ggfortify)
library(broom)
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(stargazer)
library(effects)
library(oddsratio)
library(ggResidpanel)
library(gvlma)
library(MASS)
library(LogisticDx)
library(scales)
library(testthat)
library(ggstatsplot)
setwd("~/OneDrive/Dissertation/Surveys and IRB Forms")

#removing scientfic notation
options(scipen = 99)


#Reading in data################################################################

ind_data <-read.csv("ind_data.csv", header=TRUE)
log_data <-read.csv("regression_data_logistic.csv", header = T)
reg_data <-read.csv("regression_data.csv", header = T)


#Response Rates###############################################
#creating a box plot for comparing responding versus non-responding departments, faceted on CALEA.
log_data %>%
  dplyr::select(Response, CALEA, Total.Officers, Pop, violent.crime.rate) %>%
  ggplot(aes(Response, Total.Officers)) +
  geom_boxplot() +
  facet_wrap(~CALEA) +
  labs(title = "Distribution of Independent Variable Values", x = "Variable", y = "Score") +
  theme(axis.text.x = element_text(size = 13), 
        axis.text.y = element_text(size = 13), 
        axis.title.y = element_text(size = 13),
        axis.title.x = element_text(size = 13))

#first need to select only the IV variables and then convert the table into a long dataframe.  Doing this
#enables you to create a graphic will all three variables on the same plot on one scale
long_log <-log_data %>%
  dplyr::select(Response, Total.Officers, Pop, violent.crime.rate) %>%
  pivot_longer(cols = c("Total.Officers", "Pop", "violent.crime.rate"),  names_to = "Dimension", values_to = "Count")

long_log %>%
  ggplot(aes(Dimension, Count)) +
  geom_boxplot() +
  facet_wrap(~Response) +
  labs(title = "Distribution of Independent Variable Values", x = "Variable", y = "Score") +
  theme(axis.text.x = element_text(size = 13), 
        axis.text.y = element_text(size = 13), 
        axis.title.y = element_text(size = 13),
        axis.title.x = element_text(size = 13))


#DV#Work###################################################################

#reading in the survey responses
dv_data <-read_csv("DV_Responses_04_10_22.csv")

#table for dv variables
reg_data %>%
  group_by() %>%
  summarise(across(everything(), list(mean)))

reg_data %>%
  ggplot(aes(x = "", y = Dep_Total)) +
  geom_boxplot(outlier.shape = 1) +
  geom_jitter(width = .02) +
  labs(title = "Distribution of Reform Score", x = "Score", y = "Department Reform Score") +
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_text(size = 22), 
        axis.title.y = element_text(size = 22),
        axis.title.x = element_text(size = 22),
        plot.title=element_text(size=24))

ggsave("dept_tot.png", plot = last_plot(), dpi=150)
mean(reg_data$Dep_Total, na.rm = T)



#IV work####################################################################  
indtable <-ind_data %>%
  group_by() %>%
  summarise(across(everything(), list(mean)))

#table with the mean of every item making the independent variables
indtable


#writing the created table as a csv
write.csv(indtable, "indtable.csv")

#Creating a table with all IV values
library(vtable) #https://cran.r-project.org/web/packages/vtable/vignettes/sumtable.html

st(reg_data, vars = c('Coercive','Mimetic', 'Normative'), 
   title = "Summary Statistics for Independent Variables", out = "csv", file = "st.csv")

#creating box plots for the distributions of the independent variables.

#first need to select only the IV variables and then convert the table into a long dataframe.  Doing this
#enables you to create a graphic will all three variables on the same plot on one scale
data_long <-reg_data %>%
  dplyr::select(Normative, Mimetic, Coercive) %>%
  pivot_longer(cols = c("Normative", "Coercive", "Mimetic"),  names_to = "Dimension", values_to = "Count")

#The plot with the help of https://waterdata.usgs.gov/blog/boxplots/ and
library(dataRetrieval)
library(cowplot)

data_long %>%
  ggplot(aes(Dimension, Count)) +
  geom_boxplot() +
  geom_jitter(width = .02) +
  labs(title = "Distribution of Independent Variable Values", x = "Variable", y = "Score") +
  theme(axis.text.x = element_text(size = 13), 
        axis.text.y = element_text(size = 13), 
        axis.title.y = element_text(size = 13),
        axis.title.x = element_text(size = 13))



#creating a table of descriptive statistics as recommended by Dr. Kimball
library(vtable) #https://cran.r-project.org/web/packages/vtable/vignettes/sumtable.html

st(reg_data, vars = c('Coercive','Mimetic', 'Normative'), 
   title = "Summary Statistics for Independent Variables", out = "csv", file = "st.csv")

#examining correlations between IVs as recommended by Dr. Kimball
library(corrplot)
library(corrr)
data_long

data_short <-reg_data %>%
  dplyr::select(Normative, Mimetic, Coercive) %>%
  slice(-11)

M <-cor(data_short)
#https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html
corrplot(M, method = 'number',
         tl.col = 'black', tl.srt = 90, tl.cex = 1) # colorful number



#determining the groupings for IVs analytically -----------NOT USED 
#Using the factanal method from base R.  See Crawley page 813
factanal(ind_data, 3) #This creates a matrix with the correlations among the variables and factors

#creating plots of the factors
factplot <-factanal(ind_data, 3)
windows(7,7)
par(mfrow=c(2,2))
plot(loadings(factplot)[,1], loadings(factplot)[,2], pch=16, xlab="Factor 1", ylab="Factor 2", col = "blue")

plot(loadings(factplot)[,1], loadings(factplot)[,3], pch=16, xlab="Factor 1", ylab="Factor 3", col = "red")


###############################################################################
#Regression
reg_data <-read.csv("regression_data.csv", header = T)


##############################################################################################
#New Combined Model 01/05/2022

#regression model used in the post committee review manuscript.  The model uses the theory dimensions rather than
#individual items making the dimensions.  
model2022 <-lm(Dep_Total ~ Normative + Mimetic + Coercive + violent.crime.rate + Total.Officers + Median.Income,
               data = reg_data)

summary(model2022)
plot(model2022)

model2022.b <-lm(Dep_Total ~ Normative + Mimetic + Coercive,
               data = reg_data)

summary(model2022.b)
plot(model2022.b)

#use the stargazer package with the out file as a doc.  

stargazer(model2022, out = "model2022.doc")
stargazer(model2022, type = "html",out = "model2022.doc", single.row = T,
          title = "Table XX: Model 1 Results",
          covariate.labels = c("Normative Score", "Mimetic Score", "Coercive Score", "Violent Crime Rate", "Total Officers",
                               "Median Income", "Constant"),
          dep.var.labels   = "Reform Score", report=('vc*p'))



#Assumptions to check
#The gvlma function will test all the assumptions and return a very clear result
#https://rforpoliticalscience.com/2020/10/29/check-linear-regression-assumptions-with-gvlma-package-in-r/

gvlma(model2022)

#creating an object for the checks
gtest <-gvlma(model2022)
gtest


#plotting the gvlma results
plot.gvlma(gtest)
#1. Testing for outliers
outlierTest(model2022) 
#https://stats.stackexchange.com/questions/288910/outlier-detection-using-outliertest-function/289065

#2. Testing for influential observations
# Influential Observations
# Cook's D plot
# identify D values > 4/(n-k-1) 
cutoff <- 4/((nrow(reg_data)-length(model2022$coefficients)-2)) 
plot(model2022, which=4, cook.levels=cutoff)

car::influencePlot(model2022)

#3. Testing normality
sresid <- studres(model2022) 
hist(sresid, freq=FALSE, 
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit)

#4. Testing linearity
# Evaluate Nonlinearity
# component + residual plot aka partial-residual plots
crPlots(model2022)

#5. Testing for homoscedasticity using the non-constant error variance test
ncvTest(model2022)

#using the Breush-Pagan Test
lmtest::bptest(model2022)


#6. Testing for multicollinearity.  Testing using the variance inflation factor test
#Rule of thumb is that if a value for predictor exceeds 5, a problem may exist.  
car::vif(model2022) # no values above 5

#7. Testing for independence with Durbin Watson Test.  The p-value should be greater than .05
#which means the null hypothesis of no relationship between the errors are supported.
durbinWatsonTest(model2022) #p=.7626

##############PLOTS#OF#MODEL#######################################################

#plots for the regression model using jtools from Jacob Long, https://cran.r-project.org/web/packages/jtools/vignettes/summ.html
library(jtools)
effect_plot(model2022, pred=Mimetic, interval=T, plot.points=F,y.label = "Department Reform Score", main.title =  "Effect of Normative Influence on Reform Score")

plot_summs(model2022, scale = TRUE)

plot_summs(model2022, scale = TRUE, plot.distributions = TRUE,  main.title = "Strength of Model Coefficients", 
           coefs = c("Median Income", "Total Officers", "Violent Crime Rate", "Coercive Dimension", "Mimetic Dimension", "Normative Dimension"))

plot_coefs(model2022)

#using ggplot https://mran.microsoft.com/snapshot/2016-06-20/web/packages/GGally/vignettes/ggcoef.html
#https://ggobi.github.io/ggally/articles/ggcoef_model.html

library(GGally)
ggcoef(model2022, expotentiate = TRUE, errorbar_color = "blue", errorbar_height = .25, exclude_intercept = T)

ggcoef_model(model2022, variable_labels = c(Normative = "Normative Dimension", Mimetic = "Mimetic Dimension", Coercive = "Coercive Dimension",
                                            violent.crime.rate = "Violent Crime Rate", Total.Officers = "Total Officers", Median.Income = "Median Income")) +
  xlab("Coefficients") +
  ggtitle("Strength of Model Coefficients") +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "right")


ggstatsplot::ggcoefstats(model2022)

#hypothesis 1#########################################################################################
#1.	Departments with greater normative influence will enact more change.


#hypothesis 2#########################################################################################
#1.	Departments with greater mimetic influence will enact more change.

#hypothesis 3######################################################################################################
#3.	Departments with greater coercive influence will enact more change

#extra regression #############################################################################################
#Accredited departments will be more likely to respond to the survey.

##The base case for binary variables is what comes first in the alphabet.
log_data <-read.csv("regression_data_logistic.csv", header = T)

model_log <- glm(Response ~ CALEA + violent.crime.rate + Median.Income + Total.Officers,family=binomial(link='logit'),data=log_data)

summary(model_log)
plot(model_log)


table(log_data$CALEA)
table(log_data$Response, log_data$CALEA)
1-pchisq(70.524, 50)


stargazer(model_log, type = "html",out = "model4.doc", 
          title = "Figure XX: Model 4 Results",
          covariate.labels = c("CALEA", "Violent Crime Rate", "Total Officers",
                               "Median Income", "Constant"),
          dep.var.labels   = "Response to Survey", report=('vc*p'))
#plots
#plot logistic regression curve
ggplot(log_data, aes(x=CALEA, y=Response)) + 
  geom_point(alpha=.5) +
  stat_smooth(method="glm", se=FALSE, fullrange = TRUE, method.args = list(family=binomial)) +
  

plot(allEffects(model_log))




#Diagnostics https://www.youtube.com/watch?v=nmB1XG18Xys
#Because we are looking at a binomial distribution, we dont need worry about normality of errors ro the contanst variance.  
#We can also not check the independence of the values statistically, we must rely on the research design.  
#We are modeling the log of odds.  We are assuming that the values of x are linearly related to the outcome of Y.  We need to check this.  

#VIF test for multicollinearity
car::vif(model_log) # no values above 5

#checking linearity for each independent variable.  All are linear
ggplot(log_data, aes(x=CALEA, y=Response)) + 
  geom_point(alpha=.5) +
  stat_smooth(method="glm", se = TRUE, method.args = list(family=binomial))


ggplot(log_data, aes(x=violent.crime.rate, y=Response)) + 
  geom_point(alpha=.5) +
  stat_smooth(method="glm", se = TRUE, method.args = list(family=binomial))

ggplot(log_data, aes(x=Median.Income, y=Response)) + 
  geom_point(alpha=.5) +
  stat_smooth(method="glm", se = TRUE, method.args = list(family=binomial))

ggplot(log_data, aes(x=Total.Officers, y=Response)) + 
  geom_point(alpha=.5) +
  stat_smooth(method="glm", se = TRUE, method.args = list(family=binomial))

#Checking Influential Values with Cook's distance
plot(cooks.distance(glm(Response ~ CALEA + violent.crime.rate + Median.Income + Total.Officers,family=binomial(link='logit'),data=log_data)))
#There is one significant outlier
# Extract model results
model_log.data <- augment(model_log) %>% 
  mutate(index = 1:n())
# This will identify the top 3 outliers which are 7,17, and 35
model_log.data %>% top_n(3, .cooksd)

#showing the standardized residuals.
ggplot(model_log.data, aes(index, .std.resid)) + 
  geom_point(aes(color = Response), alpha = .5) +
  theme_bw()

#removing the outlier with the slice function and running the model again
log_data2 <-log_data %>%
  slice(-7,-17,-35)

#now the revised model.  The model is actually stronger with the observations. 
model4b <- glm(Response ~ CALEA + violent.crime.rate + Median.Income + Total.Officers,family=binomial(link='logit'),data=log_data2)

summary(model4b)

#VIF test for multicollinearity
car::vif(model4b) # no values above 5

#checking linearity for each independent variable.  All are linear
ggplot(log_data2, aes(x=CALEA, y=Response)) + 
  geom_point(alpha=.5) +
  stat_smooth(method="glm", se = TRUE, method.args = list(family=binomial))


ggplot(log_data2, aes(x=violent.crime.rate, y=Response)) + 
  geom_point(alpha=.5) +
  stat_smooth(method="glm", se = TRUE, method.args = list(family=binomial))

ggplot(log_data2, aes(x=Median.Income, y=Response)) + 
  geom_point(alpha=.5) +
  stat_smooth(method="glm", se = TRUE, method.args = list(family=binomial))

ggplot(log_data2, aes(x=Total.Officers, y=Response)) + 
  geom_point(alpha=.5) +
  stat_smooth(method="glm", se = TRUE, method.args = list(family=binomial))

autoplot(model_logb)

stargazer(model_logb, type = "html",out = "model_logb.doc", 
          title = "Figure XX: Model 4 Results",
          covariate.labels = c("CALEA", "Violent Crime Rate", "Total Officers",
                               "Median Income", "Constant"),
          dep.var.labels   = "Response to Survey", report=('vc*p'))

ggplot(log_data2, aes(x=CALEA, y=Response)) + 
  geom_point(alpha=.5) +
  stat_smooth(method="glm", se=FALSE, method.args = list(family=binomial))
##############################


# Hypothesis 4 descriptors
#means for different variables of responding versus non responding departments.  
log_data %>%
  group_by(Response) %>%
  summarize(mean(violent.crime.rate, na.rm = T))

log_data %>%
  group_by(Response) %>%
  summarize(mean(Median.Income, na.rm = T))

log_data %>%
  group_by(Response) %>%
  summarize(mean(Total.Officers, na.rm = T))



#################################################################################
#Looking at TSR Data
library(googlesheets4)
TSR <-read_sheet("https://docs.google.com/spreadsheets/d/1UarpOK6g4b6yItg2rZ7RnDC7kIACbMBgLG5Cehjb1ZU/edit#gid=783367926")
#looking at the aggregate number of stops 2013 to 20216
tplot <-TSR %>%
ggplot(aes(Agency)) +
  geom_bar(aes(y=Total_Stop13), stat = "identity") +
  geom_bar(aes(y=Total_Stop19), stat = "identity") 






#creating a diverging dot plot showing the percent change from 2013 to 2019
ggplot(TSR, aes(x= reorder(Agency, change13_19), y= change13_19, label=change13_19)) +
  geom_bar(stat='identity', size=5, fill = "dodger blue" ) +
  #geom_text(aes(label=round(change13_19, digits=2), hjust = 0)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title="Percentage Change of Traffic Stops 2013-2019", 
       subtitle="St. Louis County Municipal Police Departments", caption = "Source: Missouri Attorney General", 
       x = "Police Department", y = "Percent Change") + 
  coord_flip()


#plot of percent change for black drivers
ggplot(TSR, aes(x= reorder(Agency, changeblack13_19), y= changeblack13_19, label=changeblack13_19)) +
  geom_bar(stat='identity', size=5, fill = "dodger blue" ) +
  #geom_text(aes(label=round(change13_19, digits=2), hjust = 0)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title="Percentage Change of Traffic Stops 2013-2019", 
       subtitle="St. Louis County Municipal Police Departments", caption = "Source: Missouri Attorney General", 
       x = "Police Department", y = "Percent Change") + 
  coord_flip()


ggplot(TSR, aes(x= reorder(Agency, change_white), y= change_white, label=change_white)) +
  geom_bar(stat='identity', size=5, fill = "dodger blue" ) +
  #geom_text(aes(label=round(change13_19, digits=2), hjust = 0)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title="Percentage Change of Traffic Stops 2013-2019", 
       subtitle="St. Louis County Municipal Police Departments", caption = "Source: Missouri Attorney General", 
       x = "Police Department", y = "Percent Change") + 
  coord_flip()



ggplot(TSR, aes(x= reorder(Agency, changeblack13_19), y= changeblack13_19, label=changeblack13_19)) +
  geom_bar(stat='identity', size=5, fill = "dodger blue" ) +
  #geom_text(aes(label=round(change13_19, digits=2), hjust = 0)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title="Percentage Change of Traffic Stops 2013-2019", 
       subtitle="St. Louis County Municipal Police Departments", caption = "Source: Missouri Attorney General", 
       x = "Police Department", y = "Percent Change") + 
  coord_flip()
#need a plot of the change for black and white on the same chart

