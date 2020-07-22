#setwd("~/Desktop/0006 ICA/ICA3")
#import the data
#preprocessing the data
#################################################################################################################
View(icadata)
icadata$four_regions <- factor(icadata$four_regions, levels = c("Africa", 
                                                                  "Asia", "Americas", "Europe"))
summary(worldbankregion)
icadata$worldbankregion <- factor(icadata$worldbankregion, 
      levels = c("Sub-Saharan Africa", "East Asia & Pacific", "South Asia", "Latin America & Caribbean",
                     "Middle East & North Africa", "Europe & Central Asia", "North America"))

lifeexp2018 <- icadata$lifeexp2018
agri_2016 <- icadata$agri_2016
country <- icadata$country
broadband_2016 <- icadata$broadband_2016
mobile_2016 <- icadata$mobile_2016
child_mort_2018 <- icadata$child_mort_2018
child_per_woman_2018 <- icadata$child_per_woman_2018
income_pp_2018 <- icadata$income_pp_2018
inflation_2017 <- icadata$inflation_2017
internet_2016 <- icadata$internet_2016
self_employed_2018 <- icadata$self_employed_2018
sl_emp_2017 <- icadata$sl_emp_2017
urban_pop_2017 <- icadata$urban_pop_2017
pop_dens_2018 <-icadata$pop_dens_2018
four_regions <- icadata$four_regions
worldbankregion <- icadata$worldbankregion
###########################################################################################################################
#initial exploratory analysis of the data
summary(lifeexp2018)
order(icadata$lifeexp2018)#lowest at observation 94: Lesotho and highest at observation 84:Japan
summary(internet_2016)
summary(inflation_2017) #lowest at observation 183: Venesuela
summary(pop_dens_2018) #highest at observation 146: Singapore
summary(urban_pop_2017)
summary(agri_2016)
summary(broadband_2016)
summary(worldbankregion)
summary(four_regions)
boxplot(lifeexp2018 ~ four_regions) #in ascending order
boxplot(lifeexp2018 ~ worldbankregion)#in ascending order
plot(agri_2016, lifeexp2018) #not very linear
plot(broadband_2016, lifeexp2018) #log
plot(mobile_2016, lifeexp2018)#not very linear 
plot(child_mort_2018, lifeexp2018) #linear
plot(child_per_woman_2018, lifeexp2018) #linear 
plot(income_pp_2018, lifeexp2018) #not linear but maybe linear after transformation
plot(inflation_2017, lifeexp2018) #one outlying observation at observation 183, Venesuela, very positively skewed
plot(internet_2016, lifeexp2018) #linear
plot(self_employed_2018, lifeexp2018)#linear
plot(sl_emp_2017, lifeexp2018)#linear
plot(urban_pop_2017, lifeexp2018)#linear
plot(pop_dens_2018, lifeexp2018) #one outlying observation at observation 146, Singapore, very positively skewed
plot(four_regions, lifeexp2018)
plot(worldbankregion, lifeexp2018)

#pairs(icadata[, -c(1,2,14,15,17,18,19)])
#plot(self_employed_2018, sl_emp_2017) #very much
#cor(self_employed_2018, sl_emp_2017) #0.99***************************

plot(agri_2016, internet_2016)
abline(lm(internet_2016 ~ agri_2016))
cor(agri_2016, internet_2016)#-0.75**************

plot(broadband_2016, internet_2016)
plot(sqrt(broadband_2016), internet_2016)
abline(lm(internet_2016 ~ broadband_2016))
cor(internet_2016, broadband_2016)#0.85******************



plot(child_per_woman_2018, internet_2016)
abline(lm(internet_2016 ~ child_per_woman_2018))
cor(internet_2016, child_per_woman_2018)#-0.78************

plot(child_mort_2018, internet_2016)
abline(lm(internet_2016 ~ child_mort_2018))
cor(internet_2016, child_mort_2018)#-0.80***************

plot(income_pp_2018, internet_2016)
abline(lm(internet_2016 ~ income_pp_2018))
cor(internet_2016, income_pp_2018)#0.78*************

#plot(inflation_2017, internet_2016)
#abline(lm(internet_2016 ~ inflation_2017))
#cor(internet_2016, inflation_2017)#0.02

plot(self_employed_2018, internet_2016)
abline(lm(internet_2016 ~ self_employed_2018))
cor(internet_2016, self_employed_2018)#-0.82**************

plot(sl_emp_2017, internet_2016)
abline(lm(internet_2016 ~ sl_emp_2017))
cor(internet_2016, sl_emp_2017)#0.82**************

plot(mobile_2016, internet_2016)
abline(lm(internet_2016 ~ mobile_2016))
cor(internet_2016, mobile_2016) #0.62******

plot(urban_pop_2017, internet_2016)
abline(lm(internet_2016 ~ urban_pop_2017))
cor(internet_2016, urban_pop_2017)#0.68*******

plot(child_per_woman_2018, child_mort_2018)
cor(child_mort_2018, child_per_woman_2018)

#plot(pop_dens_2018, internet_2016)
#abline(lm(internet_2016 ~ pop_dens_2018))
#cor(internet_2016, pop_dens_2018)#0.14


lm_0 <- lm(lifeexp2018 ~ agri_2016 + broadband_2016 + mobile_2016 + child_mort_2018 + child_per_woman_2018 +
             income_pp_2018 + inflation_2017 + internet_2016 + self_employed_2018 + sl_emp_2017 + urban_pop_2017 +
             four_regions + pop_dens_2018 + worldbankregion)

lm_1 <- lm(lifeexp2018 ~ broadband_2016 + child_mort_2018 + income_pp_2018 + sl_emp_2017 + mobile_2016
           + urban_pop_2017 + worldbankregion )

lm_2 <- lm(lifeexp2018 ~ broadband_2016 + child_mort_2018
              + income_pp_2018 + urban_pop_2017 + worldbankregion)

lm_3 <- lm(lifeexp2018 ~ broadband_2016 + child_mort_2018+ income_pp_2018 + 
             urban_pop_2017 + worldbankregion)

lm(lifeexp2018 ~ mobile_2016 + child_mort_2018
   + income_pp_2018 + urban_pop_2017 + worldbankregion)

summary(lm(internet_2016 ~ agri_2016 + broadband_2016 + mobile_2016 + child_mort_2018 +
     income_pp_2018 + sl_emp_2017 + urban_pop_2017))


anova(lm_1, lm_2)

summary(lm_1)
summary(lm_2)
summary(lm_3)



#model checking#########################################################################################################
#check residual plot

plot(lm_f) #1, 168, 94
summary(influence.measures(lm_f))
outliers <- icadata[c(1,94,168),c(1,2,4,6,8,12,13,15)]
View(icadata[c(94,1, 168),])
View(outliers)
summary(lifeexp2018)
summary(internet_2016)
summary(urban_pop_2017)

summary(lm(lifeexp2018 ~ I(sqrt(broadband_2016)) + (child_mort_2018)+ I(log(income_pp_2018)) + 
             urban_pop_2017 + worldbankregion, data = icadata[-c(1,168,94),]))

coplot(lifeexp2018 ~ mobile_2016 | worldbankregion, data = data)
coplot(lifeexp2018 ~ child_mort_2018 | worldbankregion, data = data)
coplot(lifeexp2018 ~ income_pp_2018 | worldbankregion, data = data)
coplot(lifeexp2018 ~ sl_emp_2017 | worldbankregion, data = data)
coplot(lifeexp2018 ~ urban_pop_2017 | worldbankregion, data = data)


interaction.plot(four_regions, worldbankregion, lifeexp2018)

data <- icadata[, -c(1,14,15,19)]

lm0 <- lm(lifeexp2018 ~ agri_2016 + broadband_2016 + mobile_2016 + child_mort_2018 
          + income_pp_2018 + inflation_2017 + internet_2016 + urban_pop_2017 + sl_emp_2017
          + pop_dens_2018 + four_regions + worldbankregion)
lm1

lm_nt <- lm(lifeexp2018 ~  broadband_2016 + child_mort_2018
           + income_pp_2018 + urban_pop_2017 + worldbankregion)
rst_nt <- rstandard(lm_nt)

par(mfrow=c(1,2))
plot(broadband_2016, rst_nt, main = "Standardized Residual Plot Before Transformation", xlab = "Percentage of Broadband Subscribers", ylab = "Standardized Residuals")
plot(sqrt(broadband_2016), rst, main = "Standardized Residual Plot After Transformation", xlab = "Square Root of Percentage of Broadband Subscribers", ylab = "Standardized Residuals")

plot(child_mort_2018, rst_nt, main = "Standardized Residual Plot Before Transformation", xlab = "Child Mortality Rate in 2018", ylab = "Standardized Residuals")
plot(log(child_mort_2018), rst, main = "Standardized Residual Plot Before Transformation", xlab = "Logarithm of Child Mortality Rate in 2018", ylab = "Standardized Residuals")

plot(income_pp_2018, rst_nt, main = "Standardized Residual Plot Before Transformation", xlab = "Income Per Person in 2018", ylab = "Standardized Residuals")
plot(log(income_pp_2018), rst, main = "Standardized Residual Plot Before Transformation", xlab = "Logarithm of Income Per Person in 2018", ylab = "Standardized Residuals")

lm_f <- lm(lifeexp2018 ~ I(sqrt(broadband_2016)) + (child_mort_2018)+ I(log(income_pp_2018)) + 
     urban_pop_2017 + worldbankregion)
plot(log(child_mort_2018), rstandard(lm_f))
plot(log(child_mort_2018), fitted.values(lm_f))

rst <- rstandard(lm_f)

summary(lm_f) #limitation p-value of sqrtbroadband big

plot(fitted(lm_f), (rst))

qqnorm(rst)
qqline(rst)

plot(rst)

