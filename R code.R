install.packages("AER")
install.packages("COUNT")

library("AER")
data(PhDPublications)
head(PhDPublications)
str(PhDPublications)
summary(PhDPublications)

PhDPublications$articles <- as.numeric(PhDPublications$articles)
PhDPublications$kids <- as.numeric(PhDPublications$kids)
PhDPublications$mentor <- as.numeric(PhDPublications$mentor)

str(PhDPublications)


####Question 1 
####Build Unregularized Model (poisson)

phd_glm <- glm(articles~.,data = PhDPublications, family = "poisson")
summary(phd_glm)

#phd_glm$df.residual
#deviance(phd_glm)
#str(phd_glm)

####Question 2

ration_modeldeviance_dfresidual <- function(x) {
  y <- deviance(x)/x$df.residual
  return(sum(y))
}


####Question 3

ration_modeldeviance_dfresidual(phd_glm)

# The ratio of the residual deviance of the model to the number of degrees of  freedom is much larger than 1
# So it can be said that the poisson model exhibits overdispersion

####Question 4

install.packages("qcc")
library("qcc")
qcc.overdispersion.test(PhDPublications$articles , type="poisson")

dispersiontest(phd_glm , trafo = 1)
dispersiontest(phd_glm )

####Question 5

# The output from the function "overdispersion.test" in the "qcc" package
# clearly shows that there is evidence of overdispersion as the p value is equal to 0.
# So we reject the null hypothesis which assumed that that there is no overdispersion in the model

####Question 6
####Build Unregularized Model (Quasi-Poisson)

phd_glm2 <- glm(articles~.,data = PhDPublications, family = "quasipoisson")
summary(phd_glm2)


####Question 7

# The output of the quasipoisson model looks quite the same with the poisson model but
# we can observe though some important differences. 
# In fact the estimates are exactly the same as before, but the standard errors are about 36% larger. which makes some of the input features
# not statistically significant, although they were significant in the first model (poisson)
# The coefficient "married" is not significant in the second model (quassipoisson) as the p value is slightly greater than 0.05. 
# In addition the p value of the coefficient "gender"  is also larger than the first model but it can still be considered as statistically significant for the model.
# Finally, it can be observed that the dispersion parameter, which was forced to be 1 in our first model, for the quasipoisson model is estimated at 1.829006

# we can verify this using the following commands

se <- function(model) sqrt(diag(vcov(model)))

round(data.frame(poisson=coef(phd_glm), quasipoisson=coef(phd_glm2),se.poisson=se(phd_glm),
                 se.quasipoisson=se(phd_glm2), ratio=se(phd_glm)/se(phd_glm2)), 4)


####Question 8


x1 <- runif(300)
x2 <- runif(300)
x3 <- runif(300)

ysum <- 2*x1 +5.7*x2 - 7.8*x3
y    <- rpois(300 , lambda = exp(ysum) ) 

my_df <- data.frame(x1,x2,x3,y)

my_df_glm  <- glm(y~.,data = my_df, family = "poisson")
my_df_glm2 <- glm(y~1 +x2 +x3,data = my_df, family = "poisson")

ration_modeldeviance_dfresidual(my_df_glm)
ration_modeldeviance_dfresidual(my_df_glm2)


####Question 9

install.packages("MASS")
library("MASS")

phd_glm_nb <- glm.nb(articles~.,data = PhDPublications)
summary(phd_glm_nb)
####Question 10

AIC(phd_glm)
AIC(phd_glm_nb)


#The Negative Binomial model seems more appropriate as the Akaike's Information Criterion is smaller.

####Question 11

# As a second criterion i am going to use the BIC criterion to choose among the 2 models
# The following command will create a dataframe with both AIC and BIC for the 2 models just to make it easier to compare all the values

ic <- data.frame(Model = c("poisson", "negative-binomial"),
                 AIC = c(AIC(phd_glm), AIC(phd_glm_nb)),
                 BIC = c(BIC(phd_glm), BIC(phd_glm_nb)),
                 stringsAsFactors = FALSE)
ic

#Both AIC (as it was tested in question 10) and BIc suggest that the negative binomial model should be selected

qchisq(0.95, df.residual(phd_glm))
deviance(phd_glm)
pr <- residuals(phd_glm,"pearson")
sum(pr^2)

#estimate the scale parameter ö 
phi <- sum(pr^2)/df.residual(phd_glm)


#Another way to test which model is more appropriate is to plot the mean-variance relationship of the two models
# The poisson and the negative binomial model have different variance functions
# The following commands will create groups based on the linear predictor, compute the mean and variance for each group


xb <- predict(phd_glm_nb)
g <- cut(xb, breaks=quantile(xb,seq(0,100,5)/100))
m <- tapply(PhDPublications$articles, g, mean)
v <- tapply(PhDPublications$articles, g, var)
png("c4afig1.png",width=500,height=400)
plot(m, v, xlab="Mean", ylab="Variance", main="Mean-Variance Relationship")

x <- seq(0.63,3.37,0.02)
lines(x, x*phi, lty="dashed")
lines(x, x*(1+x/phd_glm_nb$theta))
legend("topleft", lty=c("dashed","solid"), legend=c("Q. Poisson","Neg. Binom."), inset=0.05)

dev.off();


# It's variance function is quadratic and it does a better job on the "end"
#So the conclusion is that the negative binomial model provides a beter description of the data than the over-dispersed poisson model


