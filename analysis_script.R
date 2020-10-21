### © Juha Kreula

# Clear all variables to avoid confusion
rm(list = ls())

### Load libraries
# For plots
if(!require(ggplot2))
  install.packages("ggplot2")
# For half-normal plots
if(!require(faraway))
  install.packages("faraway")
# For quantile residuals
if(!require(statmod))
  install.packages("statmod")
# For summaries
if(!require(plyr))
  install.packages("plyr")
# For manipulating data frames
if(!require(dplyr))
  install.packages("dplyr")

# Get current directory
curr_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(curr_dir)

# Directory for figures
fig_folder <- file.path(curr_dir,"Figures/")
if(!dir.exists(fig_folder))
  dir.create(fig_folder)

# Global scaler for fonts of plots
font_scaler = 1.5

##############################################################################
########################## DATA EXPLORATION ##################################
##############################################################################

# Load data and investigate summaries
heartd <- read.csv(file.path(curr_dir,"Data/data.csv"))
head(heartd)
str(heartd)
summary(heartd)
cor(subset(heartd,select=-c(hd,sex,educ))) # Correlations between cont. variables

# Investigate female and male heart disease frequency
heartd_male <- heartd[heartd$sex == "M",]; mean(heartd_male$hd)
heartd_female <- heartd[heartd$sex == "F",]; mean(heartd_female$hd) 

# Investigate education heart disease frequency
eduA <- heartd[heartd$educ == "A",]; mean(eduA$hd)
eduB <- heartd[heartd$educ == "B",]; mean(eduB$hd)
eduC <- heartd[heartd$educ == "C",]; mean(eduC$hd)
eduD <- heartd[heartd$educ == "D",]; mean(eduD$hd)

################### CREATE BAR PLOTS ##########################
pdf(file = paste0(fig_folder,"sex_educ_counts.pdf"),width = 9, height = 4, family = "Palatino")
par(mfrow = c(1,2))
par(mar=c(5,4.5,1,2))
# Bar plot of heart disease vs sex
barplot(table(heartd$hd,heartd$sex),
        beside=TRUE,
        col=c(rgb(1,0,0,0.3),rgb(0,0.6,0.65,0.45)),
        xlab='Sex',
        ylab="Count",
        ylim=c(0,2600),
        cex.axis = font_scaler,
        cex.lab = font_scaler,
        cex.main = font_scaler,
        cex = font_scaler)
legend("topright",
       c("No heart disease","Heart disease"),
       col=c(rgb(1,0,0,0.3),rgb(0,0.6,0.65,0.45)),
       pch=15,
       bty="n",
       pt.cex = font_scaler,
       cex = font_scaler)
box() # Add box around plot
# Bar plot of heart disease vs education level
barplot(table(heartd$hd,heartd$educ),
        beside=TRUE,
        col=c(rgb(1,0,0,0.3),rgb(0,0.6,0.65,0.45)),
        xlab='Education level',
        ylab="Count",
        ylim=c(0,1600),
        cex.axis = font_scaler,
        cex.lab = font_scaler,
        cex.main = font_scaler,
        cex = font_scaler)
legend("topright",
       c("No heart disease","Heart disease"),
       col=c(rgb(1,0,0,0.3),rgb(0,0.6,0.65,0.45)),
       pch=15,
       bty="n",
       pt.cex = font_scaler,
       cex = font_scaler)
box() # Add box around plot
dev.off()
par(mfrow = c(1,1))

################### CREATE DENSITY PLOTS ##########################

# Get mean ages for each heart disease status
ages <- ddply(heartd,"hd",summarise,ages.mean=mean(age))
# Get mean cholesterol for each heart disease status
chols <- ddply(heartd,"hd",summarise,chols.mean=mean(chol))
# Get mean sbp for each heart disease status
sbps <- ddply(heartd,"hd",summarise,sbps.mean=mean(sbp))
# Get mean bmi for each heart disease status
bmis <- ddply(heartd,"hd",summarise,bmis.mean=mean(bmi))

pdf(file = paste0(fig_folder,"hd_density_age.pdf"), # Change name of file according to variable
    width = 9, 
    height = 7, 
    family = "Palatino")
# Plot heart disease density 
ggplot(heartd, aes(age, fill = as.character(hd))) + # Change age variable
  geom_density(alpha = 0.5, position = 'identity')+
  geom_vline(data = ages, aes(xintercept=ages.mean, # Change age variable
                              color=as.character(hd)),linetype="dashed",size = 2) +
  theme_bw() +
  theme(panel.grid.major = element_line(colour = grey(0.95), size=0.5),
        panel.grid.minor = element_line(colour = grey(0.95), size=0.5),
        panel.background = element_rect(colour = "black", size=1)) +
  xlab("Age (years)")+ # Change label
  ylab("Density") +
  theme(text = element_text(size=32),
        axis.text = element_text(color="black",size=32)) +
  theme(legend.position="top") +
  scale_fill_discrete(name = "Heart disease", labels = c("No", "Yes")) +
  scale_color_discrete(name = "Heart disease", labels = c("No", "Yes"))
dev.off()

################### CREATE DECILE PLOTS ##########################
# Get deciles for age of males. Change age variable for others when needed
group_M <- group_by(heartd_male, 
                     cut(age, # Change age for other variables
                         breaks=unique(quantile(age, (0:10)/10)), 
                         include.lowest = TRUE))

# Get deciles for age of females. Change age variable for others when needed
group_F <- group_by(heartd_female, 
                     cut(age, # Change age for other variables
                         breaks=unique(quantile(age, (0:10)/10)),
                         include.lowest = TRUE))

# Get summary statistics per decile
summary_group_M <- dplyr::summarise(group_M, 
                                  hd_rate = mean(hd), 
                                  variable = mean(age), # Change age if needed
                                  count = n())
# Get summary statistics per decile
summary_group_F <- dplyr::summarise(group_F, 
                                  hd_rate = mean(hd), 
                                  variable = mean(age), # Change age if needed
                                  count = n())

# Decile plot for age. Change age for other variables when needed
plot.new()
pdf(file = paste0(fig_folder,"decile_plot_age.pdf"),width = 9, height = 7, family = "Palatino")
par(mfrow = c(1,1))
par(mar=c(5,5,2,2))
plot(hd_rate ~ variable, 
     summary_group_M,
     ylim=c(0,0.6),
     col = "black",
     pch=15,
     xlab="Age (years)",
     ylab="Proportion of heart disease",
     cex.axis = font_scaler,
     cex.lab = font_scaler,
     cex.main = font_scaler,
     cex = font_scaler)
lines(hd_rate ~ variable, 
      summary_group_M,
      ylim=c(0,0.6),
      col = "black",
      pch=15,
      cex.axis = font_scaler,
      cex.lab = font_scaler,
      cex.main = font_scaler,
      cex = font_scaler)
points(hd_rate ~ variable, 
       summary_group_F, 
       col = "gray50",
       pch=1,
       cex.axis = font_scaler,
       cex.lab = font_scaler,
       cex.main = font_scaler,
       cex = font_scaler)
lines(hd_rate ~ variable, 
      summary_group_F, 
      col = "gray50",
      pch=1,
      cex.axis = font_scaler,
      cex.lab = font_scaler,
      cex.main = font_scaler,
      cex = font_scaler)
legend("topleft",
       c("Male","Female"),
       bty="n",
       pch=c(15,1),
       col=c("black","gray50"),
       pt.cex = 1,
       cex = font_scaler)
dev.off()

##############################################################################
########################## MODEL SELECTION ###################################
##############################################################################
# Function for centralising at mean
centralise_at_mean <- function(x) {
  return(x-mean(x))
}

# Centralise all continuous variables
heartd$age <- centralise_at_mean(heartd$age)
heartd$chol <- centralise_at_mean(heartd$chol)
heartd$sbp <- centralise_at_mean(heartd$sbp)
heartd$bmi <- centralise_at_mean(heartd$bmi)

# First model with main effects and two-way interactions
lmod1 <- glm(hd ~ (sex + age + chol + educ + sbp + bmi)^2, 
             family = binomial(link = "logit"), data = heartd)
summary(lmod1)
anova(lmod1, test = "Chi")

# AIC model selection for model 1
lmod2 <- step(lmod1)
summary(lmod2)
anova(lmod2,test = "Chi")

# Compute standardised quantile residuals
set.seed(91) # Uncomment to set random seed
qres <- statmod::qresid(lmod2) / sqrt(1-hatvalues(lmod2))
# Add dev. residuals, \hat{\eta} values, and quantile residuals to data frame
heartd <- mutate(heartd, residuals=rstandard(lmod2, type = "deviance"), 
                 linpred=predict(lmod2,type="link"), qres = qres)
# Create bins
grouped_df <- group_by(heartd, 
                cut(linpred, 
                    breaks=unique(quantile(linpred, (0:180)/180)),
                    include.lowest = TRUE))
# Obtain means for each bin
grouped_df_means <- dplyr::summarise(grouped_df, 
                                     residuals=mean(residuals), 
                                     linpred=mean(linpred), 
                                     qres = mean(qres))

################### CREATE DEVIANCE RESIDUAL PLOTS ##########################
#scale = 1.35
#font_scaler = font_scaler * scale
pdf(file = paste0(fig_folder,"std_deviance_residuals.pdf"),
    width = 9, 
    height = 4.2, 
    family = "Palatino")
par(mfrow = c(1,2))
par(mar=c(4,5,3.5,2))
# Plot deviance residuals for full data
plot(rstandard(lmod2) ~ predict(lmod2,type="link"),
     ylim=c(-2.5,2.5),
     xlim=c(-3,3),
     xlab=expression(hat(eta)),
     ylab="Standardised dev. residuals",
     cex.axis = font_scaler,
     cex.lab = font_scaler,
     cex.main = font_scaler,
     cex = font_scaler)
# Add threshold lines
abline(h=-2,lty="dashed",col="red",lwd=2)
abline(h=2,lty="dashed",col="red",lwd=2)
# Plot deviance residuals for binned data
plot(residuals ~ linpred, 
     grouped_df_means, 
     xlab=expression(hat(eta)),
     ylab="Standardised dev. residuals",
     xlim=c(-3,1.5),
     ylim=c(-0.6,0.6),
     cex.axis = font_scaler,
     cex.lab = font_scaler,
     cex.main = font_scaler,
     cex = font_scaler)
dev.off()

# Share of standardised dev. residuals between -2 and 2
mean(abs(rstandard(lmod2)) < 2)

############## CREATE INDIVIDUAL PREDICTOR RESIDUAL PLOTS #######################
# Group data by variable
grouped_data <- group_by(heartd,age) # Change variable if needed
# Compute means and counts per group
group_mean_count <- dplyr::summarise(grouped_data, 
                                            residuals=mean(residuals),
                                            count=n())
# Plot deviance residuals against individual predictors
pdf(file = paste0(fig_folder,"devres_age.pdf"), # Change file name if needed
    width = 9, 
    height = 7, 
    family = "Palatino")
ggplot(group_mean_count,aes(y=residuals,x=age+mean(heartd$age),size=sqrt(count))) + 
  theme_bw() +
  theme(panel.grid.major = element_line(colour = grey(0.9), size=0.5),
        panel.grid.minor = element_line(colour = grey(0.9), size=0.5),
        panel.background = element_rect(colour = "black", size=3)) +
  geom_point()+
  xlab("Age (years)")+ # Change label if needed
  ylab("Standardised dev. residuals") +
  theme(text = element_text(size=32),
        axis.text = element_text(color="black",size=32)) +
  theme(legend.position="top") +
  labs(size=expression(sqrt(count))) 
dev.off()

################### CREATE QUANTILE RESIDUAL PLOTS ##########################
# Plot quantile residuals
pdf(file = paste0(fig_folder,"quantile_residual_plots_2.pdf"),
    width = 9, 
    height = 3, 
    family = "Palatino")
par(mfrow = c(1,3))
par(mar=c(5,5,4,1))
# Quantile residuals for full data
plot(qres ~ linpred,
     heartd, # Full data
     xlab=expression(hat(eta)),
     ylab="Quantile residuals",
     main = "Quantile residuals (set 2)",
     ylim=c(-4,4),
     xlim=c(-3,3),
     cex.axis = font_scaler,
     cex.lab = font_scaler,
     cex.main = font_scaler,
     cex = font_scaler)
# QQ plot of quantile residuals of full data
qqnorm(qres,
       main = "Quantile residuals (set 2)",
       cex.axis = font_scaler,
       cex.lab = font_scaler,
       cex.main = font_scaler,
       cex = font_scaler)
qqline(qres, col = "red",lwd=3,lty="dashed")
# Quantile residuals for binned data
plot(qres ~ linpred, 
     grouped_df_means, # Binned data
     xlab=expression(hat(eta)),
     ylab="Quantile residuals",
     main = "Quantile residuals (set 2)",
     xlim=c(-3,1),
     ylim=c(-0.5,0.5),
     cex.axis = font_scaler,
     cex.lab = font_scaler,
     cex.main = font_scaler,
     cex = font_scaler)
dev.off()

# Share of standardised quantile residuals between -2 and 2
mean(abs(qres) < 2)

###################### OUTLIER ANALYSIS #############################
# Calculate Cook's distance for each data point:
cooks_dist <- cooks.distance(lmod2)

n <- dim(heartd)[1] # Number of data points
q_vars <- length(coef(lmod2)) # Number of explanatory variables in the model
cook_threshold <- 8 / (n - 2*q_vars) # Threshold value for outliers

# Possible outliers
possible_outliers <- which(cooks_dist > cook_threshold)
# A subset of outliers to be removed
outliers_removed <- which(cooks_dist > 0.0025)
# Remove outliers from data
heartd_no_outs <- heartd[-outliers_removed,]

################ Re-fit model without outliers ###########################
lmod_final <- glm(formula = hd ~ sex + age + chol + sbp + bmi + sex:chol + 
                               sex:sbp + age:bmi, 
                  family = binomial(link = "logit"), 
                  data = heartd_no_outs)
 # Check model
summary(lmod_final)
anova(lmod_final,test="Chi")
step(lmod_final) # Make sure this model has lowest AIC

# Update Cook's distance threshold for different number of points
q_vars_update <- length(coef(lmod_final))
cook_threshold_update <- 8 / (dim(heartd_no_outs)[1] - 2*q_vars_update)
# Plot Cook's distance
pdf(file = paste0(fig_folder,"cooks_distance.pdf"),
    width = 9, 
    height = 7, 
    family = "Palatino")
par(mfrow = c(2,2))
par(mar=c(4.5,5,4,2))
plot(cooks_dist, # Previous model
     ylab="Cook's distance",
     main = "Cook's distance (n = 4658)",
     cex.axis = font_scaler,
     cex.lab = font_scaler,
     cex.main = font_scaler,
     cex = font_scaler)
abline(h=cook_threshold,col="red",lty="dashed",lwd=2)
halfnorm(cooks_dist, # Previous model
         xlim=c(0,4),nlab=0,
         ylab="Sorted Cook's distance",
         main = "Cook's distance (n = 4658)",
         cex.axis = font_scaler,
         cex.lab = font_scaler,
         cex.main = font_scaler,
         cex = font_scaler)
abline(h=cook_threshold,col="red",lty="dashed",lwd=2)
plot(cooks.distance(lmod_final), # Final model
     ylab="Cook's distance",
     ylim=c(0,0.003),
     main = "Cook's distance (n = 4649)",
     cex.axis = font_scaler,
     cex.lab = font_scaler,
     cex.main = font_scaler,
     cex = font_scaler)
abline(h=cook_threshold_update,col="red",lty="dashed",lwd=2)
halfnorm(cooks.distance(lmod_final),# Final model
         ylab="Sorted Cook's distance",
         main = "Cook's distance (n = 4649)",
         yaxt = 'n',
         cex.axis = font_scaler,
         cex.lab = font_scaler,
         cex.main = font_scaler,
         cex = font_scaler)
axis(2,at = c(0.0,0.001,0.002),
     cex.axis = font_scaler,
     cex.lab = font_scaler,
     cex.main = font_scaler,
     cex = font_scaler)
abline(h=cook_threshold_update,col="red",lty="dashed",lwd=2)
dev.off()

###########################################################################
############################ GOODNESS OF FIT ##############################
###########################################################################
# Add predicted probabilities to data and update linear predictors
heartd_no_outs <- mutate(heartd_no_outs,
                         linpred=predict(lmod_final,type="link"),
                         predprob=predict(lmod_final,type="response"))
# Create bins by linear predicted response
nbins <- 20 # Change bin size if needed
gdf_HL <- group_by(heartd_no_outs, 
                        cut(linpred, 
                            breaks=unique(quantile(linpred, (0:nbins)/nbins)), 
                            include.lowest = TRUE))
# Create summary statistics per bin
hldf <- dplyr::summarise(gdf_HL, 
                         y=sum(hd), 
                         ppred=mean(predprob), 
                         count=n())
# Add binomial standard deviations for 95% CIs
hldf <- mutate(hldf, se.fit=sqrt(ppred*(1-ppred)/count))

# Plot graphical goodness of fit test
pdf(file = paste0(fig_folder,"predicted_prob_20.pdf"), # Change name if needed
    width = 9, 
    height = 7, 
    family = "Palatino")
ggplot(hldf,aes(x=ppred,y=y/count,ymin=y/count-2*se.fit,ymax=y/count+2*se.fit))+
  theme_bw() +
  theme(panel.grid.major = element_line(colour = grey(0.9), size=0.5),
        panel.grid.minor = element_line(colour = grey(0.9), size=0.5),
        panel.background = element_rect(colour = "black", size=3)) +
  geom_point(size=3)+
  geom_linerange(color=grey(0.6),size=0.5)+
  geom_abline(intercept=0,slope=1,size=1)+
  xlab("Predicted probability")+
  ylab("Observed proportion") +
  theme(text = element_text(size=32),
        axis.text = element_text(color="black",size=32))
dev.off()      

################ Hosmer–Lemeshow (HL) test #############################
# Compute HL test statistic
hlstat <- with(hldf, sum((y-count*ppred)^2/(count*ppred*(1-ppred))))
# Compute p-value
p_HL <- 1 - pchisq(hlstat, nrow(hldf)-2)

###########################################################################
############################ INTERPRETATIONS ##############################
###########################################################################
# Get final model coefficients
beta <- coefficients(lmod_final)
confint(lmod_final) # 95% confidence intervals
# Logistic function for transforming linear predictions to probabilities
logistic <- function(x) {1/(exp(-x)+1)}
# Get mean values of variables for creating plots
age_mean <- mean(heartd$age) # Note that age is centralised, so this is 0
chol_mean <- mean(heartd$chol) # Note that chol centralised, so this is 0
bmi_mean <- mean(heartd$bmi) # Note that bmi is centralised, so this is 0

# Auxiliary variable for sbp for plotting purposes
sbp <- seq(100,250,20)
# Centralise auxiliary variable to correspond to regression coefficients
sbp_central <- sbp - mean(heartd$sbp)

# Female heart disease sbp linear predictor
eta_female_sbp <- beta[1] + 
  beta[3]*age_mean + 
  beta[4]*chol_mean + 
  beta[5]*sbp_central + 
  beta[6]*bmi_mean
# Male heart disease sbp linear predictor
eta_male_sbp <- eta_female_sbp + beta[2]
# Male heart disease sbp linear predictor with interaction
eta_male_sbp_int <- eta_male_sbp + beta[8]*sbp_central

# Transform linear predictors to estimated probabilities for plotting
female_hat_prob <- logistic(eta_female_sbp)
male_hat_prob <- logistic(eta_male_sbp)
male_sbp_int_hat_prob <- logistic(eta_male_sbp_int)

############### PLOT PREDICTED PROBABILITY VERSUS SBP ########################
font_scaler = font_scaler * 1.35
pdf(file = paste0(fig_folder,"interpretations.pdf"),
    width = 9, 
    height = 7, 
    family = "Palatino")
par(mfrow = c(1,1))
par(mar=c(5,5,5,3))
plot(female_hat_prob ~ sbp,
     ylim=c(0,1),
     xlim=c(100,250),
     xlab="Systolic blood pressure (mm Hg)",
     ylab="Predicted heart disease probability",
     cex.axis = font_scaler,
     cex.lab = font_scaler,
     cex.main = font_scaler,
     cex = font_scaler,
     pch=1)
lines(female_hat_prob ~ sbp,lwd=2)
# Switch to male sex
points(male_hat_prob ~ sbp,
       cex.axis = font_scaler,
       cex.lab = font_scaler,
       cex.main = font_scaler,
       cex = font_scaler,
       pch = 16)
lines(male_hat_prob ~ sbp,
      lwd=2,
      lty="dashed")
# Include male-chol interaction
points(male_sbp_int_hat_prob ~ sbp,
       cex.axis = font_scaler,
       cex.lab = font_scaler,
       cex.main = font_scaler,
       cex = font_scaler,
       pch = 18,
       col = "red")
lines(male_sbp_int_hat_prob ~ sbp,
      lwd=2,
      lty="dotted",
      col = "red")
legend("topleft",
       c("Female reference level","Male","Male-SBP interaction"),
       pch=c(1,16,18),
       lty=c("solid","dashed","dotted"),
       col=c("black","black","red"),
       lwd=c(2,2,2),
       bty='n',
       cex=font_scaler)
dev.off()