### © Juha Kreula

source("./eda.R")

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
heartd <- dplyr::mutate(heartd, residuals=rstandard(lmod2, type = "deviance"), 
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
print(ggplot(group_mean_count,aes(y=residuals,x=age+mean(heartd$age),size=sqrt(count))) + 
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
  labs(size=expression(sqrt(count))))
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
heartd_no_outs <- dplyr::mutate(heartd_no_outs,
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
hldf <- dplyr::mutate(hldf, se.fit=sqrt(ppred*(1-ppred)/count))

# Plot graphical goodness of fit test
pdf(file = paste0(fig_folder,"predicted_prob_20.pdf"), # Change name if needed
    width = 9, 
    height = 7, 
    family = "Palatino")
print(ggplot(hldf,aes(x=ppred,y=y/count,ymin=y/count-2*se.fit,ymax=y/count+2*se.fit))+
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
        axis.text = element_text(color="black",size=32)))
dev.off()      

################ Hosmer–Lemeshow (HL) test #############################
# Compute HL test statistic
hlstat <- with(hldf, sum((y-count*ppred)^2/(count*ppred*(1-ppred))))
# Compute p-value
p_HL <- 1 - pchisq(hlstat, nrow(hldf)-2)
