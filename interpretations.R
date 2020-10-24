### © Juha Kreula

source("./model_selection_and_validation.R")

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