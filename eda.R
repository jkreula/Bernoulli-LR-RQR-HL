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
print(ggplot(heartd, aes(age, fill = as.character(hd))) + # Change age variable
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
  scale_color_discrete(name = "Heart disease", labels = c("No", "Yes")))
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