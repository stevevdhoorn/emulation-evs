#################################################
# Emulation for GMR Perth and Surrounding areas #
# NOTES:
# 
#################################################

#setwd('C:/ctm/em-public')
setwd("C:/ctm/emulation-evs/")

### load relevant R libraries
library(DiceKriging)
library(sensitivity)
library(ggplot2)
library(cowplot)
library(rlang)
library(ggtext)
library(grid)
library(paletteer)
library(hrbrthemes)
library(openxlsx)

# Load additional R functions
source('./EmodFit.R')
source('./misc functions - emulators.R')

### load training data (CTM outputs) for PM2.5 - GMR Perth (unweighted by population)
data(res_gccwgt)
myvar = 'pm25d_wgt'
pm25_t = EmodFit(myvar  = 'pm25d_wgt', res = res_gccwgt, myloc = 'Greater Perth', areaType = 'GCC_NAME16', molconv = 1)

#######################
### SCENARIO ANALYSES #
#######################

mypars = paste0('X', seq(8))
# Define scenarios based on setting parameters in the emulator at selected levels
scendat = data.frame(myscen = paste("s", seq(5), sep=""),
                     descr = c("Baseline",
                               "Zero exh emissions",
                               "s2 + SWIS_mixed1",
                               "s2 + SWIS_mixed2",
                               "s2 + SWIS_renew"),
                     X1 = c(0.59, 0, 0, 0, 0),
                     X2 = c(0.59, 0, 0, 0, 0),
                     X3 = c(0.59, 0, 0, 0, 0),
                     X4 = c(0.59, 0, 0, 0, 0) ,
                     X5 = c(0.59, 0, 0, 0, 0),
                     X6 = c(0.59, 0, 0, 0, 0),
                     X7 = c(0.75, 0.75, 0.75*1.25, 0.75*1.5, 0),
                     X8 = c(0.75, 0.75, 1.125*1.25, 0.75*1.5, 0))

#names(scendat)[3:10] = mypars
myPred <- predict(object=pm25_t$EmModel, newdata=scendat[,mypars], type="UK", checkNames=FALSE, light.return=TRUE)
myPred$mean[c(2,3,4,5)] - myPred$mean[1] # difference from baseline (2018)

### load SA4 emulators
data(sa4_ems)
regs = c('Perth - Inner', 'Perth - North East', 'Perth - North West',
         'Perth - South East', 'Perth - South West', 'Mandurah')
area_ems = sa4_ems
tab = localEm(areatype = 'sa4', area_ems = area_ems, scendat=scendat, regs=regs, myvar=myvar)
tab$pop = c(170, 251, 535, 488, 403, 97)
tab2_pm = rbind(tab, data.frame(regs = "GMR Perth",
                             s1 = sum(tab$s1*tab$pop)/sum(tab$pop),
                             s2 = sum(tab$s2*tab$pop)/sum(tab$pop),
                             s3 = sum(tab$s3*tab$pop)/sum(tab$pop),
                             s4 = sum(tab$s4*tab$pop)/sum(tab$pop),
                             s5 = sum(tab$s5*tab$pop)/sum(tab$pop),
                             pop = sum(tab$pop)))
tab2_pm$s2 - tab2_pm$s1 # Maximum reduction in exhaust emissions under Scenario 4 (RAPID) with SWIS_fixed
tab2_pm$s5 - tab2_pm$s1 # Maximum reduction in exhaust emissions under Scenario 7 (RAPID + SWIS_renew)
tab2_pm$s5 - tab2_pm$s2 # net benefit of SWIS_renew

tab2_pm$SWIS_fixed = tab2_pm$s2 - tab2_pm$s1
tab2_pm$SWIS_renew = tab2_pm$s5 - tab2_pm$s1

save(tab2_pm, file='./data/tab4_pm.RData')

#########################
## Surrounding regions ##
#########################
scendat = data.frame(myscen = paste("s", seq(3), sep=""),
                     descr = c("Baseline",
                               "SWIS_renew",
                               "SWIS_mixed"),
                     X1 = c(0.59, 0, 0),
                     X2 = c(0.59, 0, 0),
                     X3 = c(0.59, 0, 0),
                     X4 = c(0.59, 0, 0) ,
                     X5 = c(0.59, 0, 0),
                     X6 = c(0.59, 0, 0),
                     X7 = c(0.75, 0, 0.75*1.5),
                     X8 = c(0.75, 0, 0.75*1.5))

regs = c('Bunbury', 'Manjimup', 'Augusta - Margaret River - Busselton')
### load SA4 emulators
data(sa3_ems)
area_ems = sa3_ems
tab = localEm(areatype = 'sa3', area_ems = area_ems, scendat=scendat, regs=regs, myvar=myvar)
tab$pop = c(102, 23, 51)
tab4_pm = rbind(tab, data.frame(regs = "SA total", 
                             s1 = sum(tab$s1*tab$pop)/sum(tab$pop),
                             s2 = sum(tab$s2*tab$pop)/sum(tab$pop),
                             s3 = sum(tab$s3*tab$pop)/sum(tab$pop),
                             pop = sum(tab$pop)))

tab4_pm$SWIS_fixed = tab4_pm$s2 - tab4_pm$s1
tab4_pm$SWIS_renew = tab4_pm$s3 - tab4_pm$s1

save(tab4_pm, file='./data/tab4_pm.RData')
#write.xlsx(tab2, file = './output/tab2.xlsx') # output file to separate excel table

