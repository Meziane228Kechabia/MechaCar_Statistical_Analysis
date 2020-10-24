# Delivrable 1
library(dplyr) # Import the dplyr
MechaCar_mpg <- read.csv(file='MechaCar_mpg.csv',check.names=F,stringsAsFactors = F) #Import the MechaCar data
MecaCar_lm <-lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle +	ground_clearance + AWD,data=MechaCar_mpg) #Generate linear regression model.
summary(MecaCar_lm) #determine the p-value & r-squared

# Delivrable 2
Suspension_Coil <- read.csv(file='Suspension_Coil.csv',check.names=F,stringsAsFactors = F) #Import the Suspension_Coil data
total_summary <- Suspension_Coil %>% summarize(mean(PSI), median(PSI),var(PSI), sd(PSI) ) #create summary table
lot_summary <- Suspension_Coil %>% group_by(Manufacturing_Lot) %>% summarize(mean(PSI),median(PSI),var(PSI), sd(PSI)) #create summary table by lotNumber

# Delivrable 3.

t.test(Suspension_Coil$PSI,mu=1500) #compare sample versus population means
# S3 method for default
t.test(subset(Suspension_Coil,Manufacturing_Lot=="Lot1")$PSI,mu=1500)
t.test(subset(Suspension_Coil,Manufacturing_Lot=="Lot2")$PSI,mu=1500)
t.test(subset(Suspension_Coil,Manufacturing_Lot=="Lot3")$PSI,mu=1500)
