# Script to get the ouput values of PPA model and convert them into values per timestep and species
# 
# The PPA model gives a detailed output of the simulated forest, with one file for each species and year
# Each file contains the density, diameter and height of all the cohorts for that species and timestep
# The aim of this script is to summarize this information into a single file
#
#
# Aitor Ameztegui & Bastien Lecigne, UQAM, July 2014


# Summary data per species ------------------------------------------------

setwd("/Users/multivac42/userFriendly")
nyears <- 1000  # Number of simulated years
dir<-dir("/Users/multivac42/userFriendly/record") # get the name of the files
data.frame(strsplit(dir,split=" ")[1])  # Split the names by white space, get the first element (species name)

SP <- c("ABBA","ACRU","ACSA","BEPA","POGR", "POTR","QURU","TIAM") # define the simulated species
data.final <- data.frame(matrix(ncol=10))  # create the dataframe in which we will collect the data
names(data.final) <- c("Sp","Year","N","Dg","H","BA","N_Ad","Dg_Ad","H_Ad","BA_Ad") # set the names of the variables in the dataframe

time = 0  # initialize variable time


for(s in 1:length(SP)){   # for each species...
        
        data.temp <- data.frame(matrix(ncol=10)) # creates a temporal dataframe with 10 columns (number of variables we will calculate)
        names(data.temp) <- c("Sp","Year","N","Dg","H","BA","N_Ad","Dg_Ad","H_Ad","BA_Ad")  # set the names of the variables in the dataframe
        
        for(i in 1:nyears){
                dat <- read.table(paste("record/",SP[s]," iter = ",time,".data",sep=""))  # read the original files                
                sp <- SP[s]                                     # species is the value we got from splitting the filename
                N <- sum(dat[,1])                               # total stems per plot 
                H <- sum(dat[,3]*dat[,1])/sum(dat[,1])          # average Height per plot
                BA <- sum(dat[1]*(pi/4)*(dat[,2]/100)^2)        # total BA per plot
                Dg <- sqrt(BA/(0.0000785*N))                    # quadratic diameter (Dg) per plot
                
                data.temp[i,1] <- as.character(sp)
                data.temp[i,2] <- time
                data.temp[i,3] <- N
                data.temp[i,4] <- Dg
                data.temp[i,5] <- H
                data.temp[i,6] <- BA
                
                #########- with DBH > 9cm
                dat <- subset(dat,dat[,2]>=9)
                N_ad <- sum(dat[,1])                            # total adult stems per plot
                H_ad <- sum(dat[,3]*dat[,1])/sum(dat[,1])       # average adult height per plot
                dat[,4] <- dat[1]*(pi/4)*(dat[,2]/100)^2        
                BA_ad <- sum(dat[,4])                           # total adult basal area per plot        
                Dg_ad <- sqrt(BA_ad/(0.0000785*N_ad))          # Adult quadratic diameter (Dg) per plot
                
                data.temp[i,7] <- N_ad
                data.temp[i,8] <- Dg_ad
                data.temp[i,9] <- H_ad
                data.temp[i,10] <- BA_ad
                
                # Set conditions. If the timestep divided by nyears gives 0, reset time. Else, add one and go on
                if(i %% nyears == 0 ){
                        time = 0
                } else {
                        time = time + 1
                }
        }
        
        data.final <- rbind(data.final,data.temp) # Create data.final by adding the rows of each data.temp
}

data.final = data.final[-1,]   # delete the first row (it gave as a mistake)
View(data.final)
write.table(data.final,"record/SummarySpecies.txt", sep="\t", row.names=F )


# General summary data ----------------------------------------------------

N.tot <- aggregate(data.final$N,by=list(data.final$Year),FUN=sum)
BA.tot <- aggregate(data.final$BA,by=list(data.final$Year),FUN=sum)
Dg.tot <- aggregate(data.final$Dg,by=list(data.final$Year),FUN=mean)
H.tot <- aggregate(data.final$H,by=list(data.final$Year),FUN=mean)
N.ad.tot <- aggregate(data.final$N_Ad,by=list(data.final$Year),FUN=sum)
BA.ad.tot <- aggregate(data.final$BA_Ad,by=list(data.final$Year),FUN=sum)
Dg.ad.tot <- aggregate(data.final$Dg_Ad,by=list(data.final$Year),FUN=mean)
H.ad.tot <- aggregate(data.final$H_Ad,by=list(data.final$Year),FUN=mean)

summary.table <- data.frame(N.tot,BA.tot[2],Dg.tot[2],H.tot[2], N.ad.tot[2], BA.ad.tot[2], Dg.ad.tot[2], H.ad.tot[2])
names(summary.table) <- c("Timestep","N","BA","Dg","H","N_Ad", "BA_Ad","Dg_Ad","H_Ad")

View(summary.table)
write.table(summary.table,"record/SummaryTotal.txt", sep="\t", row.names=F )





