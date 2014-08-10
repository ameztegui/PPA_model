# Create plots ------------------------------------------------------------

library(RColorBrewer)

par(mfrow=c(2,1))

## Setm density
par(mar = c(2, 5, 3, 1))  #down, left, up, right
        plot(data.final$Year, data.final$N, type="n", xlab="Timesteps (years)", main="PPA T=1000",
                ylab="Stem density", xlim=c(0,nyears),ylim=c(0,1.3*max(data.final$N)) )  
                colors <-brewer.pal(length(SP), "Spectral") 
        linetype <- c(1:length(SP)) 

# add lines 
        for (i in 1:length(SP)) {
                lines(data.final$Year[
                data.final$Sp==SP[i]],data.final$N[data.final$Sp==SP[i]], type="l", lwd=3,  lty=linetype[i], col=colors[i])
        } 

legend("top", bty="n",ncol=4, SP, col=colors, cex=0.65, lty=linetype, lwd=3, horiz=FALSE)


## Basal area
par(mar = c(4, 5, 1, 1))
        plot(data.final$Year, data.final$BA, type="n", xlab="Timesteps (years)",main="",
                ylab="Basal area", xlim=c(0,nyears),ylim=c(0,1.3*max(data.final$BA)) ) 
                colors <- brewer.pal(length(SP), "Spectral") 
        linetype <- c(1:length(SP)) 

# add lines 
        for (i in 1:length(SP)) {
                lines(data.final$Year[
                data.final$Sp==SP[i]],data.final$BA[data.final$Sp==SP[i]], type="l", lwd=3,  lty=linetype[i], col=colors[i])
        } 

