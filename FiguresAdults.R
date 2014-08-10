# Create plots ------------------------------------------------------------

library(RColorBrewer)

par(mfrow=c(2,1))

## Stem density
        par(mar = c(2, 5, 3, 1))  #down, left, up, right
        plot(data.final$Year, data.final$N_Ad, type="n", xlab="Timesteps (years)", main="PPA N=100",
             ylab="Stem density", xlim=c(0,nyears),ylim=c(0,1.3*max(data.final$N_Ad, na.rm=T)) ) 
        colors <- brewer.pal(length(SP), "Spectral") 
        linetype <- c(1:length(SP)) 
        
        # add lines 
        for (i in 1:length(SP)) {
                lines(data.final$Year[data.final$Sp==SP[i]],data.final$N_Ad[data.final$Sp==SP[i]], type="l",
                      lwd=3,  lty=linetype[i], col=colors[i])
        } 
        
legend("top", bty="n",ncol=4, SP, col=colors, cex=0.65, inset=c(0,-0.05),x.intersp=0.2,y.intersp=0.6,lty=linetype, lwd=3, horiz=FALSE)


## Basal area
        par(mar = c(4, 5, 1, 1))        #down, left, up, right
        plot(data.final$Year, data.final$BA_Ad, type="n", xlab="Timesteps (years)",main="",
             ylab="Basal area", xlim=c(0,nyears),ylim=c(0,1.3*max(data.final$BA_Ad, na.rm=T)) ) 
        colors <- brewer.pal(length(SP), "Spectral") 
        linetype <- c(1:length(SP)) 
        
        # add lines 
        for (i in 1:length(SP)) {
                lines(data.final$Year[data.final$Sp==SP[i]],data.final$BA_Ad[data.final$Sp==SP[i]], type="l", 
                      lwd=3,  lty=linetype[i], col=colors[i])
        } 

# ## Mean Quadratic Dameter
#         par(mar = c(4, 5, 1, 1))  #down, left, up, right
#         plot(data.final$Year, data.final$Dg_Ad, type="n", xlab="Timesteps (years)", main="",
#              ylab="Mean Dg (cm)", xlim=c(0,nyears),ylim=c(0,1.3*max(data.final$Dg_Ad, na.rm=T)) ) 
#         colors <- brewer.pal(length(SP), "Spectral") 
#         linetype <- c(1:length(SP)) 
#         
#         # add lines 
#         for (i in 1:length(SP)) {
#                 lines(data.final$Year[data.final$Sp==SP[i]],data.final$Dg_Ad[data.final$Sp==SP[i]], type="l", lwd=3,  
#                         lty=linetype[i], col=colors[i])
#         } 
#         
# ## Stem density
#         par(mar = c(4, 4, 1, 1))  #down, left, up, right
#         plot(data.final$Year, data.final$H_Ad, type="n", xlab="Timesteps (years)", main="",
#              ylab="Mean height (m)", xlim=c(0,nyears),ylim=c(0,1.3*max(data.final$H_Ad, na.rm=T)) ) 
#         colors <- brewer.pal(length(SP), "Spectral") 
#         linetype <- c(1:length(SP)) 
#         
#         # add lines 
#         for (i in 1:length(SP)) {
#                 lines(data.final$Year[data.final$Sp==SP[i]],data.final$H_Ad[data.final$Sp==SP[i]], type="l", lwd=3,  
#                       lty=linetype[i], col=colors[i])
#         } 
#         
