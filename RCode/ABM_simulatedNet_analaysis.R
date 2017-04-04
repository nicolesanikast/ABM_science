t <- read.csv("~/Documents/Projects/version_control/ABM_DOM_PM_experiments/exp_network_simul_yesAutoregg.csv", header = F)
t <- rbind(t, c( NA,6, 227,535, -0.31, 0.563))#adding the properties of the empirical experimental network
t.pc  <- princomp(t[,-1], cor = T)
#create colors based on the innovation threshold:
innov.val <- as.numeric(levels(as.factor(t[,1])))
colors.pall <- rainbow(length(innov.val))
col.innov <- sapply(1:(nrow(t)-1), function(x) return(colors.pall[t[x,1] == innov.val]))
col.innov <- c(col.innov, "black") #add the color of the empirical network
length(col.innov)
nrow(t)
plot(t.pc$scores[,1], t.pc$scores[,2], col = col.innov, xlab = "pc1", ylab = "pc2", bty = "l")
points(t.pc$scores[nrow(t),1], t.pc$scores[nrow(t),2], cex = 2, pch = 3)#mark the position of the empirical network
legend("top", legend = innov.val, fill = colors.pall, x.intersp = 0.01, y.intersp = 0.9, bty = "n", border = NA)
#consider robust PCA