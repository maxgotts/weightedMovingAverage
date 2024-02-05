require(ggplot2)

set.seed(123)
data("beavers")

# Adjust beaver1 dataset to make our version, b1
beaver1$dayShift <- ifelse(beaver1$day==346, 0, 2350)
beaver1$realTime <- beaver1$dayShift+beaver1$time
beaver1$activ <- ifelse(beaver1$activ==0,"Inactive","Active")
b1 <- beaver1[,c("realTime","temp","activ")]
colnames(b1) <- c("Time","Temperature","Activity")
b1$Beaver <- "Number 1"
b1$Activity <- factor(b1$Activity, levels=c("Active","Inactive"))
b1[,"Random"] <- "Normal"
b1[sample(1:nrow(b1), 0.4*nrow(b1)), "Random"] <- "Different"
b1$Random <- factor(b1$Random, levels=c("Normal","Different"))

# Plot raw data as line-plot
ggplot(b1, aes(x=Time, y=Temperature))+
  geom_line()+
  theme_classic()

# Plot smoothed data with confidence interval using default Gaussian kernel
ggplot(wma(b1, alist(x=Time, y=Temperature), width=100), aes(x=Time, y=Temperature))+
  geom_ribbon(aes(ymin=Temperature-SE, ymax=Temperature+SE), alpha=0.4)+
  geom_line()+
  theme_classic()

# Plot raw data by activity
ggplot(b1, aes(x=Time, y=Temperature, color=Activity, fill=Activity))+
  geom_line()+
  theme_classic()

# Plot smoothed data by activity
ggplot(wma(b1, alist(x=Time, y=Temperature, color=Activity), width=100), aes(x=Time, y=Temperature, color=Activity, fill=Activity))+
  geom_ribbon(aes(ymin=Temperature-SE, ymax=Temperature+SE), alpha=0.4)+
  geom_line()+
  theme_classic()

# Plot raw data by activity and random
ggplot(b1, aes(x=Time, y=Temperature, color=Activity, fill=Activity, linetype=Random))+
  geom_line()+
  theme_classic()

# Plot smoothed data by activity and random
ggplot(wma(b1, alist(x=Time, y=Temperature, color=Activity, linetype=Random), width=100), aes(x=Time, y=Temperature, color=Activity, fill=Activity, linetype=Random))+
  geom_ribbon(aes(ymin=Temperature-SE, ymax=Temperature+SE), alpha=0.4)+
  geom_line()+
  theme_classic()

