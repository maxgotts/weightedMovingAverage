require(ggplot2)

data("beavers")

beaver1$dayShift <- ifelse(beaver1$day==346, 0, 2350)
beaver1$realTime <- beaver1$dayShift+beaver1$time
beaver1$activ <- ifelse(beaver1$activ==0,"Inactive","Active")
b1 <- beaver1[,c("realTime","temp","activ")]
colnames(b1) <- c("Time","Temperature","Activity")
b1$Beaver <- "Number 1"

ggplot(b1, aes(x=Time, y=Temperature))+
  geom_line()+
  theme_classic()

ggplot(wma(b1, alist(x=Time, y=Temperature), width=120), aes(x=Time, y=Temperature))+
  geom_line()+
  theme_classic()




# fake.dk <- dk2
# fake.dk$Group2 <- rbern(nrow(fake.dk))
# fake.dk$Group3 <- c("Hello","World")[rbern(nrow(fake.dk))+1]
# w <- ck(fake.dk, alist(x=MinutesSince, y=Pair, by=Inside, split=Group2, another=Group3), 75)
# 
# 
# 
# 
# 
# wma.dat <- rbind(
#   cbind(wma(
#     filter(dk2, Inside==F),
#     "MinutesSince",
#     "Pair",
#     75
#   ),Event="Outside the fence"),
#   cbind(wma(
#     filter(dk2, Inside==T),
#     "MinutesSince",
#     "Pair",
#     75
#   ),Event="Inside the fence")
# )