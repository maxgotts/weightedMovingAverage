fake.dk <- dk2
fake.dk$Group2 <- rbern(nrow(fake.dk))
fake.dk$Group3 <- c("Hello","World")[rbern(nrow(fake.dk))+1]
w <- ck(fake.dk, alist(x=MinutesSince, y=Pair, by=Inside, split=Group2, another=Group3), 75)





wma.dat <- rbind(
  cbind(wma(
    filter(dk2, Inside==F),
    "MinutesSince",
    "Pair",
    75
  ),Event="Outside the fence"),
  cbind(wma(
    filter(dk2, Inside==T),
    "MinutesSince",
    "Pair",
    75
  ),Event="Inside the fence")
)