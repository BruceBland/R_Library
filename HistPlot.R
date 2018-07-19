mean = round(mean(Shake$Value),5)
sd = round(sd(Shake$Value),5)
iqr = round(quantile(Shake$Value,0.75) - quantile(Shake$Value,0.25),5)

# Plot hist
HistPlot <- ggplot(Shake,aes(x=Value)) +
  geom_histogram(aes(x=Value),colour="Blue",fill="DarkBlue",breaks=seq(0, 0.01, by =0.0001)) +
  xlab("Movement Difference") +
  ylab("Count") +
  theme(plot.title = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        text = element_text(size = 12)) +
  ggtitle("Histogram Analysis",
          subtitle = paste("User: ",User," - Mean: ",mean,"  SD",sd,"  IRQ",iqr))
print(HistPlot)
