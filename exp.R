library(ggplot2)

set.seed(25)
numSamples = 40
numSims = 1000

data <- matrix(rexp(numSamples * numSims, 0.2), ncol = numSamples)

means <- apply(data, 1, mean) 
var <- sd(means) / sqrt(numSamples)
meanOfMeans <- round(mean(means), 4)
variance <- round(sd(means) * sqrt(numSamples), 4)
interval <- meanOfMeans + c(-1, 1) * qt(.975, numSamples - 1) * variance / sqrt(numSamples)

print(paste("mean = ", meanOfMeans))
print(paste("variance = ", variance))
print(paste("interval = ", interval))

df <- as.data.frame(means)
plot <- ggplot(df, aes(x=means)) 
plot <- plot + geom_histogram(aes(y=..density..), fill="white", binwidth=0.1)
plot <- plot + stat_function(fun=dnorm, args=list(mean=mean(meanOfMeans), sd=sd(means)))
plot <- plot + geom_vline(xintercept = meanOfMeans)
