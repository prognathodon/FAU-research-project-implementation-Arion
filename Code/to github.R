library(Rfit)
#read in data
data <- read.csv("allmissingdata.csv")
cladedata <- read.csv("cladedata.csv")
# read in data excluding outliers in terms of character number
modididata <- read.csv("allmissingdatamodified.csv")
cladedatamodi <- read.csv("cladedatanodino.csv")
# Normality tests
shapiro.test(cladedata$median)
shapiro.test(cladedata$sum) #normally distributed
shapiro.test(cladedata$median.character.number)
shapiro.test(cladedata$Dataset.number)
shapiro.test(cladedata$Root.node.age..mya.)
shapiro.test(cladedata$Phylogenetic.research.history)
shapiro.test(data$median)
shapiro.test(data$total)
shapiro.test(data$size)
# Most variables aren't normally distributed => use rfit() for regression
#Relationship between total missing data proportion (per data set) and the median missing proportion (per taxon)
model1 <- rfit(data$total~data$median) 
##plot this relationship
par(mar=c(4, 4, 1, 2) + 0.1)
plot(data$median, data$total, xlab = "Median missing proportion (per taxa)", ylab = "Total missing proportion (per data set)")
abline(model1, lwd = 3, col ="red")

#Missing characters in different clades
##Boxplot
par(mar=c(4, 7, 1, 2) + 0.1)
###missing data per taxa
boxplot(data$median~data$clade, cex = 0.2, horizontal = TRUE, las = 1, cex.axis = 0.5, xlab = "Missing data (per data set)", ylab = "")
### missing data per dataset
boxplot(data$total~data$clade, cex = 0.2, horizontal = TRUE, las = 1, cex.axis = 0.5, xlab = "Missing data (per data set)", ylab = "")
### vertebrate vs invertebrate (total missing data per dataset)
vertebrate <- data[data$Group == "Vertebrates", ]
invertebrate <- data[data$Group == "Invertebrate",]
vvsinvt <- wilcox.test(vertebrate$total, invertebrate$total) #significantly different!!
par(mar=c(4, 4, 1, 2) + 0.1)
boxplot(data$total~data$Group, xlab = "", ylab = "Total missing data proportion per data set")
### vertebrate vs invertebrate (median missing proportion per taxon)
vvsinvm <- wilcox.test(vertebrate$median, invertebrate$median)
boxplot(data$median~data$Group, xlab = "", ylab = "Median missing data proportion per taxon")
### Proportion of taxa with missing characters
par(mar=c(4, 7, 1, 2) + 0.1)
boxplot(data$numtaxa~data$clade, horizontal = TRUE, las = 1, cex.axis = 0.5, xlab = "Proportion of taxa with missing characters", ylab = "")

#missing data vs clade origination age
## with total missing character per data set
model2 <- rfit(cladedata$sum ~ cladedata$node.age)
par(mar=c(4, 5, 1, 2) + 0.1)
plot(cladedata$node.age, cladedata$sum, xlab = "Clade origination age (mya)", ylab = "Total missing character proportion (per data set)")
abline(model2, lwd = 3, col = "red")
## with median missing cahracter per taxon
model3 <- rfit(cladedata$median ~ cladedata$node.age)
plot(cladedata$node.age, cladedata$median, xlab = "Clade origination age (mya)", ylab = "Median missing character proportion (per taxon)")
abline(model3, lwd = 3, col = "red")

#missing data vs number of characters
##total missing character per dataset
modididata <- read.csv("allmissingdatamodified.csv") # exclude dataset outliers in terms of the number of characters
model4 <- rfit(modididata$total ~ modididata$size)
plot(modididata$size, modididata$total, xlab = "Number of character", ylab = "Total missing character proportion (per data set)")
abline(model4, lwd = 3, col = "red")
## median missing character per taxon
model5 <- rfit(modididata$median ~ modididata$size)
plot(modididata$size, modididata$median, xlab = "Number of character", ylab = "Median missing character proportion (per taxon)")
abline(model5, lwd = 3, col = "red")

# missing data vs the number of studies
## total missing proportions per dataset
model6 <- rfit(cladedata$sum ~ cladedata$Data.set.amounts)
plot(cladedata$Data.set.amounts, cladedata$sum, xlab = "Data set numbers", ylab = "Total missing character proportion (per dataset)")
abline(model6, lwd = 3, col = "red")
## median missing proportion per taxon
model7 <- rfit(cladedata$median ~ cladedata$Data.set.amounts)
plot(cladedata$Data.set.amounts, cladedata$median, xlab = "Data set numbers", ylab = "Median missing character proportion (per taxon)")
abline(model7, lwd = 3, col = "red")
