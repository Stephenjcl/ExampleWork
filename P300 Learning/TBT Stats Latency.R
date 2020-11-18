#Call ggplot2 functions, and a reshape package for formating the data in a way that ggplot likes
library(pacman)
p_load(ggplot2, reshape2, tidyr, plyr, psych, pastecs, gmodels, effects, compute.es, multcomp, Hmisc, car)
#Read csv from excel into R as a variable named after "trial-by-trial, peak amplitudes" and lable that it has headers.
S101 <- read.csv("TBTS101.csv", header =T)
S102 <- read.csv("TBTS102.csv", header =T)
S111 <- read.csv("TBTS111.csv", header =T)
S112 <- read.csv("TBTS112.csv", header =T)

#Rename colums by number
colnames(S101) <- (1:30)
colnames(S102) <- (1:30)
colnames(S111) <- (1:30)
colnames(S112) <- (1:30)

#Remove non-peak rows really... 
S101 <- subset(S101[113:150,])
S102 <- subset(S102[113:150,])
S111 <- subset(S111[113:150,])
S112 <- subset(S112[113:150,])

#plot(PeakbyTrial)
#Create column maxing function
colMax <- function(data) sapply(data, max, na.rm = TRUE)

#Apply maxing to data set, put in grouped data frame
PeakbyTrial <- data.frame(matrix(nrow = 30, ncol = 0))
PeakbyTrial$S101 <- as.data.frame(colMax(S101))
PeakbyTrial$S102 <- as.data.frame(colMax(S102))
PeakbyTrial$S111 <- as.data.frame(colMax(S111))
PeakbyTrial$S112 <- as.data.frame(colMax(S112))
colnames(PeakbyTrial) <- NA
colnames(PeakbyTrial) <- c("40%", "60%", "10%", "90%")
#PeakbyTrial <- NULL

#Reshape data to long format
PeakbyTrial = as.data.frame.list(PeakbyTrial)
#Add column for trial number
PeakbyTrial$Trial<- rownames(PeakbyTrial)

#Replace each square with the latency value for that peak. 
for (i in 1:nrow(PeakbyTrial)){
  PeakbyTrial[i,1] <- (which(S101[,i] == PeakbyTrial[i,1])*4)+248
  PeakbyTrial[i,2] <- (which(S102[,i] == PeakbyTrial[i,2])*4)+248
  PeakbyTrial[i,3] <- (which(S111[,i] == PeakbyTrial[i,3])*4)+248
  PeakbyTrial[i,4] <- (which(S112[,i] == PeakbyTrial[i,4])*4)+248
}

mean(PeakbyTrial[,1])
mean(PeakbyTrial[,2])
mean(PeakbyTrial[,3])
mean(PeakbyTrial[,4])
#Yup, looks good.
range(PeakbyTrial[,1:4])

#Fix names again because why the hell not
colnames(PeakbyTrial) <- NA
colnames(PeakbyTrial) <- c("P40", "P60", "P10", "P90", "Trial")


#Wide to long conversion (wide has column for each variable, we need it as a factor) melt is wide -> long, cast is long -> wide
df.molten2 <- gather(PeakbyTrial, Frequency, Latency, P40:P90, factor_key=T)

#Reorder x-axis cause it's totally botched for some reason now
df.molten2$Trial <- factor(df.molten2$Trial, levels = df.molten2$Trial)
head(df.molten2)

#Rename P%% conditions to have prettier names on the plot
df.molten2$Frequency <- revalue(df.molten2$Frequency, c("P10"="10%", "P40"="40%", "P60"="60%", "P90"="90%"))
df.molten2$Frequency <- factor(df.molten2$Frequency, levels = df.molten2$Frequency)

####PLOT####
colourscw <- c("#505050","#909090","#505050","#909090")
#Type of line
linetypecw = c("solid", "solid", "dashed", "dotted")
#Y Axis Values range
lineylim = range(250,405)
#Y Axis label
lineylab = "Latency (ms)"
#X Axis label
linexlab = "Trial Position"

#Now to plot
p <- ggplot(df.molten2, aes(as.numeric(df.molten2$Trial), df.molten2$Latency, colour = Frequency)) + geom_line(aes(group = Frequency, linetype= Frequency)) + 
  scale_color_manual(values = c("#505050","#909090","#505050","#909090")) +
  scale_linetype_manual(values = linetypecw) + 
  #scale_y_reverse(expand = c(0, 0)) + 
  coord_cartesian(ylim = lineylim) +
  scale_x_continuous(breaks = round(seq(min(0), max(30), by = 5),1),expand = c(0,0)) +
  
  #Labels
  xlab(linexlab) + #X axis label
  ylab(lineylab) +#Y axis label
  
  #Legend
  theme_bw()+
  theme(legend.position = c(0.12, 0.24)) +#Position of a legend
  theme(legend.text=element_text(size=13)) +#Text size within the legend
  theme(legend.key.size = unit(.6, "cm")) +#Size of legend box colour
  theme(legend.key = element_rect(colour = FALSE)) +#Remove borders around colours
  theme(legend.title=element_blank()) +#Removed title of legend
  
  #Background and border
  theme(plot.margin=unit(c(.5,.5,.5,.5),"cm")) +#Adds white space around your plot
  theme(axis.line.x = element_line(color="black", size = 0.5), #This adds a x axis line
        axis.line.y = element_line(color="black", size = 0.5), #This adds a y axis line
        panel.grid.major = element_blank(), #Removes grid
        panel.grid.minor = element_blank(), #Removes more grid
        panel.background = element_blank(), #Removes grey background
        panel.border = element_blank()) #Removes lines around the plot

p
