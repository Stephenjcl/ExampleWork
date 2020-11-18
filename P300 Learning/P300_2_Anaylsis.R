####Setup Section####
#Work computer working directory
setwd("D:/Luehr_Onedrive_UVic/OneDrive - University of Victoria/Data Analysis/P300L_2/P300L_2_Pz_Export")
#Macbook working directory
setwd("~/OneDrive - uvic.ca/Data Analysis/P300L_2/P300L_2_Pz_Export")
opath <- getwd()

#Windows path length to ^/Data = 57
#Mac path length 34
#Must adjust data import loop numbers accordingly... damn.

#opath links to all of the data files that you would like to manage
opath <- 'D:/Luehr_Onedrive_UVic/OneDrive - University of Victoria/Data Analysis/P300L_2/P300L_2_Pz_Export' #The is the path to the folder that contains all of your .csv files. It will ONLY scan for .csvs
#Where to output the finished data
npath <- 'D:/Luehr_Onedrive_UVic/OneDrive - University of Victoria/Data Analysis/P300L_2/'

#Library of packages to load
library(pacman)
library(ggplot2)
library(data.table)
library(dplyr)
p_load(cowplot)

#Variables to be defined before running
data <- matrix(nrow = 0, ncol = 5)
data <- as.data.frame(data)

#Confidence interval function
conf <- function(x) {
  df = length(x) - 1
  crt = qt(0.05, df)
  std = sd(x, na.rm = T)
  sqn = sqrt(length(x))
  ci = abs(crt*std/sqn)
  print(ci)
}

#TBTpeaks function
TBTpeaks <- function(dataset, stimulus){
  S111 <- dataset[dataset[,2] %in% stimulus, ]
  count <- 0
  for (n in 1:nlevels(dataset[,3])){
    count <- count + 1
    print(count)
    temp <- S111[S111$TrialPosition %in% n,]
    temp <- temp[ which(temp$ms > 250 & temp$ms < 400), ]
    temp <- droplevels(temp)
    
    #Participant Loop
    #Create blank matrix for calculations
    partP300 <- matrix(nrow = nlevels(temp$Participant), ncol = 4)
    partP300 <- as.data.frame(partP300)
    #Loop through each participant to find their P300
    for (j in 1:nlevels(temp$Participant)){
      lowerbound <- 1 + ((j-1)*37)
      upperbound <- 37 + ((j-1)*37)
      temp2 <- temp[lowerbound:upperbound,]
      
      #Find the maximum value
      peak <- which.max(temp2$uV)
      
      #Early eject failsafe to prevent selecting a P300 within the dropoff boundary.
      if (peak < 7){next} #Word version of goal: If the peak is earlier than 275ms, break the innermost loop.
      #Mean the rows around the max. 1 row = 4ms. We want +/- 24ms so +/-6 rows
      partP300[j,1] <- mean(temp2[(peak-6):(peak+6),4])
      #Find the latency of the peak
      partP300[j,2] <- temp2[peak,5]
      #Insert Trial Number
      partP300[j,3] <- n
      #Insert Stimulus Type
      partP300[j,4] <- stimulus
    }
    #Now that we have all the numbers for each participant, gather the data into a new matrix with solely the averaged numbers with confidence intervals for the amplitude
    temp3 <- matrix(nrow = 1, ncol = 5)
    temp3 <- as.data.frame(temp3)
    temp3[1,1] <- mean(partP300[,1], na.rm=T); temp3[1,2] <- mean(partP300[,2], na.rm=T); temp3[1,3] <- n; temp3[1,4] <- stimulus;
    
    #Now calculate CI of the amplitude
    temp3[1,5] <- conf(partP300[,1])
    
    #Now append this to a large data frame to hold the final results
    #Remove temp3 names to allow rbind to proceed
    names(temp3) <- names(processeddata)
    processeddata <<- rbind(processeddata, temp3)
    
    #Clear all old values
    partP300 <- NULL
    temp3 <- NULL
    temp2 <- NULL
    temp <- NULL
    peak <- NULL
    lowerbound <- NULL; upperbound <- NULL
  }
}

#Block plotting script
bbplot <- function(data, stim1, stim2, plotname){
  temp <- data[data[,4] %in% stim1, ]
  temp2 <- data[data[,4] %in% stim2, ]
  full <- rbind(temp, temp2)
  
  #Plot setup
  #Colours of your lines
  colourscw <- c("#505050","#909090")
  #Type of line
  linetypecw = c("solid", "solid")
  #Y Axis Values range
  lineylim = range(-5,30)
  #Y Axis label
  lineylab = expression(paste("Amplitude (", mu, "V) +/- 25ms", sep ="")) #Amplitude (Î¼V)
  #X Axis label
  linexlab = "Trial Position"
  
  #Now to plot
  p <- ggplot(full, aes(as.numeric(full$`Trial Position`), full$`Mean Amplitude (+/-25ms)`, colour = Stimulus)) + geom_line(aes(group = Stimulus)) + #geom_ribbon(aes(ymin=(full$`Mean Amplitude (+/-25ms)` - full$`Amplitude Confidence Interval`), ymax=(full$`Mean Amplitude (+/-25ms)` + full$`Amplitude Confidence Interval`)), linetype=2, alpha=0.05) + #Theme-ing stuff
    scale_color_manual(values = c("#505050","#909090")) +
    scale_linetype_manual(values = linetypecw) + 
    scale_y_reverse(expand = c(0, 0)) + 
    coord_cartesian(ylim = lineylim) +
    scale_x_continuous(breaks = round(seq(min(0), max(80), by = 10),1),expand = c(0,0)) +
    
    #Labels
    xlab(linexlab) + #X axis label
    ylab(lineylab) +#Y axis label
    
    #Legend
    theme_bw()+
    theme(legend.position = c(0.08, 0.16)) +#Position of a legend
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
  assign(plotname, p, envir = .GlobalEnv) 
}

#Smoothed estimation plot for blocks
sbbplot <- function(data, stim1, stim2, plotname){
  temp <- data[data[,4] %in% stim1, ]
  temp2 <- data[data[,4] %in% stim2, ]
  full <- rbind(temp, temp2)
  
  #Plot setup
  #Colours of your lines
  #colourscw <- c("#505050","#909090")
  #Type of line
  linetypecw = c("solid", "solid")
  #Y Axis Values range
  lineylim = range(-5,40)
  #Y Axis label
  lineylab = expression(paste("Amplitude (", mu, "V) +/- 25ms", sep ="")) #Amplitude (Î¼V)
  #X Axis label
  linexlab = "Trial Position"
  
  #Now to plot
  p <- ggplot(full, aes(as.numeric(full$`Trial Position`), full$`Mean Amplitude (+/-25ms)`, colour = Stimulus)) + geom_smooth(aes(group = Stimulus), se=F) + geom_ribbon(aes(ymin=(full$`Mean Amplitude (+/-25ms)` - full$`Amplitude Confidence Interval`), ymax=(full$`Mean Amplitude (+/-25ms)` + full$`Amplitude Confidence Interval`)), linetype=2, alpha=0.05) + #Theme-ing stuff
    #scale_color_manual(values = c("#505050","#909090")) +
    scale_linetype_manual(values = linetypecw) + 
    scale_y_reverse(expand = c(0, 0)) + 
    coord_cartesian(ylim = lineylim) +
    scale_x_continuous(breaks = round(seq(min(0), max(80), by = 10),1),expand = c(0,0)) +
    
    #Labels
    xlab(linexlab) + #X axis label
    ylab(lineylab) +#Y axis label
    
    #Legend
    theme_bw()+
    theme(legend.position = c(0.12, 0.16)) +#Position of a legend
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
  assign(plotname, p, envir = .GlobalEnv) 
}
####Running Script####
#Read in all of the data into a single dataframe
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
files <- list.files(path = opath, pattern = '*.txt', full.names = T)

x <- 0
pb <- txtProgressBar(min = 0, max = length(files), style= 3)
for(fname in files){
  x <- x + 1
  #Read in the data (1)
  dat <- read.table(fname)
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  #Detect if there is more than 1 trial in the data import. Correct it to an average if there is.
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  dat <- as.data.frame(t(dat))
  if (nrow(dat) > 200){
    for (i in 1:((nrow(dat)/200)-1)){
      dat[1:200, 2] <- dat[201:400,]
      dat[1:200,] <- rowMeans(dat[1:200,])
      dat <- dat[-c(201:400),]; dat[,2] <- NULL
      next
    }
  }
    
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  newdat <- matrix(nrow=nrow(dat), ncol=5)
  newdat <- as.data.frame(newdat)
  
  #Grab name of participant
  #pname <- substr(fname, 98, 104) #Windows
  pname <- substr(fname, 75, 81) #Mac
  #Grab stimulus name
  #sname <- substr(fname, 106,109) #Windows
  sname <- substr(fname, 83,86) #Mac
  
  #Grab trial number
  #tname <- substr(fname, 112, nchar(fname)) #Windows
  tname <- substr(fname, 89, nchar(fname)) #Mac
  
  tname <- sub('\\.txt$', '', tname) 
  
  #Bring in the data
  newdat[,4] <- dat
  newdat[,1] <- pname; newdat[,2] <- sname; newdat[,3] <- tname;
  
  #Insert time as a column
  newdat[,5] <- seq(-200, 596, by = 4)
  
  data <- rbind(data, newdat)
  newdat <- NULL
  dat <- NULL
  setTxtProgressBar(pb, x)
}
close(pb); pb <- NULL

#Rename the columns.
names(data) = c('Participant', 'Stimulus', "TrialPosition", "uV", "ms")
#Check column types
sapply(data, class)

#Convert Trial Position to a factor, along with stimulus and participant
data$TrialPosition <- as.factor(data$TrialPosition); data$Stimulus <- as.factor(data$Stimulus); data$Participant <- as.factor(data$Participant); data$ms <- as.numeric(data$ms)
#Reorder the factor levels
data$TrialPosition <- factor(data$TrialPosition, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 
                                                                              20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 
                                                                              30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 
                                                                              40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 
                                                                              50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 
                                                                              60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 
                                                                              70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 
                                                                              80))

####Calculating peak means####
#Now find the peak per trial position. 

#TBTpeaks requires that stimulus type, and trial number be arranged as factors in your original dataset. Dataset must be in long format as if going into ggplot2 package.
#It also requires this column layout and naming: Participant, Stimulus, TrialPosition, uV, ms
#In the original dataset.
#Remember to place the stimulus of interest in quotes.

#You also MUST run this line to produce the output data frame for all Stimuli:
processeddata <- matrix(nrow = 0, ncol = 5); processeddata <- as.data.frame(processeddata); colnames(processeddata) <- c("Mean Amplitude (+/-25ms)", "Mean Latency", "Trial Position", "Stimulus", "Amplitude Confidence Interval")

TBTpeaks(data, "S111"); TBTpeaks(data, "S112"); TBTpeaks(data, "S121"); TBTpeaks(data, "S122"); TBTpeaks(data, "S131"); TBTpeaks(data, "S132"); TBTpeaks(data, "S141"); TBTpeaks(data, "S142"); 

#Plot the data

#First, clean the data as we did before to the proper filetypes
#Convert Trial Position to a factor, along with stimulus and participant
processeddata$`Trial Position` <- processeddata$`Trial Position`; processeddata$Stimulus <- as.factor(processeddata$Stimulus); processeddata$`Mean Amplitude (+/-25ms)` <- as.numeric(processeddata$`Mean Amplitude (+/-25ms)`); processeddata$`Mean Latency` <- as.numeric(processeddata$`Mean Latency`); processeddata$`Amplitude Confidence Interval` <- as.numeric(processeddata$`Amplitude Confidence Interval`)
#Reorder the factor levels
processeddata$`Trial Position` <- factor(processeddata$`Trial Position`, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49,  50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80))

tbtplot <- ggplot(processeddata, aes(processeddata$`Trial Position`, processeddata$`Mean Amplitude (+/-25ms)`, colour = Stimulus)) + geom_line(aes(group = Stimulus)) + geom_ribbon(aes(ymin=(processeddata$`Mean Amplitude (+/-25ms)` - processeddata$`Amplitude Confidence Interval`), ymax=(processeddata$`Mean Amplitude (+/-25ms)` + processeddata$`Amplitude Confidence Interval`)), linetype=2, alpha=0.1)
tbtplot

#Subplots by block using bbplot function. Inputs are dataset, stim1, stim2, and output plot name.

bbplot(processeddata, "S111", "S112", "Block1")
Block1
bbplot(processeddata, "S121", "S122", "Block2")
Block2
bbplot(processeddata, "S131", "S132", "Block3")
Block3
bbplot(processeddata, "S141", "S142", "Block4")
Block4


sbbplot(processeddata, "S111", "S112", "Block1Smooth")
Block1Smooth
sbbplot(processeddata, "S121", "S122", "Block2Smooth")
Block2Smooth
sbbplot(processeddata, "S131", "S132", "Block3Smooth")
Block3Smooth
sbbplot(processeddata, "S141", "S142", "Block4Smooth")
Block4Smooth
#NOTE:
#Block trial structure:
# Block 1
# First half: 50/50 trial 1/2
# Second half: 25/75 trial 1/2

# Block 2
# First half: 25/75 trial 1/2
# Second half: 50/50 trial 1/2

# Block 3
# Entire block: 25/75 trial 1/2

# Block 4
# Entire block: 50/50 trial 1/2

#Produce plot sheets
plot_grid(Block1, Block2, Block3, Block4, 
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)

plot_grid(Block1Smooth, Block2Smooth, Block3Smooth, Block4Smooth, 
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)

####Grand Averages####
#Data manipulation
mean_data <- group_by(data, Stimulus, TrialPosition, ms) %>%
  summarise(uV = mean(uV, na.rm = TRUE))

#First, clean the data as we did before to the proper structure
#Convert Trial Position to a factor, along with stimulus and participant
mean_data$TrialPosition <- as.factor(mean_data$TrialPosition); mean_data$Stimulus <- as.factor(mean_data$Stimulus); mean_data$uV <- as.numeric(mean_data$uV);# mean_data$ms <- as.factor(mean_data$ms);
#Reorder the factor levels
mean_data$TrialPosition <- factor(mean_data$TrialPosition, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49,  50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80))
#mean_data$ms <- factor(mean_data$ms, levels = c(seq(-200,600,4)))
mean_data$ms <- as.numeric(mean_data$ms)

#Split into Pre and Post Flip
prevector <- seq(1,40,1)
postvector <- seq(41,80,1)
preFGAdata <- mean_data[mean_data$TrialPosition %in% prevector,]
postFGAdata <- mean_data[mean_data$TrialPosition %in% postvector,]


#Plot setup
#Colours of your lines
colourscw <- c("#505050","#909090")
#Type of line
linetypecw = c("solid", "solid")
#Y Axis Values range
lineylim = range(-5,30)
#Y Axis label
lineylab = expression(paste("Amplitude (", mu, "V) +/- 25ms", sep ="")) #Amplitude (Î¼V)
#X Axis label
linexlab = "Time (ms)"

#Now to plot
PreFGA <- ggplot(na.omit(preFGAdata), aes(ms, uV, colour = Stimulus)) + stat_summary(fun.y = mean, geom = "line") + #+ geom_line(aes(group = Stimulus)) + #geom_ribbon(aes(ymin=(full$`Mean Amplitude (+/-25ms)` - full$`Amplitude Confidence Interval`), ymax=(full$`Mean Amplitude (+/-25ms)` + full$`Amplitude Confidence Interval`)), linetype=2, alpha=0.05) + #Theme-ing stuff
  #scale_color_manual(values = c("#505050","#909090")) +
  #scale_linetype_manual(values = linetypecw) + 
  scale_y_reverse(expand = c(0, 0)) + 
  coord_cartesian(ylim = lineylim) +
  scale_x_continuous(breaks = round(seq(min(-200), max(600), by = 100),1),expand = c(0,0)) +
  
  #Labels
  xlab(linexlab) + #X axis label
  ylab(lineylab) +#Y axis label
  
  #Legend
  theme_bw()+
  theme(legend.position = c(0.08, 0.16)) +#Position of a legend
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
#Plot Grand Average
PreFGA

#Now to plot
PostFGA <- ggplot(na.omit(postFGAdata), aes(ms, uV, colour = Stimulus)) + stat_summary(fun.y = mean, geom = "line") + #+ geom_line(aes(group = Stimulus)) + #geom_ribbon(aes(ymin=(full$`Mean Amplitude (+/-25ms)` - full$`Amplitude Confidence Interval`), ymax=(full$`Mean Amplitude (+/-25ms)` + full$`Amplitude Confidence Interval`)), linetype=2, alpha=0.05) + #Theme-ing stuff
  #scale_color_manual(values = c("#505050","#909090")) +
  #scale_linetype_manual(values = linetypecw) + 
  scale_y_reverse(expand = c(0, 0)) + 
  coord_cartesian(ylim = lineylim) +
  scale_x_continuous(breaks = round(seq(min(-200), max(600), by = 100),1),expand = c(0,0)) +
  
  #Labels
  xlab(linexlab) + #X axis label
  ylab(lineylab) +#Y axis label
  
  #Legend
  theme_bw()+
  theme(legend.position = c(0.08, 0.16)) +#Position of a legend
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
#Plot Grand Average
PostFGA

####Pre-Post Flip Stats####
#Perhaps start from the mean data and add grouping for pre-post?
stats <- mean_data
stats$TrialPosition <- as.numeric(stats$TrialPosition)
#Below line sets Group to 0 if pre, 1 if post.
stats = within(stats, {
  Group = ifelse(TrialPosition <= 40, 0, 1)
})
stats <- stats[ which(stats$ms > 250 & stats$ms < 400), ] #This line truncates to just the P300 time window. Stats should only be run on this window.

#Perform the 2x2 Factorial ANOVA
#compmodel <- anova(lm(uV ~ Group * Stimulus, data = stats))
stats$Stimulus <- as.factor(stats$Stimulus)
stats$Group <- as.factor(stats$Group)
compmodel <- aov(lm(uV ~ Group * Stimulus, data = stats))
summary(compmodel)
#This Tukey comparison will show comparisons for ALL stimuli in ALL blocks. Focus on the important ones.
TukeyHSD(compmodel)

#Mean Printing Function
r1 <- aggregate(uV ~ Stimulus + Group, FUN=mean, data=stats, na.rm=T)
r1

#Display only significant Tukey's
kk<-TukeyHSD(compmodel, "Group:Stimulus", ordered = TRUE)
kk$`Group:Stimulus`
kk<-data.frame( kk$`Group:Stimulus`)
kk["p.adj"]
kk <- kk[ which(kk$p.adj < 0.05), ]
kk["p.adj"]

####Pre-Post Truncated####
stats <- stats[ which(stats$TrialPosition < 49 & stats$TrialPosition > 31), ]
compmodel <- aov(lm(uV ~ Group * Stimulus, data = stats))
summary(compmodel)
#This Tukey comparison will show comparisons for ALL stimuli in ALL blocks. Focus on the important ones.
TukeyHSD(compmodel)

#Mean Printing Function
r1 <- aggregate(uV ~ Stimulus + Group, FUN=mean, data=stats, na.rm=T)
r1

#Display only significant Tukey's
kk<-TukeyHSD(compmodel, "Group:Stimulus", ordered = TRUE)
kk$`Group:Stimulus`
kk<-data.frame( kk$`Group:Stimulus`)
kk["p.adj"]
kk <- kk[ which(kk$p.adj < 0.05), ]
kk["p.adj"]

####Delta Oddball####
# oddstats <- stats
# oddstats %>%
#   group_by_(.dots=c("Group", "Stimulus")) %>%
#   arrange(ms) %>%
#   summarize(x=mean(uV))
#Below line sets Group to 0 if pre, 1 if post.
data$TrialPosition <- as.numeric(data$TrialPosition)
data = within(data, {
  Group = ifelse(TrialPosition <= 40, 0, 1)
})
oddstats <- data[ which(data$ms > 250 & data$ms < 400), ] #This line truncates to just the P300 time window. Stats should only be run on this window.

#Folded area is what would be for t.testing the Pre-Post Oddball change, but I have rather gone for a "9 trials pre and post" approach to detect if the established oddball changed in response to learning.
temp1 <- oddstats[ which(oddstats$Group == 0 & oddstats$Stimulus == "S111"), ]; temp2 <- oddstats[ which(oddstats$Group == 0 & oddstats$Stimulus == "S112"), ]; B1Pre <- cbind(temp1, temp2); temp1 <- NULL; temp2 <- NULL
temp1 <- oddstats[ which(oddstats$Group == 1 & oddstats$Stimulus == "S111"), ]; temp2 <- oddstats[ which(oddstats$Group == 1 & oddstats$Stimulus == "S112"), ]; B1Post <- cbind(temp1, temp2); temp1 <- NULL; temp2 <- NULL
temp1 <- oddstats[ which(oddstats$Group == 0 & oddstats$Stimulus == "S121"), ]; temp2 <- oddstats[ which(oddstats$Group == 0 & oddstats$Stimulus == "S122"), ]; B2Pre <- cbind(temp1, temp2); temp1 <- NULL; temp2 <- NULL
temp1 <- oddstats[ which(oddstats$Group == 1 & oddstats$Stimulus == "S121"), ]; temp2 <- oddstats[ which(oddstats$Group == 1 & oddstats$Stimulus == "S122"), ]; B2Post <- cbind(temp1, temp2); temp1 <- NULL; temp2 <- NULL
temp1 <- oddstats[ which(oddstats$Group == 0 & oddstats$Stimulus == "S131"), ]; temp2 <- oddstats[ which(oddstats$Group == 0 & oddstats$Stimulus == "S132"), ]; B3Pre <- cbind(temp1, temp2); temp1 <- NULL; temp2 <- NULL
temp1 <- oddstats[ which(oddstats$Group == 1 & oddstats$Stimulus == "S131"), ]; temp2 <- oddstats[ which(oddstats$Group == 1 & oddstats$Stimulus == "S132"), ]; B3Post <- cbind(temp1, temp2); temp1 <- NULL; temp2 <- NULL
temp1 <- oddstats[ which(oddstats$Group == 0 & oddstats$Stimulus == "S141"), ]; temp2 <- oddstats[ which(oddstats$Group == 0 & oddstats$Stimulus == "S142"), ]; B4Pre <- cbind(temp1, temp2); temp1 <- NULL; temp2 <- NULL
temp1 <- oddstats[ which(oddstats$Group == 1 & oddstats$Stimulus == "S141"), ]; temp2 <- oddstats[ which(oddstats$Group == 1 & oddstats$Stimulus == "S142"), ]; B4Post <- cbind(temp1, temp2); temp1 <- NULL; temp2 <- NULL


#Truncate the data set to only the 9 trials pre-post the flip
oddstats <- oddstats[ which(oddstats$TrialPosition < 49 & oddstats$TrialPosition > 31), ]

#Magically chunk the data together and make variables containing only the Oddball data
oddchunk <- function(group = 0, block = 1){
  stimnum1 <- ""; stimnum2 <- ""
  stimnum1 <- paste0("S1", block, "1")
  stimnum2 <- paste0("S1", block, "2")
  
  temp1 <- oddstats[ which(oddstats$Group == group & oddstats$Stimulus == stimnum1), ]; temp2 <- oddstats[ which(oddstats$Group == group & oddstats$Stimulus == stimnum2), ];
  #Pulls the data frame together to find each ~participant~ mean.
  temp1 <- aggregate(uV ~ Participant + ms + Group + Stimulus, temp1, mean);
  temp2 <- aggregate(uV ~ Participant + ms + Group + Stimulus, temp2, mean);
  temp <-  merge(temp1, temp2, by = c("Participant", "ms", "Group"), all = T); temp1 <- NULL; temp2 <- NULL; temp$Oddball <- (temp$uV.x - temp$uV.y);
  #temp <- aggregate(. ~ Participant, temp, mean) #This line if uncommented will spit out just the mean P300 peak amplitude for that participant, stripping out the time aspect.
  #temp$Oddball #This line will set the output to contain only the Oddball uV values. If selected, comment the line below
  temp
}

#Use the function to output the data sectioned by block. ALL participant data is included in these so variability is well accounted for.
B1Pre <- oddchunk(group = 0, block = 1)
B1Post <- oddchunk(group = 1, block = 1)
B2Pre <- oddchunk(group = 0, block = 2)
B2Post <- oddchunk(group = 1, block = 2)
B3Pre <- oddchunk(group = 0, block = 3)
B3Post <- oddchunk(group = 1, block = 3)
B4Pre <- oddchunk(group = 0, block = 4)
B4Post <- oddchunk(group = 1, block = 4)

#Now compare pre and post oddball for each block
t.test(B1Pre$Oddball, B1Post$Oddball, paired = T)
t.test(B2Pre$Oddball, B2Post$Oddball, paired = T)
t.test(B3Pre$Oddball, B3Post$Oddball, paired = T)
t.test(B4Pre$Oddball, B4Post$Oddball, paired = T)

#Maybe mean the data across trial positions first?


#And now with the realization that we must plot all participants by themselves. Oh and an oddball stats set.

####Participant Plots####
PPlotS142 <- ggplot(na.omit(data[ which(data$Stimulus == "S142"), ]), aes(ms, uV, linetype = Participant)) + stat_summary(fun.y = mean, geom = "line") + 
  scale_y_reverse(expand = c(0, 0)) + 
  coord_cartesian(ylim = range(-10,30)) +
  scale_x_continuous(breaks = round(seq(min(-200), max(600), by = 100),1),expand = c(0,0)) +
  
  #Labels
  xlab(linexlab) + #X axis label
  ylab(lineylab) +#Y axis label
  
  #Legend
  theme_bw()+
  #theme(legend.position = c(0.08, 0.16)) +#Position of a legend
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
#Plot Grand Average
PPlotS142

####Difference Wave Analysis####
#Naming convention:
#PrB1 = Pre Block 1
#PoB1 = Post Block 1
PrB1 <- preFGAdata[preFGAdata$Stimulus %in% "S111", ]
temp <- preFGAdata[preFGAdata$Stimulus %in% "S112", ]
PrB1$uV <- dwdata$uV - temp$uV

# PrB2 <- preFGAdata[preFGAdata$Stimulus %in% "S111", ]
# temp <- preFGAdata[preFGAdata$Stimulus %in% "S112", ]
# PrB2$uV <- dwdata$uV - temp$uV
# 
# PrB1 <- preFGAdata[preFGAdata$Stimulus %in% "S111", ]
# temp <- preFGAdata[preFGAdata$Stimulus %in% "S112", ]
# PrB1$uV <- dwdata$uV - temp$uV
# 
# PrB1 <- preFGAdata[preFGAdata$Stimulus %in% "S111", ]
# temp <- preFGAdata[preFGAdata$Stimulus %in% "S112", ]
# PrB1$uV <- dwdata$uV - temp$uV

####Slope Anaylsis####
#Here we will take the mean peak information, calculate the instantenous slope and plot it. This should show how much the P300 amplitude is changing at any point. Block 4 should hover around zero
# slope  <-  function(x){
#   if(all(is.na(x)))
#     # if x is all missing, then lm will throw an error that we want to avoid
#     return(NA)
#   else
#     return(coef(lm(I(1:3)~x))[2])
# }

runslope <- function(df, col, stimcol, stim){
  prog <- 0
  temp <- df[df[,4] == stim, ]
  prog <- prog + 1; print(prog)
  #Collect slopes
  #temp$slope  <- apply(temp$col, 1,slope)
  for (i in (1:nrow(temp))){
    if (i < nrow(temp)){temp[i,6] <- (temp[(i+1),1]-temp[i,1])} else {temp[i,6] <- (temp[(i-1),1]-temp[i,1])}
    prog <- prog + 1; print(prog)}
  #print(temp)
  Slope <<-temp[,6]
}

runslope(processeddata, `Mean Amplitude (+/-25ms)`, "Stimulus", "S111")

processeddata$Slope <- 0

processeddata[processeddata$Stimulus == "S111"] <- Slope

processeddata[processeddata$Stimulus == "S111"] <- merge(processeddata, runslope(processeddata, `Mean Amplitude (+/-25ms)`, "Stimulus", "S111"))

processeddata$Stimulus
processeddata$`Mean Amplitude (+/-25ms)`
