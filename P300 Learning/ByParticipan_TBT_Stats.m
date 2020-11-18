%https://www.mathworks.com/help/matlab/ref/movvar.html
%Moving variance check of P300 data, using the movvar commant linked above.
%This is mean to check when the variance significantly drops, indicating an
%inflection point
close all; clear all; clc;

%Read in all the data and their filenames
%% User Input
peak_type = 2; %1 for mean peak, 2 for max peak, 3 for min peak
component = 1; %1 for P300, 2 for N200
start_time = 300/2; %Start to check for peak
end_time = 500/2; %End to check for peak
time_adjust = 50; %How wide (+/-) is the peak window

%% Code
suffix = 'a_oddball_Control Avg.txt'; %End of the desired filename
suffix2 = 'a_oddball_Oddball Avg.txt'; %End of the desired filename
suffix3 = 'b_oddball_Control Avg.txt'; %End of the desired filename
suffix4 = 'b_oddball_Oddball Avg.txt'; %End of the desired filename
%working_dir = uigetdir('','Find folder with your data'); %Point to where the data is
working_dir = 'D:\Dropbox (Krigolson Lab)\Projects\2017_EEG_LululemonYoga_Krigolson\Oddball Data\Export'
cd(working_dir); %Change directory to where the data is
filenames = dir(strcat('*', suffix)); %Find all filenames Rest
filenames2 = dir(strcat('*', suffix2)); %Find all filenames Yoga
filenames3 = dir(strcat('*', suffix3)); %Find all filenames Rest
filenames4 = dir(strcat('*', suffix4)); %Find all filenames Yoga

disp('Running...'); %Running message

for subject_counter = 1:length(filenames)
    
    %Rest Data
    Data = []; %Clear variable
    %Import Current Participant Data
    Data = importdata(filenames(subject_counter).name);
    %Raw frequency values for all participants
    FullData1(subject_counter,:) = Data.data(1,:);
    
    Data2 = []; %Clear variable
    %Import Current Participant Data
    Data2 = importdata(filenames2(subject_counter).name);
    %Raw frequency values for all participants
    FullData2(subject_counter,:) = Data2.data(1,:);
    
    Data3 = []; %Clear variable
    %Import Current Participant Data
    Data3 = importdata(filenames3(subject_counter).name);
    %Raw frequency values for all participants
    FullData3(subject_counter,:) = Data3.data(1,:);
    
    Data4 = []; %Clear variable
    %Import Current Participant Data
    Data4 = importdata(filenames4(subject_counter).name);
    %Raw frequency values for all participants
    FullData4(subject_counter,:) = Data4.data(1,:);
    
    %Mean of each frequency band per participant
    FullDataDiff(subject_counter,:) = FullData2(subject_counter,:) - FullData1(subject_counter,:);
    FullDataDiff2(subject_counter,:) = FullData4(subject_counter,:) - FullData3(subject_counter,:);
    
    if component == 1
        [MeanFreq1(subject_counter,1) MeanFreq1(subject_counter,2)] = max(FullDataDiff(subject_counter,start_time:end_time));
        [MeanFreq2(subject_counter,1) MeanFreq2(subject_counter,2)] = max(FullDataDiff2(subject_counter,start_time:end_time));
    end
    
    if component == 2
        [MeanFreq1(subject_counter,1) MeanFreq1(subject_counter,2)] = min(FullDataDiff(subject_counter,start_time:end_time));
        [MeanFreq2(subject_counter,1) MeanFreq2(subject_counter,2)] = min(FullDataDiff2(subject_counter,start_time:end_time));
    end
    
    Peak_Times(subject_counter,1) = (MeanFreq1(subject_counter,2)+start_time)*2;
    Peak_Times(subject_counter,2) = (MeanFreq2(subject_counter,2)+start_time)*2;
    
end

Mean_Peak_Time = round(mean(mean(Peak_Times(:,2)))); %Find peak
peak_start = round((Mean_Peak_Time - time_adjust)/2);
peak_end = round((Mean_Peak_Time + time_adjust)/2);

if peak_type == 1
    Peak_Means(:,1) = mean(FullDataDiff(:,peak_start:peak_end),2);
    Peak_Means(:,2) = mean(FullDataDiff2(:,peak_start:peak_end),2);
end

if peak_type == 2
    for pcount = 1:length(filenames)
        Peak_Means(pcount,1) = max(FullDataDiff(pcount,peak_start:peak_end));
        Peak_Means(pcount,2) = max(FullDataDiff2(pcount,peak_start:peak_end));
    end
end

if peak_type == 3
    for pcount = 1:length(filenames)
        Peak_Means(pcount,1) = min(FullDataDiff(pcount,peak_start:peak_end));
        Peak_Means(pcount,2) = min(FullDataDiff2(pcount,peak_start:peak_end));
    end
end

clc; disp('Complete'); %Finish message
%clear count Data Data2 FN subject_counter suffix suffix2 working_dir %Clear non-essential variables

%%
%Analysis

%Assessing point of inflection through variance measure


%Assessing point of inflection through one-sided power/linear fit tests
