% --- Executes just before NDA_EEG is made visible.
%function NDA_EEG_Setup(hObject, eventdata, handles, filename, pathname, varargin)
warning('off','MATLAB:serial:fread:unsuccessfulRead');

% BLUETOOTH CONFIGURATION
handles.dispallBT = 0; % set to 0 for displaying only NDA headset

btlist = instrhwinfo('Bluetooth');
btlistFilter = btlist.RemoteNames;

if ~handles.dispallBT && size(btlistFilter,1)~=0
    btKeep = [];
    for bt = 1:size(btlistFilter,1)
        if ~isempty(strfind(btlistFilter{bt},'NDA-C'))
            if strfind(btlistFilter{bt},'NDA-C') == 1
                btKeep = [btKeep bt];
            end
        end
        if ~isempty(strfind(btlistFilter{bt},'NDA_')) % TESTDEV devices
            if strfind(btlistFilter{bt},'NDA_') == 1
                btKeep = [btKeep bt];
            end
        end
    end
    btlistFilter = btlistFilter(btKeep);
end

if size(btlist.RemoteNames,1)==0
    msgbox({'Bluetooth not available.' 'Real-Time recording cannot be performed.'});
    %set(handles.btselect,'String','N/A');
    handles.btDev = 'N/A';
elseif size(btlistFilter,1)==0
    msgbox({'No NDA NDA-10 device found.' 'Real-Time recording cannot be performed.'...
        'Please pair your NDA NDA-10 device with the computer and click on the refresh button!'});
    %set(handles.btselect,'String','N/A');
    handles.btDev = 'N/A';
else
    %set(handles.btselect,'String',btlistFilter);
    handles.btDev = btlistFilter{1};
end

handles.recording=0;
handles.T=1;
handles.stop=0;
handles.AllChan=10;
handles.gain=ones(1,10)*1000/10^6; % converts units to uV

handles.nb = 20; % number of bits per electrode
handles.numparselet = 2;
if handles.recording
    system('NDA_EEG_Viewer&');
    return;
end % recording session already in progress

if strcmp(handles.btDev,'N/A')
    msgbox({'Device not available.' 'Please click on the refresh button.'});
    return;
end

%[filename,pathname]=uiputfile('*.mat','Select Folder to Save Session');
%VarName = evalin('base','VarNameFromBase');
pathname = evalin('base','pathname');
filename = evalin('base','filename');

if ~pathname  % in case user did not select a folder on startup of GUI
    return;
end
handles.fullname=strcat(pathname,filename);
dataloc = strcat(pathname,filename);
recLoadFlag = 1;
%guidata(hObject, handles);

save('tempdataloc.mat','dataloc');
save('recLoad.mat','recLoadFlag');

data=zeros(1,10);
time=0;
notesTIME=0;
notesNOTE={0};

try
    pause(0.01);
    save(handles.fullname,'data','time','notesTIME','notesNOTE','-v7.3');
catch
    msgbox({'Selected file unavailable.'; 'Please try again or create a new file.'},'Session File','Error')
    return
end

m=matfile(handles.fullname,'Writable',true);

%btConnEst = msgbox('Establishing File Record...');
%btChildButton = get(btConnEst,'Children');
%delete(btChildButton(1));
%drawnow;

     s = Bluetooth(handles.btDev,1);
     set(s,'InputBufferSize',1000*(handles.numparselet+(handles.nb*10/8))*handles.T);
     set(s,'Timeout',handles.T);
try
    fopen(s);
catch
    uiwait(msgbox({sprintf('%s not available.',handles.btDev); 'Please check if device is powered on.'},'Connection Not Established','Error'));
end

% try
%     s = Bluetooth(handles.btDev,1);
%     set(s,'InputBufferSize',1000*(handles.numparselet+(handles.nb*10/8))*handles.T);
%     set(s,'Timeout',handles.T);
%     fopen(s);
% catch
%     uiwait(msgbox({sprintf('%s not available.',handles.btDev); 'Please check if device is powered on.'},'Connection Not Established','Error'));
%     choice = questdlg(sprintf('Delete current session %s?',filename),'Session Data','Yes','No','No');
%     switch choice
%         case 'Yes'
%             delete(sprintf('%s%s',pathname,filename));
%             delete(btConnEst);
%             return
%         case 'No'
%             delete(btConnEst);
%             return
%         case ''
%             delete(btConnEst);
%             return
%     end
% end

%delete(btConnEst);

m.cStart=clock;
m.cStop=0;

%handles.recording=1;
% number of letters that is parsed as marker per 10 electrodes

% Choose default command line output for NDA_EEG
%handles.output = hObject;

% Update handles structure
%guidata(hObject, handles);
