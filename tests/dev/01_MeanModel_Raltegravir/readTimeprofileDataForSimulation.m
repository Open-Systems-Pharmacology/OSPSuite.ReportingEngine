function successInputCheck = readTimeprofileDataForSimulation(WSettings,dataTpFile,RunSet) 
% READTIMEPROFILEDATAFORSIMULATION read timeprofile nonmen data and save the as temporary structure
%
% successInputCheck = readTimeprofileDataForSimulation(WSettings,dataTpFile,RunSet) 
%
% Inputs:
%   WSettings (structure)    definition of properties used in all
%                   workflow functions see GETDEFAULTWORKFLOWSETTINGS
%   dataTpFile (cellarray of strings 3x2) first entry name of nonmem file
%                   second entry name of dictionary
%   RunSet (structure) PopRunSet or MeanModelSet with information of filter and output list
%
% Outputs
%   successInputCheck (boolean) is false, if serious errors occured during data read in 


% Open Systems Pharmacology Suite;  http://open-systems-pharmacology.org

successInputCheck = true; %#ok<NASGU>

% collect filters
k=0;
filterList = {};
for iSet = 1:length(RunSet)
    
    if ~isempty(RunSet(iSet).dataTpFilter)
        k=k+1;
        filterList{k} = RunSet(iSet).dataTpFilter; %#ok<AGROW>
        filterIndex(iSet,1) = k; %#ok<AGROW>
    else
        filterIndex(iSet,1) = nan; %#ok<AGROW>
    end
    
    load(fullfile('tmp',RunSet(iSet).name,'outputList.mat'),'OutputList');
    
    for iO = 1:length(OutputList)
        
        if ~isempty(OutputList(iO).dataTpFilter)
            k=k+1;
            filterList{k,:} = OutputList(iO).dataTpFilter; %#ok<AGROW>
            filterIndex(iSet,iO+1) = k; 
        else
            filterIndex(iSet,iO+1) = nan;
        end

    end
end
    
    
% read nonmefile uses WSettings and dataTpFile
[successInputCheck,X,filter,Dict] = readNonmemFile(WSettings,dataTpFile,'timeprofile',filterList);

if ~successInputCheck
    return
end



% get fieldnamees of cavariates
jj = strcmp({Dict.type},'covariate');
fn_covariate = intersect(fieldnames(X),{Dict(jj).matlabID});


% get dataset for each popset
 
for iSet = 1:length(RunSet)
    
    % get temporary directory
    tmpDir = fullfile('tmp',RunSet(iSet).name);

    
    load(fullfile('tmp',RunSet(iSet).name,'outputList.mat'),'OutputList');
    
    % initialize TP
    for iO = 1:length(OutputList)
        TP{iO} = []; %#ok<AGROW>
    end

    
    % get filter of popset
    if ~isnan(filterIndex(iSet,1))
        jj_set = filter(:,filterIndex(iSet,1));
    else
        jj_set = false(size(X.stud));
    end
    
    % get unique identifier
    uniSTUD = unique(X.stud(jj_set));
        

    for iStud = 1:length(uniSTUD)
        
        jj_STUD = jj_set & X.stud == uniSTUD(iStud);
        
        uniSID = unique(X.sid(jj_STUD));
        
        for iSID = 1:length(uniSID)
            
            jj_SID = jj_STUD & X.sid == uniSID(iSID) ;
            
            % get index of dataset
            if exist('DataTP','var')
                indx = length(DataTP)+1;
            else
                indx = 1;
            end
            
            
            for iO = 1:length(OutputList)

                % get filter of popset
                if ~isnan(filterIndex(iSet,1+iO)) && filterIndex(iSet,1+iO)>0
                    jj_O = jj_SID & filter(:,filterIndex(iSet,1+iO));
                else
                    jj_O = false(size(X.stud));
                end
                
                if any(jj_O)
                    
                    % add to data structure
                    DataTP(indx).stud = uniSTUD(iStud); %#ok<AGROW>
                    DataTP(indx).sid = uniSID(iSID) ; %#ok<AGROW>
                    
                    % timeprofile
                    TP{iO}(indx).time = X.time(jj_O); 
                    TP{iO}(indx).dv = X.dv(jj_O); 
                    if isfield(X,'tad')
                        TP{iO}(indx).tad = X.tad(jj_O); 
                    end
                    TP{iO}(indx).isLloq = false(size( TP{iO}(indx).time)); 
                    TP{iO}(indx).lloq = nan; 
                    if isfield(X,'lloq')
                        lloq =X.lloq(jj_O);
                        jj_lloq = TP{iO}(indx).dv < lloq;
                        if any(jj_lloq)
                            TP{iO}(indx).isLloq(jj_lloq) = true; 
                            tmp =  unique(lloq(jj_lloq));
                            if length(tmp) > 1
                                writeToReportLog('WARNING',sprintf('non unique lloq for STUD %d SID %d', ...
                                    DataTP(indx).stud ,DataTP(indx).sid),false);
                            end
                            TP{iO}(indx).lloq = min(tmp); 
                        end

                    end
                
                    % covariates
                    for iFn = 1:length(fn_covariate)
                        DataTP(indx).(fn_covariate{iFn}) = X.(fn_covariate{iFn})(find(jj_SID,1)); 
                
                    end
                end
            end
        end
    end
    
    % save data
    if exist('DataTP','var')
        save(fullfile(tmpDir,'dataTp.mat'),'DataTP','TP','Dict');
        
        clear DataTP;
        clear TP;
    end
end

return
