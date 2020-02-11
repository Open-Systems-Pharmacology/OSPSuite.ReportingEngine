function [figtxt,figtxtTable,legendEntries] = textVPC(WSettings,figureType,inputArray) 
% TEXTVPCMEANMODEL creates text fo figures and tables
%
% Inputs: 
%       WSettings (structure)    definition of properties used in all
%                   workflow functions see GETDEFAULTWORKFLOWSETTINGS
%       figureType (string) type of figure for which the text is needed
%       inputArray (cellarray) inputs, differ for each histogram type
%

% Open Systems Pharmacology Suite;  http://open-systems-pharmacology.org



% initialize outputs
figtxt = ''; %#ok<NASGU>
figtxtTable = '';
legendEntries = {}; %#ok<NASGU>

switch figureType
    
        % histogram for physiological properties
    case 'physHist'
        [figtxt,figtxtTable,legendEntries] = textVPCPopulationPhysHist(inputArray{1},inputArray{2},inputArray{3},...
            inputArray{4},inputArray{5},inputArray{6});

    % shaded Area for physiological properties
    case 'physShA'
        [figtxt,figtxtTable,legendEntries] = textVPCPopulationphysShA(inputArray{1},inputArray{2},inputArray{3},inputArray{4},...
            inputArray{5},inputArray{6});
        
    % Time profile
    case 'tpShadedArea'
        [figtxt,figtxtTable,legendEntries] = textVPCTimeprofileTpShadedArea(inputArray{1},inputArray{2},inputArray{3},...
            inputArray{4},inputArray{5},inputArray{6},inputArray{7},inputArray{8},inputArray{9},inputArray{10});

    % Time profile with VPC
    case 'tpVPCpar'
        [figtxt,figtxtTable,legendEntries] = textVPCTimeprofileTpVPC('parameterUncertainty',inputArray{1},inputArray{2},inputArray{3},...
            inputArray{4},inputArray{5},inputArray{6},inputArray{7});

    case 'tpVPCdata'
        [figtxt,figtxtTable,legendEntries] = textVPCTimeprofileTpVPC('dataUncertainty',inputArray{1},inputArray{2},inputArray{3},...
            inputArray{4},inputArray{5},inputArray{6},inputArray{7});

    case 'tpVPCdataPar'
        [figtxt,figtxtTable,legendEntries] = textVPCTimeprofileTpVPC('dataAndParameterUncertainty',inputArray{1},inputArray{2},inputArray{3},...
            inputArray{4},inputArray{5},inputArray{6},inputArray{7});

        
    % Boxwhisker of PK Parmeter
    case 'pkBW'
        [figtxt,figtxtTable,legendEntries] = textVPCPopulationPkBW(WSettings,inputArray{1},inputArray{2},inputArray{3},...
            inputArray{4},inputArray{5},inputArray{6});
        
        
    case 'pkBWRatio'
        [figtxt,figtxtTable,legendEntries] = textVPCPopulationPkBWRatio(WSettings,inputArray{1},...
            inputArray{2},inputArray{3},inputArray{4},inputArray{5},inputArray{6});
        
    % shaded Area for PK Parameter
    case 'pkShA'
        [figtxt,figtxtTable,legendEntries] = textVPCPopulationPkShA(inputArray{1},inputArray{2},inputArray{3},inputArray{4},...
            inputArray{5},inputArray{6},inputArray{7},inputArray{8});

    case 'pkShARatio'
        [figtxt,figtxtTable,legendEntries] = textVPCPopulationPkShARatio(inputArray{1},inputArray{2},inputArray{3},inputArray{4},...
            inputArray{5},inputArray{6},inputArray{7},inputArray{8},inputArray{9});

            
    % Predicted vs observed
    case 'tpPredVsObs'
        [figtxt,legendEntries] = textVPCPredictedVsObserved(inputArray{1},inputArray{2},inputArray{3},inputArray{4},inputArray{5});
         
     % residuals vs Time 
    case 'tpResVsTime'
        [figtxt,legendEntries] = textVPCResidualsVsTime(inputArray{1},inputArray{2},inputArray{3},inputArray{4},inputArray{5});
        
     % residuals vs Prediction
    case 'tpResVsY'
        [figtxt,legendEntries] = textVPCResidualsVsY(inputArray{1},inputArray{2},inputArray{3},inputArray{4},inputArray{5});
        
    % residuals as Histogram
    case 'histRes'
        [figtxt,legendEntries] = textVPCResidualsAsHistogram(inputArray{1},inputArray{2});


    % residuals as qqPlot
    case 'qqRes'
        [figtxt,legendEntries] = textVPCResidualsAsQQPlot(inputArray{1},inputArray{2});

    otherwise
        error('unknown figureType');

end


return


function  [figtxt,legendEntries] = textVPCPredictedVsObserved(output,simulation,nameData,scale,lloq)


% get name and figure description
figtxt = sprintf('Predicted vs observed of %s for %s. Data source: %s.',output,simulation,nameData);
                
if ~isnan(lloq)
    figtxt = sprintf('%s Data below lower limit of quantification (lloq) are plotted as open symbols as lloq/2.',figtxt);
end

switch scale
    case 'lin'
        figtxt = sprintf('%s Prediction and observations are plotted in a linear scale.',figtxt);
    case 'log'
        figtxt = sprintf('%s Prediction and observations are plotted in a logarithmic scale.',figtxt);
    otherwise
        error('unknown scale')

end


% legend % second entry Ref not needed for simulation
legendEntries{1} = sprintf('symbols: %s',nameData);
legendEntries{2} = 'line of identity';
if ~isnan(lloq)
     legendEntries{end+1} = 'lower limit of quantification';
end

return

function [figtxt,legendEntries] = textVPCResidualsVsY(output,simulation,nameData,scale,lloq)

% get name and figure description
switch scale
    case 'lin'
        figtxt = sprintf('Linear residuals of %s vs predicted values for %s. Data source: %s.',output,simulation,nameData);
    case 'log'
        figtxt = sprintf('Logarithmic residuals vs predicted values of %s for %s. Data source: %s.',output,simulation,nameData);
    otherwise
        error('unknown scale')

end

if ~isnan(lloq)
    figtxt = sprintf('%s For data below lower limit of quantification (lloq) the residuals were calculated using lloq/2 and as open symbols.',figtxt);
end


legendEntries{1} = sprintf('symbols: %s',nameData);

return

function [figtxt,legendEntries] = textVPCResidualsVsTime(output,simulation,nameData,scale,lloq)

% get name and figure description
switch scale
    case 'lin'
        figtxt = sprintf('Linear residuals vs time of %s for %s. Data source: %s.',output,simulation,nameData);
    case 'log'
        figtxt = sprintf('Logarithmic residuals vs time of %s for %s. Data source: %s.',output,simulation,nameData);
    otherwise
        error('unknown scale')

end

if ~isnan(lloq)
    figtxt = sprintf('%s For data below lower limit of quantification (lloq) the residuals were caluclated using lloq/2 and as open symbols.',figtxt);
end


legendEntries{1} = sprintf('symbols: %s',nameData);

return

function [figtxt,legendEntries] = textVPCResidualsAsHistogram(outputNameList,simulation)

figtxt = sprintf('Distribution of residuals for %s',simulation);


legendEntries = outputNameList;

return

function [figtxt,legendEntries] = textVPCResidualsAsQQPlot(outputNameList,simulation)

figtxt = sprintf('Residuals for %s as quantile-quantile plot.',simulation);


legendEntries = outputNameList;

return


function  [figtxt,figtxtTable,legendEntries] = textVPCPopulationPhysHist(physProb,population,dataSource,popLabels,nPop,nData)


figtxt = sprintf('Distribution of %s for simulated %s',physProb,population);
if any(nData>0)
    if length(nData) ==1
        figtxt = sprintf('%s in comparison to observed data %s',figtxt,dataSource{1});
    else
        figtxt = sprintf('%s in comparison to observed data',figtxt);
    end
end
figtxt = sprintf('%s.',strtrim(figtxt));

figtxtTable = figtxt;

if length(popLabels)>1
    for iPop = 1:length(popLabels)
        legendEntries{iPop} = sprintf('simulated %s (n=%d)',popLabels{iPop},nPop(iPop)); %#ok<AGROW>
    end
else
    legendEntries{1} = sprintf('simulated %s (n=%d)',population,nPop(1));
end    

for iData = 1:length(nData)
    legendEntries{end+1} = sprintf('observed data %s (n=%d)',dataSource{iData},nData(iData)); %#ok<AGROW>
end

return


function  [figtxt,figtxtTable,legendEntries] = textVPCPopulationphysShA(physProbX,physProbY,population,referencePopulation,dataSource,nInd)


figtxt = sprintf('%s%s-dependence of %s for simulated %s.',upper(physProbX(1)),physProbX(2:end),physProbY,population); % Checked with ped
% Reference
if nInd(2)>0
    figtxt = sprintf('%s in comparison to simulated %s.',figtxt(1:end-1),referencePopulation); % Checked with ped
end
% Data
if nInd(3)>0
    figtxt = sprintf('%s Data %s.',figtxt(1:end-1),dataSource{1});
end

figtxtTable = figtxt;

legendEntries = {sprintf('Simulated <xxx> for %s (n=%d)',population,nInd(1)),...
    sprintf('Simulated <xxx> for %s (n=%d)',referencePopulation,nInd(2)),...
    sprintf('Observed <xxx> for %s (n=%d)',dataSource{1},nInd(3))};

return


function [figtxt,figtxtTable,legendEntries] = textVPCTimeprofileTpVPC(flag,output,simulation,...
    nameData,scale,lloq,nData,pop)

% initialize outputs            
legendEntries={''};

% get name and figure description
if ~isempty(pop)
    figtxtTable = sprintf('Time profile of %s for %s.',...
        output,simulation);
else            
    figtxtTable = sprintf('Time profile of %s for %s for %s.',...
        output,simulation,pop);
end       

switch flag
    case 'parameterUncertainty'
        figtxtTable = sprintf('%s Ranges show the uncertainty related to parameter uncertainty.',figtxtTable);
    case 'dataUncertainty'
        figtxtTable = sprintf('%s Ranges show the uncertainty related to data uncertainty',figtxtTable);
    case 'dataAndParameterUncertainty'
        figtxtTable = sprintf('%s Ranges show the uncertainty related to parameter and data uncertainty',figtxtTable);
end

figtxt = figtxtTable;
if nData>0
    figtxt = sprintf('%s Data source: %s.',figtxt,nameData);
end

if ~isnan(lloq)
    figtxt = sprintf('%s Data below lower limit of quantification (lloq) are plotted as open symbols as lloq/2.',figtxt);
end

switch scale
    case 'lin'
        figtxt = sprintf('%s Time profiles are plotted in a linear scale.',figtxt);
    case 'log'
        figtxt = sprintf('%s Time profiles are plotted in a logarithmic scale.',figtxt);
    otherwise
        error('unknown scale')
end


% legend % second entry Ref not needed for simulation
if nData>0
    legendEntries{3} = sprintf('symbols: %s',nameData);
end
if ~isnan(lloq)
     legendEntries{end+1} = 'lower limit of quantification';
end

return

function [figtxt,figtxtTable,legendEntries] = textVPCTimeprofileTpShadedArea(output,simulation,simulationRef,...
    nameData,timerangetxt,scale,lloq,nInd,simLabel,refLabel)


% get name and figure description
figtxtTable = sprintf('Time profiles of %s for %s',...
    output,simulation);

if ~isempty(simulationRef)
    figtxtTable = sprintf('%s compared to simulated %s for %s',figtxtTable,output,simulationRef);
end

if ~isempty(timerangetxt)
    figtxtTable = sprintf('%s %s.',figtxtTable,timerangetxt);
else
    figtxtTable = sprintf('%s.',figtxtTable);
end

figtxt = figtxtTable;
if nInd(3)>0
    figtxt = sprintf('%s Data source: %s.',figtxt,nameData);
end

if ~isnan(lloq)
    figtxt = sprintf('%s Data below lower limit of quantification (lloq) are plotted as open symbols as lloq/2.',figtxt);
end

switch scale
    case 'lin'
        figtxt = sprintf('%s Time profiles are plotted in a linear scale.',figtxt);
    case 'log'
        figtxt = sprintf('%s Time profiles are plotted in a logarithmic scale.',figtxt);
    otherwise
        error('unknown scale')
end


% legend % second entry Ref not needed for simulation
if nInd(1)>1
    legendEntries = {sprintf('Simulated <xxx> for %s (n=%d)',simLabel,nInd(1)),...
        sprintf('Simulated <xxx> for %s (n=%d)',refLabel,nInd(2)),...
        sprintf('Symbols: observed data %s (n=%d)',nameData,nInd(3)),...
        sprintf('Simulated mean model for %s',simLabel)};
else
    legendEntries = {sprintf('Simulated mean individual'),...
        '',...
        sprintf('Symbols: observed data %s (n=%d)',nameData,nInd(3))};
end

if ~isnan(lloq)
     legendEntries{end+1} = 'Lower limit of quantification';
end

return


function  [figtxt,figtxtTable,legendEntries] = textVPCPopulationPkBW(WSettings,yLabel,output,reportNames,popReportNames,scale,refPopPK)


% get table header
figtxtTable = sprintf('Percentile of %s of %s.',yLabel,output);

% check if extrameas are plotted
extremaTxt = '';
if WSettings.boxwhiskerWithExtrema
 extremaTxt = ' and extremes as open circles';
end

% get figure description
figtxt = sprintf('%s of %s shown as box whisker plot, which indicate the %d^{th}, %d^{th}, %d^{th}, %d^{th}, and %d^{th} percentiles%s',...
    yLabel,output,WSettings.displayPercentiles(1),WSettings.displayPercentiles(2),WSettings.displayPercentiles(3),...
    WSettings.displayPercentiles(4),WSettings.displayPercentiles(5),extremaTxt);

% add description for refernce popPK
if ~isempty(refPopPK)
    figtxt = sprintf('%s in comparison to %s (symbols at the right)',figtxt,refPopPK);
end


% set scale text
switch scale
    case 'lin'
        figtxt = sprintf('%s in a linear scale.',figtxt);
    case 'log'
        figtxt = sprintf('%s in a logarithmic scale.',figtxt);
end

% get xLabels
if length(reportNames) ==1
    legendEntries = {''};
elseif length(unique(popReportNames)) == length(popReportNames)
    legendEntries = popReportNames;
elseif  length(unique(reportNames)) == length(reportNames)
     legendEntries = reportNames;
else
     legendEntries = strcat(popReportNames,'; ',reportNames);
end

% add description for refernce popPK
if ~isempty(refPopPK)
    legendEntries = [{refPopPK} legendEntries];
end

return

function  [figtxt,figtxtTable,legendEntries] = textVPCPopulationPkBWRatio(WSettings,yLabel,output,reportNames,popReportNames,scale,refReportName)


% get table header
figtxtTable = sprintf('Percentile of %s of %s normalized to individuals of %s.',yLabel,output,refReportName);

% check if extrameas are plotted
extremaTxt = '';
if WSettings.boxwhiskerWithExtrema
 extremaTxt = ' and extremes as open circles';
end

% get figure description
figtxt = sprintf('%s of %s shown as box whisker plot, which indicate the %d, %d, %d, %d, and %d percentiles%s',...
    yLabel,output,WSettings.displayPercentiles(1),WSettings.displayPercentiles(2),WSettings.displayPercentiles(3),...
    WSettings.displayPercentiles(4),WSettings.displayPercentiles(5),extremaTxt);

% set scale text
switch scale
    case 'lin'
        figtxt = sprintf('%s in a linear scale.',figtxt);
    case 'log'
        figtxt = sprintf('%s in a logarithmic scale.',figtxt);
end

% get xLabels
if length(reportNames) ==1
    legendEntries = {''};
elseif length(unique(popReportNames)) == length(popReportNames)
    legendEntries = popReportNames;
elseif  length(unique(reportNames)) == length(reportNames)
     legendEntries = reportNames;
else
     legendEntries = strcat(popReportNames,'; ',reportNames);
end

return


function  [figtxt,figtxtTable,legendEntries] = textVPCPopulationPkShA(xPhys,yLabel,output,reportNames,refReportName,nInd,scale,refPopPK)

% get table header
figtxtTable = sprintf('%s%s dependency of %s %s for simulated %s in comparison to simulated %s',...
    xPhys(1),xPhys(2:end),yLabel,output,reportNames,refReportName);

if ~isempty(refPopPK)
    figtxtTable = sprintf('%s and in comparison to %s%s',figtxtTable,lower(refPopPK(1)),refPopPK(2:end));
end

 
% set scale text
switch scale
    case 'lin'
        figtxt = sprintf('%s in a linear scale.',figtxtTable);
    case 'log'
        figtxt = sprintf('%s in a logarithmic scale.',figtxtTable);
    otherwise
        error('unknown scale')

end


legendEntries = {sprintf('Simulated <xxx> for %s (n=%d)',reportNames,nInd(1)),...
    sprintf('Simulated <xxx> for %s (n=%d)',refReportName,nInd(2)),...
    refPopPK};


return

function  [figtxt,figtxtTable,legendEntries] = ...
    textVPCPopulationPkShARatio(xPhys,yLabel,output,reportNames,refReportName,nInd,scale,refPopPK,legendTextMean)


% get table header
figtxtTable = sprintf('%s%s dependency of %s %s for simulated %s as fraction of %s of simulated %s',...
    xPhys(1),xPhys(2:end),yLabel,output,reportNames,legendTextMean,refReportName);

if ~isempty(refPopPK)
    figtxtTable = sprintf('%s and in comparison to %s%s',figtxtTable,lower(refPopPK(1)),refPopPK(2:end));
end


% set scale text
switch scale
    case 'lin'
        figtxt = sprintf('%s in a linear scale.',figtxtTable);
    case 'log'
        figtxt = sprintf('%s in a logarithmic scale.',figtxtTable);
    otherwise
        error('unknown scale')

end

legendEntries = {sprintf('Simulated <xxx> for %s (n=%d)',reportNames,nInd(1)),...
    sprintf('Simulated <xxx> for %s (n=%d)',refReportName,nInd(2)),...
    refPopPK};


return
