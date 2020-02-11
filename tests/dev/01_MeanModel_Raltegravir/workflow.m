% Script to start a workflow
% Type: MeanModel workflow, mode default
% Purpose:: Building and evaluation of a PBPK model for raltegravir in adults
% M&S activity:: Raltegravir
% Validation level:: B
% Original author: XXXXX 06-Feb-2020 10:11:16
% 
%  HOW TO USE
%  this script has to be filed in your working directory together with your input files like the simulation xml
%  and the poulation csv
%  adjust description for your purpose
%  set the matlab directory to your working directory
%  start the script
% 
%  Script is intendended to run with Matlab 2017b 
% 


% global settings
% there are globale settings which are used in all functions.
WSettings = getDefaultWorkflowSettings('meanModel','default');

% Definitions of sets of Mean Model simulations
clear MeanModelSet
OutputList(1) = struct('pathID','Organism|PeripheralVenousBlood|Raltegravir|Plasma (Peripheral Venous Blood)','reportName','Raltegravir','displayUnit','µg/l','dataTpFilter','strcmp(OUTPUT,"Raltegravir_PLASMA")','residualScale','log','unitFactor',NaN,'pKParameterList',[]);
OutputList(1).pKParameterList = {'C_max','C_max_norm','t_max','C_tEnd','AUC','AUC_norm','AUC_inf','AUC_inf_norm','MRT','Thalf','CL','Vss','Vd';'µg/l','kg/l','h','µg/l','µg*h/l','kg*h/l','µg*h/l','kg*h/l','h','h','ml/min/kg','ml/kg','ml/kg'};
MeanModelSet(1) = struct('name','Lactose_formulation_10mg','reportName','Raltegravir 10 mg   (lactose formulation)','xml','Raltegravir 10 mg   (lactose formulation).xml','calculatePKParameterFh','','dataTpFilter','strcmp(Grouping,"10mg_")','dataReportName','Observed_Raltegravir 10 mg   (lactose formulation)','OutputList',OutputList);
clear OutputList
OutputList(1) = struct('pathID','Organism|PeripheralVenousBlood|Raltegravir|Plasma (Peripheral Venous Blood)','reportName','Raltegravir','displayUnit','µg/l','dataTpFilter','strcmp(OUTPUT,"Raltegravir_PLASMA")','residualScale','log','unitFactor',NaN,'pKParameterList',[]);
OutputList(1).pKParameterList = {'C_max','C_max_norm','t_max','C_tEnd','AUC','AUC_norm','AUC_inf','AUC_inf_norm','MRT','Thalf','CL','Vss','Vd';'µg/l','kg/l','h','µg/l','µg*h/l','kg*h/l','µg*h/l','kg*h/l','h','h','ml/min/kg','ml/kg','ml/kg'};
MeanModelSet(2) = struct('name','Lactose_formulation_25mg','reportName','Raltegravir 25 mg  (lactose formulation)','xml','Raltegravir 25 mg  (lactose formulation).xml','calculatePKParameterFh','','dataTpFilter','strcmp(Grouping,"25mg")','dataReportName','Observed_Raltegravir 25 mg  (lactose formulation)','OutputList',OutputList);
clear OutputList
OutputList(1) = struct('pathID','Organism|PeripheralVenousBlood|Raltegravir|Plasma (Peripheral Venous Blood)','reportName','Raltegravir','displayUnit','µg/l','dataTpFilter','strcmp(OUTPUT,"Raltegravir_PLASMA")','residualScale','log','unitFactor',NaN,'pKParameterList',[]);
OutputList(1).pKParameterList = {'C_max','C_max_norm','t_max','C_tEnd','AUC','AUC_norm','AUC_inf','AUC_inf_norm','MRT','Thalf','CL','Vss','Vd';'µg/l','kg/l','h','µg/l','µg*h/l','kg*h/l','µg*h/l','kg*h/l','h','h','ml/min/kg','ml/kg','ml/kg'};
MeanModelSet(3) = struct('name','Lactose_formulation_50mg','reportName','Raltegravir 50 mg  (lactose formulation)','xml','Raltegravir 50 mg  (lactose formulation).xml','calculatePKParameterFh','','dataTpFilter','strcmp(Grouping,"50mg")','dataReportName','Observed_Raltegravir 50 mg  (lactose formulation)','OutputList',OutputList);
clear OutputList
OutputList(1) = struct('pathID','Organism|PeripheralVenousBlood|Raltegravir|Plasma (Peripheral Venous Blood)','reportName','Raltegravir','displayUnit','µg/l','dataTpFilter','strcmp(OUTPUT,"Raltegravir_PLASMA")','residualScale','log','unitFactor',NaN,'pKParameterList',[]);
OutputList(1).pKParameterList = {'C_max','C_max_norm','t_max','C_tEnd','AUC','AUC_norm','AUC_inf','AUC_inf_norm','MRT','Thalf','CL','Vss','Vd';'µg/l','kg/l','h','µg/l','µg*h/l','kg*h/l','µg*h/l','kg*h/l','h','h','ml/min/kg','ml/kg','ml/kg'};
MeanModelSet(4) = struct('name','Lactose_formulation_100mg','reportName','Raltegravir 100 mg  (lactose formulation)','xml','Raltegravir 100 mg  (lactose formulation).xml','calculatePKParameterFh','','dataTpFilter','strcmp(Grouping,"100mg")','dataReportName','Observed_Raltegravir 100 mg  (lactose formulation)','OutputList',OutputList);
clear OutputList
OutputList(1) = struct('pathID','Organism|PeripheralVenousBlood|Raltegravir|Plasma (Peripheral Venous Blood)','reportName','Raltegravir','displayUnit','µg/l','dataTpFilter','strcmp(OUTPUT,"Raltegravir_PLASMA")','residualScale','log','unitFactor',NaN,'pKParameterList',[]);
OutputList(1).pKParameterList = {'C_max','C_max_norm','t_max','C_tEnd','AUC','AUC_norm','AUC_inf','AUC_inf_norm','MRT','Thalf','CL','Vss','Vd';'µg/l','kg/l','h','µg/l','µg*h/l','kg*h/l','µg*h/l','kg*h/l','h','h','ml/min/kg','ml/kg','ml/kg'};
MeanModelSet(5) = struct('name','Lactose_formulation_200mg','reportName','Raltegravir 200 mg   (lactose formulation)','xml','Raltegravir 200 mg   (lactose formulation).xml','calculatePKParameterFh','','dataTpFilter','strcmp(Grouping,"200mg")','dataReportName','Observed_Raltegravir 200 mg   (lactose formulation)','OutputList',OutputList);
clear OutputList
OutputList(1) = struct('pathID','Organism|PeripheralVenousBlood|Raltegravir|Plasma (Peripheral Venous Blood)','reportName','Raltegravir','displayUnit','µg/l','dataTpFilter','strcmp(OUTPUT,"Raltegravir_PLASMA")','residualScale','log','unitFactor',NaN,'pKParameterList',[]);
OutputList(1).pKParameterList = {'C_max','C_max_norm','t_max','C_tEnd','AUC','AUC_norm','AUC_inf','AUC_inf_norm','MRT','Thalf','CL','Vss','Vd';'µg/l','kg/l','h','µg/l','µg*h/l','kg*h/l','µg*h/l','kg*h/l','h','h','ml/min/kg','ml/kg','ml/kg'};
MeanModelSet(6) = struct('name','Lactose_formulation_400mg','reportName','Raltegravir 400mg (lactose formulation)','xml','Raltegravir 400mg (lactose formulation).xml','calculatePKParameterFh','','dataTpFilter','strcmp(Grouping,"400mg")','dataReportName','Observed_Raltegravir 400mg (lactose formulation)','OutputList',OutputList);
clear OutputList
OutputList(1) = struct('pathID','Organism|PeripheralVenousBlood|Raltegravir|Plasma (Peripheral Venous Blood)','reportName','Raltegravir','displayUnit','µg/l','dataTpFilter','strcmp(OUTPUT,"Raltegravir_PLASMA")','residualScale','log','unitFactor',NaN,'pKParameterList',[]);
OutputList(1).pKParameterList = {'C_max','C_max_norm','t_max','C_tEnd','AUC','AUC_norm','AUC_inf','AUC_inf_norm','MRT','Thalf','CL','Vss','Vd';'µg/l','kg/l','h','µg/l','µg*h/l','kg*h/l','µg*h/l','kg*h/l','h','h','ml/min/kg','ml/kg','ml/kg'};
MeanModelSet(7) = struct('name','Lactose_formulation_800mg','reportName','Raltegravir 800 mg  (lactose formulation)','xml','Raltegravir 800 mg  (lactose formulation).xml','calculatePKParameterFh','','dataTpFilter','strcmp(Grouping,"800mg")','dataReportName','Observed_Raltegravir 800 mg  (lactose formulation)','OutputList',OutputList);
clear OutputList
OutputList(1) = struct('pathID','Organism|PeripheralVenousBlood|Raltegravir|Plasma (Peripheral Venous Blood)','reportName','Raltegravir','displayUnit','µg/l','dataTpFilter','strcmp(OUTPUT,"Raltegravir_PLASMA")','residualScale','log','unitFactor',NaN,'pKParameterList',[]);
OutputList(1).pKParameterList = {'C_max','C_max_norm','t_max','C_tEnd','AUC','AUC_norm','AUC_inf','AUC_inf_norm','MRT','Thalf','CL','Vss','Vd';'µg/l','kg/l','h','µg/l','µg*h/l','kg*h/l','µg*h/l','kg*h/l','h','h','ml/min/kg','ml/kg','ml/kg'};
MeanModelSet(8) = struct('name','Lactose_formulation_1200mg','reportName','Raltegravir 1200 mg   (lactose formulation)','xml','Raltegravir 1200 mg   (lactose formulation).xml','calculatePKParameterFh','','dataTpFilter','strcmp(Grouping,"1200mg")','dataReportName','Observed_Raltegravir 1200 mg   (lactose formulation)','OutputList',OutputList);
clear OutputList
OutputList(1) = struct('pathID','Organism|PeripheralVenousBlood|Raltegravir|Plasma (Peripheral Venous Blood)','reportName','Raltegravir','displayUnit','µg/l','dataTpFilter','strcmp(OUTPUT,"Raltegravir_PLASMA")','residualScale','log','unitFactor',NaN,'pKParameterList',[]);
OutputList(1).pKParameterList = {'C_max','C_max_norm','t_max','C_tEnd','AUC','AUC_norm','AUC_inf','AUC_inf_norm','MRT','Thalf','CL','Vss','Vd';'µg/l','kg/l','h','µg/l','µg*h/l','kg*h/l','µg*h/l','kg*h/l','h','h','ml/min/kg','ml/kg','ml/kg'};
MeanModelSet(9) = struct('name','Lactose_formulation_1600mg','reportName','Raltegravir 1600 mg  (lactose formulation)','xml','Raltegravir 1600 mg  (lactose formulation).xml','calculatePKParameterFh','','dataTpFilter','strcmp(Grouping,"1600mg")','dataReportName','Observed_Raltegravir 1600 mg  (lactose formulation)','OutputList',OutputList);
clear OutputList
OutputList(1) = struct('pathID','Organism|PeripheralVenousBlood|Raltegravir|Plasma (Peripheral Venous Blood)','reportName','Raltegravir','displayUnit','µg/l','dataTpFilter','strcmp(OUTPUT,"Raltegravir_PLASMA")','residualScale','log','unitFactor',NaN,'pKParameterList',[]);
OutputList(1).pKParameterList = {'C_max','C_max_norm','C_max_t1_t2','C_max_t1_t2_norm','C_max_tLast_tEnd','C_max_tLast_tEnd_norm','t_max','t_max_t1_t2','t_max_tLast_tEnd','C_trough_t2','C_trough_tLast','AUC_t1_t2','AUC_t1_t2_norm','AUC_tLast_minus_1_tLast','AUC_tLast_minus_1_tLast_norm','AUC_inf_t1','AUC_inf_t1_norm','AUC_inf_tLast','AUC_inf_tLast_norm','MRT','Thalf','Thalf_tLast_tEnd';'µg/l','kg/l','µg/l','kg/l','µg/l','kg/l','h','h','h','µg/l','µg/l','µg*h/l','kg*h/l','µg*h/l','kg*h/l','µg*h/l','kg*h/l','µg*h/l','kg*h/l','h','h','h'};
MeanModelSet(10) = struct('name','Filmcoated_tablet_100mg_md','reportName','Raltegravir 100 mg filmcoated tablet md','xml','Raltegravir 100 mg filmcoated tablet md.xml','calculatePKParameterFh','','dataTpFilter','strcmp(Grouping,"100mg_FCT_MD")','dataReportName','Observed_Raltegravir 100 mg filmcoated tablet md','OutputList',OutputList);
clear OutputList
OutputList(1) = struct('pathID','Organism|PeripheralVenousBlood|Raltegravir|Plasma (Peripheral Venous Blood)','reportName','Raltegravir','displayUnit','µg/l','dataTpFilter','strcmp(OUTPUT,"Raltegravir_PLASMA")','residualScale','log','unitFactor',NaN,'pKParameterList',[]);
OutputList(1).pKParameterList = {'C_max','C_max_norm','C_max_t1_t2','C_max_t1_t2_norm','C_max_tLast_tEnd','C_max_tLast_tEnd_norm','t_max','t_max_t1_t2','t_max_tLast_tEnd','C_trough_t2','C_trough_tLast','AUC_t1_t2','AUC_t1_t2_norm','AUC_tLast_minus_1_tLast','AUC_tLast_minus_1_tLast_norm','AUC_inf_t1','AUC_inf_t1_norm','AUC_inf_tLast','AUC_inf_tLast_norm','MRT','Thalf','Thalf_tLast_tEnd';'µg/l','kg/l','µg/l','kg/l','µg/l','kg/l','h','h','h','µg/l','µg/l','µg*h/l','kg*h/l','µg*h/l','kg*h/l','µg*h/l','kg*h/l','µg*h/l','kg*h/l','h','h','h'};
MeanModelSet(11) = struct('name','Filmcoated_tablet_200mg_md','reportName','Raltegravir 200 mg filmcoated tablet md','xml','Raltegravir 200 mg filmcoated tablet md.xml','calculatePKParameterFh','','dataTpFilter','strcmp(Grouping,"200mg_FCT_MD")','dataReportName','Observed_Raltegravir 200 mg filmcoated tablet md','OutputList',OutputList);
clear OutputList
OutputList(1) = struct('pathID','Organism|PeripheralVenousBlood|Raltegravir|Plasma (Peripheral Venous Blood)','reportName','Raltegravir','displayUnit','µg/l','dataTpFilter','strcmp(OUTPUT,"Raltegravir_PLASMA")','residualScale','log','unitFactor',NaN,'pKParameterList',[]);
OutputList(1).pKParameterList = {'C_max','C_max_norm','t_max','C_tEnd','AUC','AUC_norm','AUC_inf','AUC_inf_norm','MRT','Thalf','CL','Vss','Vd';'µg/l','kg/l','h','µg/l','µg*h/l','kg*h/l','µg*h/l','kg*h/l','h','h','ml/min/kg','ml/kg','ml/kg'};
MeanModelSet(12) = struct('name','Filmcoated_tablet_400mg_sd','reportName','Filmcoated_tablet_400mg_sd','xml','Raltegravir 400mg filmcoated tablet.xml','calculatePKParameterFh','','dataTpFilter','strcmp(Grouping,"400mg_FCT")','dataReportName','Observed_Raltegravir 400mg filmcoated tablet','OutputList',OutputList);
clear OutputList
OutputList(1) = struct('pathID','Organism|PeripheralVenousBlood|Raltegravir|Plasma (Peripheral Venous Blood)','reportName','Raltegravir','displayUnit','µg/l','dataTpFilter','strcmp(OUTPUT,"Raltegravir_PLASMA")','residualScale','log','unitFactor',NaN,'pKParameterList',[]);
OutputList(1).pKParameterList = {'C_max','C_max_norm','C_max_t1_t2','C_max_t1_t2_norm','C_max_tLast_tEnd','C_max_tLast_tEnd_norm','t_max','t_max_t1_t2','t_max_tLast_tEnd','C_trough_t2','C_trough_tLast','AUC_t1_t2','AUC_t1_t2_norm','AUC_tLast_minus_1_tLast','AUC_tLast_minus_1_tLast_norm','AUC_inf_t1','AUC_inf_t1_norm','AUC_inf_tLast','AUC_inf_tLast_norm','MRT','Thalf','Thalf_tLast_tEnd';'µg/l','kg/l','µg/l','kg/l','µg/l','kg/l','h','h','h','µg/l','µg/l','µg*h/l','kg*h/l','µg*h/l','kg*h/l','µg*h/l','kg*h/l','µg*h/l','kg*h/l','h','h','h'};
MeanModelSet(13) = struct('name','Filmcoated_tablet_400mg_md','reportName','Raltegravir 400 mg filmcoated tablet md','xml','Raltegravir 400 mg filmcoated tablet md.xml','calculatePKParameterFh','','dataTpFilter','strcmp(Grouping,"400mg_FCT_MD")','dataReportName','Observed_Raltegravir 400 mg filmcoated tablet md','OutputList',OutputList);
clear OutputList
OutputList(1) = struct('pathID','Organism|PeripheralVenousBlood|Raltegravir|Plasma (Peripheral Venous Blood)','reportName','Raltegravir','displayUnit','µg/l','dataTpFilter','strcmp(OUTPUT,"Raltegravir_PLASMA")','residualScale','log','unitFactor',NaN,'pKParameterList',[]);
OutputList(1).pKParameterList = {'C_max','C_max_norm','t_max','C_tEnd','AUC','AUC_norm','AUC_inf','AUC_inf_norm','MRT','Thalf','CL','Vss','Vd';'µg/l','kg/l','h','µg/l','µg*h/l','kg*h/l','µg*h/l','kg*h/l','h','h','ml/min/kg','ml/kg','ml/kg'};
MeanModelSet(14) = struct('name','Chewable_fasted_400mg','reportName','Raltegravir 400mg chewable fasted','xml','Raltegravir 400mg chewable fasted.xml','calculatePKParameterFh','','dataTpFilter','strcmp(Grouping,"Chewable_tablet_fasted")','dataReportName','Observed_Raltegravir 400mg chewable fasted','OutputList',OutputList);
clear OutputList
OutputList(1) = struct('pathID','Organism|PeripheralVenousBlood|Raltegravir|Plasma (Peripheral Venous Blood)','reportName','Raltegravir','displayUnit','µg/l','dataTpFilter','strcmp(OUTPUT,"Raltegravir_PLASMA")','residualScale','log','unitFactor',NaN,'pKParameterList',[]);
OutputList(1).pKParameterList = {'C_max','C_max_norm','t_max','C_tEnd','AUC','AUC_norm','AUC_inf','AUC_inf_norm','MRT','Thalf','CL','Vss','Vd';'µg/l','kg/l','h','µg/l','µg*h/l','kg*h/l','µg*h/l','kg*h/l','h','h','ml/min/kg','ml/kg','ml/kg'};
MeanModelSet(15) = struct('name','Chewable_fed_400mg','reportName','Raltegravir 400mg chewable fed','xml','Raltegravir 400mg chewable fed.xml','calculatePKParameterFh','','dataTpFilter','strcmp(Grouping,"Chewable_tablet_fed")','dataReportName','Observed_Raltegravir 400mg chewable fed','OutputList',OutputList);
clear OutputList
OutputList(1) = struct('pathID','Organism|PeripheralVenousBlood|Raltegravir|Plasma (Peripheral Venous Blood)','reportName','Raltegravir','displayUnit','µg/l','dataTpFilter','strcmp(OUTPUT,"Raltegravir_PLASMA")','residualScale','log','unitFactor',NaN,'pKParameterList',[]);
OutputList(1).pKParameterList = {'C_max','C_max_norm','t_max','C_tEnd','AUC','AUC_norm','AUC_inf','AUC_inf_norm','MRT','Thalf','CL','Vss','Vd';'µg/l','kg/l','h','µg/l','µg*h/l','kg*h/l','µg*h/l','kg*h/l','h','h','ml/min/kg','ml/kg','ml/kg'};
MeanModelSet(16) = struct('name','Granules_in_suspension_400mg','reportName','Raltegravir 400mg (granules in suspension)','xml','Raltegravir 400mg (granules in suspension).xml','calculatePKParameterFh','','dataTpFilter','strcmp(Grouping,"granules_suspension")','dataReportName','Observed_Raltegravir 400mg (granules in suspension)','OutputList',OutputList);
clear OutputList

% Definitions of TaskList
TaskList = struct('doVPC',1,'doSensitivityAnalysis',0,'doAbsorptionPlots',1,'checkMassbalance',1);

% List of Nonmemfiles:
% set to {}, if no data available
%  first column nonmem file, second column dictionary, third column datatype
dataFiles(1,:) = {'Raltegravir_PK.txt','tpDictionary.csv','timeprofile'};

% get Definition of default plots
if TaskList.doVPC
    VPC = getDefaultVPCSettings(WSettings,MeanModelSet);
else
    VPC = [];
end

% get Definition of default Masbalance plots
if TaskList.checkMassbalance
    MBS = getDefaultMassbalanceSettings;
else
    MBS = [];
end

% List of Parameters for sensitivity analysis:
% set to {}, if not needed
%  columns: 1. path, 2. number of steps, 3. variation range, 4. minValue 5. maxValue
sensParameterList(1,:) = {'Raltegravir|Lipophilicity',2,0.1,'Lipophilicity'};
sensParameterList(2,:) = {'Raltegravir|Specific intestinal permeability (transcellular)',2,0.1,'Specific intestinal permeability (transcellular)'};
sensParameterList(3,:) = {'Raltegravir-UGT1A9-Kassahun 2007|In vitro Vmax for liver microsomes',2,0.1,'Raltegravir-UGT1A9-Kassahun 2007|In vitro Vmax for liver microsomes'};
sensParameterList(4,:) = {'Raltegravir-UGT1A1-Kassahun 2007|In vitro Vmax for liver microsomes',2,0.1,'Raltegravir-UGT1A1-Kassahun 2007|In vitro Vmax for liver microsomes'};
sensParameterList(5,:) = {'Applications|*|filmcoated tablet (original Merck formulation)|Dissolution shape',2,0.1,'Dissolution shape'};
sensParameterList(6,:) = {'Applications|*|Weibull (granules)|Dissolution shape',2,0.1,'Dissolution shape'};
sensParameterList(7,:) = {'Applications|*|chewable tablet|Dissolution shape',2,0.1,'Dissolution shape'};

% start the execution
runMeanModelWorkflow(WSettings,TaskList,MeanModelSet,VPC,dataFiles,sensParameterList,MBS);
