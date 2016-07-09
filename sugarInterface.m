(* ::Package:: *)

BeginPackage["`userInterfaceSugarscape`"];
(* load functions *)
Get@FileNameJoin[{$UserBaseDirectory,"Applications","Sugarscape+","lib","sugarFunctions.m"}];

evaluateSugarscape:=(
session["lastEval"]=DateString[];
(* *)
initilize[
simulationParamaters["input"]["initialPopulationSize"]["value"],
simulationParamaters["input"]["initialFoodAmmount"]["value"],
simulationParamaters["input"]["gridLength"]["value"]
];
(* *)
run[
simulationParamaters["input"]["tMax"]["value"],
simulationParamaters["input"]["gridLength"]["value"],
simulationParamaters["input"]["stepEnergyLoss"]["value"],
simulationParamaters["input"]["foodEnergyGain"]["value"],
simulationParamaters["input"]["agentBreedThreshold"]["value"],
simulationParamaters["input"]["foodReplenishAmmount"]["value"]
];
(* *)
(*positions=Flatten[Last[sessionData],1]\[LeftDoubleBracket]1;;-1;;2\[RightDoubleBracket];
food=Flatten[Last[sessionData],1]\[LeftDoubleBracket]2;;-1;;2\[RightDoubleBracket];*)
session["sessionData"]["positions"]=Flatten[Last[sessionData],1][[1;;-1;;2]];
session["sessionData"]["food"]=Flatten[Last[sessionData],1][[2;;-1;;2]];

mainInterfaceMsg="Evaluation Complete";
inEval=False;
);

reviewSessionData:=(
(*positions=Flatten[Last[Flatten[session["sessionData"],1]],1]\[LeftDoubleBracket]1;;-1;;2\[RightDoubleBracket];
food=Flatten[Last[Flatten[session["sessionData"],1]],1]\[LeftDoubleBracket]2;;-1;;2\[RightDoubleBracket];*)

food=session["sessionData"]["food"];
positions=session["sessionData"]["positions"];
anchor=session["paramaters"]["input"]["gridLength"]["value"];

Manipulate[
Row[{
Graphics[{
{White,Point[Tuples[{0,anchor},2]]},
{Red,Point[{positions[[i]]}]},
{Blue,Point[{food[[i]]}]}
},ImageSize->Medium],
Show[{
Graphics[Line[{{i,0},{i,Max[{Length[food[[i]]],Length[positions[[i]]]}]}}]],
ListPlot[{
Length/@positions,
Length/@food
},Joined->{True,True},
PlotStyle->{Red,Blue}
]
},
Axes->True,
PlotRange->{{0,Length[positions]},{0,Max[{Length/@food,Length/@positions}]}},
ImageSize->Medium
]
}],
{i,1,Length[positions],1}
]
);


(* interface *)

(* initiliziation*)
initilizeMainUserInterface:=(
(* initilize default paramaters *)
inEval=False;
session=Null;
mainInterfaceMsg="";
sessionName="New_Session";
(* set initial configuration as default *)
initilizeConfiguration;
);
(* -----------------------------  *)

(* load and set preset config file *)
defaultSimulationParamaters=Uncompress[
Import[
FileNameJoin[{$UserBaseDirectory,"Applications","Sugarscape+","lib","default.config.sgr"}]
]
];

(* initilize new simulation session *)
initilizeNewSession:=(
session=<|
"paramaters"->defaultSimulationParamaters,
"sessionName"->"New Session",
"createdOn"->DateString[],
"sessionData"-><|"positions"->{},"food"->{}|>,
"lastEval"->Null
|>;
initilizeConfiguration;
openConfigurationEditor;
);

(* set initial configuration as default *)
initilizeConfiguration:=(
simulationParamaters=defaultSimulationParamaters;
configInterfaceMsg="";
);



(* functions *)
(* formats text *)
textStyle=Style[#,"Item",12]&;

(* allows editing of simulation paramaters *)
valueField=InputField[Dynamic[simulationParamaters["input"][#]["value"]],Number,FieldSize->{5,1}]&;

(* saves simulation configuration as file *)
saveConfig:=(
SetDirectory@FileNameJoin[{$UserBaseDirectory,"Applications","evoScape","config"}];
Export[StringReplace[simulationParamaters["meta"]["name"]," "->"_"]<>".config.sgr",Compress@simulationParamaters,"String"];
ResetDirectory[];
);

(* loads presaved configuration file and applys to current session *)
loadConfig:=With[{
importFilePath=SystemDialogInput["FileOpen",FileNameJoin[{$UserBaseDirectory,"Applications","evoScape","config"}],WindowTitle->"Load Configuration..."]
},
If[importFilePath=!=$Canceled,
If[Last@StringSplit[FileBaseName[importFilePath],"."]=="config",
simulationParamaters=Uncompress[Import[importFilePath]];
configInterfaceMsg="Imported Configuration: "<>simulationParamaters["meta"]["name"],
configInterfaceMsg="Invalid File Selected"
],
configInterfaceMsg="Import Cancelled"
]
];

(* saves session *)
sessionSave:=(
SetDirectory@FileNameJoin[{$UserBaseDirectory,"Applications","evoScape","session"}];
Export[StringReplace[session["sessionName"],{" "->"_","/"->"_"}]<>".session.sgr",Compress@session,"String"];
ResetDirectory[];
mainInterfaceMsg=session["sessionName"]<>" saved.";
);

(* load and apply session from file *)
sessionLoad:=With[{
importSessionPath=SystemDialogInput["FileOpen",FileNameJoin[{$UserBaseDirectory,"Applications","evoScape","session"}],WindowTitle->"Load Session"]
},
If[importSessionPath=!=$Canceled,
If[Last@StringSplit[FileBaseName[importSessionPath],"."]=="session",
session=Uncompress[Import[importSessionPath]];
simulationParamaters=session["paramaters"];
mainInterfaceMsg="Imported Session: "<>session["sessionName"],
mainInterfaceMsg="Invalid File Selected"
],
mainInterfaceMsg="Import Cancelled"
]
];

(* edit simulation paramaters configuration *)
configUI:=Deploy@Column[{
Row[{
InputField[Dynamic[simulationParamaters["meta"]["name"]],String,FieldSize->{15,1}],
(**)
ActionMenu["File",{
"Save Configuration":>saveConfig,
"Load Configuration":>loadConfig,
Delimiter,
"Revert to Default":>initilizeConfiguration
},ImageSize->{70,30},Method->"Queued"],
(**)
DefaultButton["Continue..",session["paramaters"]=simulationParamaters;DialogReturn[],ImageSize->{70,30},Method->"Queued"]
}],
(**)
Row[{
Column[
TextCell[textStyle[simulationParamaters["input"][#]["label"]]]&/@#,
Spacings->1
],
Column[
valueField[#]&/@#,
Spacings->0.8
]
},Spacer[5]
]&[Keys[simulationParamaters[["input",All]]]],
(**)
Dynamic[TextCell[configInterfaceMsg]]
},Spacings->1
];

(* launches interface for editing sim. config *)
openConfigurationEditor:=
CreateDialog[
configUI,
WindowTitle->"Configure",
Modal->False
];

(* clears all simulation data associated with session *)
clearSessionData:=CreateDialog[{
Column[{
TextCell[textStyle["Warning: \n This will delete all simulation data associated with this session.\n Session will remain unsaved. \n Continue?"]],
Row[{
Button["Continue",session["sessionData"]=<|"positions"->{},"food"->{}|>;session["lastEval"]=Null;mainInterfaceMsg="Session Data Cleared";DialogReturn[],ImageSize->{90,30}],
Button["Cancel",DialogReturn[],ImageSize->{90,30}]
}]
},Alignment->Center]
},WindowTitle->"Delete Session Data"];

(* generates toolbar for main interface *)
interfaceToolbar:=Row[{
ActionMenu["File",{
"Start New Session..":>initilizeNewSession,
"Load Session..":>sessionLoad,
Style["Save Sesion",If[session=!=Null,Black,Gray]]:>If[session=!=Null,sessionSave,mainInterfaceMsg="No session avaliable"],
Delimiter,
"View Working Directory":>SystemOpen@FileNameJoin[{$UserBaseDirectory,"Applications","evoScape"}]
},
Method->"Queued",
Enabled->Dynamic@Not[inEval]
],
(**)
ActionMenu["Data",{
Style["Review Session Data..",If[session["sessionData"]["positions"]==={}||session===Null,Gray,Black],12]:>If[session["sessionData"]["positions"]==={}||session===Null,mainInterfaceMsg="No session data avaliable",
CreateDialog[{
reviewSessionData
}]
],
Style["Clear Session Data..",If[session["sessionData"]["positions"]==={}||session===Null,Gray,Black],12]:>If[session["sessionData"]["positions"]==={}||session===Null,mainInterfaceMsg="No session data avaliable",clearSessionData]
},
Method->"Queued",
Enabled->Dynamic@Not[inEval]
],
(**)
ActionMenu["Evaluation",{
Style["Run Simulation",12,If[session=!=Null,Black,Gray]]:>If[session=!=Null,
inEval=True;
mainInterfaceMsg=Dynamic["Evaluating.  Progress: "<>ToString[N[t/session["paramaters"]["input"]["tMax"]["value"]*100,3]]<>"%"];
evaluateSugarscape,
mainInterfaceMsg=textStyle["No active session"]],
Delimiter,
(*Style["Delete All Output",12]\[RuleDelayed]FrontEndTokenExecute["DeleteGeneratedCells"],*)
Style["Abort Evaluation",12]:>FrontEndTokenExecute["EvaluatorAbort"],
Style["Quit Kernel",12]:>FrontEndTokenExecute["EvaluatorQuit"]
},
Method->"Queued",
Enabled->Dynamic@Not[inEval]
]
}];

(* displays session info pane *)
sessionToolbar:=Panel[
If[session===Null,
textStyle["No Session Active"],
sessionInfoView
],Alignment->If[session===Null,Center,Left],
ImageSize->{400,170},
Background->None
];

(* generates session info pane *)
sessionInfoView:=Deploy@
(**)
Dynamic@Column[{
Row[{
textStyle["Session Name:"],
session["sessionName"],
Button[Style["Edit..",9],CreateDialog[{
Row[{
InputField[Dynamic[session["sessionName"]],String,FieldSize->{10,1}],
DefaultButton["Continue",DialogReturn[]]
}]
},
WindowTitle->"Edit Session Name",
Modal->True
],
ImageSize->{40,20},
Enabled->Dynamic@Not[inEval]
]
},Spacer[5]],
(**)
Row[{
textStyle["Configurated with:"],
session["paramaters"]["meta"]["name"],
Button[Style["Edit..",9],openConfigurationEditor,ImageSize->{40,20},Enabled->Dynamic@Not[inEval]]
},Spacer[5]],
(**)
Row[{
textStyle["Created on:"],
textStyle[session["createdOn"]]
},Spacer[5]],
Row[{
textStyle["Last evaluated on:"],
If[session["lastEval"]===Null,
textStyle["Unevaluated"],
session["lastEval"]
]
},Spacer[5]]
},Spacings->1];

(* launches main interface *)
viewMainUserInterface:=CreatePalette[
Dynamic@Column[{
interfaceToolbar,
sessionToolbar,
TextCell[mainInterfaceMsg]
}],
Saveable->False,
WindowTitle->"evoScape v 0.1",
WindowSize->{400,230}
];
EndPackage[];
