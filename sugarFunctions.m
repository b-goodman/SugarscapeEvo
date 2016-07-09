(* ::Package:: *)

BeginPackage["`functionsSugarscape`"];
(**)
(* returns starting array for agent IDs, x/y positions, energy and generation *)
initilizeAgentArray=Compile[
{{enviromentLocations,_Integer,2},{startingPopulation,_Integer,0}},
Module[{initialLocations},
initialLocations=RandomSample[enviromentLocations,startingPopulation];
With[{
xInt=Map[First,initialLocations],
yInt=Map[Last,initialLocations]},
agentPositions=Table[
{{Part[xInt,i]},{Part[yInt,i]}},
{i,1,startingPopulation}
];
agentEnergy=Table[10,{i,1,startingPopulation}]
]
],
{{agentPositions,_Integer,3},{agentEnergy,_Real,1}}
];
(**)
(* returns {x,y} list of locations which are food *)
initilizeFoodLocations=Compile[
{{enviromentLocations,_Integer,2},{agentPositions,_Integer,3},{initialFoodAmmount,_Integer,0}},
Module[{agentLocations},
agentLocations=Table[
Join[{
Part[agentPositions,(*ID*)i,(*x*)1,(*most recent*)-1],
Part[agentPositions,(*ID*)i,(*y*)2,(*most recent*)-1]
}],
{i,1,Length[agentPositions]}
];
foodLocations=RandomSample[
Complement[
enviromentLocations,
agentLocations
],initialFoodAmmount]
],
{{foodLocations,_Integer,2}(*,{agentLocations,_Integer,2}*)}
];
(**)
(* returns position of food nearest to agent *)
nearestFoodLocation=Compile[
{{agentID,_Integer,0},{agentPositions,_Integer,3},{foodLocations,_Integer,2}},
With[{
currentAgentLocation=Join[{
Part[agentPositions,agentID,1,-1],
Part[agentPositions,agentID,2,-1]
}]
},
Flatten[
Nearest[foodLocations,currentAgentLocation,1,DistanceFunction->ManhattanDistance]
]
]
];
(**)
(* returns all 8 NN locations for agent's current position *)
NearestNeighbours=Compile[{{agentID,_Integer,0},{agentPositions,_Integer,3},{gridLength,_Integer}},
With[{
x=Part[agentPositions,agentID,1,-1],
y=Part[agentPositions,agentID,2,-1],
exclude=Table[
Join[{
Part[agentPositions,(*ID*)i,(*x*)1,(*most recent*)-1],
Part[agentPositions,(*ID*)i,(*y*)2,(*most recent*)-1]
}],
{i,1,Length[agentPositions]}
]
},
Complement[
(* returns all neighbouring locations to agents current position *)
{{x,y+1},
{x,y-1},
{x-1,y},
{x+1,y},
{x-1,y+1},
{x+1,y+1},
{x-1,y-1},
{x+1,y-1}
},
(* exclude any locations containing an other agent *)
exclude
]/.{
(* replace with boundary conditions  *)
{x_,y_}/;y==0->{x,gridLength},
{x_,y_}/;y==gridLength+1->{x,1},
{x_,y_}/;x==0->{gridLength,y},
{x_,y_}/;x==gridLength+1->{1,y}
}
]
];
(**)
(* returns NN position agent should jump to when moving towards food *)
agentMoveTowardsFood=Compile[{
{agentID,_Integer,0},{NN,_Integer,2},{NF,_Integer,1},{oldAgentPositions,_Integer,3},{oldAgentEnergy,_Real,1},{movementCost,_Real,0}
},
With[{
direction=Nearest[NN,NF,1,DistanceFunction->ManhattanDistance]
},
Module[{xStep,yStep},
xStep=Part[direction,1,1];
yStep=Part[direction,1,2];
agentPositions=ReplacePart[oldAgentPositions,xStep,{(*ID*)agentID,(*x*)1,(*latest*)-1}];
agentPositions=ReplacePart[agentPositions,yStep,{(*ID*)agentID,(*y*)2,(*latest*)-1}];
agentEnergy=ReplacePart[oldAgentEnergy,Part[oldAgentEnergy,agentID]-movementCost,agentID]
]
];
];
(**)
(* returns True of agent has food *)
agentHasFood=Compile[
{
{agentID,_Integer,0},{agentPositions,_Integer,3},{foodLocations,_Integer,2}
},
If[
Total[
Boole[
Map[
Join[{
Part[agentPositions,agentID,1,-1],
Part[agentPositions,agentID,2,-1]
}]==#&,
foodLocations]
]]==0,
False,
True]
];
(**)
agentEatFood=Compile[{
{agentID,_Integer,0},{hasFood,True|False},{oldAgentEnergy,_Real,1},{foodGain,_Integer,0}
},
If[hasFood==True,
agentEnergy=ReplacePart[oldAgentEnergy,Part[oldAgentEnergy,agentID]+foodGain,agentID],
agentEnergy=oldAgentEnergy
];
];
(**)
(* remove eaten food *)
deleteEatenFood=Compile[{
{agentID,_Integer,0},
{oldFoodLocations,_Integer,2},
{agentPositions,_Integer,3}
},
foodLocations=Complement[
oldFoodLocations,
Intersection[
Table[
Join[{
Part[agentPositions,agentID,1,-1],
Part[agentPositions,agentID,2,-1]
}],
{i,1,Length[agentPositions]}
],
oldFoodLocations
]
]
];
(**)
(* adds N units of food to random locations with every turn *)
replenishFood=Compile[
{{enviromentLocations,_Integer,2},{oldFoodLocations,_Integer,2},{agentPositions,_Integer,3},{foodReplenishAmmount,_Integer,0}},
Module[{foodReplenishLocations,agentLocations,newFoodLocations},
agentLocations=Table[
Join[{
Part[agentPositions,(*ID*)i,(*x*)1,(*most recent*)-1],
Part[agentPositions,(*ID*)i,(*y*)2,(*most recent*)-1]
}],
{i,1,Length[agentPositions]}
];
foodReplenishLocations=RandomChoice[
Complement[
enviromentLocations,
agentLocations,
oldFoodLocations
],foodReplenishAmmount];
(*newFoodLocations=Join[foodLocations,foodReplenishLocations]*)
foodLocations=Join[oldFoodLocations,foodReplenishLocations]
],
{{foodLocations,_Integer,2}}
];
(**)
agentBreed=Compile[
{
{agentID,_Integer,0},{oldAgentPositions,_Integer,3},{oldAgentEnergy,_Real,1},{breedThreshold,_Integer,0},
{nn,_Integer,2}
},
If[oldAgentEnergy[[agentID]]>breedThreshold,
With[{
newAgent=RandomChoice[nn,1],
newAgentEnergy=(oldAgentEnergy[[agentID]])/2
},
agentPositions=Insert[oldAgentPositions,{{newAgent[[1,1]]},{newAgent[[1,2]]}},-1];
agentEnergy=Insert[ReplacePart[oldAgentEnergy,newAgentEnergy,agentID],newAgentEnergy,-1]
],
agentPositions=oldAgentPositions;
agentEnergy=oldAgentEnergy
]
];
(**)
cullAgent=Compile[{
{oldAgentPositions,_Integer,3},
{oldAgentEnergy,_Real,1}
},
agentPositions=Delete[oldAgentPositions,Position[oldAgentEnergy,_?(#<=0&)]];
agentEnergy=Delete[oldAgentEnergy,Position[oldAgentEnergy,_?(#<=0&)]];
];
initilize=Function[
{
initialPopulationSize,
initialFoodAmmount,
gridLength
},
enviromentLocations=Flatten[Array[{#1,#2}&,{gridLength,gridLength}],1];
initilizeAgentArray[enviromentLocations,initialPopulationSize];
initilizeFoodLocations[enviromentLocations,agentPositions,initialFoodAmmount];
];
(**)
run=Function[
{
tMax,
gridLength,
stepEnergyLoss,
foodEnergyGain,
agentBreedThreshold,
foodReplenishAmmount
},
sessionData=Reap[
For[t=0,t<tMax,t++,
Do[
If[Length[foodLocations]>0&&Length[agentPositions]>0,
(* agent looks for nearest food and advances to the NN location which is nearest to the nearest food *)
agentMoveTowardsFood[
i,
NearestNeighbours[i,agentPositions,gridLength],
nearestFoodLocation[i,agentPositions,foodLocations],
agentPositions,
agentEnergy,
stepEnergyLoss];
(* if agent has food, inc. energy by value, return new energy list, else, return old energy list *)
agentEatFood[
i,
agentHasFood[i,agentPositions,foodLocations],
agentEnergy,
foodEnergyGain
];
(* remove any eaten food *)
deleteEatenFood[
i,
foodLocations,
agentPositions
];
(* agents multiply *)
agentBreed[
i,
agentPositions,
agentEnergy,
agentBreedThreshold,
NearestNeighbours[i,agentPositions,gridLength]
]
(* end agent[i] update *)
],
{i,1,Length[agentPositions]}
];
(* remove dead agents *)
cullAgent[agentPositions,agentEnergy];
(* replenish food *)
replenishFood[enviromentLocations,foodLocations,agentPositions,foodReplenishAmmount];
(* save state *)
(*AppendTo[sessionData\[LeftDoubleBracket]1\[RightDoubleBracket],Thread[Partition[Flatten[agentPositions],2]\[Rule]"agent"]];
AppendTo[sessionData\[LeftDoubleBracket]2\[RightDoubleBracket],Thread[foodLocations\[Rule]"food"]];*)
Sow[Partition[Flatten[agentPositions],2]];
Sow[foodLocations]
]
]
];
(**)
EndPackage[];
