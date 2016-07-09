(* ::Package:: *)

BeginPackage["`launchSugarscape`"];
(* load *)
Get[FileNameJoin[{$UserBaseDirectory,"Applications","evoScape","lib","sugarInterface.m"}]];

(* initilize main user interface - must be done on first evaluation *)
initilizeMainUserInterface;

(* (re)opens main user interface.  Will resume from last state unless re-initilized. *)
(*viewMainUserInterface;*)

Print["evoScape v0.1 \n Evaluate viewMainUserInterface to begin"] 

EndPackage[];
