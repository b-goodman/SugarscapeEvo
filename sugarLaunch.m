(* ::Package:: *)

BeginPackage["`launchSugarscape`"];
(* load *)
Get[FileNameJoin[{$UserBaseDirectory,"Applications","Sugarscape+","lib","sugarInterface.m"}]];

(* initilize main user interface - must be done on first evaluation *)
initilizeMainUserInterface;

(* (re)opens main user interface.  Will resume from last state unless re-initilized. *)
viewMainUserInterface;

Print["sugarscape+ v0.1"] 

EndPackage[];
