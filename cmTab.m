(* ::Package:: *)

BeginPackage["cmTab`"];

toTeX::usage=
 "toTeX[tab,optionss]

	input: tab (2D table of floats) table to be converted to latex

	options: sFig-> number of sigfigs for output
		   
	returns: latex formatted 2D table ";
	
fromTeX::usage=
 "fromTeX[tab,optionss]

	input: tab (2D table of floats) table to be converted from latex

	options: sFig-> number of sigfigs for output
		   
	returns: Mathematica expression formatted 2D table ";


Begin["`Private`"]

toTeX[tab_,
   OptionsPattern[{sFig -> 4}]] :=
  toTeX[tab] =
   Module[{
     tb = tab,
     sf = OptionValue[sFig]},
    Table[
     Map[TeXForm, ScientificForm[#, sf,ExponentFunction->(#&)] & /@ tb[[i]]],
      {i,Length[tb]}]];
      
fromTeX[tab_,
   OptionsPattern[{sFig -> 4}]] :=
 fromTeX[tab] =
 Module[{
     tb = tab,
     sf = OptionValue[sFig]},
 Table[
  ToExpression[ToString[tab[[j]][[i]]], TeXForm],
  {j, Length[tab]},
  {i, Length[tab[[j]]]}]];
            
End[]
EndPackage[]
