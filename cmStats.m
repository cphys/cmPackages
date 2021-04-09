(* ::Package:: *)

BeginPackage["cStats`"];
 
slope::usage = 
 "slope[xs, ys]

	input: xs (list) list of x coordinates of a function
		   ys (list) list of y coordinates of a function
	returns: slope of the best fit line for a dataset. ";

yInt::usage = 
 "yInt[xs, ys]

	input: xs (list) list of x coordinates of a function
		   ys (list) list of y coordinates of a function
	returns: y-intercept of the best fit line for a dataset. ";

rSqrd::usage = 
 "rSqrd[xs, ys]

	input: xs (list) list of x coordinates of a function
		   ys (list) list of y coordinates of a function
	returns: r-squared value of the best fit line for a dataset.";

logFit::usage = 
 "logFit[xs, ys, z]

	input: xs (list) list of x coordinates of a function
		   ys (list) list of y coordinates of a function
		   z  (char) varibale name for fit function
	returns: fit-function for logerithmic data. ";	

linFit::usage = 
 "linFit[xs, ys, z]

	input: xs (list) list of x coordinates of a function
		   ys (list) list of y coordinates of a function
		   z  (char) varibale name for fit function
	returns: fit-function for linear dataset. ";
	


Begin["`Private`"]

slope[xs_, ys_] := 
	(Length[xs]*(ys . xs)
	-(Total[ys]*Total[xs]))/(Length[xs]*Total[xs*xs] - Total[xs]^2)

yInt[xs_, ys_] := (Total[ys] - slope[xs, ys]*Total[xs])/Length[xs]

rSqrd[xs_, ys_] := 
	(Length[xs]*xs . ys-(Total[ys]*Total[xs]))^2/((Length[xs]*xs . xs - 
      Total[xs]^2)*(Length[xs]*ys . ys - Total[ys]^2))

logFit[xs_, ys_, z_] := slope[xs, ys]*Log[z] + yInt[xs, ys]

linFit[xs_, ys_, z_] := slope[xs, ys]*z + yInt[xs, ys]


End[]
EndPackage[]

