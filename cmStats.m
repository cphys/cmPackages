(* ::Package:: *)

BeginPackage["cmStats`"];
 
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
	
			
betheFits::usage = 
 "betheFits[data,model,coefficients,variable,

	input: data            data to be fit
		   model           fit model
		   coefficients    coefficients in fit equation.
		   variable        variable in fit equation.
		 
	returns: fit coefficients.";
	


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

    
betheFits[
  data_,
  model_,
  coefficients_,
  variable_,
  OptionsPattern[{
  workingPrecision -> 64}]] :=
  Module[{
   a, b, c, d, e, n,
   tms,
   zs, Es, terms,
   consts, sols,
   dt = data,
   ft = model,
   cfs = coefficients,
   var = variable,
   wp = OptionValue[workingPrecision]},
  
  tms = List @@ ft;  
  n = Length[cfs];      
  (* z from data *)
  zs = Table[dt[[i]][[1]], {i, Length[dt]}];
  (* potetntial values from data *)  
  Es = Table[dt[[i]][[2]], {i, Length[dt]}];  
  (* list of terms to be subtracted from potential *)  
  terms[zz_] := ft /. var -> zz;
    
  sols =
   Table[
    Flatten[
     Values[
      NSolve[
       SetPrecision[
        Table[
         Es[[i + j]] == -terms[zs[[i + j]]], {j, 0, n - 1}], wp],
       cfs,
       WorkingPrecision -> wp]]],
    {i, Length[zs] - n + 1}];  
  Table[
   LinearModelFit[
      sols[[All, i]],
      x,
      x]["BestFitParameters"][[1]],
   {i, n}]] 


End[]
EndPackage[]

