(* ::Package:: *)

BeginPackage["cTix`"];
 
axLab3D::usage = 
 "axLab3D[lab, rot, sty]

	input: lab (list) of strings containgin titles of each axis
		   rot (list) rotation angle for each label
		   sty        label styles
	returns: axis labels for 3d plots. ";

InsertLabels::usage =
 "InsertLabels[g_Graphics,legend__]

	input: g_Graphics (graphics) 3d plot
		   legend__   (Text{}) text to be inserten on a 3d plot
	returns: A 3D graphics object labeled with 2D text. ";
 	
labStyle::usage=
 "labStyle[fs]

	input: fs (int) fontsize for plot label
	returns: Formatted plot labels. Using Black Helvetica font";
	
powTix::usage=
 "powTix[min,max,inc]

	input: min (numb) power of 10 of the minimum tick value
		   max (numb) power of 10 of the maximum tick value
		   inc (numb) increment for the power of tick values
	returns: Plot ticks labeled in powers of 10";
	
linTix::usage=
 "linTix[min,max,inc]

	input: min (numb) minimum tick value
		   max (numb) maximum tick value
		   inc (numb) increment of tick values
	returns: Plot ticks";
	
blankPowTix::usage=
 "blankPowTix[min,max,inc]

	input: min (numb) minimum tick value
		   max (numb) maximum tick value
		   inc (numb) increment of tick values
	returns: Unlableled plot ticks to match linTix";
	
linTixSc::usage=
 "linTixSc[min,max,inc,sc]

	input: min (numb) minimum tick value
		   max (numb) maximum tick value
		   inc (numb) increment of tick values
		   sc  (numb) increment of tick values
	returns: Plot ticks scaled by 10^sc";
	
blankLinTixSc::usage=
 "blankLinTixSc[min,max,inc,sc]

	input: min (numb) minimum tick value
		   max (numb) maximum tick value
		   inc (numb) increment of tick values
		   sc  (numb) increment of tick values
	returns: Unlableled plot ticks scaled by 10^sc";
	
linTixScInt::usage=
 "linTixScInt[min,max,inc,sc]

	input: min (numb) minimum tick value
		   max (numb) maximum tick value
		   inc (numb) increment of tick values
		   sc  (numb) increment of tick values
	returns: Integer Plot ticks scaled by 10^sc";
	
blankLinTixScInt::usage=
 "blankLinTixScInt[min,max,inc,sc]

	input: min (numb) minimum tick value
		   max (numb) maximum tick value
		   inc (numb) increment of tick values
		   sc  (numb) increment of tick values
	returns: Unlabeled integer Plot ticks scaled by 10^sc";
 	
ploLabs3D::usage=
 "ploLabs3D[plt,labs,pos,rot,fs,ipad]

	input: plt  (plot) minimum tick value
		   labs (list) maximum tick value
		   pos  (list) list of x/y coordinates for each
					   label. {{x1,y1},{x2,y2},{x3,y3}}
		   rot  (list) rotation angles for each label.
		   fs   (numb) fontsize for labels
		   ipad (list) imagepadding for plots.
					   ie: {{20,All},{5,All}}
	returns: Unlableled Integerplot ticks scaled by 10^sc";	
	
NTo::usage=
 "NTo[n,p]
	input: n  (float) float to be rounded
		   p  (int)   number of sig-figs to round to
	returns: n rounded to p sig-figs for when N[n,p] won't work";
	


Begin["`Private`"]

NTo[x_?Internal`RealValuedNumericQ,p_Integer?Positive]:=Function[{m,e},N[Round[m 10^p]10^(e-p)]]@@MantissaExponent[x];

NTo[z_?NumericQ,p_Integer?Positive]:=Complex[NTo[Re[z],p],NTo[Im[z],p]];

NTo[p_Integer?Positive][x_]:=NTo[x,p];

axLab3D[lab_, rot_, sty_] := 
 Table[Style[Rotate[lab[[i]], rot[[i]]], sty], {i, 3}]

InsertLabels[g_Graphics, legend__] := 
  Show[Rasterize[g, Background -> None], 
   Graphics[{legend}, Cases[Except[PlotRangeClipping -> True]]]];

labStyle[fs_] := 
 labStyle[fs] = {FontFamily -> "Helvetica", FontSize -> fs, 
   FontColor -> Black}

powTix[min_, max_, inc_] := {
  Table[{N[10^i], ToString[Superscript[10, i], 
  FormatType -> StandardForm]}, {i,min, max, inc}],
  Table[{N[10^i], Null}, {i, min, max, inc}]}
  
linTix[min_, max_, inc_] := {
  Table[{N[i], i}, {i, min, max, inc}],
  Table[{N[i], Null}, {i, min, max, inc}]}
  
blankPowTix[min_, max_, inc_] := {
  Table[{N[10^i], Null}, {i, min, max, inc}],
  Table[{N[10^i], Null}, {i, min, max, inc}]}
  
linTixSc[min_, max_, inc_, sc_] := {
  Table[
  {N[i], NumberForm[i*sc, {4, 2}]},
  {i, min, max, inc}],
  Table[{N[i], Null}, {i, min, max, inc}]}
  
blankLinTixSc[min_, max_, inc_, sc_] := {
  Table[{N[i], Null}, {i, min, max, inc}],
  Table[{N[i], Null}, {i, min, max, inc}]}
  
linTixScInt[min_, max_, inc_, sc_] := {
  Table[{N[i], IntegerPart[i*sc]}, {i, min, max, inc}],
  Table[{N[i], Null}, {i, min, max, inc}]}
  
blankLinTixScInt[min_, max_, inc_, sc_] := {
  Table[{N[i], Null}, {i, min, max, inc}],
  Table[{N[i], Null}, {i, min, max, inc}]}

ploLabs3D[plt_, labs_, pos_, rot_, fs_, ipad_] :=
 Show[
  Rasterize[plt, Background -> None],
  Graphics[
   Table[
    Text[Style[
      Rotate[labs[[i]], rot[[i]]],
      FontColor -> Black,
      FontFamily -> "Helvetica",
      FontSize -> fs],
     ImageScaled[pos[[i]]]],
    {i, Length[labs]}],
   Cases[Except[PlotRangeClipping -> True]]],
  ImagePadding -> ipad]      
            
End[]
EndPackage[]

