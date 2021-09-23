(* ::Package:: *)

BeginPackage["cmTix`"];
 
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
	
powTixAlt::usage=
 "powTixAlt[min,max,inc]
	same as powTix, but adds blank ticks. Work in progress,
	eventually should combine with powTix

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
	
linTixScAlt::usage=
 "linTixScAlt[min,max,inc,sc]
	same as linTixSc, but adds blank ticks.
	Work in progress, eventually should combine 
	with linTixSc

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
	
blankLinTixScAlt::usage=
 "blankLinTixScAlt[min,max,inc,sc]
	same as blankLinTixSc, but adds blank ticks.
	Work in progress, eventually should combine 
	with blankLinTixSc

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
	
cmPlot3D::usage=
 "cmPlot3D[f,{x,xMin,xMax},{y,yMin,yMax}]  
			 usage the same as Plot3D with default style values set
			 for use in publication ____";
			 
fitPlot23D::usage=
 "fitPlot23D[data1, data2, model, coeffs, var,}]  

	input: data1   (3d data set) data to be fit
		   data2   (3d data set)) data not fit
		   model   (function) model for fit
		   coeffs  (list) coefficients to be found from fit
		   var     (var) variable in fitting model

	returns: 3d point plot with fitlines as shown in publication___";
	
NTo::usage=
 "NTo[n,p]
	input: n  (float) float to be rounded
		   p  (int)   number of sig-figs to round to
	returns: n rounded to p sig-figs for when N[n,p] won't work ";
	


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
 labStyle[fs] = {
  FontFamily -> 
  If[
  MemberQ[$FontFamilies,"TeX Gyre Heros"],"TeX Gyre Heros",
  If[MemberQ[$FontFamilies,"Helvetica Neue"],"Helvetica Neue",
  If[MemberQ[$FontFamilies,"Helvetica"],"Helvetica"],
  Automatic]],
  FontSize -> fs,
  FontColor -> Black}
  
powTix[min_, max_, inc_] := {
Table[{N[10^i], ToString[Superscript[10, i],FormatType -> StandardForm]},{i,min, max, inc}],
Table[{N[10^i], Null},{i,min, max, inc}]}

powTixAlt[min_, max_, inc_] := {
SortBy[
   Join[
Table[{N[10^i], ToString[Superscript[10, i],FormatType -> StandardForm],0.0175},{i,min, max, inc}],
Flatten[Table[
Table[{N[i], Null,0.0025},{i,10^(min+j*inc),10^(min+(j+1)*inc),Abs[(10^(min+(j+1)*inc))-(10^(min+j*inc))]/10}],
{j,-5,10}],1]
],
First],
 SortBy[
   Join[
Table[{N[10^i], Null,0.0175},{i,min, max, inc}],
Flatten[Table[
Table[{N[i], Null,0.0025},{i,10^(min+j*inc),10^(min+(j+1)*inc),Abs[(10^(min+(j+1)*inc))-(10^(min+j*inc))]/10}],
{j,-5,10}],1]
],
First]}
  
linTix[min_, max_, inc_] := {
  Table[{N[i], i}, {i, min, max, inc}],
  Table[{N[i], Null}, {i, min, max, inc}]}
  
blankPowTix[min_, max_, inc_] := {
  Table[{N[10^i], Null}, {i, min, max, inc}],
  Table[{N[10^i], Null}, {i, min, max, inc}]}
  
linTixSc[min_, max_, inc_, sc_, dec_ : 2] := {
  Table[{N[i], NumberForm[i*sc, {4, dec}]},{i, min, max, inc}],
  Table[{N[i], Null}, {i, min, max, inc}]}
  
linTixScAlt[min_, max_, inc_, sc_, dec_ : 2] := {
  SortBy[
   Join[
    Table[{N[i], NumberForm[i*sc, {4, dec}], 0.02}, {i, min, max, 
      inc}],
    Table[{N[i], Null, 0.0175}, {i, min - inc, max + inc, inc/2}],
    Table[{N[i], Null, 0.0125}, {i, min - inc, max + inc, inc/10}]],
   First],
  SortBy[
   Join[
    Table[{N[i], Null, 0.02}, {i, min, max, inc}],
    Table[{N[i], Null, 0.0175}, {i, min - inc, max + inc, inc/2}],
    Table[{N[i], Null, 0.0125}, {i, min - inc, max + inc, inc/10}]],
   First]}
  
blankLinTixSc[min_, max_, inc_, sc_] := {
  Table[{N[i], Null}, {i, min, max, inc}],
  Table[{N[i], Null}, {i, min, max, inc}]}
  
blankLinTixScAlt[min_, max_, inc_, sc_, dec_ : 2] := {
  SortBy[
   Join[
    Table[{N[i], Null, 0.02}, {i, min, max, inc}],
    Table[{N[i], Null, 0.0175}, {i, min - inc, max + inc, inc/2}],
    Table[{N[i], Null, 0.0125}, {i, min - inc, max + inc, inc/10}]],
   First],
  SortBy[
   Join[
    Table[{N[i], Null, 0.02}, {i, min, max, inc}],
    Table[{N[i], Null, 0.0175}, {i, min - inc, max + inc, inc/2}],
    Table[{N[i], Null, 0.0125}, {i, min - inc, max + inc, inc/10}]],
   First]}
  
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
  
cmPlot3D[
  fun_,
  xValues_,
  yValues_,
  OptionsPattern[{
    ticksStyle -> Table[{Black,labStyle[20]}, {i, 3}],
    workingPrecision -> MachinePrecision,
    performanceGoal->"Quality",
    labelStyle -> labStyle[20],
    axesStyle -> labStyle[20],
    axesLabel -> {"", "", ""},
    plotStyle -> Automatic,
    plotRange -> {Automatic, Automatic, Full},
    scalingFunctions->{"Linear","Linear","Linear"},
    ticks -> Automatic}]] := 
 Module[{
   labs,
   xs = xValues,
   ys = yValues,
   ts = OptionValue[ticksStyle],
   pg= OptionValue[performanceGoal], 
   wp = OptionValue[workingPrecision],
   sf = OptionValue[scalingFunctions],
   ls = OptionValue[labelStyle],
   as = OptionValue[axesStyle],
   ax = OptionValue[axesLabel],
   ps = OptionValue[plotStyle],
   pr = OptionValue[plotRange],
   tx = OptionValue[ticks]},
  
  labs = axLab3D[ax, {-\[Pi]/10, 9 \[Pi]/40, \[Pi]/2}, as];
  
  Plot3D[fun, xs, ys,
   PlotTheme -> "Detailed",
   PlotStyle -> ps,
   BoxRatios -> {1, .75, 1},
   ViewPoint -> {8.64566, -12.5664, 6.88637},
   PlotRange -> pr,
   ImageSize -> 450,
   ClippingStyle -> None,
   TicksStyle -> ts,
   Ticks -> tx,
   AxesLabel -> labs,
   LabelStyle -> ls,
   ScalingFunctions->sf,
   PlotLegends -> None,
   WorkingPrecision->wp,
   PlotPoints -> 500,
   Mesh -> None,
   PerformanceGoal -> pg]]
   
   
fitPlot23D[data1_, data2_, model_, coeffs_, var_,
  OptionsPattern[{
    ticksStyle -> Table[labStyle[25], {i, 3}],
    workingPrecision -> 32,
    axesLabel -> None,
    plotRange -> {Automatic, Automatic, Automatic}, 
    scalingFunctions -> {"Linear", "Linear", "Linear"},
    ticks -> Automatic}]] :=
 Module[{
   dt12d, ft, dPlot, fPlot, dPlot2,
   col, zmx,zmn, tvals,
   dt1 = data1,
   dt2 = data2,
   mdl = model,
   cf = coeffs,
   z = var,
   ts = OptionValue[ticksStyle],
   wp = OptionValue[workingPrecision],
   sf = OptionValue[scalingFunctions],
   ax = OptionValue[axesLabel],
   pr = OptionValue[plotRange],
   tx = OptionValue[ticks]},
   
  dt1=SetPrecision[dt1,wp];
  dt2=SetPrecision[dt2,wp];
  mdl=SetPrecision[mdl,wp];
   
  dt12d = Table[dt1[[iT]][[All, {1, 3}]], {iT, Length[dt1]}];
  
  tvals = 
   DeleteDuplicates@
    Flatten@Table[dt1[[iT]][[All, {2}]], {iT, Length[dt1]}];
  
  (* Create a color gradient for temperatures-to be used in plots *)
 
   col = Table[
    RGBColor[tt/Length[dt1], 0, 1 - tt/Length[dt1]], {tt, 0, 
     Length[dt1], Length[dt1]/(Length[dt1] - 1)}];
  zmx =
   N@Max[{
      Max[dt1[[1]][[All, 1]]],
      Max[dt2[[1]][[All, 1]]]}];
  zmn =
   N@Min[{
      Min[dt1[[1]][[All, 1]]],
      Min[dt2[[1]][[All, 1]]]}];
  
  ft =
   Table[{z, tvals[[iT]],
     NonlinearModelFit[
       dt12d[[iT]],
       mdl,
       cf,
       z,
       ConfidenceLevel -> .9999,
       WorkingPrecision -> Precision[dt12d[[iT]]]]["BestFit"]},
    {iT, Length[dt12d]}];
  
  dPlot =
   ListPointPlot3D[
    dt1,
    PlotTheme -> "Detailed",
    BoxRatios -> {2.5, 1.5, 1.75},
    ViewPoint -> {8, -7, 3.5},
    PlotRange -> pr,
    ScalingFunctions -> sf,
    PlotStyle -> Table[{
       PointSize[.015], col[[i]]}, {i, Length[col]}],
    ImageSize -> 475,
    TicksStyle -> ts,
    Ticks -> tx,
    AxesLabel -> ax,
    Background -> None];
  dPlot2 =
   ListPointPlot3D[
    dt2,
    PlotTheme -> "Detailed",
    BoxRatios -> {2.5, 1.5, 1.75},
    ViewPoint -> {8, -7, 3.5},
    PlotRange -> pr,
    ScalingFunctions -> sf,
    PlotStyle -> Table[{
       PointSize[.015], col[[i]]}, {i, Length[col]}],
    ImageSize -> 475,
    TicksStyle -> ts,
    Ticks -> tx,
    AxesLabel -> ax,
    Background -> None];
  fPlot =
   ParametricPlot3D[Evaluate[
     ft /. z -> zVal],
    {zVal, 0+ .01*zmn, zmx + .1*zmx},
    PlotTheme -> "Detailed",
    BoxRatios -> {2.5, 1.5, 1.75},
    ViewPoint -> {8, -7, 3.5},
    PlotRange -> pr,
    ScalingFunctions -> sf,
    PlotStyle -> Table[
      {Black, Opacity[1]},
      {i, Length[dt1]}],
    ImageSize -> 475,
    TicksStyle -> ts,
    Ticks -> tx,
    AxesLabel -> ax,
    PlotLegends -> None,
    Background -> None];
  Return[Show[{dPlot, dPlot2, fPlot}]]] 

  
  
     
            
End[]
EndPackage[]

