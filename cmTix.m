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

blankPowTixAlt::usage=
 "blankPowTixAlt[min,max,inc]

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
	
canvas::usage=
 "canvas[color_:GrayLevel[0,.35]]  
			 create a custom colored background for a 3d plot";
	
cmPlot3D::usage=
 "cmPlot3D[f,{x,xMin,xMax},{y,yMin,yMax}]  
			 usage the same as Plot3D with default style values set
			 for use in publication ____";
			 
cmDensityPlot3D::usage "
cmDensityPlot3D[f,{x,xMin,xMax},{y,yMin,yMax},{z,zMin,zMax}] 
             usage the same as DensityPlot3D with default style values set
			 for use in publication ____"; 
			 
cmDensityPlot2D::usage "
cmDensityPlot2D[f,{x,xMin,xMax},{y,yMin,yMax}] 
             usage the same as DensityPlot2D with default style values set
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

blankPowTixAlt[min_, max_, inc_] := {
SortBy[
   Join[
Table[{N[10^i], Null,0.0175},{i,min, max, inc}],
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

ploLabs3D[plt_, labs_, pos_, rot_, fs_, ipad_:Automatic] :=
 Show[
  Rasterize[plt,
   Background -> None],
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
  
canvas[
  color_ : GrayLevel[0, .25]] :=
  Module[{
    col = color},
    Graphics3D[{{
      EdgeForm[],
      col,
      EdgeForm[None],
      InfinitePlane[{
        Scaled[{0, 0, 0}],
        Scaled[{0, 1, 0}],
        Scaled[{1, 1, 0}]}],
      InfinitePlane[{
        Scaled[{0, 0, 0}],
        Scaled[{0, 0, 1}],
        Scaled[{0, 1, 1}]}],
      InfinitePlane[{
        Scaled[{0, 1, 0}],
        Scaled[{0, 1, 1}],
        Scaled[{1, 1, 1}]}]}}]];
  
cmPlot3D[
  fun_,
  xValues_,
  yValues_,
  OptionsPattern[{
    ticksStyle -> Table[{Black,labStyle[20]}, {i, 3}],
    workingPrecision -> MachinePrecision,
    performanceGoal -> "Quality",
    plotTheme -> "Detailed",
    labelStyle -> labStyle[20],
    axesStyle -> labStyle[20],
    axesLabel -> {"", "", ""},
    labelRotation -> {-\[Pi]/10, 9 \[Pi]/40, \[Pi]/2},
    plotStyle -> Automatic,
    plotRange -> {Automatic, Automatic, Full},
    scalingFunctions->{"Linear","Linear","Linear"},
    ticks -> Automatic,
    plotLegends -> None,
    plotPoints -> 500,
    imageSize -> 450,
    mesh -> None,
    clippingStyle -> None,
    boxRatios -> {1, .75, 1},
    viewPoint -> {8.64566, -12.5664, 6.88637}}]] := 
 Module[{
   labs,
   xs = xValues,
   ys = yValues,
   ts = OptionValue[ticksStyle],
   pg = OptionValue[performanceGoal],
   pt = OptionValue[plotTheme], 
   wp = OptionValue[workingPrecision],
   sf = OptionValue[scalingFunctions],
   ls = OptionValue[labelStyle],
   as = OptionValue[axesStyle],
   ax = OptionValue[axesLabel],
   lr = OptionValue[labelRotation],
   ps = OptionValue[plotStyle],
   pr = OptionValue[plotRange],
   tx = OptionValue[ticks],
   me = OptionValue[mesh],
   br = OptionValue[boxRatios],
   pl = OptionValue[plotLegends],
   pp = OptionValue[plotPoints],
   is = OptionValue[imageSize],
   cl = OptionValue[clippingStyle],
   vp = OptionValue[viewPoint]},
  
  labs = axLab3D[ax,lr, as];
  
  Plot3D[fun, xs, ys,
   PlotTheme -> pt,
   PlotStyle -> ps,
   BoxRatios -> br,
   ViewPoint -> vp,
   PlotRange -> pr,
   ImageSize -> is,
   ClippingStyle -> cl,
   TicksStyle -> ts,
   Ticks -> tx,
   AxesLabel -> labs,
   LabelStyle -> ls,
   ScalingFunctions->sf,
   PlotLegends -> pl,
   WorkingPrecision->wp,
   PlotPoints -> pp,
   Mesh -> me,
   PerformanceGoal -> pg]]
   
cmDensityPlot3D[
    \[Psi]Full_,
    xValues_,
    yValues_,
    zValues_,
    OptionsPattern[{
      performanceGoal -> "Quality",
      colorFunction -> "SunsetColors",
      plotTheme -> "Detailed",
      labelStyle -> {
        FontFamily -> "TeX Gyre Heros",
        FontSize -> 24,
        FontColor -> Black},
      plotRange -> Full,
      ticks -> Automatic,
      plotLegends -> None,
      legendLabel -> None,
      legendLayout -> "Column",
      labelingFunction -> (
        ScientificForm[
        (5.29177`*^-11)^-3*#1, {64, 1},
        NumberPadding -> {"", "0"}] &),
      plotPoints -> {50, 50, 50},
      imageSize -> 550,
      axesLabel -> None,
      viewPoint -> {8, -7, 3.5},
      background -> White,
      canvasColor -> GrayLevel[0, 0]}]] := 
    Module[{
      norm2,
      aoM, hbeV, cmM,
      zsc, zV, xV, yV,
      xinc, yinc, zinc,
      NPlot2, data3d,
      min, max,
      xVar = xValues[[1]], xmin = xValues[[2]], xmax = xValues[[3]],
      yVar = yValues[[1]], ymin = yValues[[2]], ymax = yValues[[3]],
      zVar = zValues[[1]], zmin = zValues[[2]], zmax = zValues[[3]],
      pg = OptionValue[performanceGoal],
      cf = OptionValue[colorFunction],
      pt = OptionValue[plotTheme],
      ls = OptionValue[labelStyle],
      pr = OptionValue[plotRange],
      tx = OptionValue[ticks],
      pl = OptionValue[plotLegends],
      ll = OptionValue[legendLabel],
      ly = OptionValue[legendLayout],
      lf = OptionValue[labelingFunction],
      pp = OptionValue[plotPoints],
      is = OptionValue[imageSize],
      vp = OptionValue[viewPoint],
      bg = OptionValue[background],
      xl = OptionValue[axesLabel],
      cc = OptionValue[canvasColor]},
      
    aoM = 5.2917699999999994`*^-11;
    hbeV = 6.582119568999999`*^-16;
    cmM = 3*10^8;
    
    xinc = ((xmax - xmin)/pp[[1]]);
    yinc = ((ymax - ymin)/pp[[2]]);
    zinc = ((zmax - zmin)/pp[[3]]);
    
    norm2 = Values[Flatten[
            NSolve[
              a^2*Integrate[
              Re[Conjugate[\[Psi]Full]*\[Psi]Full],
              {xVar, -\[Infinity], \[Infinity]},
              {yVar, -\[Infinity], \[Infinity]},
              {zVar, -\[Infinity], \[Infinity]}] == 1,a]]][[-1]];
              
    (* Square of the wave function *)
    NPlot2[ix_, iy_, iz_] :=   
      norm2^2* Re@(Conjugate[\[Psi]Full]*\[Psi]Full)  /. {xVar -> ix, 
          yVar -> iy, zVar -> iz};
    
     (* Create the numerical data for the plot *)
    data3d =
      Flatten[
        N@Table[
            {ix,
              iy,
              iz,
              NPlot2[ix, iy, iz] // Quiet},
            {ix, xmin, xmax, xinc},
            {iy, ymin, ymax, yinc},
            {iz, zmin, zmax, zinc}], 2];
    
    min = First@Flatten@MinimalBy[data3d[[All, {4}]], Last];
    max = First@Flatten@MaximalBy[data3d[[All, {4}]], Last];
    
    Show[{
    ListDensityPlot3D[
       data3d,
      ColorFunctionScaling -> False,
      ColorFunction -> (ColorData[cf]@
              Rescale[#1, {min, max}, {0, 1}] &),
      PerformanceGoal -> pg,
      PlotTheme -> pt,
      PlotRange -> pr,
      PlotLegends ->
        Evaluate[
          BarLegend[
            {(ColorData[cf]@Rescale[#1, {min, max}, {0, 1}] &),
              {0, max}},
            LabelStyle -> ls,
            Background -> bg,
            LabelingFunction -> lf,
            LegendLayout -> ly,
            LegendLabel -> ll]],
      AxesLabel -> xl,
      LabelStyle -> ls,
      Ticks -> tx,
      ViewPoint -> vp,
      ImageSize -> is,
      Background -> bg
      ],
      canvas[cc]}]]
      
cmDensityPlot2D[
        \[Psi]Full_,
        xValues_,
        yValues_,
        OptionsPattern[{
              performanceGoal -> "Quality",
              colorFunction -> "SunsetColors",
              plotTheme -> "Detailed",
              labelStyle -> {
                FontFamily -> "TeX Gyre Heros",
                FontSize -> 30,
                FontColor -> White},
              plotRange -> Full,
              ticks -> Automatic,
              legendLabel -> None,
              legendLayout -> Automatic,
              labelingFunction -> (
                    ScientificForm[
                       (5.29177`*^-11)^-3*#1, {64, 1},
                       NumberPadding -> {"", "0"}] &),
              plotPoints -> {125, 125},
              imageSize -> 500,
              frameLabel -> None,
              background -> Black,
              aspectRatio -> 1}]] := 
      Module[{
            norm2,
            aoM, hbeV, cmM,
            zsc, zV, xV, yV,
            xinc, yinc, zinc,
            NPlot2, dataxz, makeTicks,
            min, max,
            xVar = xValues[[1]],
            xmin = xValues[[2]],
            xmax = xValues[[3]],
            yVar = yValues[[1]],
            ymin = yValues[[2]],
            ymax = yValues[[3]],
            pg = OptionValue[performanceGoal],
            cf = OptionValue[colorFunction],
            pt = OptionValue[plotTheme],
            ls = OptionValue[labelStyle],
            pr = OptionValue[plotRange],
            tx = OptionValue[ticks],
            ll = OptionValue[legendLabel],
            ly = OptionValue[legendLayout],
            lf = OptionValue[labelingFunction],
            pp = OptionValue[plotPoints],
            is = OptionValue[imageSize],
            bg = OptionValue[background],
            fl = OptionValue[frameLabel],
            ar = OptionValue[aspectRatio]},
          
        aoM = 5.2917699999999994`*^-11;
        hbeV = 6.582119568999999`*^-16;
        cmM = 3*10^8;
        
        xinc = ((xmax - xmin)/pp[[1]]);
        yinc = ((ymax - ymin)/pp[[2]]);
        
        norm2 = Values[Flatten[NSolve[
                               a^2*Integrate[
                                  Re[Conjugate[\[Psi]Full]*\[Psi]Full],
                                  {xVar, -\[Infinity], \[Infinity]},
                                  {yVar, -\[Infinity], \[Infinity]}] == 1, a]]][[-1]];
                  
        (* Square of the wave function *)
        
  NPlot2[ix_, iy_] :=   
            norm2^2* 
          Re@(Conjugate[\[Psi]Full]*\[Psi]Full)  /. {xVar -> ix, 
     yVar -> iy};
        
       (* Create the numerical data for the plot *)
       dataxz =
            Flatten[
                N@Table[
                        {ix,
                          iy,
                          Re@NPlot2[ix, iy] // Quiet},
                        {ix, xmin, xmax, xinc},
                        {iy, ymin, ymax, yinc}], 1];
    
    min = First@Flatten@MinimalBy[dataxz[[All, {3}]], Last];
    max = First@Flatten@MaximalBy[dataxz[[All, {3}]], Last];
    makeTicks[range_, num_] := Rescale[#, {1, num}, range] & /@ Range[num];
    
    ListDensityPlot[
      dataxz,
      LabelStyle -> ls,
      ColorFunctionScaling -> False,
      ColorFunction -> (ColorData[cf]@Rescale[#1, {min, max}, {0, 1}] &),
      Background -> bg,
      PerformanceGoal -> pg,
      PlotTheme -> pt,
      PlotRange -> pr,
      AspectRatio -> ar,
      FrameLabel -> fl,
      FrameTicks -> tx,
      ImageSize -> is,
      PlotLegends ->
        Placed[
          BarLegend[
            {(ColorData[cf]@Rescale[#1, {min, max}, {0, 1}] &), {0, max}},
            LabelStyle -> {
           FontFamily -> "TeX Gyre Heros",
           FontSize -> 22,
           FontColor -> White},
        Ticks -> makeTicks[{0, .88*max}, 4],
        LegendLayout -> ly,
        Background -> bg,
        LabelingFunction -> lf,
        LegendLabel -> ll], {Above, Right}]]]
   
   
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

