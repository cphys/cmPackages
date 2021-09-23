(* ::Package:: *)

BeginPackage["cmQuad`"];

numDif::usage = 
 "numDif[f1, f2]
 
 	returns: The number of decimal places which are the same between
	 two numbers";

nPtGauss2D::usage =
 "nPtGauss2D[integrand,{x,xmin,xmax,nx},{y,ymin,ymax,ny}]
 
	returns: The result of a two dimensional Gauss-Legendre quadrature 
	where nx is the number of abscissas in the x-direction and ny is
	the number of abscissas in the y-direction";
 	
tanSinhQuad2D::usage=
 "tanShinhQuad2D[integrand,{x,xmin,xmax,nx},{y,ymin,ymax,ny}]

	returns: The result of a two dimensional Tanh-Sinh quadrature";
	
tanSinhQuad1D::usage=
 "tanShinhQuad2D[integrand,{x,xmin,xmax,nx}]

	returns: The result of a one dimensional Tanh-Sinh quadrature";

nPtGauss1D::usage =
 "nPtGauss2D[integrand,{x,xmin,xmax,nx}]
 
 	returns: The result of a one dimensional Gauss-Legendre where
 	nx is the number of abscissas in the x-direction";

tsglQuad2D::usage =
 "tsglQuad2D[integrand,{x,xmin,xmax,nx},{y,ymin,ymax,ny}]
 
 	returns: The result of a two dimensional integral, in which
 	Gauss-Legendre quadrature is performed over the x-coodinates,
 	and tan-sinh quadrature is performed over the y-coordinates";
 	
gaussLaguerre1D::usage =
 "gaussLaguerre1D[integrand,{x,xmin,xmax,nx}]
 
 	returns: The result of a one dimensional integral, in which
 	Gauss-Laguerre quadrature is performed over the x-coodinates";
 	
gaussLaguerre2D::usage =
 "gaussLaguerre2D[integrand,{x,xmin,xmax,nx},{y,ymin,ymax,ny}]
 
 	returns: The result of a two dimensional Gauss-Laguerre quadrature";

gaussChebyshev1D::usage =
 "gaussChebyshev1D[integrand,{x,xmin,xmax,nx}]
 
 	returns: The result of a one dimensional integral, in which
 	Gauss-Chebyshev quadrature is performed over the x-coodinates";
 	
gaussChebyshev2D::usage =
 "gaussLaguerre2D[integrand,{x,xmin,xmax,nx},{y,ymin,ymax,ny}]
 
 	returns: The result of a two dimensional Gauss-Laguerre quadrature";
 	
 			
convergance1D::usage =
 "convergance1D[integrand,{x,xmin,xmax,nx},Nx]
 
 	returns: Performs 1D Gauss-Legendre quadrature on integration on
 	the integrand, gradually increasing the number of abscissas from \
xmin to xmax in steps of nx counting the number of \
decimals which are unchanged in each step. Nx are the number of \
desired 'convergent' decimal places before stopping the integration.";
 		
convergance::usage =
 "convergance[integrand,{x,xmin,xmax,nx},{y,ymin,ymax,ny},{Nx,Ny}]
 
 	returns: Performs 2D Gauss-Legendre quadrature on integration on
 	the integrand, gradually increasing the number of abscissas from \
xmin/ymin to xmax/ymax in steps of nx/yx counting the number of \
decimals which are unchanged in each step. Nx/Ny are the number of \
desired 'convergent' decimal places before stopping the integration.";


Begin["`Private`"]

<< "NumericalDifferentialEquationAnalysis`"
ParallelNeeds["NumericalDifferentialEquationAnalysis`"]
wper = 64;

(* Test the number of matching decimal places between two functions *)
numDif[f1_, f2_] := 
 numDif[f1, f2] = Log[10, Abs[f1]] - Log[10, Abs[f1 - f2]]

       
nPtGauss2D[
   func_,
   tParams_?ListQ, prec_ : wper,
   uParams_?ListQ, prec_ : wper,
   OptionsPattern[{
   progress -> False, 
   workingPrecision -> wper}]] :=
  
  nPtGauss2D[func, tParams, uParams] =
   Module[{
      nodest, nodesu, x, wt, wu, deltt, deltu,
      f = func,
      tVal = tParams[[1]],
      at = tParams[[2]], bt = tParams[[3]], nt = tParams[[4]],
      uVal = uParams[[1]],
      au = uParams[[2]], bu = uParams[[3]], nu = uParams[[4]],
      pr = OptionValue[progress],
      prc = OptionValue[workingPrecision]},
     
     weights[n_, min_, max_] :=      
      weights[n, min, max] = 
       GaussianQuadratureWeights[n , min, max, prc];
     
     wt = SetPrecision[weights[nt + 1, -1, 1], prc];
     wu = SetPrecision[weights[nu + 1, -1, 1], prc];
     deltt = (bt - at)/2;
     deltu = (bu - au)/2;
     
     
     (* View progress of calculation *)
     If[pr,
      Column[{PrintTemporary[
         Row[{ProgressIndicator[Dynamic[it], {1, Length[wt]}], 
           "Nt = " Dynamic[it]}]],
        PrintTemporary[
         Row[{ProgressIndicator[Dynamic[iu], {1, Length[wu]}], 
           "Nu = " Dynamic[iu]}]]}]];
     
     SetPrecision[deltt*deltu*
       Sum[
        wt[[it]][[2]]* wu[[iu]][[2]]* 
           f /. {tVal -> deltt*wt[[it]][[1]] + (bt + at)/2, 
          uVal -> deltu*wu[[iu]][[1]] + (bu + au)/2},
        {iu, Length[wu]},
        {it, Length[wt]}], prc]] // Quiet;
        
nPtGauss1D[
  integrand_,
  tParams_?ListQ, prec_ : wper,
  OptionsPattern[{
  progress -> False,
  workingPrecision -> wper}]] :=
  Module[{
    wt, deltt,
    f = integrand,
    tVal = tParams[[1]],
    at = tParams[[2]], bt = tParams[[3]], nt = tParams[[4]],
    pr = OptionValue[progress],
    prc = OptionValue[workingPrecision]},
   
   
   weights[n_, min_, max_] :=    
    weights[n, min, max] = 
     GaussianQuadratureWeights[n , min, max, prc];
   
   wt = SetPrecision[weights[nt + 1, -1, 1], prc];
   
   deltt = (bt - at)/2;
   (* View progress of calculation *)
   If[pr,
    PrintTemporary[
     Row[{ProgressIndicator[Dynamic[it], {1, Length[wt]}], 
       "Nt = " Dynamic[it]}]]];

   SetPrecision[deltt*Sum[
     wt[[it]][[2]]*
       f /. {tVal -> deltt*wt[[it]][[1]] + (bt + at)/2},
     {it, Length[wt]}],prc]]// Quiet;
     
tanSinhQuad1D[
   integrand_,
   tParams_?ListQ, prec_ : wper,
   OptionsPattern[{
   progress -> False,
   workingPrecision -> wper}]] :=
  
  tanSinhQuad1D[integrand, tParams] =
   Module[{
     ht, deltt,
     f = integrand,
     tVal = tParams[[1]],
     at = tParams[[2]], bt = tParams[[3]], Nt = tParams[[4]],
     pr = OptionValue[progress],
     prc = OptionValue[workingPrecision]},
   
    ht = 1/100;
    
    arg[k_, h_] := arg[k, h] = 1/2 * \[Pi] * Sinh[k*h];
    
    wgts[k_, h_] := 
     wgts[k, h] = (1/2*\[Pi]*Cosh[k*h])/Cosh[arg[k, h]]^2;
    
    abscs[k_, h_] := abscs[k, h] = Tanh[arg[k, h]];
    
    deltt = (bt - at)/2;
    (* View progress of calculation *)
    If[pr,
     PrintTemporary[
        Row[{ProgressIndicator[Dynamic[kt], {-Nt, Nt}], 
          "Nt = " Dynamic[kt]}]]];

     N[SetPrecision[ht*deltt*Sum[wgts[kt, ht]*f/.
     {tVal -> deltt*abscs[kt, ht]+(bt + at)/2},
     {kt,-Nt, Nt}],prc],prc]]//Quiet;  
     
tanSinhQuad2D[
   integrand_,
   tParams_?ListQ, prec_ : wper,
   uParams_?ListQ, prec_ : wper,
   OptionsPattern[{
   progress -> False,
   workingPrecision -> wper}]] :=
  
  tanSinhQuad2D[integrand, tParams, uParams] =
   Module[{
     hu,ht, wt, deltt,deltu,
     f = integrand,
     tVal = tParams[[1]],
     at = tParams[[2]], bt = tParams[[3]], Nt = tParams[[4]],
     uVal = uParams[[1]],
     au = uParams[[2]], bu = uParams[[3]], Nu = uParams[[4]],
     pr = OptionValue[progress],
     prc = OptionValue[workingPrecision]},
    
    hu = 1/100;
    ht = 1/100;
    
    arg[k_, h_] := arg[k, h] = N[1/2 * \[Pi] * Sinh[k*h],prc];
    
    wgts[k_, h_] := 
     wgts[k, h] = N[(1/2*\[Pi]*Cosh[k*h])/Cosh[arg[k, h]]^2,prc];
    
    abscs[k_, h_] := abscs[k, h] = N[Tanh[arg[k, h]],prc];
    
     deltt = (bt - at)/2;
     deltu = (bu - au)/2;

    (* View progress of calculation *)
    If[pr,
     Column[{PrintTemporary[
        Row[{ProgressIndicator[Dynamic[kt], {-Nt, Nt}], 
          "Nt = " Dynamic[kt]}]],
       PrintTemporary[
        Row[{ProgressIndicator[Dynamic[ku], {-Nu, Nu}], 
          "Nu = " Dynamic[ku]}]]}]]; 
    
    N[deltt*deltu*hu*ht*Sum[wgts[kt, ht]*
      Sum[wgts[ku, hu]*
          f/. {tVal -> deltt*abscs[kt, ht]+(bt + at)/2,
          uVal -> deltu*abscs[ku, hu]+(bu + au)/2},
        {ku, -Nu, Nu}],
      {kt,-Nt, Nt}],prc]]//Quiet;  
     
tsglQuad2D[
   integrand_,
   tParams_?ListQ, prec_ : wper,
   uParams_?ListQ, prec_ : wper,
   OptionsPattern[{
   progress -> False, 
   workingPrecision -> wper}]] :=
  
  tsglQuad2D[integrand, tParams, uParams] =
   Module[{
     hu, wt, deltt,deltu,
     f = integrand,
     tVal = tParams[[1]],
     at = tParams[[2]], bt = tParams[[3]], nt = tParams[[4]],
     uVal = uParams[[1]],
     au = uParams[[2]], bu = uParams[[3]], Nu = uParams[[4]],
     pr = OptionValue[progress],
     prc = OptionValue[workingPrecision]},
    
    hu = 1/100;
    
    weights[n_, min_, max_] :=
     weights[n, min, max] = 
      GaussianQuadratureWeights[n , min, max, prc];
    
    wt = weights[nt + 1, -1, 1];
    
    arg[k_, h_] := arg[k, h] = 1/2 * \[Pi] * Sinh[k*h];
    
    wgts[k_, h_] := 
     wgts[k, h] = (1/2*\[Pi]*Cosh[k*h])/Cosh[arg[k, h]]^2;
    
    abscs[k_, h_] := abscs[k, h] = Tanh[arg[k, h]];
    
    deltt = (bt - at)/2;
    deltu = (bu - au)/2;
    (* View progress of calculation *)
    If[pr,
     Column[{PrintTemporary[
        Row[{ProgressIndicator[Dynamic[it], {1, Length[wt]}], 
          "Nt = " Dynamic[it]}]],
       PrintTemporary[
        Row[{ProgressIndicator[Dynamic[ku], {-Nu, Nu}], 
          "Nu = " Dynamic[ku]}]]}]]; 
    
    hu*deltu*deltt*Sum[
      wt[[it]][[2]]*Sum[ wgts[ku, hu]*
          f/. {tVal -> deltt*wt[[it]][[1]] + (bt + at)/2,
          uVal -> deltu*abscs[ku, hu] + (bu + au)/2},
        {ku, -Nu, Nu}],
      {it, Length[wt]}]]// Quiet;
      
      
gaussLaguerre1D[
      func_,
      tParams_?ListQ, prec_ : wper,
      OptionsPattern[{
      progress -> False, 
      workingPrecision -> wper}]] :=
  
  gaussLaguerre1D[func, tParams] =
    Module[{
      x, wt, absc, absct,
      f = func,
      tVal = tParams[[1]],
      at = tParams[[2]], bt = tParams[[3]], nt = tParams[[4]],
      pr = OptionValue[progress],
      prc = OptionValue[workingPrecision]},
     
     absc[n_] :=      
      absc[n] = 
       Sort[Flatten[
         Values[#] & /@ ToRules[#] & /@ 
          List @@ Assuming[x > 0, 
            Chop@N[Roots[LaguerreL[n, x] == 0, x], prc]]]];
     
     wt = 
      Table[absc[nt][[i]]/((nt + 1)^2*
        Abs[LaguerreL[nt + 1, absc[nt][[i]]]]^2), {i, 
        Length[absc[nt]]}];
     
     absct = absc[nt];
     
     
     (* View progress of calculation *)
     If[pr,
      Column[{
        PrintTemporary[
         Row[{ProgressIndicator[Dynamic[it], {1, Length[wt]}],
           "Nt = " Dynamic[it]}]]}]];
     
     SetPrecision[
      Sum[
       wt[[it]]* Exp[absct[[it]]]*
         f /. {tVal -> absct[[it]]},
        {it, Length[wt]}], prc]] // Quiet;
        
gaussLaguerre2D[
      func_,
      tParams_?ListQ, prec_ : wper,
      uParams_?ListQ, prec_ : wper,
      OptionsPattern[{
      progress -> False, 
      workingPrecision -> wper}]] :=
  
  gaussLaguerre2D[func, tParams, uParams] =
    Module[{
            x, wt, wu, absc, absct, abscu,
            f = func,
            tVal = tParams[[1]],
            at = tParams[[2]], bt = tParams[[3]], nt = tParams[[4]],
            uVal = uParams[[1]],
            au = uParams[[2]], bu = uParams[[3]], nu = uParams[[4]],
            pr = OptionValue[progress],
            prc = OptionValue[workingPrecision]},
     
     absc[n_] :=      
      absc[n] = 
       Sort[Flatten[
         Values[#] & /@ ToRules[#] & /@ 
          List @@ Assuming[x > 0, 
            Chop@N[Roots[LaguerreL[n, x] == 0, x], prc]]]];
     
     wt = 
      Table[absc[nt][[i]]/((nt + 1)^2*
        Abs[LaguerreL[nt + 1, absc[nt][[i]]]]^2), {i, Length[absc[nt]]}];
     wu = 
      Table[absc[nu][[i]]/((nu + 1)^2*
        Abs[LaguerreL[nu + 1, absc[nu][[i]]]]^2), {i, Length[absc[nu]]}];
     
     absct = absc[nt];
     abscu = absc[nu];
     
     (* View progress of calculation *)
     If[pr,
      Column[{
        PrintTemporary[
         Row[{ProgressIndicator[Dynamic[it], {1, Length[wt]}],
           "Nt = " Dynamic[it]}]],
        PrintTemporary[
         Row[{ProgressIndicator[Dynamic[iu], {1, Length[wu]}],
           "Nu = " Dynamic[iu]}]]}]];
     
     SetPrecision[
      Sum[
       wt[[it]]*Exp[absct[[it]]]* wu[[iu]]*Exp[abscu[[iu]]]*
         f /. {tVal -> absct[[it]], uVal -> abscu[[iu]]},
       {iu, Length[wu]},
        {it, Length[wt]}], prc]] // Quiet;
        
gaussChebyshev1D[
         func_,
         tParams_?ListQ, prec_ : wper,
         OptionsPattern[{
         progress -> False, 
         workingPrecision -> wper}]] :=  
    gaussChebyshev1D[func, tParams] =
       Module[{
             x, wt, absc, absct, dt, deltt,
            f = func,
            tVal = tParams[[1]],
            at = tParams[[2]], bt = tParams[[3]], nt = tParams[[4]],
            pr = OptionValue[progress],
            prc = OptionValue[workingPrecision]},
     
     wt =  Table[\[Pi]/nt, {i, 1, nt}];
          
          absct = Table[ Cos[(2*i - 1)/(2*nt)*\[Pi]], {i, 1, nt}];
     
     deltt = (bt - at)/2;
          
          
          (* View progress of calculation *)
     	If[pr,
      Column[{
        PrintTemporary[          
         Row[{ProgressIndicator[Dynamic[it], {1, Length[wt]}], 
           "Nt = " Dynamic[it]}]]}]];
     
      SetPrecision[deltt*
       Sum[
         wt[[it]]*Sqrt[1 - absct[[it]]^2]*
          f /. {tVal -> deltt* absct[[it]] + (bt + at)/2},
         {it, Length[wt]}], prc]] // Quiet;
         
gaussChebyshev2D[
         func_,
         tParams_?ListQ, prec_ : wper,
         uParams_?ListQ, prec_ : wper,
         OptionsPattern[{
         progress -> False, 
         workingPrecision -> wper}]] :=  
    gaussChebyshev2D[func, tParams, uParams] =
       Module[{
             x, wt, wu, abscu, absct, dt, deltt, deltu,
            f = func,
            uVal = uParams[[1]],
            au = uParams[[2]], bu = uParams[[3]], nu = uParams[[4]],
            tVal = tParams[[1]],
            at = tParams[[2]], bt = tParams[[3]], nt = tParams[[4]],
            pr = OptionValue[progress],
            prc = OptionValue[workingPrecision]},
     
          wt =  Table[\[Pi]/nt, {i, 1, nt}];
          wu =  Table[\[Pi]/nt, {i, 1, nu}];
          
          absct = Table[ Cos[(2*i - 1)/(2*nt)*\[Pi]], {i, 1, nt}];
          abscu = Table[ Cos[(2*i - 1)/(2*nu)*\[Pi]], {i, 1, nu}];
     
          deltt = (bt - at)/2;
          deltu = (bu - au)/2;
          
          
          (* View progress of calculation *)
     If[pr,
      Column[{
        PrintTemporary[          
         Row[{ProgressIndicator[Dynamic[it], {1, Length[wt]}], 
           "Nt = " Dynamic[it]}]],
        PrintTemporary[          
         Row[{ProgressIndicator[Dynamic[iu], {1, Length[wu]}], 
           "Nu = " Dynamic[iu]}]]}]];
     
      SetPrecision[deltt*deltu*
       Sum[
         wt[[it]]*wu[[iu]]*Sqrt[1 - absct[[it]]^2]*
          Sqrt[1 - abscu[[iu]]^2]*
          f /. {tVal -> deltt* absct[[it]] + (bt + at)/2, 
          uVal -> deltu* abscu[[iu]] + (bu + au)/2},
        {iu, Length[wu]},
         {it, Length[wt]}], prc]] // Quiet;
         
convergance1D[
   integrand_,
   tParams_?ListQ,
   nConv_,
   OptionsPattern[{range -> {0, 1},
     method -> nPtGauss1D, 
     progress -> True, 
     workingPrecision -> wper}]] :=
  
  convergance1D[integrand, tParams, nConv] =
   Module[{
      diff, dt, eq,
      f = integrand,
      at = OptionValue[range][[1]], 
      bt = OptionValue[range][[2]],
      mth = OptionValue[method],
      pr = OptionValue[progress],
      pd = OptionValue[percentDiff],
      tVal = tParams[[1]],
      tMin = tParams[[2]], tMax = tParams[[3]], tInt = tParams[[4]], 
      tPer = nConv},
     
     
     diff[dt_, nt_] :=
       numDif[
       mth[f, {tVal, at, bt, dt + nt}],
       mth[f, {tVal, at, bt, dt}]];
       
     eq[dt_, nt_] :=
       FullSimplify[
       mth[f, {tVal, at, bt, dt + nt}]===
       mth[f, {tVal, at, bt, dt}]];
     
     dt = tMin;
          
     (* View progress of calculation *)
     If[pr,
      Column[{
        PrintTemporary[
         Row[{ProgressIndicator[Dynamic[dt], {tMin, tMax}], 
           " Nt = " Dynamic[dt]}]]}]];
     Do[
     If[Or[diff[dt,tInt] >= tPer, eq[dt, tInt]],
         Return[{dt, mth[f, {tVal, at, bt, dt}]}]],
       {dt, tMin, tMax, tInt}]] // Quiet;
         
     
convergance[
   integrand_,
   tParams_?ListQ, prec_ : wper,
   uParams_?ListQ, prec_ : wper,
   nConv_?ListQ,
   OptionsPattern[{
   range -> {{-1, 1}, {-1, 1}},
     method -> nPtGauss2D, 
     progress -> False, 
     workingPrecision -> wper}]] :=
  
  convergance[integrand, tParams, uParams, nConv] =
   Module[{
      diff, dt, du,eq,
      f = integrand,
      au = OptionValue[range][[2]][[1]], 
      bu = OptionValue[range][[2]][[2]],
      at = OptionValue[range][[1]][[1]], 
      bt = OptionValue[range][[1]][[2]],
      mth = OptionValue[method],
      pr = OptionValue[progress],
      pd = OptionValue[percentDiff],
      tVal = tParams[[1]],
      tMin = tParams[[2]], tMax = tParams[[3]], tInt = tParams[[4]], 
      tPer = nConv[[1]],
      uVal = uParams[[1]],
      uMin = uParams[[2]], uMax = uParams[[3]], uInt = uParams[[4]], 
      uPer = nConv[[2]]},
     
     
     diff[dt_, du_, nt_, nu_] :=
       numDif[
       mth[f, {tVal, at, bt, dt + nt}, {uVal, au, bu, du + nu}],
       mth[f, {tVal, at, bt, dt}, {uVal, au, bu, du }]];
       
     eq[dt_, du_, nt_, nu_] :=
       FullSimplify[
       mth[f, {tVal, at, bt, dt + nt}, {uVal, au, bu, du + nu}]===
       mth[f, {tVal, at, bt, dt}, {uVal, au, bu, du }]];
     
     dt = tMin;
     du = uMin;
     
     (* View progress of calculation *)
     If[pr,
      Column[{
        PrintTemporary[
         Row[{ProgressIndicator[Dynamic[dt], {tMin, tMax}], 
           " Nt = " Dynamic[dt]}]],
        PrintTemporary[
         Row[{ProgressIndicator[Dynamic[du], {uMin, uMax}], 
           " Nu = " Dynamic[du]}]]}]];
     
     Do[
      If[Or[diff[dt, du, 0, uInt] >= uPer, eq[dt, du, 0, uInt]],
       Return[
        Do[
         If[Or[diff[dt, du, tInt, 0] >= tPer, eq[dt, du, tInt, 0]],
         Return[{dt, du, mth[f, {tVal, at, bt, dt}, {uVal, au, bu, du}]}]],
         {dt, tMin, tMax, tInt}]]],
      {du, uMin, uMax, uInt}]] // Quiet;
      
            
End[]
EndPackage[]




























