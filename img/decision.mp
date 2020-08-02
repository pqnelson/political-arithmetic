
outputformat := "png";
outputtemplate := "%j-%c.%o";
hppp := 0.6;
vppp := 0.6;
outputtemplate := "statistical-experiment.%o";
color lightgray;
lightgray = 0.75[black,white];

% verbatimtex \font\tenamsb=msbm10 \font\sevenamsb=msbm7 \font\fiveamsb=msbm5 etex
% verbatimtex \newfam\bbfam etex
% verbatimtex \textfont\bbfam=\tenamsb etex
% verbatimtex \scriptfont\bbfam=\sevenamsb etex
% verbatimtex \scriptscriptfont\bbfam=\fiveamsb etex
% verbatimtex \def\bbb{\fam\bbfam} etex;

% verbatimtex \input{amstex}\loadmsbm etex

beginfig(1);
  numeric u;
  u = 1pc;
  z[1] = (0, 0);
  z[2] = (4.5u, 0);
  z[3] = z2 + (4u, 0);
  z[4] = z3 + (5u, 0);
  z5 = z4 + (4u, 0);
  label(btex $\theta$ etex, z[1]);
  drawarrow z[1]+(.5u,0)--z[2]+(-2.25u,0);

  draw z2+(-2.25u,1.5u) -- z2+(-2.25u,-1.5u) -- z2+(2.25u,-1.5u) -- z2+(2.25u,1.5u) --cycle;
  label(btex Statistical etex, (z2+(0,u)));
  label(btex Experiment etex, z2);
  label(btex $P_{\theta}(\cdot)$ etex, (z2+(0, -u)));

  drawarrow z[2]+(2.25u,0)--z[3];
  label.rt(btex $X$ etex, z3);

  drawarrow z3+(u,0) -- z4+(-2.25u,0) withcolor lightgray;
  label(btex Decision etex, (z4+(0,u))) withcolor lightgray;
  label(btex Making etex, z4) withcolor lightgray;
  label(btex $\tau(\cdot)$ etex, (z4+(0, -u))) withcolor lightgray;

  draw z4+(-2.25u,1.5u) -- z4+(-2.25u,-1.5u) -- z4+(2.25u,-1.5u) -- z4+(2.25u,1.5u) --cycle withcolor lightgray;
  drawarrow z[4]+(2.25u,0)--z[5] withcolor lightgray;
  label.rt(btex $\tau(X) = \hat{T}$ etex, z5) withcolor lightgray;
endfig;

outputtemplate := "decision-making.%o";
beginfig(1);
  numeric u;
  % color lightgray;
  % lightgray = 0.75[black,white];
  u = 1pc;
  z[1] = (0, 0);
  z[2] = (4.5u, 0);
  z[3] = z2 + (4u, 0);
  z[4] = z3 + (5u, 0);
  z5 = z4 + (4u, 0);

  label(btex $\theta$ etex, z[1]) withcolor lightgray;
  drawarrow z[1]+(.5u,0)--z[2]+(-2.25u,0) withcolor lightgray;

  draw z2+(-2.25u,1.5u) -- z2+(-2.25u,-1.5u) -- z2+(2.25u,-1.5u) -- z2+(2.25u,1.5u) --cycle withcolor lightgray;
  label(btex Statistical etex, (z2+(0,u))) withcolor lightgray;
  label(btex Experiment etex, z2) withcolor lightgray;
  label(btex $P_{\theta}(\cdot)$ etex, (z2+(0, -u))) withcolor lightgray;

  drawarrow z[2]+(2.25u,0)--z[3] withcolor lightgray;
  label.rt(btex $X$ etex, z3);

  drawarrow z3+(u,0) -- z4+(-2.25u,0);
  label(btex Decision etex, (z4+(0,u)));
  label(btex Making etex, z4);
  label(btex $\tau(\cdot)$ etex, (z4+(0, -u)));

  draw z4+(-2.25u,1.5u) -- z4+(-2.25u,-1.5u) -- z4+(2.25u,-1.5u) -- z4+(2.25u,1.5u) --cycle;
  drawarrow z[4]+(2.25u,0)--z[5];
  label.rt(btex $\tau(X) = \hat{T}$ etex, z5);
endfig;



outputtemplate := "performance-evaluation.%o";
beginfig(1);
  numeric u;
  u = 1pc;
  z[1] = (0, 0);
  z[2] = (4.5u, 0);
  z[3] = z2 + (4u, 0); % X
  z[4] = z3 + (5u, 0);
  z5 = z4 + (4u, 0); % start of \tau(X) = \hat{T}

  z6 = z1 + (2u,-3.5u); % theta
  z7 = z6 + (4u, 0); % T(-)
  z8 = z7 + (4u, 0); % T(\theta)
  z9 = z8 + (5u, 0); % loss function\n l(-,-)
  z[10] = z9 + (6u,0); % \hat{T}

  z11 = z9 + (0, -4.5u); % E_{X \sim P_{\theta}}[\cdot]
  z12 = z11 + (5u, 0); % L_{\theta}(\tau)

  % LAYER 1
  label(btex $\theta$ etex, z[1]) withcolor lightgray;
  drawarrow z[1]+(.5u,0)--z[2]+(-2.25u,0) withcolor lightgray;

  draw z2+(-2.25u,1.5u) -- z2+(-2.25u,-1.5u) -- z2+(2.25u,-1.5u) -- z2+(2.25u,1.5u) --cycle withcolor lightgray;
  label(btex Statistical etex, (z2+(0,u))) withcolor lightgray;
  label(btex Experiment etex, z2) withcolor lightgray;
  label(btex $P_{\theta}(\cdot)$ etex, (z2+(0, -u))) withcolor lightgray;

  drawarrow z[2]+(2.25u,0)--z[3] withcolor lightgray;
  label.rt(btex $X$ etex, z3) withcolor lightgray;

  drawarrow z3+(u,0) -- z4+(-2.25u,0) withcolor lightgray;
  label(btex Decision etex, (z4+(0,u))) withcolor lightgray;
  label(btex Making etex, z4) withcolor lightgray;
  label(btex $\tau(\cdot)$ etex, (z4+(0, -u))) withcolor lightgray;

  draw z4+(-2.25u,1.5u) -- z4+(-2.25u,-1.5u) -- z4+(2.25u,-1.5u) -- z4+(2.25u,1.5u) --cycle withcolor lightgray;
  drawarrow z[4]+(2.25u,0)--z[5] withcolor lightgray;
  label.rt(btex $\tau(X) = \hat{T}$ etex, z5) withcolor lightgray;

  % LAYER 2
  drawarrow z5+(3.5u,-.5u) -- z10+(0,.5u) dashed evenly;
  drawarrow z1+(0,-.5u) -- z1+(0,-3.5u) -- z6+(-0.5u,0) dashed evenly;
  label(btex $\theta$ etex, z6);
  drawarrow z6+(.5u,0)--z7+(-u,0);

  draw z7+(-u,.5u)--z7+(-u,-.5u)--z7+(u,-.5u)--z7+(u,.5u)--cycle;
  label(btex $T(\cdot)$ etex, z7);

  drawarrow z7+(u,0) -- z8+(-u,0);
  label(btex $T(\theta)$ etex, z8);
  drawarrow z8+(u,0)--z9+(-2.75u,0);

  draw z9+(-2.75u,u)--z9+(-2.75u,-u) -- z9+(2.75u,-u) -- z9+(2.75u,u) -- cycle;
  label(btex Loss Function etex, z9+(0,.5u));
  label(btex $l(\cdot, \cdot)$ etex, z9+(0,-.5u));

  label(btex $\hat{T}$ etex, z[10]);
  drawarrow z[10]+(-.5u,0)--z9+(2.75u,0);

  % LAYER 3
  draw z11+(-2u,-.5u) -- z11+(-2u,.5u) -- z11+(2u, .5u) -- z11+(2u,-.5u) -- cycle;
  label(btex ${\rm E}_{X\sim P_{\theta}}[\cdot]$ etex, z11);

  drawarrow z9+(0,-u) -- z11+(0,.5u);
  label.rt(btex $l(T(\theta),\tau(X))$ etex, .5[z9+(0,-u), z11+(0,.5u)]);

  drawarrow z11+(2u,0) -- z12+(-u,0);
  label(btex $L_{\theta}(\tau)$ etex, z12);


endfig;

end.