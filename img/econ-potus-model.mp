outputformat := "png";
outputtemplate := "%j-%c.%o";
hppp := 0.6;
vppp := 0.6;
numeric pi; pi := 3.1415926;

vardef exp(expr x) = (mexp(256)**x) enddef;
vardef normPdf(expr x, s) = exp(-0.5*((x/s)**2))/(s*sqrt(2*pi)) enddef;
vardef invlogit(expr x) = (if x>10: (1/(1+exp(-x))) else: (exp(x)/(1+exp(x))) fi) enddef;
vardef ln primary x = (if x=0: 0 else: mlog(x)/256 fi) enddef;
vardef logit(expr x) = (ln(x) - ln(1-x)) enddef;

color lightgray;
lightgray = 0.75[black,white];
numeric u;
u = 1pc;

% outputtemplate := "statistical-experiment.%o";
beginfig(1);
  draw (0,0) -- (0, 10u);
  drawarrow (0,0) -- (15u, 0);

  numeric election;
  election := 12u;
  draw (election, 0)--(election, 10u) withcolor lightgray;

  label.rt(btex time etex, (15u, 0));
  label.lft(btex probability etex, (0, 9u));
  label.bot(btex election day etex, (election, 0));
  label(btex $\bullet$ etex, (election, 0));

  draw (0, 5u)--(15u, 5u) dashed evenly; % 50% probability
  label.lft(btex $50\%$ etex, (0, 5u));

  draw (0, 3u)--(15u, 3u);
  label.lft(btex prior etex, (0, 3u));
  label(btex $\bullet$ etex, (0, 3u));
  label(btex $\bullet$ etex, (election, 3u));
endfig;

beginfig(2);
  draw (0,0) -- (0, 10u);
  drawarrow (0,0) -- (15u, 0);

  numeric election;
  election := 12u;
  draw (election, 0)--(election, 10u) withcolor lightgray;

  label.rt(btex time etex, (15u, 0));
  label.lft(btex probability etex, (0, 9u));
  label.bot(btex election day etex, (election, 0));
  label(btex $\bullet$ etex, (election, 0));

  draw (0, 5u)--(15u, 5u) dashed evenly; % 50% probability
  label.lft(btex $50\%$ etex, (0, 5u));

  draw (0, 3u)--(15u, 3u);
  label.lft(btex prior etex, (0, 3u));
  label(btex $\bullet$ etex, (0, 3u));
  label(btex $\bullet$ etex, (election, 3u));

  path pdf;
  numeric sd;
  numeric endpoint;
  sd := 0.5;
  endpoint := 1 + (15/sd);
  pdf := (election-2u*normPdf(-3*sd, sd), 3u-(3u*sd))
    for i=1 upto endpoint:
    ..(election-2u*normPdf(-3sd+i*0.1, sd), 3u-3u*sd+u*0.1*i)
  endfor;

  draw pdf withcolor 0.35[red,black];

  sd := 1;
  endpoint := 1 + (15*4/sd);

  pdf := (election-2u*normPdf(-3*sd, sd), 3u-(3u*sd))
    for i=1 upto endpoint:
    ..(election-2u*normPdf(-3*sd+i*0.1, sd), 3u-3u*sd+u*0.1*i)
  endfor;
  draw pdf withcolor 0.35[green,black];
endfig;


beginfig(3);
  draw (0,0) -- (0, 10u);
  drawarrow (0,0) -- (15u, 0);

  numeric election;
  election := 12u;
  draw (election, 0)--(election, 10u) withcolor lightgray;

  label.rt(btex time etex, (15u, 0));
  label.lft(btex probability etex, (0, 9u));
  label.bot(btex election day etex, (election, 0));
  label(btex $\bullet$ etex, (election, 0));

  draw (0, 5u)--(15u, 5u) dashed evenly; % 50% probability
  label.lft(btex $50\%$ etex, (0, 5u));

  draw (0, 3u)--(15u, 3u);
  label.lft(btex prior etex, (0, 3u));
  label(btex $\bullet$ etex, (0, 3u));
  label(btex $\bullet$ etex, (election, 3u));

  path pdf;
  numeric sd;
  numeric endpoint;
  sd := 0.5;
  endpoint := 1 + (15/sd);
  pdf := (election-2u*normPdf(-3*sd, sd), 3u-(3u*sd))
    for i=1 upto endpoint:
    ..(election-2u*normPdf(-3sd+i*0.1, sd), 3u-3u*sd+u*0.1*i)
  endfor;

  draw pdf withcolor 0.35[red,black];
  label(btex $\bullet$ etex, (election, 3.4u)) withcolor 0.25[red,black];

endfig;

beginfig(4);
  drawdblarrow (0,-5u) -- (0, 5u);
  drawarrow (0,0) -- (15u, 0) dashed evenly;
  numeric prior;
  prior := logit(0.3); % -0.36797678529;

  numeric election;

  election := 12u;

  path pdf;
  numeric sd;
  numeric endpoint;
  sd := 0.5;
  endpoint := 1 + (15/sd);
  pdf := (election-2u*normPdf(-3*sd, sd), prior*u-(3u*sd))
    for i=1 upto endpoint:
    ..(election-2u*normPdf(-3sd+i*0.1, sd), prior*u-(3u*sd)+(u*0.1*i))
  endfor;
  numeric tBottom;
  numeric tTop;
  tBottom = ypart (((election, prior*u-sd*u)--(election-5*u, (prior-sd)*u)) intersectiontimes pdf);
  tTop = ypart (((election, prior*u+sd*u)--(election-5*u, (prior+sd)*u)) intersectiontimes pdf);
  z[0] = point tBottom of pdf;
  z[1] = point tTop of pdf;
  path p[];
  % p0 = subpath(tBottom, tTop) of pdf;
  % draw p0 withpen pencircle scaled 5;

  p[0] = (election, (prior-sd)*u)--z0--(subpath(tBottom, tTop) of pdf)--z1--(election,(prior+sd)*u)--cycle;

  fill p0 withcolor 0.75[red,white];
  draw (election, (prior-sd)*u)--z0 withcolor 0.35[red,black];
  draw z1--(election,(prior+sd)*u) withcolor 0.35[red, black];

  draw (election, -5u)--(election, 5u) withcolor lightgray;

  label.rt(btex time etex, (15u, 0));
  label.lft(btex log-odds etex, (0, 4u));
  label.lrt(btex election day etex, (election, prior*u));
  label(btex $\bullet$ etex, (election, 0));

  label.ulft(btex $0$ etex, (0, 0));

  draw (0, prior*u)--(15u, prior*u);
  label.llft(btex prior etex, (0, prior*u));
  label(btex $\bullet$ etex, (0, prior*u));
  label(btex $\bullet$ etex, (election, prior*u));
  draw pdf withcolor 0.35[red,black];

  numeric guess;
  guess :=  logit(0.34); % prior+1;
  label(btex $\bullet$ etex, (election, guess*u)) withcolor 0.25[red,black];

endfig;

beginfig(5);
  numeric prior;
  prior := logit(0.3); % -0.36797678529;
  numeric sd;
  sd := 0.5;
  numeric endpoint;
  endpoint := 1 + (15/sd);
  numeric guess;
  guess :=  logit(0.34); % prior+1;

  numeric election;
  election := 12u;

  fill (election, guess*u)--(election-8u,guess*u+2u)--(election-8u,guess*u-2u)--cycle withcolor 0.75[red,white];
  drawdblarrow (0,-5u) -- (0, 5u);
  drawarrow (0,0) -- (15u, 0) dashed evenly;


  draw (election, -5u)--(election, 5u) withcolor lightgray;

  label.rt(btex time etex, (15u, 0));
  label.lft(btex log-odds etex, (0, 4u));
  label.lrt(btex election day etex, (election, prior*u));
  label(btex $\bullet$ etex, (election, 0));

  label.ulft(btex $0$ etex, (0, 0));

  draw (0, prior*u)--(15u, prior*u);
  label.llft(btex prior etex, (0, prior*u));
  label(btex $\bullet$ etex, (0, prior*u));
  label(btex $\bullet$ etex, (election, prior*u));

  path pdf;
  pdf := (election-2u*normPdf(-3*sd, sd), prior*u-(3u*sd))
    for i=1 upto endpoint:
    ..(election-2u*normPdf(-3sd+i*0.1, sd), prior*u-(3u*sd)+(u*0.1*i))
  endfor;
  draw pdf withcolor 0.35[red,black];

  label(btex $\bullet$ etex, (election, guess*u)) withcolor 0.25[red,black];

  draw (election, guess*u)--(election-8u,guess*u-2u) withcolor 0.25[red,black];
  draw (election, guess*u)--(election-8u,guess*u+2u) withcolor 0.25[red,black];
endfig;

beginfig(6);
  numeric prior;
  prior := logit(0.3); % -0.36797678529;
  numeric sd;
  sd := 0.5;
  numeric endpoint;
  endpoint := 1 + (15/sd);
  numeric guess;
  guess := logit(0.34); % prior+1;

  numeric election;
  election := 12u;

  path upperBound;
  upperBound := (election-6u,guess*u-u)..(election-7u,guess*u+0u)..(election-8u,guess*u+u)..(election-9u,guess*u+1.5u)..(election-10u,guess*u+1.75u)..(election-12u,guess*u+2u);
  path lowerBound;
  lowerBound := (election-6u,guess*u+u)..(election-7u,guess*u+1.5u)..(election-8u,guess*u+2u)..(election-9u,guess*u+2.25u)..(election-10u,guess*u+2.5u)..(election-12u,guess*u+3.5u);
  fill (election-6u,guess*u+u)--lowerBound--(election-12u,guess*u+2u)--reverse(upperBound)--cycle withcolor 0.75[red,white];
  fill (election, guess*u)--(election-6u,guess*u+u)--(election-6u,guess*u-u)--cycle withcolor 0.75[red,white];
  drawdblarrow (0,-5u) -- (0, 5u);
  drawarrow (0,0) -- (15u, 0) dashed evenly;


  draw (election, -5u)--(election, 5u) withcolor lightgray;

  label.rt(btex time etex, (15u, 0));
  label.lft(btex log-odds etex, (0, 4u));
  label.lrt(btex election day etex, (election, prior*u));
  % label(btex $\bullet$ etex, (election, 0));

  label.ulft(btex $0$ etex, (0, 0));

  draw (0, prior*u)--(15u, prior*u);
  label.llft(btex prior etex, (0, prior*u));
  % label(btex $\bullet$ etex, (0, prior*u));
  % label(btex $\bullet$ etex, (election, prior*u));

  path pdf;
  pdf := (election-2u*normPdf(-3*sd, sd), prior*u-(3u*sd))
    for i=1 upto endpoint:
    ..(election-2u*normPdf(-3sd+i*0.1, sd), prior*u-(3u*sd)+(u*0.1*i))
  endfor;
  draw pdf withcolor 0.35[red,black];

  label(btex $\bullet$ etex, (election, guess*u)) withcolor 0.25[red,black];

  draw (election, guess*u)--(election-6u,guess*u-u) withcolor 0.25[red,black];
  draw (election, guess*u)--(election-6u,guess*u+u) withcolor 0.25[red,black];


  draw (election-6u,guess*u+u)--(election-6u,guess*u-u) dashed evenly withcolor 0.5[black,red];
  draw upperBound withcolor 0.25[red,black];

  draw lowerBound withcolor 0.25[red,black];

  label(btex $\times$ etex, (election-6u, guess*u));
  label(btex $\times$ etex, (election-6.5u, guess*u+u));
  label(btex $\times$ etex, (election-7u, guess*u+2u));
  label(btex $\times$ etex, (election-7.5u, guess*u+3u));
  label(btex $\times$ etex, (election-8u, guess*u+1.2u));
  label(btex $\times$ etex, (election-8.5u, guess*u+2u));
  label(btex $\times$ etex, (election-9u, guess*u-u));
  label(btex $\times$ etex, (election-9.5u, guess*u+u));
  label(btex $\times$ etex, (election-10u, guess*u+2.5u));
  label(btex $\times$ etex, (election-10.5u, guess*u+2u));
  label(btex $\times$ etex, (election-11u, guess*u+3u));
  label(btex $\times$ etex, (election-11.5u, guess*u+2.7u));

endfig;


beginfig(7);
  numeric prior;
  prior := 3; %
  numeric sd;
  sd := 0.5;
  numeric endpoint;
  endpoint := 1 + (15/sd);
  numeric logitPrior;
  logitPrior := logit(0.3); % -0.8472979;
  numeric guess;
  guess := logit(0.34); % 1+logitPrior; % invlogit(1+logitPrior);

  numeric election;
  election := 12u;

  guess := logit(0.34); % (1+logitPrior);
  path upperBound;
  upperBound := (election-6u,invlogit(guess-1)*10u)..(election-7u,invlogit(guess)*10u)..(election-8u,invlogit(guess+1)*10u)..(election-9u,invlogit(guess+1.5)*10u)..(election-10u,invlogit(guess+1.75)*10u)..(election-12u,invlogit(guess+2)*10u);
  path lowerBound;
  lowerBound := (election-6u,invlogit(guess+1)*10u)..(election-7u,invlogit(guess+1.5)*10u)..(election-8u,invlogit(guess+2)*10u)..(election-9u,invlogit(guess+2.25)*10u)..(election-10u,invlogit(guess+2.5u)*10u)..(election-12u,invlogit(guess+3.5u)*10u);
  fill (election-6u,invlogit(guess+1)*10u)--lowerBound--(election-12u,invlogit(guess+2)*10u)--reverse(upperBound)--cycle withcolor 0.75[red,white];
  fill (election, invlogit(guess)*10u)--(election-6u,invlogit(guess+1)*10u)--(election-6u,invlogit(guess-1)*10u)--cycle withcolor 0.75[red,white];


  draw (0,0u) -- (0, 10u);
  drawarrow (0,0) -- (15u, 0);


  draw (election, 0)--(election, 10u) withcolor lightgray;

  label.rt(btex time etex, (15u, 0));
  label.llft(btex probability etex, (0, 10u));

  label.lft(btex $50\%$ etex, (0, 5u));


  draw (0, prior*u)--(election, prior*u);

  label.lft(btex prior etex, (0, prior*u));
  label(btex $\bullet$ etex, (0, prior*u));
  label(btex $\bullet$ etex, (election, invlogit(guess)*10u)) withcolor 0.5[black,red];

  % % path pdf;
  % % pdf := (election-2u*normPdf(-3*sd, sd), prior*u-(3u*sd))
  % %   for i=1 upto endpoint:
  % %   ..(election-2u*normPdf(-3sd+i*0.1, sd), prior*u-(3u*sd)+(u*0.1*i))
  % % endfor;
  % % draw pdf withcolor 0.35[red,black];

  % label(btex $\bullet$ etex, (election, guess*u)) withcolor 0.25[red,black];

  label.bot(btex election day etex, (election, 0));
  draw (election, invlogit(guess)*10u)--(election-6u,invlogit(guess-1)*10u) withcolor 0.25[red,black];
  draw (election, invlogit(guess)*10u)--(election-6u,invlogit(guess+1)*10u) withcolor 0.25[red,black];


  draw (election-6u,invlogit(guess+1)*10u)--(election-6u,invlogit(guess-1)*10u) dashed evenly withcolor 0.5[black,red];
  draw upperBound withcolor 0.25[red,black];

  draw lowerBound withcolor 0.25[red,black];

  label(btex $\times$ etex, (election-6u, invlogit(guess)*10u));
  label(btex $\times$ etex, (election-6.5u, invlogit(guess+1)*10u));
  label(btex $\times$ etex, (election-7u, invlogit(guess+2)*10u));
  label(btex $\times$ etex, (election-7.5u, invlogit(guess+3)*10u));
  label(btex $\times$ etex, (election-8u, invlogit(guess+1.2)*10u));
  label(btex $\times$ etex, (election-8.5u, invlogit(guess+2)*10u));
  label(btex $\times$ etex, (election-9u, invlogit(guess-1)*10u));
  label(btex $\times$ etex, (election-9.5u, invlogit(guess+1)*10u));
  label(btex $\times$ etex, (election-10u, invlogit(guess+2.5)*10u));
  label(btex $\times$ etex, (election-10.5u, invlogit(guess+2)*10u));
  label(btex $\times$ etex, (election-11u, invlogit(guess+3)*10u));
  label(btex $\times$ etex, (election-11.5u, invlogit(guess+2.7)*10u));

  draw (0,5u)--(election, 5u) dashed evenly withcolor 0.25[black,white];
endfig;





end;