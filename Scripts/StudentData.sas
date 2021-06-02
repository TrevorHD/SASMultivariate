/* Initialise data ------------------------------------------------------------------------------------------------------------------- */

ods pdf file = 'D:\Documents\GitHub\SASMultivariate\Outputs\StudentDataOutput.pdf';

options ls = 78;

/* Load data from CSV and create unique ID for each student */
data StudentData;
  infile "D:\Documents\GitHub\SASMultivariate\Data\StudentData.csv" firstobs = 2 delimiter = ',';
  input Math Physics English History AdvM $ GPA NSECH;
  IDs = _n_;
run;





/* Assess normality of data ---------------------------------------------------------------------------------------------------------- */

/* Assess normality of each variable */
proc univariate data = StudentData noprint;
  histogram Math Physics English History GPA NSECH / normal(mu = est sigma = est);
  qqplot Math Physics English History GPA NSECH / normal(mu = est sigma = est);
run;

/* Create multivariate Q-Q plot */
proc princomp data = StudentData noprint std out = PComps; 
  var Math Physics English History GPA NSECH;
run;
data PComps;
  set PComps;
  d2 = uss(of prin1-prin6);  *Squared statistial distances;
run;
proc sort data = PComps;
  by d2;
run;
data plotdata;
  set PComps;
  prb = (_n_ -0.5)/40; 		 *n = 40;
  CsqQuant = cinv(prb, 6);   *Chi-squared quantiles;
run;
proc gplot data = plotdata;
  plot d2*CsqQuant;
run; quit;                   *Overall, data are multivariate normal





/* Examine correlations between variables -------------------------------------------------------------------------------------------- */

/* Create matrix of scatterplots */;
ods graphics on;
proc corr data = StudentData noprob nosimple nocorr plots (maxpoints = 100000) = matrix(nvar = all);
  var Math Physics English History GPA NSECH;
run;
ods graphics off;

/* Estimate sample correlation between subjects (and other metrics), with 95% CI on individual intervals */
proc corr data = StudentData cov fisher(alpha = 0.05 biasadj = no);
  var Math Physics English History;
run;

/* Estimate sample correlation between subjects (and other metrics), with 95% CI on simultaneous intervals via Bonferroni */
proc corr data = StudentData cov fisher(alpha = 0.0083333 biasadj = no); *alpha = 0.05/6 for 6 unique correlations;
  var Math Physics English History;
run;





/* Compare means to those for student population ------------------------------------------------------------------------------------- */

/* Sort by AdvM */
proc sort data = StudentData;
  by AdvM;
run;

/* Calculate sample covariance matrix */
proc corr cov noprob nosimple nocorr out = CorrMatrix;
  var Math Physics English History GPA NSECH;
run;

/* Construct 95% CIs and conduct hypothesis test with T^2 */
proc iml;
  use CorrMatrix;
  read all var _NUM_ where(_TYPE_ = "MEAN") into xbar[colname = varnames];
  read all var _NUM_ where(_TYPE_ = "COV") into S;
  read all var _NUM_ where(_TYPE_ = "N") into n;
  varnames = t(varNames);                         *Names of the variables;
  s2 = vecdiag(S);                                *Vector of variances;
  n = n[1];
  p = nrow(s);
  xbar = t(xbar);

  *Test against population means;
  mu0 = {77.7, 74.8, 86.3, 85.1, 3.62, 6.7};      *Values of mu to test aganst;
  t2 = n*t(xbar-mu0)*inv(s)*(xbar-mu0);           *T^2 test statistic;
  f = t2*(n-p)/p/(n-1);                           *F test statistic;
  alpha = 0.05;
  df1 = p;
  df2 = n-p;
  pval = 1-probf(f, df1, df2);
  fcrit = finv(1-alpha,df1,df2);
  print varnames xbar mu0;
  print t2 f fcrit df1 df2 pval;

  *Construct confidence intervals;
  t1 = tinv(1-alpha/2, n-1);
  tb = tinv(1-alpha/2/p, n-1);
  f = finv(1-alpha, p, n-p);
  LoI = xbar-t1*sqrt(s2/n);                       *Lower individual interval;
  UpI = xbar+t1*sqrt(s2/n);                       *Upper individual interval;
  LoB = xbar-tb*sqrt(s2/n);                       *Lower simultaneous interval (Bonferroni);
  UpB = xbar+tb*sqrt(s2/n);                       *Upper simultaneous interval (Bonferroni);
  LoT = xbar-sqrt(p*(n-1)*f*s2/(n-p)/n);          *Lower simultaneous interval (T^2);
  UpT = xbar+sqrt(p*(n-1)*f*s2/(n-p)/n);          *Upper simultaneous interval (T^2);
  print varNames LoI UpI LoB UpB LoT UpT;
quit;

/* Create profile plots of each variable */
  %let p = 6;
data SDP2;
  set StudentData;
  Vars = "Math"; 	 Ratio = Math/73.1; 	output;
  Vars = "Physics";  Ratio = Physics/70.8;  output;
  Vars = "English";  Ratio = English/86.3;  output;
  Vars = "History";  Ratio = History/85.1;  output;
  Vars = "GPA";   	 Ratio = GPA/3.62;      output;
  Vars = "NSECH";    Ratio = NSECH/6.7;     output;
  keep Vars Ratio;
run;
proc sort data = SDP2;
  by Vars;
run;
proc means data = SDP2;
  by Vars;
  var Ratio;
  output out = SDP3 n = n mean = xbar var = s2;
run;
data SDP4;
  set SDP3;
  f = finv(0.95, &p, n-&p);
  ratio = xbar; output;
  ratio = xbar-sqrt(&p*(n-1)*f*s2/(n-&p)/n); output;     *Lower simultaneous interval (T^2);
  ratio = xbar+sqrt(&p*(n-1)*f*s2/(n-&p)/n); output;     *Upper simultaneous interval (T^2);
run;
proc gplot data = SDP4;
  axis1 length = 4 in;
  axis2 length = 6 in;
  plot Ratio*Vars / vaxis = axis1 haxis = axis2 vref = 1 lvref = 21;
  symbol v = none i = hilot color = black;
run; quit;





/* T^2 test to examine if scores are different between AdvM and non-AdvM ------------------------------------------------------------- */

/* Sort by AdvM */
proc sort data = StudentData;
  by AdvM;
run;

/* Calculate sample covariance matrix */
proc corr cov nocorr nosimple out = CorrMatrix;
  by AdvM;
  var Math Physics English History;
run;

/* Construct 99% CIs and conduct hypothesis test with T^2 */
proc iml;
use CorrMatrix;
  read all var _NUM_ where(_TYPE_ = "MEAN" & AdvM = 'No') into xbar1[colname = varnames];
  read all var _NUM_ where(_TYPE_ = "MEAN" & AdvM = 'Yes') into xbar2;
  read all var _NUM_ where(_TYPE_ = "N" & AdvM = 'No') into n1;
  read all var _NUM_ where(_TYPE_ = "N" & AdvM = 'Yes') into n2;
  read all var _NUM_ where(_TYPE_ = "COV" & AdvM = 'No') into S1;
  read all var _NUM_ where(_TYPE_ = "COV" & AdvM = 'Yes') into S2;
  varnames = t(varNames);
  xbar1 = t(xbar1);
  xbar2 = t(xbar2);
  n1 = n1[1];
  n2 = n2[1];
  p = nrow(S1);
  Sp = ((n1-1)*S1+(n2-1)*S2)*(1/(n1+n2-2));
  print varnames xbar1 xbar2;
  print s1 s2 sp;

  *Test group means against each other;
  t2 = t(xbar1-xbar2)*inv(Sp*(1/n1+1/n2))*(xbar1-xbar2);
  fstat = t2*(n1+n2-p-1)/p/(n1+n2-2);
  df1 = p;
  df2 = n1+n2-p-1;
  pval = 1-probf(fstat, df1, df2);
  print t2 fstat df1 df2 pval;

  *Construct confidence intervals;
  alpha = 0.05;
  s2 = vecdiag(Sp);
  t1 = tinv(1-alpha/2, n1+n2-2);
  tb = tinv(1-alpha/2/p, n1+n2-2);
  f = finv(1-alpha, p, n1+n2-p-1);
  LoI = xbar1-xbar2-t1*sqrt((1/n1+1/n2)*s2);                             *Lower individual interval;
  UpI = xbar1-xbar2+t1*sqrt((1/n1+1/n2)*s2);                             *Upper individual interval;
  LoB = xbar1-xbar2-tb*sqrt((1/n1+1/n2)*s2);                             *Lower simultaneous interval (Bonferroni);
  UpB = xbar1-xbar2+tb*sqrt((1/n1+1/n2)*s2);                             *Upper simultaneous interval (Bonferroni);
  LoT = xbar1-xbar2-sqrt(p*(n1+n2-2)*f*(1/n1+1/n2)*s2/(n1+n2-p-1));      *Lower simultaneous interval (T^2);
  UpT = xbar1-xbar2+sqrt(p*(n1+n2-2)*f*(1/n1+1/n2)*s2/(n1+n2-p-1));      *Upper simultaneous interval (T^2);
  print varnames LoI UpI LoB UpB LoT UpT;
quit;





/* MANOVA test to examine if scores are different between AdvM and non-AdvM ---------------------------------------------------------- */

/* Manova overall test, also gives univariate anova results */
proc glm data = StudentData;
  class AdvM;
  model Math Physics English History = AdvM;
  contrast 'Difference' AdvM  1 -1;
  estimate 'Difference' AdvM  1 -1;
  output out = resids r = rMath rPhysics rEnglish rHistory;
  manova h = AdvM / printe printh;
run; quit;

/* Assess if covariance matrices are constant */
proc sort data = resids;
  by AdvM;
run;
proc corr cov nocorr nosimple data = resids;
  by AdvM;
  var rMath rPhysics rEnglish rHistory;
run;
proc discrim noclassify data = StudentData pool = test;
  class AdvM;
  var Math Physics English History;
run;





/* Classify students into AdvM (or not) based on test scores, using discriminant analysis -------------------------------------------- */

/* Create scatterplots to examine subjects useful for classification */
proc sgscatter data = StudentData datasymbols = (CircleFilled) datacontrastcolors = (blue red); 
  compare y = Math x = Physics / group = AdvM;
run;
proc sgscatter data = StudentData datasymbols = (CircleFilled) datacontrastcolors = (blue red); 
  compare y = English x = History / group = AdvM;
run;
proc sgscatter data = StudentData datasymbols = (CircleFilled) datacontrastcolors = (blue red); 
  compare y = Math x = English / group = AdvM;
run;
proc sgscatter data = StudentData datasymbols = (CircleFilled) datacontrastcolors = (blue red); 
  compare y = Math x = History / group = AdvM;
run;
proc sgscatter data = StudentData datasymbols = (CircleFilled) datacontrastcolors = (blue red); 
  compare y = Physics x = English / group = AdvM;
run;
proc sgscatter data = StudentData datasymbols = (CircleFilled) datacontrastcolors = (blue red); 
  compare y = Physics x = History / group = AdvM;
run;

/* Create scatterplots to examine subjects if GPA and NSECH are useful for classification */
proc sgscatter data = StudentData datasymbols = (CircleFilled) datacontrastcolors = (blue red); 
  compare y = GPA x = NSECH / group = AdvM;
run;

/* Data points to classify */
data SDTest;
  input Math Physics English History GPA NSECH;
  cards;
  96 94 90 91 3.83 12
  82 77 89 88 3.69 6
  74 73 93 87 3.65 5
  94 90 80 75 3.70 7
  85 77 97 90 3.68 8
; run;

/* Classification via discriminant analysis, using all subjects as well as GPA and NSECH */
proc discrim data = StudentData pool = test crossvalidate testdata = SDTest testout = SDTestR1;
  class AdvM;
  var Math Physics English History GPA NSECH;
  priors 'No' = 0.5 'Yes' = 0.5;
run;
proc print data = SDTestR1;
run;

* CV matrix yields false positive rate of 20% and false negative rate of 20%; 

/* Classification via discriminant analysis, using all subjects */
proc discrim data = StudentData pool = test crossvalidate testdata = SDTest testout = SDTestR2;
  class AdvM;
  var Math Physics English History;
  priors 'No' = 0.5 'Yes' = 0.5;
run;
proc print data = SDTestR2;
run;

* CV matrix yields false positive rate of 15% and false negative rate of 25%;

/* Classification via discriminant analysis, using ONLY math and physics scores */
proc discrim data = StudentData pool = test crossvalidate testdata = SDTest testout = SDTestR3;
  class AdvM;
  var Math Physics;
  priors 'No' = 0.5 'Yes' = 0.5;
run;
proc print data = SDTestR3;
run;

* CV matrix yields false positive rate of 20% and false negative rate of 30%;





/* Classify students into AdvM (or not) based on test scores, using cluster analysis ------------------------------------------------- */

/* Sort data by ID and run cluster analysis using Ward's method */
proc sort data = StudentData;
  by IDs;
run;
proc cluster data = StudentData method = ward outtree = SDCluster1;
  var Math Physics English History GPA NSECH;
  id IDs;
run;

/* Assume 2 clusters and calculate resulting statistics */
proc tree data = SDCluster1 nclusters = 2 out = SDCluster2;
  id IDs;
run;
proc sort data = SDCluster2;
  by IDs;
run;
data SDCluster3;
  merge StudentData SDCluster2;
  by IDs;
run;
proc glm data = SDCluster3;
  class cluster;
  model Math Physics English History GPA NSECH = cluster;
  means cluster;
run; quit;

/* Create frequency table similar to CV table from discriminant analysis */
proc sort data = SDCluster3;
  by AdvM cluster;
run;
proc freq data = SDCluster3;
  tables AdvM*cluster;
run;

* Cluster 1 is AdvM and cluster 2 is not... false positive rate of 60% and false negative rate of 10%;

ods pdf close
