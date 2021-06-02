/* Initialise data ------------------------------------------------------------------------------------------------------------------- */

ods pdf file = 'D:\Documents\GitHub\SASMultivariate\Outputs\CoordinationDataOutput.pdf';

options ls = 78;

/* Load data from CSV */
data CData;
  infile "D:\Documents\GitHub\SASMultivariate\Data\CoordinationData.csv" firstobs = 2 delimiter = ',';
  input ID Group $ C1 C2 C3 C4;
run;

/* Convert data to long form for split-plot ANOVA */
data CData2;
  set CData;
  Time = 12;  Coord = C1; output;
  Time = 18;  Coord = C2; output;
  Time = 24;  Coord = C3; output;
  Time = 30;  Coord = C4; output;
  drop C1 C2 C3 C4;
run;





/* Tests for Group, Time, and Group*Time effects ------------------------------------------------------------------------------------- */

/* Conduct split-plot ANOVA using long-form data */
proc glm data = CData2;
  class Group ID Time;
  model Coord = Group ID(Group) Time Group*Time;
  test h = Group e = ID(Group);
run; quit;

/* Conduct MANOVA using wide-form data */
proc glm data = CData;
  class Group;
  model C1 C2 C3 C4 = Group;
  manova h = Group m = C2-C1, C3-C2, C4-C3 / printh printe;   *Test for Group/Time interaction;
  manova h = Group m = C1+C2+C3+C4 / printh printe;           *Test for Group main effect;
run; quit;

/* T^2 test for time effects */
data CData3;
  set CData;
  d1 = C2-C1;
  d2 = C3-C2;
  d3 = C4-C3;
run;
proc iml;
  start TimeTest;
  mu0 = {0, 0, 0};                                       *Null hypothesis: no time effects;
  ones = j(nrow(x), 1, 1);
  id = i(nrow(x));
  ybar = x`*one/nrow(x);
  s = x`*(id-ones*ones`/nrow(x))*x/(nrow(x)-1.0);
  print mu0 ybar;                                        *Print out differences to compare to null values;
  print s;                                               *Print out covariance matrix;
  t2 = nrow(x)*(ybar-mu0)`*inv(s)*(ybar-mu0);
  f = (nrow(x)-ncol(x))*t2/ncol(x)/(nrow(x)-1);
  df1 = ncol(x);
  df2 = nrow(x)-ncol(x);
  p = 1-probf(f, df1, df2);
  print t2 f df1 df2 p;                                  *Print T^2 test data;
  finish;
  use CData3;
  read all var{d1 d2 d3} into x;
run TimeTest; quit;





/* Visualise data -------------------------------------------------------------------------------------------------------------------- */

/* Create profile plots */
proc sort data = CData2;
  by Group Time;
  run;
proc means data = CData2;
  by Group Time;
  var Coord;
  output out = PP mean = mean;
  run;
proc gplot data = PP;
  axis1 length = 4 in;
  axis2 length = 6 in;
  plot mean*Time = Group / vaxis = axis1 haxis = axis2;
  symbol1 v = K f = special h = 2 i = join color = black;
  symbol2 v = K f = special h = 2 i = join color = blue;
  symbol3 v = K f = special h = 2 i = join color = red;
run; quit;

* It's pretty obvious there are Group, Time, and Group*Time effects;

ods pdf close
