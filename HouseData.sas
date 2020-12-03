/* Initialise data ------------------------------------------------------------------------------------------------------------------- */

ods pdf file = 'D:\Documents\GitHub\SASMultivariate\HouseDataOutput.pdf';

options ls = 78;

/* Load data from CSV */
data HouseData;
  infile "D:\Documents\GitHub\SASMultivariate\HouseData.csv" firstobs = 2 delimiter = ',';
  input Nbr Nba Stry Sqft Price HOA Rec Edu Crm Grn Trn;
run;


/* Create matrix of scatterplots */;
ods graphics on;
proc corr data = HouseData noprob plots (maxpoints = 100000) = matrix;
  var Nbr Nba Stry Sqft Price HOA Rec Edu Crm Grn Trn;
run;
ods graphics off;


proc corr data = HouseData out = CorrMatrix cov noprob;
  var Nbr Nba Stry Sqft Price HOA Rec Edu Crm Grn Trn;
  run;
