/* Initialise data ------------------------------------------------------------------------------------------------------------------- */

ods pdf file = 'D:\Documents\GitHub\SASMultivariate\Outputs\HouseDataOutput.pdf';

options ls = 78;

/* Load data from CSV */
data HouseData;
  infile "D:\Documents\GitHub\SASMultivariate\Data\HouseData.csv" firstobs = 2 delimiter = ',';
  input Nbr Nba Stry Sqft Price HOA Rec Edu Crm Grn Trn;
run;





/* Examine correlations between variables -------------------------------------------------------------------------------------------- */

/* Create matrix of scatterplots */
ods graphics on;
proc corr data = HouseData plots (maxpoints = 100000) = matrix(nvar = all);
  var Nbr Nba Stry Sqft Price HOA Rec Edu Crm Grn Trn;
run;
ods graphics off;

/* Examine relationship between house price and number of bedrooms */
proc sgscatter data = HouseData datasymbols = (CircleFilled); 
  compare y = Price  x = Nbr;
run;

/* Examine relationship between house price and number of bathrooms */
proc sgscatter data = HouseData datasymbols = (CircleFilled); 
  compare y = Price  x = Nba;
run;

/* Examine relationship between house price and square footage */
proc sgscatter data = HouseData datasymbols = (CircleFilled); 
  compare y = Price  x = Sqft;
run;





/* Use PCA to quantify price as a combination of size and location variables --------------------------------------------------------- */

/* PCA using standardised variables (correlation approach) */
proc princomp data = HouseData out = PComps;
  var Nbr Nba Stry Sqft HOA Rec Edu Crm Grn Trn;
run;
proc print data = PComps; 
  var prin1-prin10;
run;
proc corr data = PComps;
  var prin1 prin2 prin3 Nbr Nba Stry Sqft HOA Rec Edu Crm Grn Trn;
run;
proc gplot;
  axis1 length = 5 in;
  axis2 length = 5 in;
  plot prin2*prin1 / vaxis = axis1 haxis = axis2;
  symbol v = J f = special h = 2 i = none color = black;
run; quit;

/* Plots of first principal component against price */
proc sgscatter data = PComps datasymbols = (CircleFilled) datacontrastcolors = (black blue red orange); 
  compare y = Price  x = Prin1 / group = Nbr;
run;
proc sgscatter data = PComps datasymbols = (CircleFilled) datacontrastcolors = (black blue red orange yellow); 
  compare y = Price  x = Prin1 / group = Nba;
run;





/* Use regression to quantify price based on size and location variables ------------------------------------------------------------- */

/* Standardise Sqft since it is on a wildly different scale compared to other variables */
proc standard data = HouseData mean = 0 std = 1 out = StanHouseData;
  var Sqft;
run;

/* Run linear regression; calculate VIF */
proc reg data = StanHouseData;
  model Price = Nbr Nba Stry Sqft HOA Rec Edu Crm Grn Trn / vif tol collin;
run;

/* Drop Sqft since it is redundant and run regression again */
proc reg data = StanHouseData;
  model Price = Nbr Nba Stry HOA Rec Edu Crm Grn Trn / vif tol collin;
run;





/* Examine relationships between house size and location quality --------------------------------------------------------------------- */

/* Canonical correlation between house size and location quality */;
proc corr data = HouseData out = CorrMatrix cov noprob;
  var Nbr Nba Stry Sqft Price HOA Rec Edu Crm Grn Trn;
  run;
proc cancorr data = HouseData out = CannCorrs vprefix = HouseSize vname = "HouseSize"
             wprefix = LocationQuality wname = "LocationQuality";
  var Nbr Nba Stry Sqft;     *House size variables;
  with Rec Edu Crm Grn Trn;  *Location quality variables;
run;
proc gplot data = CannCorrs;
  axis1 length = 3 in;
  axis2 length = 4.5 in;
  plot HouseSize1*LocationQuality1 / vaxis = axis1 haxis = axis2;
  symbol v = J f = special h = 2 i = r color = black;
run; quit;

/* Scatterplot from above, but colour-coded by price */
proc sgplot data = CannCorrs;
  scatter x = LocationQuality1 y = HouseSize1 / colorresponse = Price colormodel = (yellow orange red)
          markerattrs = (symbol = CircleFilled size = 8);
run;

/* Run linear regression for price as function of HouseSize1 and LocationQuality1 */
proc reg data = CannCorrs;
  model Price = HouseSize1 LocationQuality1 / vif tol collin;
run;

ods pdf close

