/* Initialise data ------------------------------------------------------------------------------------------------------------------- */

ods pdf file = 'D:\Documents\GitHub\SASMultivariate\HouseDataOutput.pdf';

options ls = 78;

/* Load data from CSV */
data HouseData;
  infile "D:\Documents\GitHub\SASMultivariate\HouseData.csv" firstobs = 2 delimiter = ',';
  input Nbr Nba Stry Sqft Price HOA Rec Edu Crm Grn Trn;
run;
