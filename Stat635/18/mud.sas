*Arthur Lui;

data mud;
  infile 'mud.dat';
  input week $ tank tmt total baet lest other ;
  lchiro=log(baet);
run;
*proc print;

proc glimmix;
   class week tmt;
   model lchiro = week tmt total;
   random intercept / subject=tmt; 
run;
