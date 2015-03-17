data sim;
  infile 'data.txt';
  input y xl z;
run;

proc MIXED data=sim;
  class z;
  model y = xl / s;
  random intercept / subject = z G V SOLUTION;
run;
