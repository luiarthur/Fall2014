/*
Steps, using child growth dataset:

1. Propose 6 covariance structures for the fixed effects gender, age, and age*gender

2. Of those 6, select 1 by using/evaluating/analyzing 2 information criteria 
   of your choice.

3. Using your chosen model, test whether the interaction effect of age*gender 
   should be left in the model using all of the following: 
     - 2 information criteria of your choice, 
     - Likelihood Ratio Test, and 
     - Wald Test.  
*/

data child;
  infile 'child.dat';
  input kid sex $ age len;
run;
*proc print;

*1);
proc MIXED data=child method=ml;
  class sex;
  model len = sex age age*sex / s;
  random intercept / subject = kid;
run;

*2);
proc MIXED data=child method=ml;
  class sex;
  model len = sex age age*sex / s;
  random intercept / subject = kid type=vs;
run;

*3);
proc MIXED data=child method=ml;
  class sex;
  model len = sex age age*sex / s;
  random intercept / subject = kid type=toep;
run;

*4);
proc MIXED data=child method=ml;
  class sex;
  model len = sex age age*sex / s;
  random intercept / subject = kid type=toep(1);
run;

*5);
proc MIXED data=child method=ml;
  class sex;
  model len = sex age age*sex / s;
  random intercept / subject = kid type=ar(1);
run;

*6);
proc MIXED data=child method=ml;
  class sex;
  model len = sex age age*sex / s;
  random intercept / subject = kid type=arh(1);
run;
