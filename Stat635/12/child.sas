/*
Steps, using child growth dataset:

1. Propose 6 covariance structures for the fixed effects gender, age, and age*gender
   - Done.

2. Of those 6, select 1 by using/evaluating/analyzing 2 information criteria 
   of your choice.
   - Done. Choose #1: Lowest AIC (440.6)

3. Using your chosen model, test whether the interaction effect of age*gender 
   should be left in the model using all of the following: 
     - 2 information criteria of your choice, (AIC,BIC => interaction)
     - Likelihood Ratio Test, and (6.3 => keep)
     - Wald Test (p-val=.013 => keep)
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
*AIC = 440.6 ;
*BIC = 448.4 ;
*-2ll = 428.6;

proc MIXED data=child method=ml;
  class sex;
  model len = sex age / s;
  random intercept / subject = kid;
run;
*AIC = 444.9 ;
*BIC = 451.3 ;
*-2ll = 434.9;


/*
*2);
proc MIXED data=child method=ml;
  class sex;
  model len = sex age age*sex / s;
  random age/ subject = kid;
run;
*AIC = 443.7;

*3);
proc MIXED data=child method=ml;
  class sex;
  model len = sex age age*sex / s;
  random intercept age/ subject = kid;
run;
*AIC = 442.1;

*4);
proc MIXED data=child method=ml;
  class sex;
  model len = sex age age*sex / s;
  random sex/ subject = kid type=hf;
run;
*AIC = 444.4;

*5);
proc MIXED data=child method=ml;
  class sex;
  model len = sex age age*sex / s;
  random intercept sex age sex*age/ subject = kid;
run;
*AIC = 444.1;

*6);
proc MIXED data=child method=ml;
  class sex;
  model len = sex age age*sex / s;
  random Intercept / subject = kid type=ar(1);
run;
*AIC = 442.6;
