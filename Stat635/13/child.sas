*Name: Arthur Lui;

/*
Using Child Growth data, use model from HW12 (with random child).

  - Do residuals at
    - error level 
    - child level 

  - Do influence plots at
    - error level 
    - child level 

  - Make conclusions.
*/

data child;
  infile 'child.dat';
  input kid sex $ age len;
run;
*proc print;

ods graphics on;
  proc MIXED data=child method=ml;
    class sex;
    model len = sex age age*sex / s
                outpred=p1 outpredm=p2 vciry influence;
    random intercept / subject = kid;
  run;

  proc sgplot data=p2;
    scatter y = scaledresid x=pred/group=kid;
    loess y=scaledresid x=pred/nomarkers;
  run;
ods graphics off;
