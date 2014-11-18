data collin;
  infile 'cleanData.txt';
  input text $
        frstPrsn inrThnk thnkPstv thnkNgtv thnkAhd thnkBck reason socTies
        drctAct intract notify lnrGuide wrdPict spceInt motion pastEvnt timeInt
        shftEvnt txtCvrg genre counter corpus corpGen;
run;  
*proc print;

*proc factor corr mineigin=1 scree preplot rotate=varimax reorder plot;
*  var frstPrsn inrThnk thnkPstv thnkNgtv thnkAhd thnkBck reason socTies
*  drctAct intract notify lnrGuide wrdPict spceInt motion pastEvnt timeInt
*  shftEvnt;
*run;  

PROC DISCRIM method=normal pool=yes list crosslist manova;
   CLASS group;
   var wdim circum fbeye eyehd earhd jaw;
   title 'LINEAR Classification Analysis';
run;
