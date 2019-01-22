/*********************************************************************************************************************************/
/*The following codes are for paper "Childhood psychosocial challenges and risk for obesity in U.S. men and women", M. M. Wall etc */

/* at Translational Psychiatry */

/**********************************************************************************************************************************/


/*******************************************************************************************************************************/
/* This part of codes  is for figure and tables in the paper. Another file (Childhood psychosocial challenges and risk for     */ 
/* obesity in U.S. men and women data preprocessing.sas) is for data preprocessing                                             */
/*******************************************************************************************************************************/

/*******************************************************************************************************************************/
/*                                                                                                                             */   
/*  DATA ANALYSIS FOR "Childhood psychosocial challenges and risk for obesity in U.S. men and women"                           */
/*                                                                                                                             */
/*******************************************************************************************************************************/


libname nesarc1 "XXXXXXXXXXXXXXXXXX";

/*mysmalldata, does not contain subjects with overweight under 13*/
data mydata;
set nesarc1.mydata_bmi0821;

if n1q8f =1 or n1q8h =1 then parentdeath=1;
else if n1q8f =2 or n1q8h =2 then parentdeath=2;

if n1q8a = 9 then parentdivorce =.;
else if (n1q8a = 1) or (n1q8a =2) then parentdivorce = n1q8a;
else if n1q8a = . then parentdivorce =3;
if chvb = 1 or chpa =1 or chsa =1 then childabuse=1;
else if chvb = 2 and chpa =2 and chsa =2 then childabuse=2;


if N1Q11A = 99 then earlykid1 = .;  /*with early kid (<=17)*/
 else if N1Q11A = 0 then earlykid1 = 0;
 else if N1Q11B = 99 then earlykid1 = .;
 else if N1Q11B >17 then earlykid1 = 0;
 else earlykid1 =1;

if bmi2 =1 or bmi2=2 then obese=0;
else obese =1; 

if bmi2 = 5 then severeobese=1;
else severeobese=0;

if bmi2 =1 or bmi2=2 then bmi_obese=0;
else bmi_obese =bmi2; 

/*chabuse  the highest frequency of child abuse*/
chabuse = max(N13Q1G, N13Q1H, N13Q1I, N13Q1J,N13Q2A, N13Q2B, N13Q2C, N13Q2D);

/*combine mdd18 anxiety18 or ptsd18 as one new variable*/
if mdd18 =1 or anxiety18 = 1 or ptsd18 = 1 then mdd_anxiety_ptsd18 =1;
else if mdd18 =0 and anxiety18 = 0 and ptsd18 = 0 then mdd_anxiety_ptsd18 =0;

/*combine alcohol18 nicotine18 or drug18 as one new variable*/
if alcohol18 = 1 or nicotine18 = 1 or drug18 =1 then aud_dud_tud18 =1;
else if alcohol18 = 0 and nicotine18 = 0 and drug18 =0 then aud_dud_tud18 =0;

/*combine alcohol18 or drug18 as one new variable*/
if alcohol18 = 1  or drug18 =1 then aud_dud18 =1;
else if alcohol18 = 0 and drug18 =0 then aud_dud18 =0;

stress_abuse = 2- childabuse;
/*childabuse =1, yes, =2 no */
stress_parentdeath = 2 - parentdeath;
if parentdivorce =1 or parentdivorce = 3 then stress_parentdivorce =1;
else if parentdivorce =2 then stress_parentdivorce =0;
/*parentdivorce =1, divorced, =2 not divorced, =3 NA (never married and others) */
stress_poverty = 2-n1q21a;
stress_edu = 2-edu2;
stress1 = stress_abuse + stress_parentdeath + stress_parentdivorce +mdd18 + nicotine18 + earlykid + stress_poverty + stress_edu;

if stress1 < 5 then stress =stress1;
else if stress1 >= 5 then stress =5;


run;

/*remove all subjects with overweight under 13 */
data mysmalldata;
set mydata;
if N1Q42A ne 2 then delete;
run;


proc sort data=mysmalldata;
by nsex;
run;


/* keep all subjects with overweight under 13*/
data mydata_overweight;
set mydata;
if N1Q42A ne 1 then delete;
run;




/********************************************************************************/
/*                                                                              */
/*           TABLE 1                                                            */
/*                                                                              */
/********************************************************************************/

proc surveyfreq data=mydata;
tables nsex*bmi2 nsex*n1q42a/chisq;
strata VARSTRAT;
cluster VARUNIT;
weight AUDWEIGHT;
run;

proc surveyfreq data=mysmalldata;
tables nsex*obese nsex*severeobese/col row chisq;
strata VARSTRAT;
cluster VARUNIT;
weight AUDWEIGHT;
run;

proc surveyfreq data=mydata_overweight;
tables nsex*obese nsex*severeobese/col row chisq;
strata VARSTRAT;
cluster VARUNIT;
weight AUDWEIGHT;
run;


/********************************************************************************/
/*                                                                              */
/*          END  OF  TABLE 1                                                    */
/*                                                                              */
/********************************************************************************/




/********************************************************************************/
/*                                                                              */
/*           TABLE 2                                                            */
/*                                                                              */
/********************************************************************************/


proc surveyfreq data=mysmalldata;
tables obese*(age1 nethrace nregion   n1q2b edu2  N1Q21A  childabuse  earlykid  mdd18 nicotine18 parentdeath parentdivorce PARMEND PARSUBD anxiety18 ptsd18 alcohol18 drug18)/col row;
strata VARSTRAT;
cluster VARUNIT;
weight AUDWEIGHT;
by nsex;
run;



proc surveylogistic data=mysmalldata;


class age1(ref='1') nregion(ref='1') nethrace(ref='1')  N1Q42A(ref='2') nicotine18(ref='0');
class  edu2(ref='2') earlykid(ref='0') N1Q21A(ref='2') n1q2b(ref='1') mdd18(ref='0');
class chvb(ref='2') chpa(ref='2') chsa(ref='2') childabuse(ref='2') parentdivorce(ref='2') parentdeath(ref ='2');
model obese(order =INTERNAL ref='0')=age1 nregion nethrace  n1q2b/link=glogit;
strata VARSTRAT;
cluster VARUNIT;
weight AUDWEIGHT;
by nsex;
run;


/*Estimate OR of each stressor, adjusted by demorgraphic variables,  and for male and female only respectively**/

%let var1=childabuse;
%let var2=parentdeath;
%let var3=parentdivorce;
%let var4=mdd18;
%let var5=earlykid;
%let var6=N1Q21A;
%let var7=edu2;
%let var9=PARMEND;
%let var10=PARSUBD;
%let var11=ptsd18;
%let var12=alcohol18;
%let var13=drug18;
%let var14=anxiety18;

%macro test3a;
%do i = 1%to 14;

proc surveylogistic data=mysmalldata;


class age1(ref='1') nregion(ref='1') nethrace(ref='1')  N1Q42A(ref='2') nicotine18(ref='0');
class  edu2(ref='2') earlykid(ref='0') N1Q21A(ref='2') n1q2b(ref='1') mdd18(ref='0');
class chvb(ref='2') chpa(ref='2') chsa(ref='2') childabuse(ref='2') parentdivorce(ref='2') parentdeath(ref ='2');;
model obese(order =INTERNAL ref='0')=  &&var&i age1 nregion nethrace    n1q2b   /link=glogit;
strata VARSTRAT;
cluster VARUNIT;
weight AUDWEIGHT;
by nsex;
run;
%end;
%mend test3A;



%test3A;


/********************************************************************************/
/*                                                                              */
/*          END  OF  TABLE 2                                                    */
/*                                                                              */
/********************************************************************************/




/********************************************************************************/
/*                                                                              */
/*           TABLE 3                                                            */
/*                                                                              */
/********************************************************************************/

data mysmalldataF;
set mysmalldata;
if nsex = 2;
run;

data mysmalldataM;
set mysmalldata;
if nsex = 1;
run;


proc freq data = mysmalldataF;
  tables (N1Q21A childabuse parentdeath parentdivorce PARMEND PARSUBD mdd18 nicotine18 ptsd18 alcohol18 drug18 anxiety18 earlykid edu2)*(N1Q21A childabuse parentdeath parentdivorce PARMEND PARSUBD mdd18 nicotine18 ptsd18 alcohol18 drug18 anxiety18 earlykid edu2) /plcorr;
  ods output measures=mycorrF (where=(statistic="Tetrachoric Correlation"
                                     or statistic="Polychoric Correlation")
                              keep = statistic table value);
run;

proc freq data = mysmalldataM;
  tables (N1Q21A childabuse parentdeath parentdivorce PARMEND PARSUBD mdd18 nicotine18 ptsd18 alcohol18 drug18 anxiety18 earlykid edu2)*(N1Q21A childabuse parentdeath parentdivorce PARMEND PARSUBD mdd18 nicotine18 ptsd18 alcohol18 drug18 anxiety18 earlykid edu2) /plcorr;
  ods output measures=mycorrM (where=(statistic="Tetrachoric Correlation"
                                     or statistic="Polychoric Correlation")
                              keep = statistic table value);
run;


/********************************************************************************/
/*                                                                              */
/*          END  OF  TABLE 3                                                    */
/*                                                                              */
/********************************************************************************/




/********************************************************************************/
/*                                                                              */
/*           TABLE 4                                                            */
/*                                                                              */
/********************************************************************************/

/* OR estimate*/
proc surveylogistic data=mysmalldata;


class age1(ref='1') nregion(ref='1') nethrace(ref='1')  N1Q42A(ref='2') nicotine18(ref='0');
class  edu2(ref='2') earlykid(ref='0') N1Q21A(ref='2') n1q2b(ref='1') mdd18(ref='0');
class chvb(ref='2') chpa(ref='2') chsa(ref='2') childabuse(ref='2') parentdivorce(ref='2') parentdeath(ref ='2');
class aud_dud_tud18(ref = '0') mdd_anxiety_ptsd18(ref = '0') PARMEND(ref = '0') PARSUBD(ref = '0') aud_dud18(ref='0');
model obese(order =INTERNAL ref='0')=  age1 nregion nethrace    n1q2b N1Q21A childabuse parentdeath parentdivorce   PARMEND PARSUBD mdd_anxiety_ptsd18 aud_dud18  nicotine18 earlykid   edu2   /link=glogit;
strata VARSTRAT;
cluster VARUNIT;
weight AUDWEIGHT;
by nsex;
run;


proc surveylogistic data=mysmalldata;


class age1(ref='1') nregion(ref='1') nethrace(ref='1')  N1Q42A(ref='2') nicotine18(ref='0') aud_dud18(ref='0');
class  edu2(ref='2') earlykid(ref='0') N1Q21A(ref='2') n1q2b(ref='1') mdd18(ref='0');
class chvb(ref='2') chpa(ref='2') chsa(ref='2') childabuse(ref='2') parentdivorce(ref='2') parentdeath(ref ='2');
class aud_dud_tud18(ref = '0') mdd_anxiety_ptsd18(ref = '0') PARMEND(ref = '0') PARSUBD(ref = '0');
model severeobese(order =INTERNAL ref='0')=  age1 nregion nethrace    n1q2b N1Q21A childabuse parentdeath parentdivorce   PARMEND PARSUBD mdd_anxiety_ptsd18 aud_dud18 nicotine18  earlykid   edu2   /link=glogit;
strata VARSTRAT;
cluster VARUNIT;
weight AUDWEIGHT;
by nsex;
run;




/***comparison of OR between male and female****************/


proc surveylogistic data=mysmalldata;


class nsex(ref='1') age1(ref='1') nregion(ref='1') nethrace(ref='1')  N1Q42A(ref='2') nicotine18(ref='0');
class  edu2(ref='2') earlykid(ref='0') N1Q21A(ref='2') n1q2b(ref='1') mdd18(ref='0') aud_dud18(ref='0');
class chvb(ref='2') chpa(ref='2') chsa(ref='2') childabuse(ref='2') parentdivorce(ref='2') parentdeath(ref ='2');
class aud_dud_tud18(ref = '0') mdd_anxiety_ptsd18(ref = '0') PARMEND(ref = '0') PARSUBD(ref = '0');
model obese(order =INTERNAL ref='0')=  nsex age1 nregion nethrace    n1q2b N1Q21A childabuse parentdeath parentdivorce   PARMEND PARSUBD mdd_anxiety_ptsd18 aud_dud18 nicotine18  earlykid   edu2   
      nsex*age1 nsex*nregion nsex*nethrace    nsex*n1q2b nsex*N1Q21A nsex*childabuse nsex*parentdeath nsex*parentdivorce   nsex*PARMEND nsex*PARSUBD nsex*mdd_anxiety_ptsd18 
nsex*aud_dud18  nsex*nicotine18 nsex*earlykid   nsex*edu2  /link=glogit;
strata VARSTRAT;
cluster VARUNIT;
weight AUDWEIGHT;
run;



proc surveylogistic data=mysmalldata;


class nsex(ref='1') age1(ref='1') nregion(ref='1') nethrace(ref='1')  N1Q42A(ref='2') nicotine18(ref='0');
class  edu2(ref='2') earlykid(ref='0') N1Q21A(ref='2') n1q2b(ref='1') mdd18(ref='0') aud_dud18(ref='0');
class chvb(ref='2') chpa(ref='2') chsa(ref='2') childabuse(ref='2') parentdivorce(ref='2') parentdeath(ref ='2');
class aud_dud_tud18(ref = '0') mdd_anxiety_ptsd18(ref = '0') PARMEND(ref = '0') PARSUBD(ref = '0');
model severeobese(order =INTERNAL ref='0')=  nsex age1 nregion nethrace    n1q2b N1Q21A childabuse parentdeath parentdivorce   PARMEND PARSUBD mdd_anxiety_ptsd18 aud_dud18 nicotine18  earlykid   edu2   
      nsex*age1 nsex*nregion nsex*nethrace    nsex*n1q2b nsex*N1Q21A nsex*childabuse nsex*parentdeath nsex*parentdivorce   nsex*PARMEND nsex*PARSUBD nsex*mdd_anxiety_ptsd18 
nsex*aud_dud18  nsex*nicotine18  nsex*earlykid   nsex*edu2  /link=glogit;
strata VARSTRAT;
cluster VARUNIT;
weight AUDWEIGHT;
run;


/********************************************************************************/
/*                                                                              */
/*          END  OF  TABLE 4                                                    */
/*                                                                              */
/********************************************************************************/








/********************************************************************************/
/*                                                                              */
/*          FIGURE 1                                                            */
/*                                                                              */
/********************************************************************************/

proc surveyfreq data =mydata;
tables  nsex*obese*nage nsex*severeobese*nage/col row;
strata VARSTRAT;
cluster VARUNIT;
weight AUDWEIGHT;
run;


/********************************************************************************/
/*                                                                              */
/*          END OF FIGURE 1                                                     */
/*                                                                              */
/********************************************************************************/


















/********************************************************************************/
/*                                                                              */
/*          SUPPLEMENT TABLE 1                                                  */
/*                                                                              */
/********************************************************************************/

proc sort data=mydata;
by nsex;
run;


proc surveyfreq data=mydata;
tables nsex*(age1 nethrace nregion   n1q2b) nsex*n1q42a*(age1 nethrace nregion   n1q2b) nsex*obese*(age1 nethrace nregion   n1q2b) nsex*severeobese*(age1 nethrace nregion   n1q2b)/col row;
strata VARSTRAT;
cluster VARUNIT;
weight AUDWEIGHT;
run;


/***childhood overweight****/
proc surveylogistic data=mydata;


class age1(ref='1') nregion(ref='1') nethrace(ref='1') nicotine18(ref='0');
class  edu2(ref='2') earlykid(ref='0') N1Q21A(ref='2') n1q2b(ref='1') mdd18(ref='0');
class chvb(ref='2') chpa(ref='2') chsa(ref='2') childabuse(ref='2') parentdivorce(ref='2') parentdeath(ref ='2');;
model N1Q42A(ref='2')=  age1 nregion nethrace    n1q2b /link=glogit;
strata VARSTRAT;
cluster VARUNIT;
weight AUDWEIGHT;
by nsex;
run;

/*adult obese******************/
proc surveylogistic data=mydata;


class age1(ref='1') nregion(ref='1') nethrace(ref='1')  N1Q42A(ref='2') nicotine18(ref='0');
class  edu2(ref='2') earlykid(ref='0') N1Q21A(ref='2') n1q2b(ref='1') mdd18(ref='0');
class chvb(ref='2') chpa(ref='2') chsa(ref='2') childabuse(ref='2') parentdivorce(ref='2') parentdeath(ref ='2');;
model obese(order =INTERNAL ref='0')=  age1 nregion nethrace    n1q2b /link=glogit;
strata VARSTRAT;
cluster VARUNIT;
weight AUDWEIGHT;
by nsex;
run;


/* adult severe obese************/

proc surveylogistic data=mydata;


class age1(ref='1') nregion(ref='1') nethrace(ref='1')  N1Q42A(ref='2') nicotine18(ref='0');
class  edu2(ref='2') earlykid(ref='0') N1Q21A(ref='2') n1q2b(ref='1') mdd18(ref='0');
class chvb(ref='2') chpa(ref='2') chsa(ref='2') childabuse(ref='2') parentdivorce(ref='2') parentdeath(ref ='2');;
model severeobese(order =INTERNAL ref='0')=  age1 nregion nethrace    n1q2b /link=glogit;
strata VARSTRAT;
cluster VARUNIT;
weight AUDWEIGHT;
by nsex;
run;


/********************************************************************************/
/*                                                                              */
/*          END  OF SUPPLEMENT TABLE 1                                          */
/*                                                                              */
/********************************************************************************/




/********************************************************************************/
/*                                                                              */
/*          SUPPLEMENT TABLE 2                                                  */
/*                                                                              */
/********************************************************************************/




proc sort data=mydata;
by nsex;
run;


proc surveyfreq data=mydata;
tables nsex*(n1q2b edu2  N1Q21A  childabuse  earlykid  mdd18 nicotine18 parentdeath parentdivorce PARMEND PARSUBD ptsd18 alcohol18 drug18)/col row chisq;
strata VARSTRAT;
cluster VARUNIT;
weight AUDWEIGHT;
run;


/********************************************************************************/
/*                                                                              */
/*          END  OF SUPPLEMENT TABLE 2                                          */
/*                                                                              */
/********************************************************************************/








/********************************************************************************/
/*                                                                              */
/*          SUPPLEMENT TABLE 3                                                  */
/*                                                                              */
/********************************************************************************/

%let var1=chvb;
%let var2=chpa;
%let var3=chsa;
%let var4=chabuse;


%macro supptable2a;
%do i = 1%to 4;


proc surveylogistic data=mysmalldata;


class age1(ref='1') nregion(ref='1') nethrace(ref='1')  N1Q42A(ref='2') nicotine18(ref='0') aud_dud18(ref='0');
class  edu2(ref='2') earlykid(ref='0') N1Q21A(ref='2') n1q2b(ref='1') mdd18(ref='0') chabuse(ref = '1');
class chvb(ref='2') chpa(ref='2') chsa(ref='2') childabuse(ref='2') parentdivorce(ref='2') parentdeath(ref ='2');
class aud_dud_tud18(ref = '0') mdd_anxiety_ptsd18(ref = '0') PARMEND(ref = '0') PARSUBD(ref = '0');
model obese(order =INTERNAL ref='0')=  &&var&i age1 nregion nethrace    n1q2b N1Q21A  parentdeath parentdivorce   PARMEND PARSUBD mdd_anxiety_ptsd18 aud_dud18 nicotine18   earlykid   edu2   /link=glogit;
strata VARSTRAT;
cluster VARUNIT;
weight AUDWEIGHT;
by nsex;
run;


%end;
%mend supptable2a;

%macro supptable2b;
%do i = 1%to 4;


proc surveylogistic data=mysmalldata;


class age1(ref='1') nregion(ref='1') nethrace(ref='1')  N1Q42A(ref='2') nicotine18(ref='0') aud_dud18(ref='0');
class  edu2(ref='2') earlykid(ref='0') N1Q21A(ref='2') n1q2b(ref='1') mdd18(ref='0') chabuse(ref = '1');
class chvb(ref='2') chpa(ref='2') chsa(ref='2') childabuse(ref='2') parentdivorce(ref='2') parentdeath(ref ='2');
class aud_dud_tud18(ref = '0') mdd_anxiety_ptsd18(ref = '0') PARMEND(ref = '0') PARSUBD(ref = '0');
model severeobese(order =INTERNAL ref='0')=  &&var&i age1 nregion nethrace    n1q2b N1Q21A  parentdeath parentdivorce   PARMEND PARSUBD mdd_anxiety_ptsd18 aud_dud18 nicotine18   earlykid   edu2   /link=glogit;
strata VARSTRAT;
cluster VARUNIT;
weight AUDWEIGHT;
by nsex;
run;


%end;
%mend supptable2b;

%macro supptable2c;
%do i = 1%to 4;

proc surveylogistic data=mysmalldata;

class nsex(ref='1') age1(ref='1') nregion(ref='1') nethrace(ref='1')  N1Q42A(ref='2') nicotine18(ref='0')aud_dud18(ref='0');
class  edu2(ref='2') earlykid(ref='0') N1Q21A(ref='2') n1q2b(ref='1') mdd18(ref='0') chabuse(ref = '1');
class chvb(ref='2') chpa(ref='2') chsa(ref='2') childabuse(ref='2') parentdivorce(ref='2') parentdeath(ref ='2');
class aud_dud_tud18(ref = '0') mdd_anxiety_ptsd18(ref = '0') PARMEND(ref = '0') PARSUBD(ref = '0');
model obese(order =INTERNAL ref='0')= nsex*&&var&i &&var&i nsex age1 nregion nethrace    n1q2b N1Q21A  parentdeath parentdivorce   PARMEND PARSUBD mdd_anxiety_ptsd18 aud_dud18 nicotine18   
earlykid   edu2   nsex*age1 nsex*nregion nsex*nethrace    nsex*n1q2b nsex*N1Q21A  nsex*parentdeath nsex*parentdivorce   nsex*PARMEND nsex*PARSUBD nsex*mdd_anxiety_ptsd18 
nsex*aud_dud18 nsex*nicotine18   nsex*earlykid   nsex*edu2  /link=glogit;
strata VARSTRAT;
cluster VARUNIT;
weight AUDWEIGHT;
run;


%end;
%mend supptable2c;

%macro supptable2D;
%do i = 1%to 4;
proc surveylogistic data=mysmalldata;


class nsex(ref='1') age1(ref='1') nregion(ref='1') nethrace(ref='1')  N1Q42A(ref='2') nicotine18(ref='0')aud_dud18(ref='0');
class  edu2(ref='2') earlykid(ref='0') N1Q21A(ref='2') n1q2b(ref='1') mdd18(ref='0') chabuse(ref = '1');
class chvb(ref='2') chpa(ref='2') chsa(ref='2') childabuse(ref='2') parentdivorce(ref='2') parentdeath(ref ='2');
class aud_dud_tud18(ref = '0') mdd_anxiety_ptsd18(ref = '0') PARMEND(ref = '0') PARSUBD(ref = '0');
model severeobese(order =INTERNAL ref='0')= nsex*&&var&i &&var&i nsex age1 nregion nethrace    n1q2b N1Q21A  parentdeath parentdivorce   PARMEND PARSUBD mdd_anxiety_ptsd18 aud_dud18 nicotine18   
earlykid   edu2   nsex*age1 nsex*nregion nsex*nethrace    nsex*n1q2b nsex*N1Q21A  nsex*parentdeath nsex*parentdivorce   nsex*PARMEND nsex*PARSUBD nsex*mdd_anxiety_ptsd18 
nsex*aud_dud18 nsex*nicotine18   nsex*earlykid   nsex*edu2  /link=glogit;
strata VARSTRAT;
cluster VARUNIT;
weight AUDWEIGHT;
run;

%end;
%mend supptable2d;

%supptable2a;

%supptable2b;


%supptable2c;

%supptable2d;



/********************************************************************************/
/*                                                                              */
/*          END OF SUPPLEMENT TABLE 3                                           */
/*                                                                              */
/********************************************************************************/





