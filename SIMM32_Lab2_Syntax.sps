* Encoding: UTF-8.
*Open file and data management
  
 GET DATA
  /TYPE=XLSX
  /FILE='C:\Users\MoO97\Desktop\Global studies\Semester 2\SIMM32\W2 (lab 2)\lab_2_assignment_dataset.xlsx'
  /SHEET=name 'home_sample_1'
  /CELLRANGE=FULL
  /READNAMES=ON
  /DATATYPEMIN PERCENTAGE=95.0
  /HIDDEN IGNORE=YES.
EXECUTE.

DATASET NAME DataSet4 WINDOW=FRONT.
RECODE sex ('female'=1) (ELSE=0) INTO female.
EXECUTE.

*Regression complex model + VIF
  
  REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) BCOV R ANOVA COLLIN TOL
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT pain
  /METHOD=ENTER age STAI_trait pain_cat cortisol_serum cortisol_saliva mindfulness female
  /SCATTERPLOT=(*ZRESID ,*ZPRED)
  /RESIDUALS DURBIN HISTOGRAM(ZRESID) NORMPROB(ZRESID)
  /SAVE COOK RESID ZRESID.

* Cook's Distance
* Diagrammerstellung.
GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES=ID COO_1 MISSING=LISTWISE REPORTMISSING=NO
  /GRAPHSPEC SOURCE=INLINE
  /FITLINE TOTAL=NO.
BEGIN GPL
  SOURCE: s=userSource(id("graphdataset"))
  DATA: ID=col(source(s), name("ID"), unit.category())
  DATA: COO_1=col(source(s), name("COO_1"))
  GUIDE: axis(dim(1), label("ID"))
  GUIDE: axis(dim(2), label("Cook's Distance"))
  GUIDE: text.title(label("Einfaches Streudiagramm  von Cook's Distance Schritt: ID"))
  SCALE: linear(dim(2), include(0))
  ELEMENT: point(position(ID*COO_1))
END GPL.

SORT CASES BY COO_1(D).

*Regression with fixed data

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) BCOV R ANOVA COLLIN TOL
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT pain
  /METHOD=ENTER age STAI_trait pain_cat cortisol_serum mindfulness female
  /SCATTERPLOT=(*ZRESID ,*ZPRED)
  /RESIDUALS DURBIN HISTOGRAM(ZRESID) NORMPROB(ZRESID)
  /SAVE COOK RESID ZRESID.


* Cook's distance graph

GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES=ID COO_2 MISSING=LISTWISE REPORTMISSING=NO
  /GRAPHSPEC SOURCE=INLINE
  /FITLINE TOTAL=NO.
BEGIN GPL
  SOURCE: s=userSource(id("graphdataset"))
  DATA: ID=col(source(s), name("ID"), unit.category())
  DATA: COO_2=col(source(s), name("COO_2"))
  GUIDE: axis(dim(1), label("ID"))
  GUIDE: axis(dim(2), label("Cook's Distance"))
  GUIDE: text.title(label("Einfaches Streudiagramm  von Cook's Distance Schritt: ID"))
  SCALE: linear(dim(2), include(0))
  ELEMENT: point(position(ID*COO_2))
END GPL.

*Normality Test

EXAMINE VARIABLES=RES_2
  /PLOT BOXPLOT STEMLEAF HISTOGRAM NPPLOT
  /COMPARE GROUPS
  /STATISTICS DESCRIPTIVES
  /CINTERVAL 95
  /MISSING LISTWISE
  /NOTOTAL.

*Linearity Test

* Diagrammerstellung.
GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES=age pain MISSING=LISTWISE REPORTMISSING=NO
  /GRAPHSPEC SOURCE=INLINE
  /FITLINE TOTAL=NO.
BEGIN GPL
  SOURCE: s=userSource(id("graphdataset"))
  DATA: age=col(source(s), name("age"))
  DATA: pain=col(source(s), name("pain"))
  GUIDE: axis(dim(1), label("age"))
  GUIDE: axis(dim(2), label("pain"))
  GUIDE: text.title(label("Einfaches Streudiagramm  von pain Schritt: age"))
  ELEMENT: point(position(age*pain))
END GPL.
* Diagrammerstellung.
GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES=female pain MISSING=LISTWISE REPORTMISSING=NO
  /GRAPHSPEC SOURCE=INLINE
  /FITLINE TOTAL=NO.
BEGIN GPL
  SOURCE: s=userSource(id("graphdataset"))
  DATA: female=col(source(s), name("female"), unit.category())
  DATA: pain=col(source(s), name("pain"))
  GUIDE: axis(dim(1), label("female"))
  GUIDE: axis(dim(2), label("pain"))
  GUIDE: text.title(label("Einfaches Streudiagramm  von pain Schritt: female"))
  SCALE: linear(dim(2), include(0))
  ELEMENT: point(position(female*pain))
END GPL.

*Homoscedasticity Test

COMPUTE RES_sq=RES_2 * RES_2.
EXECUTE.
REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT RES_sq
  /METHOD=ENTER age STAI_trait pain_cat cortisol_serum mindfulness female
  /SCATTERPLOT=(*ZRESID ,*ZPRED).

*Regression Model Comparison

REGRESSION
  /DESCRIPTIVES MEAN STDDEV CORR SIG N
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) BCOV R ANOVA CHANGE
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT pain
  /METHOD=ENTER age female
  /METHOD=ENTER STAI_trait pain_cat cortisol_serum mindfulness
  /SCATTERPLOT=(*ZRESID ,*ZPRED).

*AIC

REGRESSION
/MISSING LISTWISE
/STATISTICS COEFF OUTS CI(95) R ANOVA SELECTION
/CRITERIA=PIN(.05) POUT(.10)
/NOORIGIN
/DEPENDENT pain
/METHOD=ENTER age female
/METHOD=ENTER STAI_trait pain_cat cortisol_serum mindfulness

