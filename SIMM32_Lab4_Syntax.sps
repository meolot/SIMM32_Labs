* Encoding: UTF-8.
GET
  FILE='C:\Users\MoO97\Desktop\Global studies\Semester 2\SIMM32\W3 (lab 4)\lab_4_assignment_dataset_A.sav'.
DATASET NAME DataSet1 WINDOW=FRONT.
DATASET ACTIVATE DataSet1.
RECODE sex ('female'=1) ('male'=0) INTO female.
EXECUTE.

* Graph clustered data.
GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES=pain_cat pain hospital MISSING=LISTWISE
    REPORTMISSING=NO
  /GRAPHSPEC SOURCE=INLINE
  /FITLINE TOTAL=NO SUBGROUP=YES.
BEGIN GPL
  SOURCE: s=userSource(id("graphdataset"))
  DATA: pain_cat=col(source(s), name("pain_cat"))
  DATA: pain=col(source(s), name("pain"))
  DATA: hospital=col(source(s), name("hospital"), unit.category())
  GUIDE: axis(dim(1), label("pain_cat"))
  GUIDE: axis(dim(2), label("pain"))
  GUIDE: legend(aesthetic(aesthetic.color.interior), label("hospital"))
  GUIDE: text.title(label("Grouped Scatterplot Pain by  Pain Catastrophizing"))
  ELEMENT: point(position(pain_cat*pain), color.interior(hospital))
END GPL.

*simple Regression without random effects.
REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT pain
  /METHOD=ENTER age STAI_trait pain_cat cortisol_serum mindfulness female.

*random intercept model.
MIXED pain WITH age STAI_trait pain_cat cortisol_serum mindfulness female
  /CRITERIA=DFMETHOD(SATTERTHWAITE) CIN(95) MXITER(100) MXSTEP(10) SCORING(1)
    SINGULAR(0.000000000001) HCONVERGE(0, ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)
  /FIXED=age STAI_trait pain_cat cortisol_serum mindfulness female | SSTYPE(3)
  /METHOD=REML
  /PRINT=SOLUTION
  /RANDOM=INTERCEPT | SUBJECT(hospital) COVTYPE(VC).


*compute regression equation for dataset B.
GET
  FILE='C:\Users\MoO97\Desktop\Global studies\Semester 2\SIMM32\W3 (lab 4)\lab_4_assignment_dataset_B.sav'.
DATASET NAME DataSet1 WINDOW=FRONT.
COMPUTE regression_equation=2.677925 + (-0.022091 * age) + (-0.045165 * STAI_trait) + (0.081544 *
    pain_cat) + (0.630702 * cortisol_serum) + (-0.186708 * mindfulness) + (-0.195297 * female).
EXECUTE.
DESCRIPTIVES VARIABLES=regression_equation
  /STATISTICS=MEAN STDDEV VARIANCE MIN MAX.

EXAMINE VARIABLES=regression_equation
  /PLOT BOXPLOT STEMLEAF HISTOGRAM NPPLOT
  /COMPARE GROUPS
  /STATISTICS DESCRIPTIVES
  /CINTERVAL 95
  /MISSING LISTWISE
  /NOTOTAL.

 * comparison R² calculations for dataset B.
 COMPUTE Residuals=pain - Y.
EXECUTE.
DESCRIPTIVES VARIABLES=Residuals
  /STATISTICS=MEAN STDDEV VARIANCE MIN MAX.

COMPUTE Residuals_mean=pain - 4.85.
EXECUTE.
COMPUTE Residuals_mean_sq=Residuals_mean * Residuals_mean.
EXECUTE.
FREQUENCIES VARIABLES=Residuals_mean_sq
  /STATISTICS=SUM
  /ORDER=ANALYSIS.
FREQUENCIES VARIABLES=Residuals_sq
  /STATISTICS=SUM
  /ORDER=ANALYSIS.


