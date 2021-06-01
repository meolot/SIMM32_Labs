* Encoding: UTF-8.


GET
  FILE='C:\Users\MoO97\Desktop\Global studies\Semester 2\SIMM32\W4 (lab 5)\lab_5_assignment_dataset.sav'.
DATASET NAME DataSet1 WINDOW=FRONT.
RECODE sex ('male'=0) ('female'=1) INTO female.
EXECUTE.

* combine pain values.
SAVE OUTFILE='C:\Users\MoO97\Desktop\Global studies\Semester 2\SIMM32\W4 (lab '+
    '5)\lab_5_data_restructured.sav'
  /COMPRESSED.
VARSTOCASES
  /MAKE pain_value FROM pain1 pain2 pain3 pain4
  /INDEX=time(4)
  /KEEP=ID age STAI_trait pain_cat cortisol_serum mindfulness female
  /NULL=KEEP.



*  Regression Mixed Model Random Intercept.
DATASET ACTIVATE DataSet1. 
SAVE OUTFILE='C:\Users\MoO97\Desktop\Global studies\Semester 2\SIMM32\W4 (lab '+ 
    '5)\lab_5_data_restructured.sav' 
  /COMPRESSED. 
MIXED pain_value WITH age STAI_trait pain_cat cortisol_serum mindfulness female time 
  /CRITERIA=DFMETHOD(SATTERTHWAITE) CIN(95) MXITER(100) MXSTEP(10) SCORING(1) 
    SINGULAR(0.000000000001) HCONVERGE(0, ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE) 
  /FIXED=age STAI_trait pain_cat cortisol_serum mindfulness female time | SSTYPE(3) 
  /METHOD=REML 
  /PRINT=SOLUTION 
  /RANDOM=INTERCEPT | SUBJECT(ID) COVTYPE(VC) 
  /SAVE=PRED.

*Random Slope Model.
DATASET ACTIVATE DataSet1. 
MIXED pain_value WITH age STAI_trait pain_cat cortisol_serum mindfulness female time 
  /CRITERIA=DFMETHOD(SATTERTHWAITE) CIN(95) MXITER(100) MXSTEP(10) SCORING(1) 
    SINGULAR(0.000000000001) HCONVERGE(0, ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE) 
  /FIXED=age STAI_trait pain_cat cortisol_serum mindfulness female time | SSTYPE(3) 
  /METHOD=REML 
  /PRINT=CORB  SOLUTION 
  /RANDOM=INTERCEPT time | SUBJECT(ID) COVTYPE(UN) 
  /SAVE=PRED.

*compare models.
VARSTOCASES
  /MAKE pain_value FROM pain_value PRED_int PRED_slope
  /INDEX=obs_or_pred(pain_value)
  /KEEP=ID time 
  /NULL=KEEP.

SAVE OUTFILE='C:\Users\MoO97\Desktop\Global studies\Semester 2\SIMM32\W4 (lab '+
    '5)\lab_5_data_recoded_long.sav'
  /COMPRESSED.
SORT CASES  BY ID.
SPLIT FILE SEPARATE BY ID.

* Graph.
GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES=time pain_value obs_or_pred MISSING=LISTWISE
    REPORTMISSING=NO
  /GRAPHSPEC SOURCE=INLINE.
BEGIN GPL
  SOURCE: s=userSource(id("graphdataset"))
  DATA: time=col(source(s), name("time"), unit.category())
  DATA: pain_value=col(source(s), name("pain_value"), unit.category())
  DATA: obs_or_pred=col(source(s), name("obs_or_pred"), unit.category())
  GUIDE: axis(dim(1), label("time"))
  GUIDE: axis(dim(2), label("pain_value"))
  GUIDE: legend(aesthetic(aesthetic.color.interior), label("obs_or_pred"))
  GUIDE: text.title(label("Mehrere Linien  von pain_value Schritt: time Schritt: obs_or_pred"))
  ELEMENT: line(position(time*pain_value), color.interior(obs_or_pred), missing.wings())
END GPL.

*Time centered and squared.
GET
  FILE='C:\Users\MoO97\Desktop\Global studies\Semester 2\SIMM32\W4 (lab 5)\lab_5_data_restructured.sav'.
DATASET NAME DataSet2 WINDOW=FRONT.
COMPUTE time_centered=time - 2.50.
EXECUTE.
COMPUTE time_sq=time_centered * time_centered.
EXECUTE.

*Regressino: Mixed random slope model (time²).
MIXED pain_value WITH age STAI_trait pain_cat cortisol_serum mindfulness female time_sq
    time_centered
  /CRITERIA=DFMETHOD(SATTERTHWAITE) CIN(95) MXITER(100) MXSTEP(10) SCORING(1)
    SINGULAR(0.000000000001) HCONVERGE(0, ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)
  /FIXED=age STAI_trait pain_cat cortisol_serum mindfulness female time_sq time_centered | SSTYPE(3)
  /METHOD=REML
  /PRINT=CORB DESCRIPTIVES  SOLUTION
  /RANDOM=INTERCEPT time_sq time_centered | SUBJECT(ID) COVTYPE(UN)
  /SAVE=PRED RESID.

*Model Comparison Graphs.
VARSTOCASES
/MAKE pain_value FROM pain_value PRED_1 PRED_slope
/INDEX=obs_or_pred(pain_value)
/KEEP=ID time time_centered time_sq
/NULL=KEEP.

SORT CASES BY ID. 
SPLIT FILE SEPARATE BY ID.

GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES=time MEAN(pain_value)[name="MEAN_pain_value"]
    obs_or_pred MISSING=LISTWISE REPORTMISSING=NO
  /GRAPHSPEC SOURCE=INLINE.
BEGIN GPL
  SOURCE: s=userSource(id("graphdataset"))
  DATA: time=col(source(s), name("time"), unit.category())
  DATA: MEAN_pain_value=col(source(s), name("MEAN_pain_value"))
  DATA: obs_or_pred=col(source(s), name("obs_or_pred"), unit.category())
  GUIDE: axis(dim(1), label("time"))
  GUIDE: axis(dim(2), label("Mean pain_value"))
  GUIDE: legend(aesthetic(aesthetic.color.interior), label("obs_or_pred"))
  GUIDE: text.title(label("Grouped Scatterplot: Pain over time (predicted through different ",
    "models)"))
  SCALE: linear(dim(2), include(0))
  ELEMENT: line(position(time*MEAN_pain_value), color.interior(obs_or_pred), missing.wings())
END GPL.

* Model Diagnostics.

*1. Identify Outliers.
EXAMINE VARIABLES=pain_value BY ID
  /PLOT BOXPLOT STEMLEAF HISTOGRAM NPPLOT
  /COMPARE GROUPS
  /STATISTICS DESCRIPTIVES
  /CINTERVAL 95
  /MISSING LISTWISE
  /NOTOTAL.

EXAMINE VARIABLES=RESID_1 BY ID
  /PLOT BOXPLOT STEMLEAF HISTOGRAM NPPLOT
  /COMPARE GROUPS
  /STATISTICS DESCRIPTIVES
  /CINTERVAL 95
  /MISSING LISTWISE
  /NOTOTAL.

GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES=time MEAN(pain_value)[name="MEAN_pain_value"] ID
    MISSING=LISTWISE REPORTMISSING=NO
  /GRAPHSPEC SOURCE=INLINE.
BEGIN GPL
  SOURCE: s=userSource(id("graphdataset"))
  DATA: time=col(source(s), name("time"), unit.category())
  DATA: MEAN_pain_value=col(source(s), name("MEAN_pain_value"))
  DATA: ID=col(source(s), name("ID"), unit.category())
  GUIDE: axis(dim(1), label("time"))
  GUIDE: axis(dim(2), label("Mittelwert pain_value"))
  GUIDE: legend(aesthetic(aesthetic.color.interior), label("ID"))
  GUIDE: text.title(label("Mehrere Linien Mittelwert  von pain_value Schritt: time Schritt: ID"))
  SCALE: linear(dim(2), include(0))
  ELEMENT: line(position(time*MEAN_pain_value), color.interior(ID), missing.wings())
END GPL.

* Normality.
EXAMINE VARIABLES=RESID_1
  /PLOT BOXPLOT STEMLEAF HISTOGRAM NPPLOT
  /COMPARE GROUPS
  /STATISTICS DESCRIPTIVES
  /CINTERVAL 95
  /MISSING LISTWISE
  /NOTOTAL.

EXAMINE VARIABLES=RESID_1 BY ID
  /PLOT BOXPLOT STEMLEAF HISTOGRAM NPPLOT
  /COMPARE GROUPS
  /STATISTICS DESCRIPTIVES
  /CINTERVAL 95
  /MISSING LISTWISE
  /NOTOTAL.

*Linearity and Homoscedasticity.
GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES=PRED_1 RESID_1 MISSING=LISTWISE REPORTMISSING=NO
  /GRAPHSPEC SOURCE=INLINE
  /FITLINE TOTAL=NO.
BEGIN GPL
  SOURCE: s=userSource(id("graphdataset"))
  DATA: PRED_1=col(source(s), name("PRED_1"))
  DATA: RESID_1=col(source(s), name("RESID_1"))
  GUIDE: axis(dim(1), label("Vorhergesagte Werte"))
  GUIDE: axis(dim(2), label("Residuen"))
  GUIDE: text.title(label("Einfaches Streudiagramm  von Residuen Schritt: Vorhergesagte Werte"))
  ELEMENT: point(position(PRED_1*RESID_1))
END GPL.

* Age.
GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES=age RESID_1 MISSING=LISTWISE REPORTMISSING=NO
  /GRAPHSPEC SOURCE=INLINE
  /FITLINE TOTAL=NO.
BEGIN GPL
  SOURCE: s=userSource(id("graphdataset"))
  DATA: age=col(source(s), name("age"))
  DATA: RESID_1=col(source(s), name("RESID_1"))
  GUIDE: axis(dim(1), label("age"))
  GUIDE: axis(dim(2), label("Residuen"))
  GUIDE: text.title(label("Einfaches Streudiagramm  von Residuen Schritt: age"))
  ELEMENT: point(position(age*RESID_1))
END GPL.

*STAI.
GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES=STAI_trait RESID_1 MISSING=LISTWISE REPORTMISSING=NO
  /GRAPHSPEC SOURCE=INLINE
  /FITLINE TOTAL=NO.
BEGIN GPL
  SOURCE: s=userSource(id("graphdataset"))
  DATA: STAI_trait=col(source(s), name("STAI_trait"))
  DATA: RESID_1=col(source(s), name("RESID_1"))
  GUIDE: axis(dim(1), label("STAI_trait"))
  GUIDE: axis(dim(2), label("Residuen"))
  GUIDE: text.title(label("Einfaches Streudiagramm  von Residuen Schritt: STAI_trait"))
  ELEMENT: point(position(STAI_trait*RESID_1))
END GPL.

* pain_cat.
GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES=pain_cat RESID_1 MISSING=LISTWISE REPORTMISSING=NO
  /GRAPHSPEC SOURCE=INLINE
  /FITLINE TOTAL=NO.
BEGIN GPL
  SOURCE: s=userSource(id("graphdataset"))
  DATA: pain_cat=col(source(s), name("pain_cat"))
  DATA: RESID_1=col(source(s), name("RESID_1"))
  GUIDE: axis(dim(1), label("pain_cat"))
  GUIDE: axis(dim(2), label("Residuen"))
  GUIDE: text.title(label("Einfaches Streudiagramm  von Residuen Schritt: pain_cat"))
  ELEMENT: point(position(pain_cat*RESID_1))
END GPL.

* cortisol.
GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES=cortisol_serum RESID_1 MISSING=LISTWISE
    REPORTMISSING=NO
  /GRAPHSPEC SOURCE=INLINE
  /FITLINE TOTAL=NO.
BEGIN GPL
  SOURCE: s=userSource(id("graphdataset"))
  DATA: cortisol_serum=col(source(s), name("cortisol_serum"))
  DATA: RESID_1=col(source(s), name("RESID_1"))
  GUIDE: axis(dim(1), label("cortisol_serum"))
  GUIDE: axis(dim(2), label("Residuen"))
  GUIDE: text.title(label("Einfaches Streudiagramm  von Residuen Schritt: cortisol_serum"))
  ELEMENT: point(position(cortisol_serum*RESID_1))
END GPL.

* mindfulness.
GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES=mindfulness RESID_1 MISSING=LISTWISE REPORTMISSING=NO
  /GRAPHSPEC SOURCE=INLINE
  /FITLINE TOTAL=NO.
BEGIN GPL
  SOURCE: s=userSource(id("graphdataset"))
  DATA: mindfulness=col(source(s), name("mindfulness"))
  DATA: RESID_1=col(source(s), name("RESID_1"))
  GUIDE: axis(dim(1), label("mindfulness"))
  GUIDE: axis(dim(2), label("Residuen"))
  GUIDE: text.title(label("Einfaches Streudiagramm  von Residuen Schritt: mindfulness"))
  ELEMENT: point(position(mindfulness*RESID_1))
END GPL.

* gender.
GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES=female RESID_1 MISSING=LISTWISE REPORTMISSING=NO
  /GRAPHSPEC SOURCE=INLINE
  /FITLINE TOTAL=NO.
BEGIN GPL
  SOURCE: s=userSource(id("graphdataset"))
  DATA: female=col(source(s), name("female"), unit.category())
  DATA: RESID_1=col(source(s), name("RESID_1"))
  GUIDE: axis(dim(1), label("female"))
  GUIDE: axis(dim(2), label("Residuen"))
  GUIDE: text.title(label("Einfaches Streudiagramm  von Residuen Schritt: female"))
  SCALE: linear(dim(2), include(0))
  ELEMENT: point(position(female*RESID_1))
END GPL.

* time_centered.
GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES=time_centered RESID_1 MISSING=LISTWISE
    REPORTMISSING=NO
  /GRAPHSPEC SOURCE=INLINE
  /FITLINE TOTAL=NO.
BEGIN GPL
  SOURCE: s=userSource(id("graphdataset"))
  DATA: time_centered=col(source(s), name("time_centered"))
  DATA: RESID_1=col(source(s), name("RESID_1"))
  GUIDE: axis(dim(1), label("time_centered"))
  GUIDE: axis(dim(2), label("Residuen"))
  GUIDE: text.title(label("Einfaches Streudiagramm  von Residuen Schritt: time_centered"))
  ELEMENT: point(position(time_centered*RESID_1))
END GPL.


* time_sq.
GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES=time_sq RESID_1 MISSING=LISTWISE REPORTMISSING=NO
  /GRAPHSPEC SOURCE=INLINE
  /FITLINE TOTAL=NO.
BEGIN GPL
  SOURCE: s=userSource(id("graphdataset"))
  DATA: time_sq=col(source(s), name("time_sq"))
  DATA: RESID_1=col(source(s), name("RESID_1"))
  GUIDE: axis(dim(1), label("time_sq"))
  GUIDE: axis(dim(2), label("Residuen"))
  GUIDE: text.title(label("Einfaches Streudiagramm  von Residuen Schritt: time_sq"))
  ELEMENT: point(position(time_sq*RESID_1))
END GPL.


*Multicollinearity.
CORRELATIONS
  /VARIABLES=age STAI_trait pain_cat cortisol_serum mindfulness female time_sq time_centered time
  /PRINT=TWOTAIL NOSIG FULL
  /MISSING=PAIRWISE.


*Constant variance of residuals across clusters.
SPSSINC CREATE DUMMIES VARIABLE=ID
ROOTNAME1=ID_dummy
/OPTIONS ORDER=A USEVALUELABELS=YES USEML=YES OMITFIRST=NO.
COMPUTE Res_sq=RESID_1 * RESID_1. 
EXECUTE. 
REGRESSION 
  /MISSING LISTWISE 
  /STATISTICS COEFF OUTS R ANOVA 
  /CRITERIA=PIN(.05) POUT(.10) 
  /NOORIGIN 
  /DEPENDENT Res_sq 
  /METHOD=ENTER ID_dummy_2 ID_dummy_3 ID_dummy_4 ID_dummy_5 ID_dummy_6 ID_dummy_7 ID_dummy_8 
    ID_dummy_9 ID_dummy_10 ID_dummy_11 ID_dummy_12 ID_dummy_13 ID_dummy_14 ID_dummy_15 ID_dummy_16 
    ID_dummy_17 ID_dummy_18 ID_dummy_19 ID_dummy_20.

*Normal distribution of random effects.
MIXED pain_value WITH age STAI_trait pain_cat cortisol_serum mindfulness female time_sq
    time_centered
  /CRITERIA=DFMETHOD(SATTERTHWAITE) CIN(95) MXITER(100) MXSTEP(10) SCORING(1)
    SINGULAR(0.000000000001) HCONVERGE(0, ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)
  /FIXED=age STAI_trait pain_cat cortisol_serum mindfulness female time_sq time_centered | SSTYPE(3)
  /METHOD=REML
  /PRINT=CORB  SOLUTION
  /RANDOM=INTERCEPT time_sq time_centered | SUBJECT(ID) COVTYPE(UN) SOLUTION.
NEW FILE.
DATASET NAME DataSet7 WINDOW=FRONT.
EXAMINE VARIABLES=VAR00001
  /PLOT BOXPLOT STEMLEAF HISTOGRAM NPPLOT
  /COMPARE GROUPS
  /STATISTICS DESCRIPTIVES
  /CINTERVAL 95
  /MISSING LISTWISE
  /NOTOTAL.

*dependency of random effects.
GET
  FILE='C:\Users\MoO97\Desktop\Global studies\Semester 2\SIMM32\W4 (lab 5)\lab_5_data_restructured.sav'.
DATASET NAME DataSet8 WINDOW=FRONT.
MIXED pain_value WITH age STAI_trait pain_cat cortisol_serum mindfulness female time_sq
    time_centered
  /CRITERIA=DFMETHOD(SATTERTHWAITE) CIN(95) MXITER(100) MXSTEP(10) SCORING(1)
    SINGULAR(0.000000000001) HCONVERGE(0, ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)
  /FIXED=age STAI_trait pain_cat cortisol_serum mindfulness female time_sq time_centered | SSTYPE(3)
  /METHOD=REML
  /PRINT=CORB  SOLUTION TESTCOV
  /RANDOM=INTERCEPT time_sq time_centered | SUBJECT(ID) COVTYPE(UN).
