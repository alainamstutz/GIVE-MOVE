---
title: "GIVE-MOVE"
author: "J. Brown & A. Amstutz"
date: "2023-08-18"
output:
  html_document:
    keep_md: yes
    toc: yes
    toc_float: yes
    code_folding: hide
  pdf_document:
    toc: yes
---

# Estimation of "a" per-protocol effect for GIVE-MOVE (there are several) 

Based on: 
Guidelines for estimating causal effects in pragmatic randomized trials
Eleanor J. Murray1, Sonja A. Swanson2, Miguel A. Hernán

Causal estimand of interest: The effect of receiving the assigned treatment strategies
= The effect of successfully receiving GRT-guided strategy in intervention and VL-guided strategy in control
= Estimation of a per-protocol effect for a point intervention

We estimated the per-protocol effect of GRT-guided strategy, which was defined as the effect of GRT-guided strategy if all the participants who were randomly assigned to the intervention group had received the GRT-guided strategy. The risk of VL failure may have differed between the participants who received GRT-guided strategy and those who were assigned to GRT-guided strategy but did not receive it; therefore, our analysis should adjust for the baseline covariates of the participants and sites.

Note: We assume a point treatment situation. However, we could also argue for a sustained intervention strategy (e.g. since no-one in control should have received GRT over the entire study period). Then, we would need to adjust for time-varying confounding over study period (using g-methods).

Define non-adherence to treatment strategy in each group:

Intervention group:
1) Participants not receiving a GRT although they were supposed to have one: 3 participants
2) Participants not having the decision visit within six months (24 weeks) after randomization: 26 participants
- of which LTFU: XXX ?
- otherwise: missing/delayed GRT (that's already part of number 1?! why not summarized under 2?)

Control group:
1) Participants receiving a GRT although they were not supposed to have one: 3 participants
2) Participants not receiving a VL although they were supposed to have one: 1 participant
3) Participants not having the decision visit within six months (24 weeks) after randomization: 10 participants
- of which LTFU: XXX ?
- otherwise: missing/delayed VL (that's already part of number 2?! why not summarized under 3?)

Note: Not having a VL result within the 9-month window and not having reached the primary endpoint before or at the 9-month visit (13 participants) is part of the primary endpoint/estimand. If not part of primary causal estimand, then use multiple imputation or IPW for LTFU.

Define DAG: See slide in folder "DAG"

Define method used:
Because a point intervention is delivered at or close to the time of randomization, only covariates at or before the time of randomization can influence adherence to a point intervention. To validly estimate the per-protocol effect, baseline variables which predict adherence and are prognostic for the outcome need to be accounted for, either through direct adjustment or via an instrumental variable analysis.
Inverse probability and standardization allow calculation of absolute risks in the study population and preserve the marginal (unconditional) interpretation and are therefore preferable for direct adjustment. 
Other commonly used adjustment methods, like outcome regression and propensity score adjustment or matching, typically make strong assumptions about no effect heterogeneity (rely on the assumption of effect homogeneity across levels of all covariates), and do not easily yield unconditional absolute risks.
(as a exploratory analysis, we may add an instrumental variable analyis, however, this analysis would rely on stronger assumptions and harder to interpret)
Since point exposures happen only once, they typically require only control for baseline confounding. (However, estimating causal effects of point exposures on time to event may still require adjustment for time-varying confounders of loss to follow-up and the outcome.)


# Load packages

```r
library(tidyverse)
library(readxl)
library(tableone)
library(data.table)
library(writexl)
library(lme4)
library(msm)
library(sjPlot) # for tab_model
library(jtools) # for summ()
library(sandwich) # for robust sandwich SE
```

# Load Data


# Explore Data and Primary ITT analysis

```r
## protocol deviation variables
# table(analysis$arm, analysis$prot_dev1) #All: not having the decision visit within six months (24 weeks) after randomization
# table(analysis$arm, analysis$prot_dev2) #Cont P: not receiving a VL although was supposed to have one
# table(analysis$arm, analysis$prot_dev3) #Int P: Not receiving a GRT although they were supposed to have one
# table(analysis$arm, analysis$prot_dev4) #All: Not having a VL result within the 9-month window and not having reached the primary endpoint before or at the 9-month visit
# table(analysis$arm, analysis$prot_dev5) #Cont P: GRT although they were not supposed to have one

## define non-adherence to treatment strategy/protocol
analysis <- analysis %>%
  mutate(ppadh = case_when(prot_dev1 == 1 | prot_dev2 == 1
                           | prot_dev3 == 1 | prot_dev4 == 1
                           | prot_dev5 == 1 ~ 1,
                           TRUE ~ 0))
# table(analysis$arm, analysis$ppadh, useNA = "always") #adherence to protocol variable
# table(analysis$arm, useNA = "always") #arm variable
# table(analysis$arm, analysis$endpoint, useNA = "always") #primary outcome variable // no missing
# table(analysis$arm, analysis$agegr_der, useNA = "always") #age stratification variable
# table(analysis$arm, analysis$stsite_der2, useNA = "always") #country stratification variable
# table(analysis$arm, analysis$regimen_der2, useNA = "always") #regimen stratification variable

## ITT analysis, adjusted for stratification variables
ITT <- analysis %>% 
  glm(endpoint ~ arm + agegr_der + stsite_der2 + regimen_der2,
              family = binomial(link = "logit"), data=.)
tab_model(ITT)
```

<table style="border-collapse:collapse; border:none;">
<tr>
<th style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm;  text-align:left; ">&nbsp;</th>
<th colspan="3" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">endpoint</th>
</tr>
<tr>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  text-align:left; ">Predictors</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">Odds Ratios</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">CI</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">p</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">(Intercept)</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.68</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.35&nbsp;&ndash;&nbsp;1.29</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.241</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">arm</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.79</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.49&nbsp;&ndash;&nbsp;1.27</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.337</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">agegr der [>= 12 and <<br>19]</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">1.23</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.69&nbsp;&ndash;&nbsp;2.26</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.488</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">stsite der2 [Tanzania]</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">1.08</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.63&nbsp;&ndash;&nbsp;1.84</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.781</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">regimen der2<br>[NNRTI-based]</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">1.19</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.36&nbsp;&ndash;&nbsp;3.80</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.763</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">regimen der2 [PI-based]</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">2.47</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">1.34&nbsp;&ndash;&nbsp;4.63</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "><strong>0.004</strong></td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm; border-top:1px solid;">Observations</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="3">284</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">R<sup>2</sup> Tjur</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">0.037</td>
</tr>

</table>

```r
summ(ITT, exp = T, confint = T, model.info = F, model.fit = F, digits = 3)
```

```
## Warning in !is.null(rmarkdown::metadata$output) && rmarkdown::metadata$output
## %in% : 'length(x) = 2 > 1' in coercion to 'logical(1)'
```

  <table class="table table-striped table-hover table-condensed table-responsive" style="width: auto !important; margin-left: auto; margin-right: auto;border-bottom: 0;">
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> exp(Est.) </th>
   <th style="text-align:right;"> 2.5% </th>
   <th style="text-align:right;"> 97.5% </th>
   <th style="text-align:right;"> z val. </th>
   <th style="text-align:right;"> p </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;font-weight: bold;"> (Intercept) </td>
   <td style="text-align:right;"> 0.681 </td>
   <td style="text-align:right;"> 0.359 </td>
   <td style="text-align:right;"> 1.295 </td>
   <td style="text-align:right;"> -1.172 </td>
   <td style="text-align:right;"> 0.241 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> arm </td>
   <td style="text-align:right;"> 0.793 </td>
   <td style="text-align:right;"> 0.493 </td>
   <td style="text-align:right;"> 1.274 </td>
   <td style="text-align:right;"> -0.960 </td>
   <td style="text-align:right;"> 0.337 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> agegr_der&gt;= 12 and 
   </td>
<td style="text-align:right;"> 1.233 </td>
   <td style="text-align:right;"> 0.682 </td>
   <td style="text-align:right;"> 2.231 </td>
   <td style="text-align:right;"> 0.693 </td>
   <td style="text-align:right;"> 0.488 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> stsite_der2Tanzania </td>
   <td style="text-align:right;"> 1.078 </td>
   <td style="text-align:right;"> 0.633 </td>
   <td style="text-align:right;"> 1.838 </td>
   <td style="text-align:right;"> 0.278 </td>
   <td style="text-align:right;"> 0.781 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> regimen_der2NNRTI-based </td>
   <td style="text-align:right;"> 1.193 </td>
   <td style="text-align:right;"> 0.378 </td>
   <td style="text-align:right;"> 3.763 </td>
   <td style="text-align:right;"> 0.302 </td>
   <td style="text-align:right;"> 0.763 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> regimen_der2PI-based </td>
   <td style="text-align:right;"> 2.466 </td>
   <td style="text-align:right;"> 1.331 </td>
   <td style="text-align:right;"> 4.572 </td>
   <td style="text-align:right;"> 2.867 </td>
   <td style="text-align:right;"> 0.004 </td>
  </tr>
</tbody>
<tfoot><tr><td style="padding: 0; " colspan="100%">
<sup></sup> Standard errors: MLE</td></tr></tfoot>
</table>

```r
## ITT analysis, unadjusted
ITT_unad <- analysis %>% 
  glm(endpoint ~ arm ,
      # + agegr_der
      # + stsite_der2
      # + regimen_der2,
              family = binomial(link = "logit"), data=.)
tab_model(ITT_unad)
```

<table style="border-collapse:collapse; border:none;">
<tr>
<th style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm;  text-align:left; ">&nbsp;</th>
<th colspan="3" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">endpoint</th>
</tr>
<tr>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  text-align:left; ">Predictors</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">Odds Ratios</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">CI</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">p</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">(Intercept)</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">1.09</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.78&nbsp;&ndash;&nbsp;1.52</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.612</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">arm</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.80</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.50&nbsp;&ndash;&nbsp;1.27</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.344</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm; border-top:1px solid;">Observations</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="3">284</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">R<sup>2</sup> Tjur</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">0.003</td>
</tr>

</table>

```r
summ(ITT_unad, exp = T, confint = T, model.info = F, model.fit = F, digits = 3)
```

  <table class="table table-striped table-hover table-condensed table-responsive" style="width: auto !important; margin-left: auto; margin-right: auto;border-bottom: 0;">
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> exp(Est.) </th>
   <th style="text-align:right;"> 2.5% </th>
   <th style="text-align:right;"> 97.5% </th>
   <th style="text-align:right;"> z val. </th>
   <th style="text-align:right;"> p </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;font-weight: bold;"> (Intercept) </td>
   <td style="text-align:right;"> 1.090 </td>
   <td style="text-align:right;"> 0.782 </td>
   <td style="text-align:right;"> 1.518 </td>
   <td style="text-align:right;"> 0.507 </td>
   <td style="text-align:right;"> 0.612 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> arm </td>
   <td style="text-align:right;"> 0.799 </td>
   <td style="text-align:right;"> 0.501 </td>
   <td style="text-align:right;"> 1.273 </td>
   <td style="text-align:right;"> -0.946 </td>
   <td style="text-align:right;"> 0.344 </td>
  </tr>
</tbody>
<tfoot><tr><td style="padding: 0; " colspan="100%">
<sup></sup> Standard errors: MLE</td></tr></tfoot>
</table>
# TABLE characteristics by arm and by adherence to treatment strategy



