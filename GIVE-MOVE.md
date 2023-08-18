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
# Load packages

```r
library(tidyverse)
library(readxl)
library(writexl)
library(tableone)
```

# Load Data


# Identify the non-compliers in each group
Die Kriterien für Ausschluss aus der PPA waren:
1) Not attending the decision visit within six months (defined as 24 weeks) after randomization: 27 Personen
2) Individuals in the control arm not receiving a VL to inform the decision visit: 1 Person
3) Individuals in the intervention arm not receiving GRT to inform their decision visit, except for cases where GRT was not possible due to resuppression to <400 copies/mL: 3 personen
4) Not having a VL result within the 9-month window and not having reached the primary endpoint before or at the 9-month visit (Figure 1): 13 Personen
5) Individuals in the control arm receiving a GRT before completing the study: 3 Personen
- Kriterien 2,3,5: site level issues
- Kriterium 1: Kann bei LTFU (Problem auf individueller Ebene) eintreffen, was aber eher selten war. Kriterium 1 ist aber auch erfüllt, wenn die decision visit aufgrund der fehlenden GRT verzögert war, also Site-Ebene. Dies war der häufigere Fall. Heisst: die Teilnehmenden waren dann noch "in care" und haben ihre protocol-defined visits besucht, aber diese wurden nicht als "decision visit" definiert.
- Kriterium 4: individual level (und soweit ich mich erinnere komplett LTFU/TO, ohne missing data)
Es existieren Variablen für alle Kriterien (prot_dev_1 usw).



