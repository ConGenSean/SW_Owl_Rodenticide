## Overview
This repository contains the data and analysis code for:

**Davis, R.A., Lohr, M.T., Yaschenko, N., & Buckley, S.J.**
*High exposure of Australian Masked Owls and Eastern Barn Owls to Second-generation anticoagulant rodenticides in South-western Australia.*

The study investigates exposure to anticoagulant rodenticides (ARs) in **Australasian Masked Owls (*Tyto novaehollandiae*)** and **Eastern Barn Owls (*Tyto javanica*)** from south-western Western Australia. Liver samples from 44 deceased owls were analysed for residues of eight anticoagulant rodenticides using LC-MS/MS methods. The study compares exposure between species and between urban and rural landscapes.

---
## Repository Contents
### Data
**Owl_rodenticide_raw_corrected.csv**

Raw dataset containing individual owl records and measured liver concentrations (mg/Kg) for eight anticoagulant rodenticides:
- Brodifacoum
- Bromadiolone
- Difenacoum
- Difethialone
- Flocoumafen
- Pindone
- Warfarin
- Coumatetralyl

The dataset also includes:
- Species identification
- Urban/rural classification
- Geographic region and locality
- Total anticoagulant rodenticide concentration (`Total_AR`)
- Number of detected rodenticides (`Number_Ars`)
- Approximate locality coordinates

Dataset summary:
- Total samples: **44 owls**
- Barn Owls: **27**
- Masked Owls: **17**
---

### Code
**Owl_rodenticide_statistics_plots.R**

Accompanying R script for all analyses and figure-generation code, including:
- Data wrangling and preparation
- Boxplots of individual rodenticide concentrations
- Cumulative exposure curves
- Total rodenticide concentration visualisations
- Donut plots based on toxicity categories
- Species comparisons
- Urban versus rural comparisons
- Two-way ANOVA and MANOVA analyses
- Graphical abstract figures
---

## Software and Packages
Analyses were conducted in **R 4.3.2** using the following packages:
```r
tidyverse
ggplot2
ggh4x
broom
car
ARTool
ggtext
extrafont
