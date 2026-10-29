library(tidyverse)
library(ggplot2)
library(ggh4x)
library(broom)
library(car)
setwd("D:/ECU/Owl_rodenticide/DraftPaper/")

####Load data and arrange for plotting####
raw_data <- read.csv(file = "./Owl_rodenticide_raw.csv")
rd2 <- raw_data %>%
  pivot_longer(cols = 6:13, names_to = "Rodenticide", values_to = "Exposure")
rd2$Rodenticide <- as.factor(rd2$Rodenticide)
rd2$Species <- as.factor(rd2$Species)
rd2$Region <- as.factor(rd2$Region)
rd2$UrbanType <- as.factor(rd2$UrbanType)
rd2$Group <- as.factor(rd2$Group)

summary <- rd2 %>%
  group_by(Species, UrbanType) %>%
  reframe(n = n()/8)

####Plot boxplots####
spec_cols <- c("#82B0D9","#8CCFB4","#26547C","#307358")

gg_box_dosage <- ggplot(data = rd2, aes(x = Exposure, y = Rodenticide)) +
  geom_boxplot(aes(fill = interaction(Species, UrbanType))) +
  geom_text(data = summary, aes(x = 0.85, y = 0.75, label = paste0("n=", n)), colour = "#2A3439", size = 4) +
  facet_grid2(UrbanType ~ Species, axes = "all", remove_labels = "all") +
  scale_x_continuous(name = "AR concentration (mg/Kg)", limits = c(0,0.9), 
                     breaks = seq(0,0.8,0.4)) +
  scale_fill_manual(values = spec_cols) +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        axis.title.x = element_text(size=16),
        axis.title.y = element_blank(),
        axis.ticks = element_line(colour = "black", linewidth = 1),
        axis.line = element_line(colour = "black", linewidth = 1),
        axis.text.x = element_text(size=12, colour = "black"),
        axis.text.y = element_text(size=12, colour = "black", hjust = 1, vjust = 0.5),
        panel.border = element_rect(fill = NA, colour = NA),
        panel.spacing.x = unit(1, "lines"),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 16),
        plot.margin = margin(0.1,0.1,0.1,0.1,"cm"),
        legend.position = "none")
plot(gg_box_dosage)
ggsave(filename = "Owl_rodenticide_boxplot_dosage_relabelled.png", plot = gg_box_dosage, device = "png", width = 6, height = 6, units = "in", dpi = 600)

####Plot cumulative curves####
cumulative_df <- raw_data %>%
  group_by(Group, Total_AR) %>%
  arrange(Total_AR) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(
    CumCount = cumsum(n),                     # cumulative count
    CumProp = CumCount / sum(n)               # cumulative proportion
  ) %>%
  ungroup()

final_point <- cumulative_df %>%
  group_by(Group) %>%
  summarise(
    Total_AR = 0.807,
    CumCount = max(CumCount),
    CumProp  = 1)

first_point <- cumulative_df %>%
  group_by(Group) %>%
  summarise(
    Total_AR = 0,
    CumCount = 0,
    CumProp  = 0) %>%
  filter(Group!="BarnRural")

cumulative_df <- bind_rows(cumulative_df,
                           first_point,
                           final_point) %>%
  arrange(Group, Total_AR)

curve_cols <- c("#82B0D9","#26547C","#8CCFB4","#307358")

gg_curve <- ggplot(data = cumulative_df, aes(x = Total_AR, y = CumProp)) +
  geom_vline(xintercept = 0.01, linetype = "dotted") +
  geom_vline(xintercept = 0.1, linetype = "dotted") +
  geom_vline(xintercept = 0.2, linetype = "dotted") +
  geom_vline(xintercept = 0.5, linetype = "dotted") +
  geom_vline(xintercept = 0.7, linetype = "dotted") +
  geom_point(aes(group = Group, color = Group), shape = 22, size = 0) +
  geom_line(aes(group = Group, color = Group), linewidth = 1, show.legend = F) +
  scale_color_manual(values = curve_cols, labels = c("Barn, Rural", "Barn, Urban", "Masked, Rural", "Masked, Urban")) +
  scale_x_continuous(name = "Total AR concentration (mg/Kg)", 
                     limits = c(0,0.807), expand = c(0,0),
                     labels = scales::label_number(drop0trailing=TRUE)) +
  scale_y_continuous(name = "Cumulative proportion of owls", 
                     limits = c(0,1.01), expand = c(0,0),
                     labels = scales::label_number(drop0trailing=TRUE)) +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        axis.title = element_text(size=16),
        axis.ticks = element_line(colour = "black", linewidth = 1),
        axis.line = element_line(colour = "black", linewidth = 1),
        axis.text.x = element_text(size=12, colour = "black"),
        axis.text.y = element_text(size=12, colour = "black", hjust = 1, vjust = 0.5),
        panel.border = element_rect(fill = NA, colour = NA),
        panel.spacing.x = unit(1, "lines"),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 16),
        plot.margin = margin(0.1,0.3,0.1,0.1,"cm"),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 12)) +
  guides(color = guide_legend(nrow = 1, override.aes = list(size = 5, fill = curve_cols, colour = "black")))
plot(gg_curve)
ggsave(filename = "Owl_rodenticide_cumulative_dosage_updated.png", plot = gg_curve, device = "png", width = 6, height = 6, units = "in", dpi = 600)

##Boxplot for Total AR only####
spec_cols <- c("#82B0D9","#26547C","#8CCFB4","#307358")
raw_data$Group <- factor(raw_data$Group, levels = c("BarnRural","BarnUrban","MaskedRural","MaskedUrban"),
                         labels = c("Barn, Rural", "Barn, Urban", "Masked, Rural", "Masked, Urban"))

gg_box_total <- ggplot(data = raw_data, aes(x = Total_AR, y = Group)) +
  geom_boxplot(aes(fill = Group)) +
  scale_x_continuous(name = "Total AR concentration (mg/Kg)", limits = c(0,0.9), 
                     breaks = seq(0,0.8,0.4)) +
  scale_y_discrete(labels = c("Barn, Rural", "Barn, Urban", "Masked, Rural", "Masked, Urban")) +
  scale_fill_manual(values = spec_cols) +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        axis.title.x = element_text(size=16),
        axis.title.y = element_blank(),
        axis.ticks.x = element_line(colour = "black", linewidth = 1),
        axis.ticks.y = element_blank(),
        axis.line = element_line(colour = "black", linewidth = 1),
        axis.text.x = element_text(size=12, colour = "black"),
        axis.text.y = element_blank(),
        panel.border = element_rect(fill = NA, colour = NA),
        panel.spacing.x = unit(1, "lines"),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 16),
        plot.margin = margin(0.1,0.1,0.1,0.1,"cm"),
        legend.position = "top",
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 16),
        legend.spacing = unit(5,"in")) +
  guides(colour = "none",
         fill = guide_legend(title.position = "top", title.hjust = 0.5, nrow = 2))
plot(gg_box_total)

ggsave(filename = "Owl_rodenticide_boxplot_total_relabelled.png", plot = gg_box_total, device = "png", width = 6, height = 6, units = "in", dpi = 600)

####Donut plots of categorical Total AR####
##Categorise estimates based on thresholds defined in Lohr, 2018
cat_data <- raw_data %>%
  mutate(tox_cat = case_when(
    Total_AR < 0.001 ~ "None", #No rodenticide
    Total_AR < 0.01 ~ "Cat1", #Probably no toxicity
    Total_AR < 0.1 ~ "Cat2", #Possible Toxicity/Unlikely Lethal
    Total_AR < 0.2 ~ "Cat3", #Possibly Lethal/Likely Toxicity
    Total_AR < 0.5 ~ "Cat4", #Possibly Lethal/Likely Toxicity
    Total_AR < 0.7 ~ "Cat5", #Probably Lethal
    Total_AR > 0.7 ~ "Cat6")) #Lethal

cat_data$tox_cat <- factor(x = cat_data$tox_cat, 
                           levels = c("None","Cat1","Cat2","Cat3","Cat4","Cat5","Cat6"),
                           labels = c("<0.001 mg/Kg",
                                      "0.001-0.01 mg/Kg",
                                      "0.01-0.1 mg/Kg",
                                      "0.1-0.2 mg/Kg",
                                      "0.2-0.5 mg/Kg",
                                      "0.5-0.7 mg/Kg",
                                      ">0.7 mg/Kg"))

summ_cat <- cat_data %>%
  group_by(Species, UrbanType, tox_cat) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Species, UrbanType) %>%
  mutate(
    prop = n / sum(n),
    ymax = cumsum(prop),
    ymin = lag(ymax, default = 0))

summ_comb <- cat_data %>%
  group_by(tox_cat) %>%
  count() %>%
  ungroup() %>%
  mutate(perc = (n / sum(n)) * 100)

gg_donut <- ggplot(data = summ_cat, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = tox_cat)) +
  geom_rect(colour = "black", linewidth = 1) +
  geom_text(data = summ_comb, aes(hjust = 0.5, x = 2, y = 0.5, 
                                  label = paste0("n = ", n),
                                  colour = interaction(Species,UrbanType)), 
            inherit.aes = FALSE, size = 5) +
  coord_polar(theta = "y") +
  xlim(c(0, 4)) +
  scale_fill_brewer(name = "AR concentration category", palette = "YlOrRd") +
  scale_colour_manual(values = c("#82B0D9","#8CCFB4","#26547C","#307358"), ) +
  facet_grid(UrbanType ~ Species) +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        panel.border = element_rect(fill = NA, colour = NA),
        panel.spacing.x = unit(1, "lines"),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 16),
        plot.margin = margin(0.1,0.1,0.1,0.1,"cm"),
        legend.position = "top",
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 16),
        legend.spacing = unit(5,"in")) +
  guides(colour = "none",
         fill = guide_legend(title.position = "top", title.hjust = 0.5))
plot(gg_donut)
ggsave(filename = "Total_AR_donuts_relabelled.png", plot = gg_donut, device = "png", width = 6, height = 6, dpi = 600, units = "in")

####Species comparisons####
###Kruskal-Wallis test for between species for total AR
kruskal.test(Total_AR ~ Species, data = raw_data)
#Kruskal-Wallis chi-squared = 10.99, df = 1, p-value = 0.0009159 - v. sig.

###KW test between species for each AR
species_t_test <- rd2 %>%
  group_by(Rodenticide) %>%
  summarise(chi_sq = kruskal.test(Exposure ~ Species)$statistic,
            p.value = kruskal.test(Exposure ~ Species)$p.value) %>%
  ungroup()
#Rodenticide   chi_sq p.value
#<fct>          <dbl>   <dbl>
#1 Brodifacoum   7.71   0.00550 ***
#2 Bromadiolone  0.618  0.432  
#3 Coumatetralyl 0.0373 0.847  
#4 Difenacoum    0.765  0.382  
#5 Difethialone  0.0120 0.913  
#6 Flocoumafen   5.97   0.0145 *
#7 Pindone       1.59   0.208  
#8 Warfarin      1.59   0.208   

####Land-type comparisons####
###Species together###
##Kruskal-Wallis test between urban types for total AR
kruskal.test(Total_AR ~ UrbanType, data = raw_data)
#Kruskal-Wallis chi-squared = 2.1915, df = 1, p-value = 0.1388 - non. sig.

##KW test between urban types for each AR
urbantype_t_test <- rd2 %>%
  group_by(Rodenticide) %>%
  summarise(chi_sq = kruskal.test(Exposure ~ UrbanType)$statistic, 
              p.value = kruskal.test(Exposure ~ UrbanType)$p.value) %>%
  ungroup()
#Rodenticide   chi_sq p.value
#<fct>          <dbl>   <dbl>
#1 Brodifacoum    3.30   0.0692 - marginal not
#2 Bromadiolone   2.15   0.142 
#3 Coumatetralyl  0.107  0.743 
#4 Difenacoum     1.47   0.225 
#5 Difethialone   3.23   0.0722 - marginal not
#6 Flocoumafen    4.67   0.0307 - yes
#7 Pindone        0.692  0.405 
#8 Warfarin       0.692  0.405 

###Species separate####
###Barn Owls###
barn_only <- raw_data %>%
  filter(Species=="Barn")

###Kruskal-Wallis test of total AR between urban types for barn owls only
kruskal.test(Total_AR ~ UrbanType, data = barn_only)
#Kruskal-Wallis chi-squared = 14.138, df = 1, p-value = 0.0001699

###Kruskal-Wallis test of individual ARs between urban types for barn owls only
barn_long <- rd2 %>% #Has individual ARs as rows
  filter(Species=="Barn") 

urbantype_kw_barn <- barn_long  %>%
  group_by(Rodenticide) %>%
  summarise(chi_sq = kruskal.test(Exposure ~ UrbanType)$statistic, 
            p.value = kruskal.test(Exposure ~ UrbanType)$p.value) %>%
  ungroup()
#Rodenticide     chi_sq    p.value
#<fct>            <dbl>      <dbl>
#1 Brodifacoum    12.6      0.000390 - v. sig.
#2 Bromadiolone    2.49     0.115   
#3 Coumatetralyl   0.0462   0.830   
#4 Difenacoum      3.03     0.0817   - marginal non-sig.
#5 Difethialone    2.59     0.108   
#6 Flocoumafen     1.25     0.264   
#7 Pindone       NaN      NaN       
#8 Warfarin      NaN      NaN      

###Masked Owls###
##Kruskal-Wallis test between urban types for masked owls only
mask_only <- raw_data %>%
  filter(Species=="Masked")
kruskal.test(Total_AR ~ UrbanType, data = mask_only)
#Kruskal-Wallis chi-squared = 0.14286, df = 1, p-value = 0.7055

###Kruskal-Wallis test of individual ARs between urban types for masked owls only
mask_long <- rd2 %>% #Has individual ARs as rows
  filter(Species=="Masked") 

urbantype_kw_mask <- mask_long  %>%
  group_by(Rodenticide) %>%
  summarise(chi_sq = kruskal.test(Exposure ~ UrbanType)$statistic, 
            p.value = kruskal.test(Exposure ~ UrbanType)$p.value) %>%
  ungroup()
#Rodenticide   chi_sq p.value
#<fct>          <dbl>   <dbl>
#1 Brodifacoum   0.482    0.488
#2 Bromadiolone  1.13     0.287
#3 Coumatetralyl 0.214    0.643
#4 Difenacoum    0.0448   0.832
#5 Difethialone  1.26     0.261
#6 Flocoumafen   1.37     0.241
#7 Pindone       0.214    0.643
#8 Warfarin      0.214    0.643     

####Interaction between species and landtype####
##Two-way anova: considers interaction of species and urbantype on total AR##
two_way_total <- aov(Total_AR ~ Species * UrbanType, data = raw_data)
summary(two_way_total)
#                  Df SumSq   MeanSq  Fvalue Pr(>F)    
#Species            1 0.4060  0.4060  17.028 0.000181 *** - v. sig.
#UrbanType          1 0.1964  0.1964   8.236 0.006532 **  - sig.
#Species:UrbanType  1 0.0369  0.0369   1.548 0.220738     - non.sig
#Residuals         40 0.9538  0.0238                     

##Two-way manova: considers interaction of species and urbantype on all rodenticide dosages##
two_way_manova <- manova(cbind(Warfarin, Pindone, Flocoumafen, Difethialone, Difenacoum, Coumatetralyl, Bromadiolone, Brodifacoum) ~ Species * UrbanType, data = raw_data)
summary(two_way_manova)
#                 Df  Pillai    approx F numDf  denDf Pr(>F)   
#Species            1 0.45930   3.5040      8     33 0.004918 ** - significant effect of species
#UrbanType          1 0.31781   1.9217      8     33 0.089782 . - near significant effect of urban type
#Species:UrbanType  1 0.14669   0.7091      8     33 0.681530   - non-significant interaction
#Residuals          40 

##Two-way anova: considers interaction of species and urbantype on brodifacoum##
two_way_brod <- aov(Brodifacoum ~ Species * UrbanType, data = raw_data)
summary(two_way_brod)
#                  Df SumSq   MeanSq  Fvalue Pr(>F)    
#Species            1 0.2512 0.25116  12.870 0.00090 ***
#UrbanType          1 0.1870 0.18700   9.583 0.00358 ** 
#Species:UrbanType  1 0.0164 0.01638   0.839 0.36509    
#Residuals         40 0.7806 0.01952    

####Alternative two-way ANOVA approach - not reported####
##Aligned Rank Transform ANOVA: Non-parametric equivalent to two-way ANOVA
library(ARTool)
raw_data$UrbanType <- as.factor(raw_data$UrbanType)
raw_data$Species <- as.factor(raw_data$Species)
m <- art(Total_AR ~ Species * UrbanType, data = raw_data)
summary(m)
#Column sums of aligned responses (should all be ~0):
#Species         UrbanType Species:UrbanType 
#0                 0                 0 

#F values of ANOVAs on aligned responses not of interest (should all be ~0):
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.0000  0.0000  0.0000  0.2174  0.1301  1.1311

#F values of ANOVAs on aligned responses not of interest are not all ~0.
#ART may not be appropriate.
anova(m)
#Analysis of Variance of Aligned Rank Transformed Data
#Table Type: Anova Table (Type III tests) 
#Model: No Repeated Measures (lm)
#Response: art(Total_AR)

#Df Df.res F value     Pr(>F)    
#1 Species            1     40 14.5431 0.00046416 *** - v. sig. effect of species
#2 UrbanType          1     40  1.7339 0.19540291    
#3 Species:UrbanType  1     40  3.1816 0.08205791   . - marginally non-sig. interaction

#Signif. codes:   0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

####Figures for graphical abstract####
###Species comparisons
##For adding relevant fonts
library(ggtext)
library(extrafont)
#font_import()
loadfonts(device = "win")

spec_summ <- raw_data %>%
  group_by(Species) %>%
  reframe(mean.ar = mean(Total_AR),
          sd.ar = sd(Total_AR),
          n.ar = n())
spec_summ$Species <- factor(spec_summ$Species, levels = c("Masked","Barn"))
spec_cols <- c("#307358","#26547C")

gg_spec <- ggplot(data = spec_summ, aes(x = Species)) +
  geom_col(aes(y = mean.ar, fill = Species), colour = "black") +
  geom_errorbar(aes(ymin = mean.ar - sd.ar, ymax = mean.ar + sd.ar), 
                width = 0.15, linewidth = 0.5) +
  geom_text(aes(x = Species, y = mean.ar + sd.ar + 0.05, label = round(mean.ar, 2)), size = 10, family = "Barlow") +
  scale_fill_manual(values = spec_cols) +
  scale_y_continuous(name = "Mean AR\nconcentration (mg/Kg)", breaks = seq(0, 0.6, 0.2),
                     limits = c(0,0.7),
                     expand = expansion(mult = c(0,0.01))) +
  scale_x_discrete(labels = c("<b>Masked Owl</b><br>(<i>n</i> = 17)",
                              "<b>Barn Owl</b><br>(<i>n</i> = 27)")) +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 20, colour = "black", face = "bold", family = "Barlow"),
        axis.ticks = element_line(colour = "black", linewidth = 1),
        axis.line = element_line(colour = "black", linewidth = 1),
        axis.text.x = element_markdown(size = 20, colour = "black", family = "Barlow"),
        axis.text.y = element_text(size = 16, colour = "black", angle= 90, hjust = 0.5, family = "Barlow"),
        panel.border = element_rect(fill = NA, colour = NA),
        panel.spacing.x = unit(1, "lines"),
        panel.background = element_rect(fill = "transparent", color = "transparent"),
        plot.background = element_rect(fill = "transparent", color = "transparent"),
        strip.background = element_blank(),
        strip.text = element_text(size = 16),
        plot.margin = margin(0.5,0.1,0.1,0.1,"cm"),
        legend.position = "none")
gg_spec

ggsave(filename = "Owl_comparison_for_graphical_abstract.png", plot = gg_spec, device = "png", path = "./", width = 8, height = 4, units = "in", dpi = 600)

###Landscape comparison
barn_land_comp <- raw_data %>%
  filter(Species=="Barn") %>%
  group_by(UrbanType) %>%
  reframe(mean.ar = mean(Total_AR),
          sd.ar = sd(Total_AR),
          n.ar = n())
barn_land_comp$UrbanType <- factor(barn_land_comp$UrbanType, levels = c("Urban","Rural"))
lt_cols <- c("#26547C", "#82B0D9")

gg_land <- ggplot(data = barn_land_comp, aes(x = UrbanType)) +
  geom_col(aes(y = mean.ar, fill = UrbanType), colour = "black") +
  geom_errorbar(aes(ymin = mean.ar - sd.ar, ymax = mean.ar + sd.ar), 
                width = 0.15, linewidth = 0.5) +
  geom_text(aes(x = UrbanType, y = mean.ar + sd.ar + 0.05, label = round(mean.ar, 2)), size = 10, family = "Barlow") +
  scale_fill_manual(values = lt_cols) +
  scale_y_continuous(name = "Mean AR\nconcentration (mg/Kg)", breaks = seq(0, 0.4, 0.2),
                     limits = c(0,0.5),
                     expand = expansion(mult = c(0,0.01))) +
  scale_x_discrete(labels = c("<b>Urban Perth</b><br>(<i>n</i> = 12)",
                              "<b>Rural regions</b><br>(<i>n</i> = 15)")) +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 20, colour = "black", face = "bold", family = "Barlow"),
        axis.ticks = element_line(colour = "black", linewidth = 1),
        axis.line = element_line(colour = "black", linewidth = 1),
        axis.text.x = element_markdown(size = 20, colour = "black", family = "Barlow"),
        axis.text.y = element_text(size = 16, colour = "black", angle= 90, hjust = 0.5, family = "Barlow"),
        panel.border = element_rect(fill = NA, colour = NA),
        panel.spacing.x = unit(1, "lines"),
        panel.background = element_rect(fill = "transparent", color = "transparent"),
        plot.background = element_rect(fill = "transparent", color = "transparent"),
        strip.background = element_blank(),
        strip.text = element_text(size = 16),
        plot.margin = margin(0.5,0.1,0.1,0.1,"cm"),
        legend.position = "none")
plot(gg_land)
ggsave(filename = "Landtype_comparison_for_graphical_abstract.png", plot = gg_land, device = "png", path = "./", width = 8, height = 4, units = "in", dpi = 600)
