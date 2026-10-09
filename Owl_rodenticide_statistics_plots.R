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
  scale_x_continuous(name = "AR dosage (mg/Kg)", limits = c(0,0.9), 
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
ggsave(filename = "Owl_rodenticide_boxplot_dosage.png", plot = gg_box_dosage, device = "png", width = 6, height = 6, units = "in", dpi = 600)

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
  scale_x_continuous(name = "Total AR dosage (mg/Kg)", 
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
  scale_x_continuous(name = "Total AR dosage (mg/Kg)", limits = c(0,0.9), 
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

ggsave(filename = "Owl_rodenticide_boxplot_total_updated.png", plot = gg_box_total, device = "png", width = 6, height = 6, units = "in", dpi = 600)

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
  group_by(Species, UrbanType) %>%
  count()

gg_donut <- ggplot(data = summ_cat, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = tox_cat)) +
  geom_rect(colour = "black", linewidth = 1) +
  geom_text(data = summ_comb, aes(hjust = 0.5, x = 2, y = 0.5, 
                                  label = paste0("n = ", n),
                                  colour = interaction(Species,UrbanType)), 
            inherit.aes = FALSE, size = 5) +
  coord_polar(theta = "y") +
  xlim(c(0, 4)) +
  scale_fill_brewer(name = "Toxicity category", palette = "YlOrRd") +
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
ggsave(filename = "Total_AR_donuts.png", plot = gg_donut, device = "png", width = 6, height = 6, dpi = 600, units = "in")

####Some statistics####
##KW test between species for each AR##
species_t_test <- rd2 %>%
  group_by(Rodenticide) %>%
  summarise(p.value = kruskal.test(Exposure ~ Species)$p.value) %>%
  ungroup()
#Rodenticide   p.value
#<fct>           <dbl>
#1 Brodifacoum   0.00550 - v. v. sig.
#2 Bromadiolone  0.432  
#3 Coumatetralyl 0.847  
#4 Difenacoum    0.382  
#5 Difethialone  0.913  
#6 Flocoumafen   0.0145 -v. sig.
#7 Pindone       0.208  
#8 Warfarin      0.208   

##KW test between urban types for each AR##
urbantype_t_test <- rd2 %>%
  group_by(Rodenticide) %>%
  summarise(p.value = kruskal.test(Exposure ~ UrbanType)$p.value) %>%
  ungroup()
#Rodenticide   p.value
#<fct>           <dbl>
#1 Brodifacoum    0.0692 - only just non-sig.
#2 Bromadiolone   0.142 
#3 Coumatetralyl  0.743 
#4 Difenacoum     0.225 
#5 Difethialone   0.0722
#6 Flocoumafen    0.0307 - sig.
#7 Pindone        0.405 
#8 Warfarin       0.405 

##KW test between interactions for each AR##
both_test <- rd2 %>%
  group_by(Rodenticide) %>%
  summarise(p.value = kruskal.test(Exposure ~ Group)$p.value) %>%
  ungroup()
#Rodenticide   p.value
#<fct>           <dbl>
#1 Brodifacoum   0.000530 - yes
#2 Bromadiolone  0.228   
#3 Coumatetralyl 0.962   
#4 Difenacoum    0.285   
#5 Difethialone  0.262   
#6 Flocoumafen   0.0296  - ues
#7 Pindone       0.543   
#8 Warfarin      0.543  


##Two-way manova: considers interaction of species and urbantype on all rodenticide dosages##
two_way_manova <- manova(cbind(Warfarin, Pindone, Flocoumafen, Difethialone, Difenacoum, Coumatetralyl, Bromadiolone, Brodifacoum) ~ Species * UrbanType, data = raw_data)
summary(two_way_manova)
#                 Df  Pillai    approx F numDf  denDf Pr(>F)   
#Species            1 0.45930   3.5040      8     33 0.004918 ** - significant effect of species
#UrbanType          1 0.31781   1.9217      8     33 0.089782 . - near significant effect of urban type
#Species:UrbanType  1 0.14669   0.7091      8     33 0.681530   - non-significant interaction
#Residuals          40 

##Two-way anova: considers interaction of species and urbantype on Brodifacoum only
brod_anova <- aov(Brodifacoum ~ Species * UrbanType, data = raw_data)
qqPlot(brod_anova$residuals, id = F)
shapiro.test(brod_anova$residuals) #W = 0.9224, p-value = 0.005731 - definitely not normally distributed
leveneTest(brod_anova)
#Levene's Test for Homogeneity of Variance (center = median)
#      Df F value  Pr(>F)  
#group  3  3.1187 0.03657 * - not equal variance either
#      40

summary(brod_anova)
#                  Df SumSq  MeanSq   Fvalue Pr(>F)    
#Species            1 0.2512 0.25116  12.870 0.00090 *** - v. significant effect of species
#UrbanType          1 0.1870 0.18700   9.583 0.00358 ** - sig. effect of urban type
#Species:UrbanType  1 0.0164 0.01638   0.839 0.36509    - non-sig. interaction
#Residuals         40 0.7806 0.01952

##Two-way anova: considers interaction of species and urbantype on total AR only
total_anova <- aov(Total_AR ~ Species * UrbanType, data = raw_data)
qqPlot(total_anova$residuals, id = F)
shapiro.test(total_anova$residuals) #W = 0.93994, p-value = 0.02352 - definitely not normally distributed
leveneTest(total_anova)
#Levene's Test for Homogeneity of Variance (center = median)
#      Df F value  Pr(>F)  
#group  3   4.016 0.01372 * - not equal variance either
#      40
summary(total_anova)
#                   Df SumSq  MeanSq   Fvalue   Pr(>F)    
#Species            1 0.4060  0.4060  17.028 0.000181 *** - v. significant effect of species
#UrbanType          1 0.1964  0.1964   8.236 0.006532 ** - sig. effect of urban type
#Species:UrbanType  1 0.0369  0.0369   1.548 0.220738    - non-sig. interaction
#Residuals         40 0.9538  0.0238 

##One way anova between groups
total_aov <- aov(Total_AR ~ Group, data = raw_data)
summary(total_aov)
#            Df SumSq   MeanSq   F     Pr(>F)    
#Group        3 0.6393 0.21311   8.937 0.000118 *** - v. significant difference between combination of groups
#Residuals   40 0.9538 0.02385       

##Kruskal-Wallis test between groups (species x land type)
kruskal.test(Total_AR ~ Group, data = raw_data)
#Kruskal-Wallis chi-squared = 20.455, df = 3, p-value = 0.0001366 - v. sig.

##Kruskal-Wallis test between species
kruskal.test(Total_AR ~ Species, data = raw_data)
#Kruskal-Wallis chi-squared = 10.99, df = 1, p-value = 0.0009159 - v. sig.

##Kruskal-Wallis test between urban types
kruskal.test(Total_AR ~ UrbanType, data = raw_data)
#Kruskal-Wallis chi-squared = 2.1915, df = 1, p-value = 0.1388 - non. sig.

##Kruskal-Wallis test between urban types for barn owls only
barn_only <- raw_data %>%
  filter(Species=="Barn")
kruskal.test(Total_AR ~ UrbanType, data = barn_only)
#Kruskal-Wallis chi-squared = 14.138, df = 1, p-value = 0.0001699

##Kruskal-Wallis test between urban types for masked owls only
mask_only <- raw_data %>%
  filter(Species=="Masked")
kruskal.test(Total_AR ~ UrbanType, data = mask_only)
#Kruskal-Wallis chi-squared = 0.14286, df = 1, p-value = 0.7055

##Aligned Rank Transform ANOVA: Non-parametric equivalent to two-way ANOVA
library(ARTool)
raw_data$UrbanType <- as.factor(raw_data$UrbanType)
raw_data$Species <- as.factor(raw_data$Species)
m <- art(Brodifacoum ~ Species * UrbanType, data = raw_data)
summary(m)
anova(m)
