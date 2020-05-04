library(tidyverse)
library(forcats)
library(ggplot2)
df_sensitivity <- read_csv("C:/Users/akihi/OneDrive/Documents/sensitivity_data.csv")
df_specificity <- read_csv("C:/Users/akihi/OneDrive/Documents/specificity_data.csv")
#sensitivity
df_sensitivity  %>% mutate(Test = fct_relevel(Test,
                                              "Lomas (collapse)",
                                              "Lomas (consolidation)",
                                              "Lomas (flap)",
                                              "Qureshi (hepatic metastases)",
                                              "Bugalho (hepatic metastases)",
                                              "Bugalho (peripheral parenchymal lung lesion)",
                                              "Bugalho (chest wall invasion)",
                                              "Yang (parenchymal lesion)",
                                              "Bugalho (lung air bronchogram sign)",
                                              "Qureshi (diaphragmatic nodularity)",
                                              "Bugalho (diaphragmatic nodularity)",
                                              "Qureshi (visceral pleural thickening/nodularity)",
                                              "Qureshi (smooth parietal pleural)",
                                              "Qureshi (nodular parietal pleural)",
                                              "Bugalho (parietal pleural nodularity)",
                                              "Bugalho (visceral pleural nodularity)",
                                              "Faheem (pleural nodularity)",
                                              "Yang (pleural nodularity)",
                                              "Bugalho (total nodularity)",
                                              "Qureshi (smooth diaphragmatic thickening)",
                                              "Qureshi (nodular diaphragmatic thickening)",
                                              "Qureshi (parietal pleural thickening <= 10mm)",
                                              "Qureshi (parietal pleural thickening > 10mm)",
                                              "Qureshi (parietal pleural thickening)",
                                              "Bugalho (diaphragmatic thickning > 10mm)",
                                              "Bugalho (diaphragmatic thickning > 7mm)",
                                              "Qureshi (diaphragmatic thickening)",
                                              "Bugalho (parietal pleural thickning > 10mm)",
                                              "Bugalho (parietal pleural thickning > 3mm)",
                                              "Bugalho (visceral pleural thickning > 10mm)",
                                              "Bugalho (visceral pleural thickning > 3mm)",
                                              "Bugalho (pleural thickness > 10mm)",
                                              "Bugalho (pleural thickness)",
                                              "Faheem (pleural thickness)",
                                              "Yang (pleural thickness)",
                                              "Lomas (swirling sign)",
                                              "Bugalho (swirling sign)",
                                              "Bugalho (septated echogenecity)",
                                              "Bugalho (heterogeneous echogenicity)",
                                              "Bugalho (homogeneous echogenicity)",
                                              "Yang (homogeneous echogenicity)",
                                              "Qureshi (echogenicity)",
                                              "Marcun (echogenicity)",
                                              "Asciak (echogenicity)",
                                              "Yang (complex septa)",
                                              "Yang (complex nonsepta)",
                                              "Marcun (complex with fibrin)",
                                              "Marcun (complex with septae)",
                                              "Lomas (secondary loculation)",
                                              "Lomas (primary loculation)",
                                              "Faheem (encysted pleural effusion)",
                                              "Qureshi (anechoic/septa)",
                                              "Qureshi (anechoic/simple)",
                                              "Bugalho (anecho)",
                                              "Yang (anecho)"
)) %>% 
  ggplot(aes(x=Test, y=Sensitivity, ymin=sn_lower, ymax=sn_upper)) +
  geom_linerange(size=2, colour="#a6d8f0") +
  geom_point(size=2, shape=21, fill="#008fd5", colour = "white", stroke = 1) +
  scale_y_continuous(limits = c(0, 1)) + 
  coord_flip() + 
  ggtitle("Sensitivity") +
  theme_minimal() -> forest_sensitivity 

forest_sensitivity 
#specificity
df_specificity %>% mutate(Test = fct_relevel(Test,
                                             "Lomas (collapse)",
                                             "Lomas (consolidation)",
                                             "Lomas (flap)",
                                             "Qureshi (hepatic metastases)",
                                             "Bugalho (hepatic metastases)",
                                             "Bugalho (peripheral parenchymal lung lesion)",
                                             "Bugalho (chest wall invasion)",
                                             "Yang (parenchymal lesion)",
                                             "Bugalho (lung air bronchogram sign)",
                                             "Qureshi (diaphragmatic nodularity)",
                                             "Bugalho (diaphragmatic nodularity)",
                                             "Qureshi (visceral pleural thickening/nodularity)",
                                             "Qureshi (smooth parietal pleural)",
                                             "Qureshi (nodular parietal pleural)",
                                             "Bugalho (parietal pleural nodularity)",
                                             "Bugalho (visceral pleural nodularity)",
                                             "Faheem (pleural nodularity)",
                                             "Yang (pleural nodularity)",
                                             "Bugalho (total nodularity)",
                                             "Qureshi (smooth diaphragmatic thickening)",
                                             "Qureshi (nodular diaphragmatic thickening)",
                                             "Qureshi (parietal pleural thickening <= 10mm)",
                                             "Qureshi (parietal pleural thickening > 10mm)",
                                             "Qureshi (parietal pleural thickening)",
                                             "Bugalho (diaphragmatic thickning > 10mm)",
                                             "Bugalho (diaphragmatic thickning > 7mm)",
                                             "Qureshi (diaphragmatic thickening)",
                                             "Bugalho (parietal pleural thickning > 10mm)",
                                             "Bugalho (parietal pleural thickning > 3mm)",
                                             "Bugalho (visceral pleural thickning > 10mm)",
                                             "Bugalho (visceral pleural thickning > 3mm)",
                                             "Bugalho (pleural thickness > 10mm)",
                                             "Bugalho (pleural thickness)",
                                             "Faheem (pleural thickness)",
                                             "Yang (pleural thickness)",
                                             "Lomas (swirling sign)",
                                             "Bugalho (swirling sign)",
                                             "Bugalho (septated echogenecity)",
                                             "Bugalho (heterogeneous echogenicity)",
                                             "Bugalho (homogeneous echogenicity)",
                                             "Yang (homogeneous echogenicity)",
                                             "Qureshi (echogenicity)",
                                             "Marcun (echogenicity)",
                                             "Asciak (echogenicity)",
                                             "Yang (complex septa)",
                                             "Yang (complex nonsepta)",
                                             "Marcun (complex with fibrin)",
                                             "Marcun (complex with septae)",
                                             "Lomas (secondary loculation)",
                                             "Lomas (primary loculation)",
                                             "Faheem (encysted pleural effusion)",
                                             "Qureshi (anechoic/septa)",
                                             "Qureshi (anechoic/simple)",
                                             "Bugalho (anecho)",
                                             "Yang (anecho)",
)) %>% 
  ggplot(aes(x=Test, y=Specificity, ymin=sp_lower, ymax=sp_upper)) +
  geom_linerange(size=2, colour="pink") +
  geom_point(size=2, shape=21, fill="#008fd5", colour = "white", stroke = 1) +
  scale_y_continuous(limits = c(0, 1)) + 
  coord_flip() + 
  ggtitle("Specificity") +
  theme_minimal() -> forest_specificity
forest_specificity