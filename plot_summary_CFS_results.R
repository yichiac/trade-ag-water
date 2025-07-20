library(ggplot2)

summary_cfs <- read.csv('Irr_area_IV_2_csv_next_yr.csv')

summary_cfs$Prediction <- as.character(summary_cfs$Prediction)
summary_cfs$Prediction_rename <- summary_cfs$Prediction
#summary_cfs$Prediction_rename[which(summary_cfs$Prediction=="Same Year Irrigated Area")] <- "Same Year\nIrrigated Area"
#summary_cfs$Prediction_rename[which(summary_cfs$Prediction=="Subsequent Year Irrigated Area")] <- "Subsequent Year\nIrrigated Area"
summary_cfs$Prediction_rename[which(summary_cfs$Prediction=="Subsequent Year Irrigated Area")] <- "Irrigated Area"

summary_cfs$Trade_Year[which(summary_cfs$Trade_Year==2012)]<-2013
summary_cfs$Trade_Year[which(summary_cfs$Trade_Year==2017)]<-2018
summary_cfs$Trade_Year[which(summary_cfs$Trade_Year==2022)]<-2023

summary_cfs <- summary_cfs[which(summary_cfs$Model!="M5" & summary_cfs$Model!="M6"),]

group.colors <- c("p-value > 0.1" = "black", "p-value < 0.05" = "red", "p-value < 0.1" ="darkred")


ggplot(data = summary_cfs, mapping = aes(x = Model, y = Mean_Coeff, color = P.value)) +
  theme_bw()+
  geom_pointrange(mapping = aes(ymin = coeff_min, ymax = coeff_max, color=P.value))+
  facet_grid(Trade_Year~Prediction_rename)+ theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  ylab('Coefficient of Constructed \n Openness in Stage 2')+xlab('Model')+
  geom_hline(yintercept=0, linetype='dotted', col = 'black')+
  scale_color_manual(values=group.colors)+theme(legend.position="bottom")+
  guides(color=guide_legend(title="Significance level"))


ggsave('CFS_results_summary_irrigation_area.png', plot = last_plot(),
       width=7, height = 5,
       units = 'in')
ggsave('CFS_results_summary_irrigation_area.jpg', plot = last_plot(),
       width=7, height = 5,
       units = 'in')
ggsave('CFS_results_summary_irrigation_area.pdf', plot = last_plot(),
       width=7, height = 5,
       units = 'in')

plt1<-ggplot(data = summary_cfs, mapping = aes(x = Model, y = Mean_Coeff, color = P.value)) +
  theme_bw()+
  geom_pointrange(mapping = aes(ymin = coeff_min, ymax = coeff_max, color=P.value))+
  facet_grid(Trade_Year~Prediction_rename)+ theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  ylab('Coefficient of Constructed \n Openness in Stage 2')+xlab('Model')+
  geom_hline(yintercept=0, linetype='dotted', col = 'black')+
  scale_color_manual(values=group.colors)+theme(legend.position="bottom")+
  guides(color=guide_legend(title="Significance level"))+
  labs(caption = "(a) Instrumental Variable Stage 2 for Irrigated Area")+
  theme(plot.caption = element_text(hjust = 0.5, size=10))

plt1 <- plt1 + theme(legend.position = "none")


summary_cfs <- read.csv('Irr_water_IV_2_csv_next_yr.csv')

summary_cfs$Prediction_rename <- summary_cfs$Prediction
#summary_cfs$Prediction_rename[which(summary_cfs$Prediction=="Subsequent Year Irrigation Water Withdrawals")] <- "Subsequent Year\nIrrigation Water Withdrawals"
#summary_cfs$Prediction_rename[which(summary_cfs$Prediction=="Subsequent Year Irrigation Water Withdrawals GW")] <- "Subsequent Year\nIrrigation Water Withdrawals\nGW"
#summary_cfs$Prediction_rename[which(summary_cfs$Prediction=="Subsequent Year Irrigation Water Withdrawals SW")] <- "Subsequent Year\nIrrigation Water Withdrawals\nSW"

summary_cfs$Prediction_rename[which(summary_cfs$Prediction=="Subsequent Year Irrigation Water Withdrawals")] <- "Total Irrigation\nWater Withdrawals"
summary_cfs$Prediction_rename[which(summary_cfs$Prediction=="Subsequent Year Irrigation Water Withdrawals GW")] <- "Groundwater\nIrrigation Withdrawals"
summary_cfs$Prediction_rename[which(summary_cfs$Prediction=="Subsequent Year Irrigation Water Withdrawals SW")] <- "Surface water\nIrrigation Withdrawals"

summary_cfs$Trade_Year[which(summary_cfs$Trade_Year==2012)]<-2013
summary_cfs$Trade_Year[which(summary_cfs$Trade_Year==2017)]<-2018
summary_cfs$Trade_Year[which(summary_cfs$Trade_Year==2022)]<-2023

group.colors <- c("p-value > 0.1" = "black", "p-value < 0.05" = "red", "p-value < 0.1" ="darkred")


ggplot(data = summary_cfs, mapping = aes(x = Model, y = Mean_Coeff, color = P.value)) +
  theme_bw()+
  geom_pointrange(mapping = aes(ymin = coeff_min, ymax = coeff_max, color=P.value))+
  facet_grid(Trade_Year~Prediction_rename)+ theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  ylab('Coefficient of Constructed \n Openness in Stage 2')+xlab('Model')+
  geom_hline(yintercept=0, linetype='dotted', col = 'black')+
  scale_color_manual(values=group.colors)+theme(legend.position="bottom")+
  guides(color=guide_legend(title="Significance level"))


ggsave('CFS_results_summary_irrigation_water.png', plot = last_plot(),
       width=7, height = 5,
       units = 'in')
ggsave('CFS_results_summary_irrigation_water.jpg', plot = last_plot(),
       width=7, height = 5,
       units = 'in')
ggsave('CFS_results_summary_irrigation_water.pdf', plot = last_plot(),
       width=7, height = 5,
       units = 'in')

plt2<-ggplot(data = summary_cfs, mapping = aes(x = Model, y = Mean_Coeff, color = P.value)) +
  theme_bw()+
  geom_pointrange(mapping = aes(ymin = coeff_min, ymax = coeff_max, color=P.value))+
  facet_grid(Trade_Year~Prediction_rename)+ theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  ylab('Coefficient of Constructed \n Openness in Stage 2')+xlab('Model')+
  geom_hline(yintercept=0, linetype='dotted', col = 'black')+
  scale_color_manual(values=group.colors)+theme(legend.position="bottom")+
  guides(color=guide_legend(title="Significance level"))+
  labs(caption = "(b) Instrumental Variable Stage 2 for Irrigation Water Withdrawals")+
  theme(plot.caption = element_text(hjust = 0.5, size=10))

leg <- get_legend(plt2)
plt2 <- plt2 + theme(legend.position = "none")


combined_plot <- ggarrange(
  plt1, plt2,
  leg,
  ncol = 1, # One column (vertical arrangement)
  nrow = 3 , # Two rows
  heights = c(1, 1, 0.2), # Optional: Adjust relative heights if needed
  widths = c(1, 1, 1)
)

combined_plot

ggsave('CFS_results_summary_irrigation_area_water_combined.png', plot = last_plot(),
       width=7, height = 8.5,
       units = 'in')
ggsave('CFS_results_summary_irrigation_area_water_combined.jpg', plot = last_plot(),
       width=7, height = 8.5,
       units = 'in')
ggsave('CFS_results_summary_irrigation_area_water_combined.pdf', plot = last_plot(),
       width=7, height = 8.5,
       units = 'in')


##### Irrigated area by source

summary_cfs <- read.csv('Irr_area_IV_2_csv_next_yr_source.csv')

summary_cfs$Prediction_rename <- summary_cfs$Prediction
#summary_cfs$Prediction_rename[which(summary_cfs$Prediction=="Subsequent Year Irrigation Area")] <- "Subsequent Year\nIrrigated Area"
#summary_cfs$Prediction_rename[which(summary_cfs$Prediction=="Subsequent Year Irrigation Area GW")] <- "Subsequent Year\nIrrigated Area\nGW"
#summary_cfs$Prediction_rename[which(summary_cfs$Prediction=="Subsequent Year Irrigation Area SW")] <- "Subsequent Year\nIrrigated Area\nSW"

summary_cfs$Prediction_rename[which(summary_cfs$Prediction=="Subsequent Year Irrigation Area")] <- "Total\nIrrigated Area"
summary_cfs$Prediction_rename[which(summary_cfs$Prediction=="Subsequent Year Irrigation Area GW")] <- "Groundwater\nIrrigated Area"
summary_cfs$Prediction_rename[which(summary_cfs$Prediction=="Subsequent Year Irrigation Area SW")] <- "Surface water\nIrrigated Area"

summary_cfs$Trade_Year[which(summary_cfs$Trade_Year==2012)]<-2013
summary_cfs$Trade_Year[which(summary_cfs$Trade_Year==2017)]<-2018
summary_cfs$Trade_Year[which(summary_cfs$Trade_Year==2022)]<-2023

summary_cfs <- summary_cfs[which(summary_cfs$Model!="M5" & summary_cfs$Model!="M6"),]
# summary_cfs <- summary_cfs[which(summary_cfs$Model!="M6"),]

group.colors <- c("p-value > 0.1" = "black", "p-value < 0.05" = "red", "p-value < 0.1" ="darkred")


ggplot(data = summary_cfs, mapping = aes(x = Model, y = Mean_Coeff, color = P.value)) +
  theme_bw()+
  geom_pointrange(mapping = aes(ymin = coeff_min, ymax = coeff_max, color=P.value))+
  facet_grid(Trade_Year~Prediction_rename)+ theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  ylab('Coefficient of Constructed \n Openness in Stage 2')+xlab('Model')+
  geom_hline(yintercept=0, linetype='dotted', col = 'black')+
  scale_color_manual(values=group.colors)+theme(legend.position="bottom")+
  guides(color=guide_legend(title="Significance level"))


ggsave('CFS_results_summary_irrigated_area_source.png', plot = last_plot(),
       width=7, height = 5,
       units = 'in')
ggsave('CFS_results_summary_irrigated_area_source.jpg', plot = last_plot(),
       width=7, height = 5,
       units = 'in')
ggsave('CFS_results_summary_irrigated_area_source.pdf', plot = last_plot(),
       width=7, height = 5,
       units = 'in')

plt3<-ggplot(data = summary_cfs, mapping = aes(x = Model, y = Mean_Coeff, color = P.value)) +
  theme_bw()+
  geom_pointrange(mapping = aes(ymin = coeff_min, ymax = coeff_max, color=P.value))+
  facet_grid(Trade_Year~Prediction_rename)+ theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  ylab('Coefficient of Constructed \n Openness in Stage 2')+xlab('Model')+
  geom_hline(yintercept=0, linetype='dotted', col = 'black')+
  scale_color_manual(values=group.colors)+theme(legend.position="bottom")+
  guides(color=guide_legend(title="Significance level"))+
  labs(caption = "(a) Instrumental Variable Stage 2 for Irrigated Area")+
  theme(plot.caption = element_text(hjust = 0.5, size=10))

leg <- get_legend(plt3)
plt3 <- plt3 + theme(legend.position = "none")



combined_plot <- ggarrange(
  plt3, plt2,
  leg,
  ncol = 1, # One column (vertical arrangement)
  nrow = 3 , # Two rows
  heights = c(1, 1, 0.2), # Optional: Adjust relative heights if needed
  widths = c(1, 1, 1)
)

combined_plot

ggsave('CFS_results_summary_irrigation_source_area_water_combined.png', plot = last_plot(),
       width=7, height = 8.5,
       units = 'in')
ggsave('CFS_results_summary_irrigation_source_area_water_combined.jpg', plot = last_plot(),
       width=7, height = 8.5,
       units = 'in')
ggsave('CFS_results_summary_irrigation_source_area_water_combined.pdf', plot = last_plot(),
       width=7, height = 8.5,
       units = 'in')
