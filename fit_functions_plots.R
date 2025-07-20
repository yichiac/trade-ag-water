library(ggplot2)
library(ggpubr)
library(MASS)

save_folder = '.\\Analysis_2012\\'

read_data <- read.csv(paste(save_folder,'instrument_training.csv',
                                     sep=""))

d <- data.frame(as.list(1:nrow(read_data)))  # simple example construction
idx_d <- which(d%%2==1)
idx_no_self <- which(read_data[, 1]!=read_data[, 2])

subset_data<- read_data[intersect(idx_d, idx_no_self), ]
# subset_data<- read_data

self_just_once <- intersect(which(d%%2==1), which(read_data[, 1]==read_data[, 2]))
subset_data_2 <- read_data[self_just_once,]

subset_data_3 <- rbind(subset_data, subset_data_2)

column_names<- c('Exporter', 'Importer', 'Shipments', 'Distance', 
                 'Area_exporter', 'Area_importer', 
                 'Population_exporter', 'Population_importer', 
                 'GDP_exporter', 'GDP_importer', 
                 'Intrastate_or_not', 'Share_Border', 
                 'BorderxArea_exporter', 'BorderxArea_importer',
                 'BorderxPop_exporter', 'BorderxPop_importer', 
                 'BorderxDist', 'sum_LL',
                 'Borderxsum_LL', 'Long_ex', 'Lat_ex', 'Long_im', 
                 'Lat_im', 'Is_DC_involved', 'LL_ex', 'LL_im',
                 'num_FAF_ex', 'num_FAF_im', 'remoteness_ex', 'remoteness_im')



take_log_columns = c('Shipments', 'Distance', 
                     'Area_exporter', 'Area_importer', 
                     'Population_exporter', 'Population_importer', 
                     'GDP_exporter', 'GDP_importer',
                     'remoteness_ex', 'remoteness_im')

subset_data_3[,take_log_columns] <- log(subset_data_3[,take_log_columns])

subset_data_3$Interstate_or_not <- 1-subset_data_3$Intrastate_or_not

subset_data_3$num_FAF_involved <- subset_data_3$num_FAF_ex+subset_data_3$num_FAF_im


row.names(subset_data_3) <- NULL

subset_data_3$Is_importer_north_rel <- pmax(0,sign(subset_data_3$Lat_im-subset_data_3$Lat_ex))
subset_data_3$Is_importer_east_rel <- pmax(0,sign(subset_data_3$Long_im-subset_data_3$Long_ex))

subset_data_3$Share_Border[which(subset_data_3$Exporter==subset_data_3$Importer)]<-1

subset_data_3$inter_B <- subset_data_3$Share_Border*subset_data_3$Interstate_or_not
subset_data_3$inter_D <- subset_data_3$Distance*subset_data_3$Interstate_or_not

subset_data_3$inter_sum_LL <-subset_data_3$Interstate_or_not*subset_data_3$sum_LL
subset_data_3$inter_num_FAF_involved <-subset_data_3$Interstate_or_not*subset_data_3$num_FAF_involved

subset_data_3$inter_A_im <-subset_data_3$Interstate_or_not*subset_data_3$Area_importer
subset_data_3$inter_P_im <-subset_data_3$Interstate_or_not*subset_data_3$Population_importer

subset_data_3$inter_is_im_n <-subset_data_3$Interstate_or_not*subset_data_3$Is_importer_north_rel
subset_data_3$inter_is_im_e <-subset_data_3$Interstate_or_not*subset_data_3$Is_importer_east_rel

lm_instrument <- glmmPQL(Shipments~
                           Interstate_or_not+
                           inter_D+
                           Area_importer+
                           Population_importer+
                           sum_LL+
                           num_FAF_involved+
                           inter_is_im_n+inter_is_im_e+
                           inter_A_im+inter_P_im+
                           inter_sum_LL+inter_num_FAF_involved+
                           inter_B
                         ,data = subset_data_3, family = gaussian, random=~ 1 | SCTG)


lm_instrument$fitted.values <- predict(lm_instrument,data = subset_data_3)

subset_data_3$Predicted.Shipments <- lm_instrument$fitted.values

plot(subset_data_3$Shipments, lm_instrument$fitted.values,
     xlab="Real Shipments", ylab="Predicted Shipments")
abline(0, 1, col = "red", lwd = 2)  # Intercept = 0, slope = 1

plot1<-ggplot(subset_data_3, aes(x = Shipments, y = Predicted.Shipments)) +
  theme_bw()+
  geom_point() +  # Scatter plot
  geom_abline(intercept = 0, slope = 1, color = "red", linewidth = 1) +  # Line with slope = 1, intercept = 0
  labs(x = "Real shipments", y = "Predicted shipments")+
  xlim(0, 13)+ylim(0, 15)+
  labs(caption = "(a) Predicted shipments vs \nactual shipments in 2012")+
  theme(plot.caption = element_text(hjust = 0.5, size=10))


read_data_1 <- read.csv(paste(save_folder,'IV_training.csv',
                              sep=""))



plot(read_data_1$Real_Openness, read_data_1$Constructed_Openness,
     xlab="Real Trade Openness", ylab = "Constructed Trade Openness")
abline(0, 1, col = "red", lwd = 2)  # Intercept = 0, slope = 1

plot2<-ggplot(read_data_1, aes(x = Real_Openness, y = Constructed_Openness)) +
  theme_bw()+
  geom_point() +  # Scatter plot
  geom_abline(intercept = 0, slope = 1, color = "red", linewidth = 1) +  # Line with slope = 1, intercept = 0
  labs(x = "Real trade openness", y = "Constructed trade openness")+
  xlim(0, 2.5)+ylim(0, 3)+
  labs(caption = "(b) Constructed trade openness vs \nreal trade openness for 2012")+
  theme(plot.caption = element_text(hjust = 0.5, size=10))

#######################
#######################

save_folder = '.\\Analysis_2017\\'

read_data <- read.csv(paste(save_folder,'instrument_training.csv',
                            sep=""))

d <- data.frame(as.list(1:nrow(read_data)))  # simple example construction
idx_d <- which(d%%2==1)
idx_no_self <- which(read_data[, 1]!=read_data[, 2])

subset_data<- read_data[intersect(idx_d, idx_no_self), ]
# subset_data<- read_data

self_just_once <- intersect(which(d%%2==1), which(read_data[, 1]==read_data[, 2]))
subset_data_2 <- read_data[self_just_once,]

subset_data_3 <- rbind(subset_data, subset_data_2)

column_names<- c('Exporter', 'Importer', 'Shipments', 'Distance', 
                 'Area_exporter', 'Area_importer', 
                 'Population_exporter', 'Population_importer', 
                 'GDP_exporter', 'GDP_importer', 
                 'Intrastate_or_not', 'Share_Border', 
                 'BorderxArea_exporter', 'BorderxArea_importer',
                 'BorderxPop_exporter', 'BorderxPop_importer', 
                 'BorderxDist', 'sum_LL',
                 'Borderxsum_LL', 'Long_ex', 'Lat_ex', 'Long_im', 
                 'Lat_im', 'Is_DC_involved', 'LL_ex', 'LL_im',
                 'num_FAF_ex', 'num_FAF_im', 'remoteness_ex', 'remoteness_im')



take_log_columns = c('Shipments', 'Distance', 
                     'Area_exporter', 'Area_importer', 
                     'Population_exporter', 'Population_importer', 
                     'GDP_exporter', 'GDP_importer',
                     'remoteness_ex', 'remoteness_im')

subset_data_3[,take_log_columns] <- log(subset_data_3[,take_log_columns])

subset_data_3$Interstate_or_not <- 1-subset_data_3$Intrastate_or_not

subset_data_3$num_FAF_involved <- subset_data_3$num_FAF_ex+subset_data_3$num_FAF_im


row.names(subset_data_3) <- NULL

subset_data_3$Is_importer_north_rel <- pmax(0,sign(subset_data_3$Lat_im-subset_data_3$Lat_ex))
subset_data_3$Is_importer_east_rel <- pmax(0,sign(subset_data_3$Long_im-subset_data_3$Long_ex))

subset_data_3$Share_Border[which(subset_data_3$Exporter==subset_data_3$Importer)]<-1

subset_data_3$inter_B <- subset_data_3$Share_Border*subset_data_3$Interstate_or_not
subset_data_3$inter_D <- subset_data_3$Distance*subset_data_3$Interstate_or_not

subset_data_3$inter_sum_LL <-subset_data_3$Interstate_or_not*subset_data_3$sum_LL
subset_data_3$inter_num_FAF_involved <-subset_data_3$Interstate_or_not*subset_data_3$num_FAF_involved

subset_data_3$inter_A_im <-subset_data_3$Interstate_or_not*subset_data_3$Area_importer
subset_data_3$inter_P_im <-subset_data_3$Interstate_or_not*subset_data_3$Population_importer

subset_data_3$inter_is_im_n <-subset_data_3$Interstate_or_not*subset_data_3$Is_importer_north_rel
subset_data_3$inter_is_im_e <-subset_data_3$Interstate_or_not*subset_data_3$Is_importer_east_rel

lm_instrument <- glmmPQL(Shipments~
                           Interstate_or_not+
                           inter_D+
                           Area_importer+
                           Population_importer+
                           sum_LL+
                           num_FAF_involved+
                           inter_is_im_n+inter_is_im_e+
                           inter_A_im+inter_P_im+
                           inter_sum_LL+inter_num_FAF_involved+
                           inter_B
                         ,data = subset_data_3, family = gaussian, random=~ 1 | SCTG)


lm_instrument$fitted.values <- predict(lm_instrument,data = subset_data_3)

subset_data_3$Predicted.Shipments <- lm_instrument$fitted.values

plot(subset_data_3$Shipments, lm_instrument$fitted.values,
     xlab="Real Shipments", ylab="Predicted Shipments")
abline(0, 1, col = "red", lwd = 2)  # Intercept = 0, slope = 1

plot3<-ggplot(subset_data_3, aes(x = Shipments, y = Predicted.Shipments)) +
  theme_bw()+
  geom_point() +  # Scatter plot
  geom_abline(intercept = 0, slope = 1, color = "red", linewidth = 1) +  # Line with slope = 1, intercept = 0
  labs(x = "Real shipments", y = "Predicted shipments")+
  xlim(0, 13)+ylim(0, 15)+
  labs(caption = "(c) Predicted shipments vs \nactual shipments in 2017")+
  theme(plot.caption = element_text(hjust = 0.5, size=10))

read_data_1 <- read.csv(paste(save_folder,'IV_training.csv',
                              sep=""))




plot(read_data_1$Real_Openness, read_data_1$Constructed_Openness,
     xlab="Real Trade Openness", ylab = "Constructed Trade Openness")
abline(0, 1, col = "red", lwd = 2)  # Intercept = 0, slope = 1

plot4<-ggplot(read_data_1, aes(x = Real_Openness, y = Constructed_Openness)) +
  theme_bw()+
  geom_point() +  # Scatter plot
  geom_abline(intercept = 0, slope = 1, color = "red", linewidth = 1) +  # Line with slope = 1, intercept = 0
  labs(x = "Real trade openness", y = "Constructed trade openness")+
  xlim(0, 2.5)+ylim(0, 3)+
  labs(caption = "(d) Constructed trade openness vs \nreal trade openness for 2017")+
  theme(plot.caption = element_text(hjust = 0.5, size=10))

#######################
#######################
### 2022
save_folder = '.\\Analysis_2022\\'

read_data <- read.csv(paste(save_folder,'instrument_training.csv',
                            sep=""))

d <- data.frame(as.list(1:nrow(read_data)))  # simple example construction
idx_d <- which(d%%2==1)
idx_no_self <- which(read_data[, 1]!=read_data[, 2])

subset_data<- read_data[intersect(idx_d, idx_no_self), ]
# subset_data<- read_data

self_just_once <- intersect(which(d%%2==1), which(read_data[, 1]==read_data[, 2]))
subset_data_2 <- read_data[self_just_once,]

subset_data_3 <- rbind(subset_data, subset_data_2)

column_names<- c('Exporter', 'Importer', 'Shipments', 'Distance', 
                 'Area_exporter', 'Area_importer', 
                 'Population_exporter', 'Population_importer', 
                 'GDP_exporter', 'GDP_importer', 
                 'Intrastate_or_not', 'Share_Border', 
                 'BorderxArea_exporter', 'BorderxArea_importer',
                 'BorderxPop_exporter', 'BorderxPop_importer', 
                 'BorderxDist', 'sum_LL',
                 'Borderxsum_LL', 'Long_ex', 'Lat_ex', 'Long_im', 
                 'Lat_im', 'Is_DC_involved', 'LL_ex', 'LL_im',
                 'num_FAF_ex', 'num_FAF_im', 'remoteness_ex', 'remoteness_im')



take_log_columns = c('Shipments', 'Distance', 
                     'Area_exporter', 'Area_importer', 
                     'Population_exporter', 'Population_importer', 
                     'GDP_exporter', 'GDP_importer',
                     'remoteness_ex', 'remoteness_im')

subset_data_3[,take_log_columns] <- log(subset_data_3[,take_log_columns])

subset_data_3$Interstate_or_not <- 1-subset_data_3$Intrastate_or_not

subset_data_3$num_FAF_involved <- subset_data_3$num_FAF_ex+subset_data_3$num_FAF_im


row.names(subset_data_3) <- NULL

subset_data_3$Is_importer_north_rel <- pmax(0,sign(subset_data_3$Lat_im-subset_data_3$Lat_ex))
subset_data_3$Is_importer_east_rel <- pmax(0,sign(subset_data_3$Long_im-subset_data_3$Long_ex))

subset_data_3$Share_Border[which(subset_data_3$Exporter==subset_data_3$Importer)]<-1

subset_data_3$inter_B <- subset_data_3$Share_Border*subset_data_3$Interstate_or_not
subset_data_3$inter_D <- subset_data_3$Distance*subset_data_3$Interstate_or_not

subset_data_3$inter_sum_LL <-subset_data_3$Interstate_or_not*subset_data_3$sum_LL
subset_data_3$inter_num_FAF_involved <-subset_data_3$Interstate_or_not*subset_data_3$num_FAF_involved

subset_data_3$inter_A_im <-subset_data_3$Interstate_or_not*subset_data_3$Area_importer
subset_data_3$inter_P_im <-subset_data_3$Interstate_or_not*subset_data_3$Population_importer

subset_data_3$inter_is_im_n <-subset_data_3$Interstate_or_not*subset_data_3$Is_importer_north_rel
subset_data_3$inter_is_im_e <-subset_data_3$Interstate_or_not*subset_data_3$Is_importer_east_rel

lm_instrument <- glmmPQL(Shipments~
                           Interstate_or_not+
                           inter_D+
                           Area_importer+
                           Population_importer+
                           sum_LL+
                           num_FAF_involved+
                           inter_is_im_n+inter_is_im_e+
                           inter_A_im+inter_P_im+
                           inter_sum_LL+inter_num_FAF_involved+
                           inter_B
                         ,data = subset_data_3, family = gaussian, random=~ 1 | SCTG)


lm_instrument$fitted.values <- predict(lm_instrument,data = subset_data_3)

subset_data_3$Predicted.Shipments <- lm_instrument$fitted.values

plot(subset_data_3$Shipments, lm_instrument$fitted.values,
     xlab="Real Shipments", ylab="Predicted Shipments")
abline(0, 1, col = "red", lwd = 2)  # Intercept = 0, slope = 1

plot5<-ggplot(subset_data_3, aes(x = Shipments, y = Predicted.Shipments)) +
  theme_bw()+
  geom_point() +  # Scatter plot
  geom_abline(intercept = 0, slope = 1, color = "red", linewidth = 1) +  # Line with slope = 1, intercept = 0
  labs(x = "Real shipments", y = "Predicted shipments")+
  xlim(0, 13)+ylim(0, 15)+
  labs(caption = "(e) Predicted shipments vs \nactual shipments in 2022")+
  theme(plot.caption = element_text(hjust = 0.5, size=10))

read_data_1 <- read.csv(paste(save_folder,'IV_training.csv',
                              sep=""))


read_data_1$Real_Openness <- log10(read_data_1$Real_Openness+1)
read_data_1$Constructed_Openness <- log10(read_data_1$Constructed_Openness+1)

plot(read_data_1$Real_Openness, read_data_1$Constructed_Openness,
     xlab="Real Trade Openness", ylab = "Constructed Trade Openness")
abline(0, 1, col = "red", lwd = 2)  # Intercept = 0, slope = 1

plot6<-ggplot(read_data_1, aes(x = Real_Openness, y = Constructed_Openness)) +
  theme_bw()+
  geom_point() +  # Scatter plot
  geom_abline(intercept = 0, slope = 1, color = "red", linewidth = 1) +  # Line with slope = 1, intercept = 0
  labs(x = "Real trade openness", y = "Constructed trade openness")+
  xlim(0, 2.5)+ylim(0, 3)+
  labs(caption = "(f) Constructed trade openness vs \nreal trade openness for 2022")+
  theme(plot.caption = element_text(hjust = 0.5, size=10))

#################

combined_plot <- ggarrange(
  plot1, plot2, plot3, plot4, plot5, plot6,
  ncol = 2, # One column (vertical arrangement)
  nrow = 3 , # Two rows
  heights = c(1, 1, 1, 1, 1, 1) # Optional: Adjust relative heights if needed
)+ bgcolor("white")

ggsave('R_combined_plots.png', plot = last_plot(),
       width=7, height = 8,
       units = 'in')
ggsave('R_combined_plots.pdf', plot = last_plot(),
       width=7, height = 8,
       units = 'in')
ggsave('R_combined_plots.jpg', plot = last_plot(),
       width=7, height = 8,
       units = 'in')


plot2<-plot2+
  labs(caption = "(a) Constructed trade openness vs \nreal trade openness for 2012")+
  theme(plot.caption = element_text(hjust = 0.5, size=10))

plot4<-plot4+
  labs(caption = "(b) Constructed trade openness vs \nreal trade openness for 2017")+
  theme(plot.caption = element_text(hjust = 0.5, size=10))

plot6<-plot6+
  labs(caption = "(c) Constructed trade openness vs \nreal trade openness for 2022")+
  theme(plot.caption = element_text(hjust = 0.5, size=10))

combined_plot <- ggarrange(
  plot2, plot4, plot6,
  ncol = 2, # One column (vertical arrangement)
  nrow = 2 , # Two rows
  heights = c(1, 1, 1) # Optional: Adjust relative heights if needed
)+ bgcolor("white")

ggsave('R_combined_plots_to.png', plot = last_plot(),
       width=7, height = 5.5, bg="white",
       units = 'in')
ggsave('R_combined_plots_to.pdf', plot = last_plot(),
       width=7, height = 5.5, bg="white",
       units = 'in')
ggsave('R_combined_plots_to.jpg', plot = last_plot(),
       width=7, height = 5.5, bg="white",
       units = 'in')
