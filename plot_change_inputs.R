library(ggplot2)
library(ggpubr)

#### symlog
symlog_trans <- function(base = 10, thr = 1, scale = 1){
  trans <- function(x)
    ifelse(abs(x) < thr, x, sign(x) * 
             (thr + scale * suppressWarnings(log(sign(x) * x / thr, base))))
  
  inv <- function(x)
    ifelse(abs(x) < thr, x, sign(x) * 
             base^((sign(x) * x - thr) / scale) * thr)
  
  breaks <- function(x){
    sgn <- sign(x[which.max(abs(x))])
    if(all(abs(x) < thr))
      pretty_breaks()(x)
    else if(prod(x) >= 0){
      if(min(abs(x)) < thr)
        sgn * unique(c(pretty_breaks()(c(min(abs(x)), thr)),
                       log_breaks(base)(c(max(abs(x)), thr))))
      else
        sgn * log_breaks(base)(sgn * x)
    } else {
      if(min(abs(x)) < thr)
        unique(c(sgn * log_breaks()(c(max(abs(x)), thr)),
                 pretty_breaks()(c(sgn * thr, x[which.min(abs(x))]))))
      else
        unique(c(-log_breaks(base)(c(thr, -x[1])),
                 pretty_breaks()(c(-thr, thr)),
                 log_breaks(base)(c(thr, x[2]))))
    }
  }
  trans_new(paste("symlog", thr, base, scale, sep = "-"), trans, inv, breaks)
}

df_og<-read.csv(".\\Analysis_2012\\R_openness.csv")
df_2017<-read.csv(".\\Analysis_2017\\R_openness.csv")
df_2022<-read.csv(".\\Analysis_2022\\R_openness.csv")

col_exp <- c("Income_next_yr", "Population_next_yr",
             "Tot_Employment_next_yr", "Agri_Employment_next_yr",
             "Precipitation_next_yr", "Temperature_next_yr",
             "next_yr_Irrigated_area",
             "next_yr_Irrigated_water", "next_yr_Irrigated_water_gw",
             "next_yr_Irrigated_water_sw")

df_og[,col_exp]<-exp(df_og[,col_exp])
df_2017[,col_exp]<-exp(df_2017[,col_exp])
df_2022[,col_exp]<-exp(df_2022[,col_exp])

df_change_2017 <- (df_2017 - df_og)*100/df_og
df_change_2022 <- (df_2022 - df_og)*100/df_og

col_socioeco_names <- c("Income_next_yr", "Population_next_yr",
               "Tot_Employment_next_yr", "Agri_Employment_next_yr",
               "Precipitation_next_yr", "Temperature_next_yr")


df_long_2017 <- df_change_2017[,col_socioeco_names] %>%
  pivot_longer(cols = col_socioeco_names, names_to = "variable", values_to = "value")

df_long_2017$variable[which(df_long_2017$variable=="Agri_Employment_next_yr")]="Primary sector\nEmployment"
df_long_2017$variable[which(df_long_2017$variable=="Income_next_yr")]="Income"
df_long_2017$variable[which(df_long_2017$variable=="Population_next_yr")]="Population"
df_long_2017$variable[which(df_long_2017$variable=="Precipitation_next_yr")]="Precipitation"
df_long_2017$variable[which(df_long_2017$variable=="Temperature_next_yr")]="Temperature"
df_long_2017$variable[which(df_long_2017$variable=="Tot_Employment_next_yr")]="Total Employment"


plt1<-ggplot(df_long_2017, aes(x = variable, y = value)) +
  geom_boxplot(fill = "white", color = "black") +
  theme_bw() +
  labs(x = "Variable",
       y = "Percent Change (%)")+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))+
  labs(caption = "(a) Percent change in 2018 compared to 2013")+
  theme(plot.caption = element_text(hjust = 0.5, size=10))+
  scale_y_continuous(trans = symlog_trans(base = 10, thr = 1, scale = 1),
                     breaks = c(-500, -50, 0, 50, 2000))

df_long_2022 <- df_change_2022[, col_socioeco_names] %>%
  pivot_longer(cols = col_socioeco_names, names_to = "variable", values_to = "value")

df_long_2022$variable[which(df_long_2022$variable=="Agri_Employment_next_yr")]="Primary sector\nEmployment"
df_long_2022$variable[which(df_long_2022$variable=="Income_next_yr")]="Income"
df_long_2022$variable[which(df_long_2022$variable=="Population_next_yr")]="Population"
df_long_2022$variable[which(df_long_2022$variable=="Precipitation_next_yr")]="Precipitation"
df_long_2022$variable[which(df_long_2022$variable=="Temperature_next_yr")]="Temperature"
df_long_2022$variable[which(df_long_2022$variable=="Tot_Employment_next_yr")]="Total Employment"

plt2<-ggplot(df_long_2022, aes(x = variable, y = value)) +
  geom_boxplot(fill = "white", color = "black") +
  theme_bw() +
  labs(x = "Variable",
       y = "Percent Change (%)")+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))+
  labs(caption = "(b) Percent change in 2023 compared to 2013")+
  theme(plot.caption = element_text(hjust = 0.5, size=10))+
  scale_y_continuous(trans = symlog_trans(base = 10, thr = 1, scale = 1),
                     breaks = c(-500, -50, 0, 50, 2000))

combined_plot <- ggarrange(plt1, plt2, nrow=2)+bgcolor("white")

ggsave(".\\percent_changes\\socioeco_climate_change.png",
       plot = combined_plot, units = "in",
       height = 5.5, width=7)


##############

col_irr_names <- c("next_yr_Irrigated_area", "next_yr_Irrigated_area_gw",
                   "next_yr_Irrigated_area_sw",
                   "next_yr_Irrigated_water", "next_yr_Irrigated_water_gw",
                   "next_yr_Irrigated_water_sw",
                   "next_yr_gravity_percent",
                   "next_yr_sprinkler_percent",
                   "next_yr_drip_percent")

df_long_2017 <- df_change_2017 %>%
  pivot_longer(cols = col_irr_names, names_to = "variable", values_to = "value")

df_long_2017$variable[which(df_long_2017$variable=="next_yr_Irrigated_area")]="Irrigated\narea"
df_long_2017$variable[which(df_long_2017$variable=="next_yr_Irrigated_area_gw")]="Irrigated\narea-GW"
df_long_2017$variable[which(df_long_2017$variable=="next_yr_Irrigated_area_sw")]="Irrigated\narea-SW"
df_long_2017$variable[which(df_long_2017$variable=="next_yr_Irrigated_water")]="Irrigation water\nwithdrawals"
df_long_2017$variable[which(df_long_2017$variable=="next_yr_Irrigated_water_gw")]="Irrigation water\nwithdrawals-GW"
df_long_2017$variable[which(df_long_2017$variable=="next_yr_Irrigated_water_sw")]="Irrigation water\nwithdrawals-SW"
df_long_2017$variable[which(df_long_2017$variable=="next_yr_gravity_percent")]="% area with\ngravity irrigation"
df_long_2017$variable[which(df_long_2017$variable=="next_yr_sprinkler_percent")]="% area with\nsprinkler irrigation"
df_long_2017$variable[which(df_long_2017$variable=="next_yr_drip_percent")]="% area with\ndrip irrigation"


plt1<-ggplot(df_long_2017, aes(x = variable, y = value)) +
  geom_boxplot(fill = "white", color = "black") +
  theme_bw() +
  labs(x = "Variable",
       y = "Percent Change (%)")+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))+
  labs(caption = "(a) Percent change in 2018 compared to 2013")+
  theme(plot.caption = element_text(hjust = 0.5, size=10))+
  scale_y_continuous(trans = symlog_trans(base = 10, thr = 1, scale = 1),
                     breaks = c(-500, -50, 0, 50, 2000))

df_long_2022 <- df_change_2022 %>%
  pivot_longer(cols = col_irr_names, names_to = "variable", values_to = "value")

df_long_2022$variable[which(df_long_2022$variable=="next_yr_Irrigated_area")]="Irrigated\narea"
df_long_2022$variable[which(df_long_2022$variable=="next_yr_Irrigated_area_gw")]="Irrigated\narea-GW"
df_long_2022$variable[which(df_long_2022$variable=="next_yr_Irrigated_area_sw")]="Irrigated\narea-SW"
df_long_2022$variable[which(df_long_2022$variable=="next_yr_Irrigated_water")]="Irrigation water\nwithdrawals"
df_long_2022$variable[which(df_long_2022$variable=="next_yr_Irrigated_water_gw")]="Irrigation water\nwithdrawals-GW"
df_long_2022$variable[which(df_long_2022$variable=="next_yr_Irrigated_water_sw")]="Irrigation water\nwithdrawals-SW"
df_long_2022$variable[which(df_long_2022$variable=="next_yr_gravity_percent")]="% area with\ngravity irrigation"
df_long_2022$variable[which(df_long_2022$variable=="next_yr_sprinkler_percent")]="% area with\nsprinkler irrigation"
df_long_2022$variable[which(df_long_2022$variable=="next_yr_drip_percent")]="% area with\ndrip irrigation"

plt2<-ggplot(df_long_2022, aes(x = variable, y = value)) +
  geom_boxplot(fill = "white", color = "black") +
  theme_bw() +
  labs(x = "Variable",
       y = "Percent Change (%)")+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))+
  labs(caption = "(b) Percent change in 2023 compared to 2013")+
  theme(plot.caption = element_text(hjust = 0.5, size=10))+
  scale_y_continuous(trans = symlog_trans(base = 10, thr = 1, scale = 1),
                     breaks = c(-500, -50, 0, 50, 2000))

combined_plot <- ggarrange(plt1, plt2, nrow=2)+bgcolor("white")

ggsave(".\\percent_changes\\irr_change.png",
       plot = combined_plot, units = "in",
       height = 5.5, width=7)
