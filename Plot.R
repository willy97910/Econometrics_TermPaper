

# Population Plot

# 桃園縣市
subset(Population, COUNTY_ID %in%c("68000", "10003")) %>%
  group_by(INFO_TIME) %>%
  summarize(Total_PNT = sum(P_CNT),
            Time = as.character(str_to_d(INFO_TIME))) %>%
  ggplot(aes(x = Time, y = Total_PNT, group = 1, col = Total_PNT)) +
  geom_line(size=2, position = "identity", na.rm = TRUE) +
  geom_point(position = "identity", na.rm = TRUE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("") + ylab("")

# 台中市 + 縣
subset(Population, COUNTY %in% c("臺中市", "臺中縣")) %>%
  group_by(INFO_TIME) %>%
  summarize(Total_PNT = sum(P_CNT),
            Time = as.character(str_to_d(INFO_TIME))) %>%
  ggplot(aes(x = Time, y = Total_PNT, group = 1, col = Total_PNT)) +
  geom_line(size=2, position = "identity", na.rm = TRUE) +
  geom_point(position = "identity", na.rm = TRUE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("") + ylab("")

# 台南市 + 縣
subset(Population, COUNTY_ID %in% c("10021", "10011", "67000")) %>%
  group_by(INFO_TIME) %>%
  summarize(Total_PNT = sum(P_CNT),
            Time = as.character(str_to_d(INFO_TIME))) %>%
  ggplot(aes(x = Time, y = Total_PNT, group = 1, col = Total_PNT)) +
  geom_line(size=2, position = "identity", na.rm = TRUE) +
  geom_point(position = "identity", na.rm = TRUE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("") + ylab("")

# 苗栗縣
subset(Population, COUNTY %in% c("苗栗縣")) %>%
  group_by(INFO_TIME) %>%
  summarize(Total_PNT = sum(P_CNT),
            Time = as.character(str_to_d(INFO_TIME))) %>%
  ggplot(aes(x = Time, y = Total_PNT, group = 1, col = Total_PNT)) +
  geom_line(size=2, position = "identity", na.rm = TRUE) +
  geom_point(position = "identity", na.rm = TRUE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("") + ylab("")

# 彰化縣
subset(Population, COUNTY %in% c("彰化縣")) %>%
  group_by(INFO_TIME) %>%
  summarize(Total_PNT = sum(P_CNT),
            Time = as.character(str_to_d(INFO_TIME))) %>%
  ggplot(aes(x = Time, y = Total_PNT, group = 1, col = Total_PNT)) +
  geom_line(size=2, position = "identity", na.rm = TRUE) +
  geom_point(position = "identity", na.rm = TRUE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("") + ylab("")

#雲林縣
subset(Population, COUNTY %in% c("雲林縣")) %>%
  group_by(INFO_TIME) %>%
  summarize(Total_PNT = sum(P_CNT),
            Time = as.character(str_to_d(INFO_TIME))) %>%
  ggplot(aes(x = Time, y = Total_PNT, group = 1, col = Total_PNT)) +
  geom_line(size=2, position = "identity", na.rm = TRUE) +
  geom_point(position = "identity", na.rm = TRUE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("") + ylab("")
# 嘉義市 + 縣
subset(Population, COUNTY %in% c("嘉義市", "嘉義縣")) %>%
  group_by(INFO_TIME) %>%
  summarize(Total_PNT = sum(P_CNT),
            Time = as.character(str_to_d(INFO_TIME))) %>%
  ggplot(aes(x = Time, y = Total_PNT, group = 1, col = Total_PNT)) +
  geom_line(size=2, position = "identity", na.rm = TRUE) +
  geom_point(position = "identity", na.rm = TRUE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("") + ylab("")
# 屏東縣
subset(Population, COUNTY_ID == "10013") %>%
  group_by(INFO_TIME) %>%
  summarize(Total_PNT = sum(P_CNT),
            Time = as.character(str_to_d(INFO_TIME))) %>%
  ggplot(aes(x = Time, y = Total_PNT, group = 1, col = Total_PNT)) +
  geom_line(size=2, position = "identity", na.rm = TRUE) +
  geom_point(position = "identity", na.rm = TRUE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("") + ylab("")


# -------------------------------------------------------------------------------

# 桃園中壢、大園、桃園、平鎮
subset(Population, TOWN_ID %in% c("10003020", "68000020", "10003010",
                                  "68000010", "10003060", "68000060", "10003100", "68000100")) %>%
  group_by(INFO_TIME) %>%
  summarize(Total_PNT = sum(P_CNT),
            Time = as.character(str_to_d(INFO_TIME))) %>%
  ggplot(aes(x = Time, y = (Total_PNT), group = 1, col = Total_PNT)) +
  # ggplot(aes(x = Time, y = (Total_PNT), group = TOWN_ID, col =TOWN_ID)) +
  geom_line(size=2, position = "identity", na.rm = TRUE) +
  geom_point(position = "identity", na.rm = TRUE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("") + ylab("")

# 雲林斗六、斗南、虎尾、土庫、西螺、崙背、褒忠
subset(Population, TOWN_ID %in% c("10009010", "10009020", "10009030", "10009040", "10009050")) %>%
  group_by(INFO_TIME) %>%
  summarize(Total_PNT = sum(P_CNT),
            Time = as.character(str_to_d(INFO_TIME))) %>%
  ggplot(aes(x = Time, y = Total_PNT, group = 1, col = Total_PNT)) +
  geom_line(size=2, position = "identity", na.rm = TRUE) +
  geom_point(position = "identity", na.rm = TRUE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("") + ylab("")




# -------------------------------------------------------------------------------

HousingPrice_Taoyuan_S_norelated$Time <- paste0(plot_tao$TransYear, "_", plot_tao$TransMonth)
plot(log(HousingPrice_Taoyuan_S_norelated$TotalPrice))

plot_tao <- HousingPrice_Taoyuan_S_norelated %>%
  group_by(TransYear, TransMonth, TransQtr) %>%
  summarize(MeanTotalPrice = mean(TotalPrice),
            MedianTotalPrice = median(TotalPrice))
plot_tao$Time = paste0(plot_tao$TransYear, "_", plot_tao$TransMonth)          
ggplot(plot_tao, aes(x = Time, y = MeanTotalPrice, group = 1)) +
  geom_line(size=2, position = "identity", na.rm = TRUE) +
  geom_point(position = "identity", na.rm = TRUE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("") + ylab("")

subset(HousingPrice_Taoyuan_S, TOWN_ID %in% c("10003020", "68000020", "10003010",
                                    "68000010", "10003060", "68000060", "10003100", "68000100")) %>%
  group_by(paste0(TransYear, TransQtr)) %>%
  summarize(MeanUnitPrice = mean(as.numeric(UnitPrice), na.rm = TRUE)) %>%
  ggplot(aes(x = `paste0(TransYear, TransQtr)`, y = log(MeanUnitPrice), group = 1)) +
  geom_point(position = "identity", na.rm = TRUE) +
  geom_line(size=2, position = "identity", na.rm = TRUE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))




subset(HousingPrice_Taoyuan_S, TOWN_ID %in% c("10003020", "68000020", "10003010",
                                    "68000010", "10003060", "68000060", "10003100", "68000100")) %>%
  group_by(paste0(TransYear, TransQtr)) %>%
  summarize(MeanUnitPrice = mean(as.numeric(UnitPrice), na.rm = TRUE)) %>%
  ggplot(aes(x = `paste0(TransYear, TransQtr)`, y = log(MeanUnitPrice), group = 1, col = log(MeanUnitPrice))) +
  geom_point(position = "identity", na.rm = TRUE) +
  geom_line(size=2, position = "identity", na.rm = TRUE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  + xlab("") + ylab("")
subset(HousingPrice_Taoyuan_S, TOWN_ID %in% c("10003020", "68000020", "10003010",
                                              "68000010", "10003060", "68000060", "10003100", "68000100")) %>%
  group_by(paste0(TransYear, TransQtr)) %>%
  summarize(MedianUnitPrice = median(as.numeric(UnitPrice), na.rm = TRUE)) %>%
  ggplot(aes(x = `paste0(TransYear, TransQtr)`, y = (MedianUnitPrice), group = 1, col = (MedianUnitPrice))) +
  geom_point(position = "identity", na.rm = TRUE) +
  geom_line(size=2, position = "identity", na.rm = TRUE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  + xlab("") + ylab("")

subset(HousingPrice_Taoyuan_S, TOWN_ID %in% c("10003020", "68000020", "10003010",
                                              "68000010", "10003060", "68000060", "10003100", "68000100")) %>%
  group_by(paste0(TransYear, TransQtr)) %>%
  summarize(MeanTotalPrice = mean(as.numeric(TotalPrice), na.rm = TRUE)) %>%
  ggplot(aes(x = `paste0(TransYear, TransQtr)`, y = log(MeanTotalPrice), group = 1, col = log(MeanTotalPrice))) +
  geom_point(position = "identity", na.rm = TRUE) +
  geom_line(size=2, position = "identity", na.rm = TRUE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  + xlab("") + ylab("")
subset(HousingPrice_Taoyuan_S, TOWN_ID %in% c("10003020", "68000020", "10003010",
                                              "68000010", "10003060", "68000060", "10003100", "68000100")) %>%
  group_by(paste0(TransYear, TransQtr)) %>%
  summarize(MedianTotalPrice = median(as.numeric(TotalPrice), na.rm = TRUE)) %>%
  ggplot(aes(x = `paste0(TransYear, TransQtr)`, y = (MedianTotalPrice), group = 1, col = (MedianTotalPrice))) +
  geom_point(position = "identity", na.rm = TRUE) +
  geom_line(size=2, position = "identity", na.rm = TRUE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  + xlab("") + ylab("")






subset(HousingPrice_Yunlin_S, TOWN_ID %in% 
         c("10009010", "10009020", "10009030", "10009040", "10009050")) %>%
  group_by(paste0(TransYear, TransQtr)) %>%
  summarize(MedianUnitPrice = median(as.numeric(UnitPrice), na.rm = TRUE)) %>%
  ggplot(aes(x = `paste0(TransYear, TransQtr)`, y = (MedianUnitPrice), group = 1, col = (MedianUnitPrice))) +
  geom_point(position = "identity", na.rm = TRUE) +
  geom_line(size=2, position = "identity", na.rm = TRUE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  + xlab("") + ylab("")
subset(HousingPrice_Yunlin_S, TOWN_ID %in% 
         c("10009010", "10009020", "10009030", "10009040", "10009050")) %>%
  group_by(paste0(TransYear, TransQtr)) %>%
  summarize(MeanUnitPrice = mean(as.numeric(UnitPrice), na.rm = TRUE)) %>%
  ggplot(aes(x = `paste0(TransYear, TransQtr)`, y = (MeanUnitPrice), group = 1, col = (MeanUnitPrice))) +
  geom_point(position = "identity", na.rm = TRUE) +
  geom_line(size=2, position = "identity", na.rm = TRUE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  + xlab("") + ylab("")
