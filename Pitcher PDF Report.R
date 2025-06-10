# Libraries
library(ggplot2)
library(dplyr)
library(gridExtra)
library(gtable)
library(grid)

# Working Directory and CSV Data
setwd("~/Downloads/UConnCSV")
data <- read.csv("UConnSeason.csv")

# Filtered data for the pitcher
pitcher_data <- data %>% filter(Pitcher == "Abbadessa, Jude")

# Extract and format pitcher name
Pitcher_name <- unique(pitcher_data$Pitcher)[1]
name_split <- strsplit(Pitcher_name, ", ")[[1]]
formatted_name <- paste(name_split[2], name_split[1])  # "Jude Abbadessa"

# Pitch Location Map for BatterSide = "Left"
pitch_location_map_left <- ggplot(
  pitcher_data %>% filter(BatterSide == "Left"),
  aes(x = PlateLocSide, y = PlateLocHeight, fill = TaggedPitchType)
) +
  geom_point(shape = 21, size = 3, alpha = 0.7) +
  scale_fill_manual(values = c("red", "blue", "green", "yellow", "purple", "orange")) +
  labs(
    title = "Pitch Location Map for LHH",
    x = "Plate Location (Side)",
    y = "Plate Location (Height)",
    fill = "Pitch Type"
  ) +
  theme_minimal() +
  geom_rect(aes(xmin = -1, xmax = 1, ymin = 1.6, ymax = 3.4), fill = NA, color = "black", size = 1) +
  xlim(-1.8, 1.8) +
  ylim(1, 4)

# Pitch Location Map for BatterSide = "Right"
pitch_location_map_right <- ggplot(
  pitcher_data %>% filter(BatterSide == "Right"),
  aes(x = PlateLocSide, y = PlateLocHeight, fill = TaggedPitchType)
) +
  geom_point(shape = 21, size = 3, alpha = 0.7) +
  scale_fill_manual(values = c("red", "blue", "green", "yellow", "purple", "orange")) +
  labs(
    title = "Pitch Location Map for RHH",
    x = "Plate Location (Side)",
    y = "Plate Location (Height)",
    fill = "Pitch Type"
  ) +
  theme_minimal() +
  geom_rect(aes(xmin = -1, xmax = 1, ymin = 1.6, ymax = 3.4), fill = NA, color = "black", size = 1) +
  xlim(-1.8, 1.8) +
  ylim(1, 4)

# Hit Chart
hit_chart <- ggplot(
  pitcher_data %>%
    filter(!PlayResult %in% c("CaughtStealing", "FieldersChoice", "Error", "StolenBase", "Sacrifice", "Undefined")),
  aes(x = PlateLocSide, y = PlateLocHeight, color = PlayResult, shape = TaggedPitchType)
) +
  geom_point(size = 3, alpha = 0.7) +
  scale_color_manual(values = c("red", "blue", "green", "orange", "purple", "yellow", "black", "cyan", "magenta", "brown")) +
  labs(
    title = "BIP Chart (Hits/Outs)",
    x = "Horizontal Pitch Location",
    y = "Vertical Pitch Location",
    color = "Play Result",
    shape = "Pitch Type"
  ) +
  theme_minimal() +
  geom_rect(aes(xmin = -1, xmax = 1, ymin = 1.6, ymax = 3.4), fill = NA, color = "black", size = 1) +
  xlim(-1.8, 1.8) +
  ylim(1, 4)

# StrikeSwinging Pitch Location Map
strike_swinging_chart <- ggplot(
  pitcher_data %>% filter(PitchCall == "StrikeSwinging"),
  aes(x = PlateLocSide, y = PlateLocHeight, fill = TaggedPitchType)
) +
  geom_point(shape = 21, size = 3, alpha = 0.7) +
  scale_fill_manual(values = c("red", "blue", "green", "yellow", "purple", "orange")) +
  labs(
    title = "Whiff Chart",
    x = "Plate Location (Side)",
    y = "Plate Location (Height)",
    fill = "Pitch Type"
  ) +
  theme_minimal() +
  geom_rect(aes(xmin = -1, xmax = 1, ymin = 1.6, ymax = 3.4), fill = NA, color = "black", size = 1) +
  xlim(-1.8, 1.8) +
  ylim(1, 4)

# Combined Hit Chart and StrikeSwinging Chart
combined_hit_and_strike_chart <- arrangeGrob(
  hit_chart + theme(plot.margin = unit(c(1, 1, 1, 1), "cm")),
  strike_swinging_chart + theme(plot.margin = unit(c(1, 1, 1, 1), "cm")),
  ncol = 2
)

# Pitch Metrics Table
pitch_metrics_table <- pitcher_data %>%
  group_by(TaggedPitchType) %>%
  summarise(
    Pitches = n(),
    `Avg Velocity` = round(mean(RelSpeed, na.rm = TRUE), 1),
    `Max Velocity` = round(max(RelSpeed, na.rm = TRUE), 1),
    `Avg Spin Rate` = round(mean(SpinRate, na.rm = TRUE), 1),
    `Max Spin Rate` = round(max(SpinRate, na.rm = TRUE), 1),
    `iVB` = round(mean(InducedVertBreak, na.rm = TRUE), 1),
    `Horz Break` = round(mean(HorzBreak, na.rm = TRUE), 1),
    `Rel Height` = round(mean(RelHeight, na.rm = TRUE), 1),
    `Rel Side` = round(mean(RelSide, na.rm = TRUE), 1),
    `Ext` = round(mean(Extension, na.rm = TRUE), 1)
  )%>%
  arrange(desc(Pitches))

# Title to the Pitch Metrics Table
pitch_metrics_title <- textGrob(
  "Pitch Metrics",
  gp = gpar(fontsize = 14),
  just = "center"
)

# Convert Pitch Metrics Table to Grob
pitch_metrics_grob <- tableGrob(
  pitch_metrics_table,
  rows = NULL,
  theme = ttheme_default(
    core = list(fg_params = list(cex = 0.6)),
    colhead = list(fg_params = list(cex = 0.7))
  )
)

# Combine the title and the Pitch Metrics table into a single grob
pitch_metrics_grob_with_title <- grid.arrange(
  pitch_metrics_title,
  pitch_metrics_grob,
  ncol = 1,
  heights = c(0.1, 0.9) 
)



# Pitch Performance Statistics
data_filtered <- pitcher_data %>%
  mutate(
    InStrikeZone = PlateLocSide >= -1 & PlateLocSide <= 1 & PlateLocHeight >= 1.40 & PlateLocHeight <= 3.6,
    Swing = PitchCall %in% c("FoulBallNotFieldable", "FoulBallFieldable", "StrikeSwinging", "InPlay"),
    Chase = ifelse(!InStrikeZone & Swing, 1, 0),
    ZSwing = ifelse(InStrikeZone & Swing, 1, 0)
  )

statistics <- data_filtered %>%
  group_by(TaggedPitchType) %>%
  summarise(
    Pitches = n(),
    Strikes = sum(PitchCall %in% c("StrikeCalled", "StrikeSwinging", "FoulBallNotFieldable", "FoulBallFieldable", "InPlay")),
    Whiffs = sum(PitchCall == "StrikeSwinging"),
    Swing = sum(Swing),
    GroundBall = sum(TaggedHitType == "GroundBall"),
    FlyBall = sum(TaggedHitType == "FlyBall"),
    TotalBallsInPlay = sum(TaggedHitType %in% c("GroundBall", "FlyBall", "Popup", "LineDrive")),
    InStrikeZone = sum(InStrikeZone),
    Chase = sum(Chase),
    ZSwingCount = sum(ZSwing),
    SO = sum(KorBB == "Strikeout"),
    BB = sum(KorBB == "Walk"),
    HBP = sum(PitchCall == "HitByPitch"),
    AB = sum((PitchCall %in% c("InPlay") | KorBB %in% c("Strikeout")) & !KorBB %in% c("Walk", "HitbyPitch") & !PlayResult %in% c("Sacrifice")),
    H = sum(PlayResult %in% c("Single", "Double", "Triple", "HomeRun")),
    X1B = sum(PlayResult == "Single"),
    X2B = sum(PlayResult == "Double"),
    X3B = sum(PlayResult == "Triple"),
    HR = sum(PlayResult == "HomeRun"),
    AvgExitVelocity = round(mean(ExitSpeed[PitchCall == "InPlay"], na.rm = TRUE), 1),
    ZWhiffs = sum(PitchCall == "StrikeSwinging" & InStrikeZone, na.rm = TRUE)
  ) %>%
  mutate(
    `Swing%` = round(Swing / Pitches * 100, 1),
    `Whiff%` = round(Whiffs / Swing * 100, 1),
    `Chase%` = round(Chase / Swing * 100, 1),
    `Strike%` = round(Strikes / Pitches * 100, 1),
    `Strikeout%` = round(SO / AB * 100, 1),
    `Walk%` = round(BB / AB * 100, 1),
    `GroundBall%` = round(GroundBall / TotalBallsInPlay * 100, 1),
    `FlyBall%` = round(FlyBall / TotalBallsInPlay * 100, 1),
    `Zone%` = round(InStrikeZone / Pitches * 100, 1),
    `ZWhiff%` = round(ifelse(ZSwingCount > 0, ZWhiffs / ZSwingCount * 100, 0), 1),
    `ZSwing%` = round(ZSwingCount / InStrikeZone * 100, 1),
    BAA = round(H / AB, 3)
  ) %>%
  select(TaggedPitchType, Pitches, `Swing%`, `Whiff%`, `Chase%`, `Strike%`, `GroundBall%`, `FlyBall%`, `Zone%`, `ZWhiff%`, `ZSwing%`, AvgExitVelocity, BAA) %>%
  arrange(desc(Pitches))


# Add a title to the Pitch Performance Table
performance_title <- textGrob(
  "Pitch Performance",
  gp = gpar(fontsize = 14),
  just = "center"
)

# Convert the Pitch Performance Table to Grob
performance_grob <- tableGrob(
  statistics,
  rows = NULL,
  theme = ttheme_default(
    core = list(fg_params = list(cex = 0.6)),
    colhead = list(fg_params = list(cex = 0.7))
  )
)

# Combine the title and the performance table into a single grob
performance_grob_with_title <- grid.arrange(
  performance_title,
  performance_grob,
  ncol = 1,
  heights = c(0.1, 0.9) 
)


# Calculate Statistics
calculateStatistics <- function(data_filtered) {
  data_filtered <- data_filtered %>%
    mutate(
      InStrikeZone = PlateLocSide >= -1 & PlateLocSide <= 1 & PlateLocHeight >= 1.40 & PlateLocHeight <= 3.6,
      Swing = PitchCall %in% c("FoulBallNotFieldable", "FoulBallFieldable", "StrikeSwinging", "InPlay"),
      Chase = ifelse(InStrikeZone == 0 & Swing == 1, 1, 0)
    )
  
  first_pitch_strikes <- data_filtered %>%
    filter(PitchofPA == 1 & PitchCall %in% c("FoulBallNotFieldable", "FoulBallFieldable", "InPlay", "StrikeCalled", "StrikeSwinging"))
  
  first_pitch_strikes_count <- nrow(first_pitch_strikes)
  first_pitch_strike_percentage <- round(first_pitch_strikes_count / nrow(data_filtered %>% filter(PitchofPA == 1)) * 100, 1)
  
  earned_runs <- sum((data_filtered %>% filter(PlayResult != "Error"))$RunsScored, na.rm = TRUE)
  
  statistics <- data_filtered %>%
    summarise(
      Pitches = n(),
      PA = sum(PitchCall %in% c("InPlay") | KorBB %in% c("Strikeout", "Walk", "HitbyPitch")),
      AB = sum((PitchCall %in% c("InPlay") | KorBB %in% c("Strikeout")) & !KorBB %in% c("Walk", "HitbyPitch") & !PlayResult %in% c("Sacrifice")),
      IP = round(sum((OutsOnPlay == "1" | KorBB == "Strikeout")) / 3, 2),
      ER = earned_runs,
      ERA = round((earned_runs / sum((OutsOnPlay == "1" | KorBB == "Strikeout") / 3)) * 9, 2),
      H = sum(PlayResult %in% c("Single", "Double", "Triple", "HomeRun")),
      SO = sum(KorBB == "Strikeout"),
      BB = sum(KorBB == "Walk"),
      HBP = sum(PitchCall == "HitByPitch"),
      `Whiff%` = round(sum(PitchCall == "StrikeSwinging", na.rm = TRUE) / sum(Swing, na.rm = TRUE) * 100, 1),
      `Chase%` = round(sum(Chase, na.rm = TRUE) / sum(Swing, na.rm = TRUE) * 100, 1),
      `Strike%` = round(sum(PitchCall %in% c("StrikeCalled", "StrikeSwinging", "FoulBallNotFieldable", "FoulBallFieldable", "InPlay")) / Pitches * 100, 2),
      `FPS%` = first_pitch_strike_percentage,
      BAA = round(H / AB, 3)
    )
  
  return(statistics)
}

statistics_table <- calculateStatistics(pitcher_data)

# Add a title to the Statistics Table
statistics_title <- textGrob(
  "Game Statistics",
  gp = gpar(fontsize = 14),
  just = "center",
  y = 0.5
)

# Convert Statistics Table to Grob
statistics_grob <- tableGrob(
  statistics_table,
  rows = NULL,
  theme = ttheme_default(
    core = list(fg_params = list(cex = 0.6)),
    colhead = list(fg_params = list(cex = 0.7))
  )
)

# Combine the title and the statistics table into a single grob
statistics_grob_with_title <- grid.arrange(
  statistics_title,
  statistics_grob,
  ncol = 1,
  heights = c(0.05, 0.95) 
)

# Fancy Title
title_grob <- textGrob(
  paste(formatted_name, "Post Game Report"),
  x = 0.5, y = 0.5, just = "center",
  gp = gpar(fontsize = 20, fontface = "bold", col = "black", fontfamily = "sans", underline = TRUE)
)

# Enhanced table theme with borders
fancy_theme <- ttheme_minimal(
  core = list(
    fg_params = list(fontsize = 8, fontface = "plain", fontfamily = "sans"),
    bg_params = list(fill = "white", col = "black")
  ),
  colhead = list(
    fg_params = list(fontsize = 9, fontface = "bold", col = "white"),
    bg_params = list(fill = "navyblue", col = "black")
  )
)

# Section headers
statistics_title <- textGrob(
  "Game Statistics",
  gp = gpar(fontsize = 14, fontface = "bold.italic", col = "black", fontfamily = "sans"),
  just = "center", y = 0.5
)

pitch_metrics_title <- textGrob(
  "Pitch Metrics",
  gp = gpar(fontsize = 14, fontface = "bold.italic", col = "black", fontfamily = "sans"),
  just = "center"
)

performance_title <- textGrob(
  "Pitch Performance",
  gp = gpar(fontsize = 14, fontface = "bold.italic", col = "black", fontfamily = "sans"),
  just = "center"
)

# Apply table theme
pitch_metrics_grob <- tableGrob(pitch_metrics_table, rows = NULL, theme = fancy_theme)
performance_grob <- tableGrob(statistics, rows = NULL, theme = fancy_theme)
statistics_grob <- tableGrob(statistics_table, rows = NULL, theme = fancy_theme)

# Combine titles with tables
pitch_metrics_grob_with_title <- grid.arrange(pitch_metrics_title, pitch_metrics_grob, ncol = 1, heights = c(0.1, 0.9))
performance_grob_with_title <- grid.arrange(performance_title, performance_grob, ncol = 1, heights = c(0.1, 0.9))
statistics_grob_with_title <- grid.arrange(statistics_title, statistics_grob, ncol = 1, heights = c(0.05, 0.95))

# Combine all tables
combined_tables_grob <- arrangeGrob(
  grobs = list(statistics_grob_with_title, pitch_metrics_grob_with_title, performance_grob_with_title),
  nrow = 3,
  heights = c(0.3, 0.5, 0.5)
)

# Fancy plot theming
fancy_plot_theme <- theme(
  plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
  legend.title = element_text(face = "bold", size = 10),
  legend.text = element_text(size = 9)
)

# Apply to all plots
pitch_location_map_left <- pitch_location_map_left + fancy_plot_theme
pitch_location_map_right <- pitch_location_map_right + fancy_plot_theme
hit_chart <- hit_chart + fancy_plot_theme
strike_swinging_chart <- strike_swinging_chart + fancy_plot_theme


hit_chart <- hit_chart + fancy_plot_theme
strike_swinging_chart <- strike_swinging_chart + fancy_plot_theme

# Combine the updated plots
combined_hit_and_strike_chart <- arrangeGrob(
  hit_chart + theme(plot.margin = unit(c(1, 1, 1, 1), "cm")),
  strike_swinging_chart + theme(plot.margin = unit(c(1, 1, 1, 1), "cm")),
  ncol = 2
)

# Final layout
report_layout <- arrangeGrob(
  grobs = list(
    title_grob,
    combined_tables_grob,
    arrangeGrob(
      pitch_location_map_left + theme(plot.margin = unit(c(1, 1, 1, 1), "cm")),
      pitch_location_map_right + theme(plot.margin = unit(c(1, 1, 1, 1), "cm")),
      ncol = 2
    ),
    combined_hit_and_strike_chart
  ),
  nrow = 4,
  heights = c(0.3, 1.2, 1.2, 1.2)
)

# Save report as PDF
filename_safe <- gsub(", ", "_", Pitcher_name)
pdf_filename <- paste0(formatted_name, " Pitching Report.pdf")
pdf(pdf_filename, width = 10, height = 14, family = "Helvetica")
grid.draw(report_layout)
dev.off()

cat("PDF saved as '", pdf_filename, "'\n", sep = "")