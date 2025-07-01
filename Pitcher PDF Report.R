# Libraries
library(ggplot2)
library(dplyr)
library(gridExtra)
library(gtable)
library(grid)

# Working Directory and CSV Data
setwd("~/Downloads/UConnCSV/UConnSeason")
data <- read.csv("UConn Season.csv")

# Get list of all pitchers on UCO_HUS team
pitchers <- data %>%
  filter(PitcherTeam == "UCO_HUS") %>%
  pull(Pitcher) %>%
  unique()

pitch_type_colors <- c(
  "Fastball" = "red",
  "Slider" = "blue",
  "ChangeUp" = "green",
  "Curveball" = "purple",
  "Sinker" = "orange",
  "Cutter" = "black",
  "Splitter" = "yellow"
)

for (pitcher in pitchers) {
  pitcher_data <- data %>% filter(Pitcher == pitcher)
  
  if (nrow(pitcher_data) == 0) next
  
  # Extract formatted name
  name_split <- strsplit(pitcher, ", ")[[1]]
  formatted_name <- paste(name_split[2], name_split[1])
  
  movement_chart <- ggplot(pitcher_data, aes(x = HorzBreak, y = InducedVertBreak, color = TaggedPitchType)) +
    geom_point(size = 3, na.rm = TRUE) +
    labs(
      x = "Horizontal Break (HB)", y = "Induced Vertical Break (IVB)", color = "Pitch Type",
      title = "Pitch Movement Chart"
    ) +
    xlim(-18, 18) + ylim(-30, 30) +  # Narrow the width to resemble PlateLocSide scale
    geom_segment(aes(x = 0, y = -30, xend = 0, yend = 30), size = 1, color = "grey55") +
    geom_segment(aes(x = -18, y = 0, xend = 18, yend = 0), size = 1, color = "grey55") +
    scale_color_manual(
      values = pitch_type_colors,
      guide = guide_legend(nrow = 2)
    ) +
    coord_fixed(ratio = .5) +  # Adjust aspect ratio for shape consistency
    theme_bw() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      legend.position = "bottom",
      legend.text = element_text(size = 10),
      axis.title = element_text(size = 10)
    )
  
  
  release_chart <- ggplot(pitcher_data, aes(x = RelSide, y = RelHeight, color = TaggedPitchType)) +
    geom_point(size = 3, na.rm = TRUE) +
    labs(
      x = "Horizontal Release Point",
      y = "Vertical Release Point",
      color = "Pitch Type",
      title = paste("Release Point Chart")
    ) +
    geom_segment(aes(x = 0, y = 0, xend = 0, yend = 10), size = 1, color = "grey55") +
    geom_segment(aes(x = -4, y = 0, xend = 4, yend = 0), size = 1, color = "grey55") +
    coord_cartesian(xlim = c(-4, 4), ylim = c(2, 7)) +
    coord_fixed(ratio = 0.5) +  # Match movement chart dimensions
    scale_color_manual(
      values = pitch_type_colors,
      guide = guide_legend(nrow = 2)
    ) +
    theme_bw() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      legend.position = "bottom",
      legend.text = element_text(size = 10),
      axis.title = element_text(size = 10),
      plot.margin = unit(c(1, 1, 1, 1), "cm")
    )
  
  
  
  # Pitch Location Map for BatterSide = "Left"
  pitch_location_map_left <- ggplot(
    pitcher_data %>% filter(BatterSide == "Left"),
    aes(x = PlateLocSide, y = PlateLocHeight, fill = TaggedPitchType)
  ) +
    geom_point(shape = 21, size = 3, alpha = 0.7) +
    scale_fill_manual(values = pitch_type_colors) +
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
    scale_fill_manual(values = pitch_type_colors) +
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
    aes(x = PlateLocSide, y = PlateLocHeight, color = TaggedPitchType, shape = PlayResult)
  ) +
    geom_point(size = 3, alpha = 0.7) +
    scale_color_manual(values = pitch_type_colors) +
    labs(
      title = "BIP Chart (Hits/Outs)",
      x = "Horizontal Pitch Location",
      y = "Vertical Pitch Location",
      color = "Pitch Type",
      shape = "Play Result"
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
    scale_fill_manual(values = pitch_type_colors) +
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
  
  total_pitches <- nrow(data_filtered)
  
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
      `Zone%` = round(ifelse(Pitches > 0, InStrikeZone / Pitches * 100, 0), 1),
      `ZWhiff%` = round(ifelse(ZSwingCount > 0, ZWhiffs / ZSwingCount * 100, 0), 1),
      `ZSwing%` = round(ifelse(InStrikeZone > 0, ZSwingCount / InStrikeZone * 100, 0), 1),
      BAA = round(H / AB, 3),
      `Pitch%` = round(Pitches / total_pitches * 100, 1)
    ) %>%
    select(TaggedPitchType, Pitches, `Pitch%`, `Swing%`, `Whiff%`, `Chase%`, `Strike%`, `GroundBall%`, `FlyBall%`, `Zone%`, `ZWhiff%`, `ZSwing%`, AvgExitVelocity, BAA) %>%
    arrange(desc(Pitches))
  
  # Calculate "All" row
  all_row <- data_filtered %>%
    summarise(
      TaggedPitchType = "All",
      Pitches = n(),
      Swing = sum(Swing),
      Whiffs = sum(PitchCall == "StrikeSwinging"),
      Chase = sum(Chase),
      Strikes = sum(PitchCall %in% c("StrikeCalled", "StrikeSwinging", "FoulBallNotFieldable", "FoulBallFieldable", "InPlay")),
      GroundBall = sum(TaggedHitType == "GroundBall"),
      FlyBall = sum(TaggedHitType == "FlyBall"),
      TotalBallsInPlay = sum(TaggedHitType %in% c("GroundBall", "FlyBall", "Popup", "LineDrive")),
      InStrikeZone = sum(InStrikeZone),
      ZSwingCount = sum(ZSwing),
      ZWhiffs = sum(PitchCall == "StrikeSwinging" & InStrikeZone, na.rm = TRUE),
      AB = sum((PitchCall %in% c("InPlay") | KorBB %in% c("Strikeout")) & !KorBB %in% c("Walk", "HitbyPitch") & !PlayResult %in% c("Sacrifice")),
      SO = sum(KorBB == "Strikeout"),
      BB = sum(KorBB == "Walk"),
      H = sum(PlayResult %in% c("Single", "Double", "Triple", "HomeRun")),
      AvgExitVelocity = round(mean(ExitSpeed[PitchCall == "InPlay"], na.rm = TRUE), 1)
    ) %>%
    mutate(
      `Pitch%` = 100,
      `Swing%` = round(Swing / Pitches * 100, 1),
      `Whiff%` = round(Whiffs / Swing * 100, 1),
      `Chase%` = round(Chase / Swing * 100, 1),
      `Strike%` = round(Strikes / Pitches * 100, 1),
      `GroundBall%` = round(GroundBall / TotalBallsInPlay * 100, 1),
      `FlyBall%` = round(FlyBall / TotalBallsInPlay * 100, 1),
      `Zone%` = round(ifelse(Pitches > 0, InStrikeZone / Pitches * 100, 0), 1),
      `ZWhiff%` = round(ifelse(ZSwingCount > 0, ZWhiffs / ZSwingCount * 100, 0), 1),
      `ZSwing%` = round(ifelse(InStrikeZone > 0, ZSwingCount / InStrikeZone * 100, 0), 1),
      BAA = round(H / AB, 3)
    ) %>%
    select(TaggedPitchType, Pitches, `Pitch%`, `Swing%`, `Whiff%`, `Chase%`, `Strike%`, `GroundBall%`, `FlyBall%`, `Zone%`, `ZWhiff%`, `ZSwing%`, AvgExitVelocity, BAA)
  
  statistics <- bind_rows(statistics, all_row)
  
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
  statistics_grob <- tableGrob(statistics_table, rows = NULL, theme = fancy_theme)
  
  # Dynamically adjust font size for the Pitch Performance table if > 5 rows
  performance_font_size <- if (nrow(statistics) > 5) 4.5 else 8 
  
  performance_theme_dynamic <- ttheme_minimal(
    core = list(
      fg_params = list(fontsize = performance_font_size, fontface = "plain", fontfamily = "sans"),
      bg_params = list(fill = "white", col = "black")
    ),
    colhead = list(
      fg_params = list(fontsize = performance_font_size + 1, fontface = "bold", col = "white"),
      bg_params = list(fill = "navyblue", col = "black")
    )
  )
  
  # Apply to Pitch Performance table
  performance_grob <- tableGrob(statistics, rows = NULL, theme = performance_theme_dynamic)
  
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
  
  pitch_location_section <- arrangeGrob(
    movement_chart + theme(plot.margin = unit(c(1, 1, 1, 1), "cm")),
    arrangeGrob(
      pitch_location_map_left + theme(plot.margin = unit(c(1, 1, 1, 1), "cm")),
      pitch_location_map_right + theme(plot.margin = unit(c(1, 1, 1, 1), "cm")),
      ncol = 2
    ),
    ncol = 1
  )
  
  report_layout <- arrangeGrob(
    grobs = list(
      title_grob,
      combined_tables_grob,
      arrangeGrob(
        movement_chart + theme(plot.margin = unit(c(1, 1, 1, 1), "cm")),
        release_chart + theme(plot.margin = unit(c(1, 1, 1, 1), "cm")),
        ncol = 2
      ),
      arrangeGrob(
        pitch_location_map_left + theme(plot.margin = unit(c(1, 1, 1, 1), "cm")),
        pitch_location_map_right + theme(plot.margin = unit(c(1, 1, 1, 1), "cm")),
        ncol = 2
      ),
      combined_hit_and_strike_chart
    ),
    nrow = 5,
    heights = c(0.3, 1.2, 1.1, 1.2, 1.2)
  )
  
  
  
  # Save report
  pdf_filename <- paste0(formatted_name, " Pitching Report.pdf")
  pdf(pdf_filename, width = 10, height = 18, family = "Helvetica")
  grid.draw(report_layout)
  dev.off()
  
  cat("PDF saved as '", pdf_filename, "'\n", sep = "")
}