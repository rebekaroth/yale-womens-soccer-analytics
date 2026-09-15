library(ggplot2)
library(dplyr)
library(readxl)
library(stringr)
library(fmsb)
library(gridExtra)
library(ggrepel)
library(rlang)
library(tidyr)
library(pubtheme)



################################################################################
# Scatter Plot for outside players
scatter_plot_avg <- function(data, player_names, x, y, position = 'all', relative_to="league") {
  
  league_avg_x = mean(data[[x]], na.rm = TRUE)
  league_avg_y = mean(data[[y]], na.rm = TRUE)
  
  axis_labels <- list(
    "goals_per90" = "Average Number of Goals per 90 Minutes",
    "xg_per90" = "Average Expected Goals per 90 Minutes",
    "long_per90" = "Average number of long passes per 90 Minutes",
    "long_pct" = "Average Long Pass Accuracy (%)",
    "duels_per90" = "Average Number of Duels per 90 Minutes",
    "duels_pct" = "Average Duel Success Rate (%)",
    "shots_per90" = "Average Number of Shots per 90 Minutes",
    "shots_pct" = "Average Shots on Target (%)",
    "loss_per90" = "Average Number of Losses per 90 Minutes",
    "rec_per90" = "Average Number of Recoveries per 90 Minutes",
    "aerial_per90" = "Average Number of Aerial Duels per 90 Minutes",
    "aerial_pct" = "Average Aerial Duel Success Rate (%)",
    "pass_per90" = "Average Number of Passes per 90 Minutes",
    "pass_pct" = "Average Pass Accuracy (%)",
    "drib_per90" = "Average Number of Dribbles per 90 Minutes",
    "drib_pct" = "Average Dribble Success Rate (%)",
    "actions_per90" = "Average Number of Actions per 90 Minutes",
    "actions_pct" = "Average Action Success Rate (%)",
    "cross_per90" = "Average Number of Crosses per 90 Minutes",
    "cross_pct" = "Average Cross Accuracy (%)"
  )
  
  # Lookup table for titles
  plot_titles <- list(
    "goals_per90-xg_per90" = "Finishing Efficiency",
    "long_per90-long_pct" = "Long Passing Ability",
    "duels_per90-duels_pct" = "Duel Efficiency",
    "shots_per90-shots_pct" = "Shooting Efficiency",
    "loss_per90-rec_per90" = "Possession Impact",
    "aerial_per90-aerial_pct" = "Aerial Duel Performance",
    "pass_per90-pass_pct" = "Passing Efficiency",
    "drib_per90-drib_pct" = "Dribbling Effectiveness",
    "actions_per90-actions_pct" = "Overall Action Efficiency",
    "cross_per90-cross_pct" = "Crossing Ability"
  )
  
  # Get axis labels and title
  x_label <- axis_labels[[x]]
  y_label <- axis_labels[[y]]
  plot_title <- plot_titles[[paste(x, y, sep = "-")]]
  
  # Filter data based on position (if not 'all')
  if (position != 'all') {
    data <- data %>%
      filter(pos == position)
  }
  
  # Filter data for specified players and calculate averages for x and y
  avg_data <- data %>%
    filter(player_name %in% player_names) %>%
    group_by(player_name) %>%
    summarize(
      x_avg = mean(!!sym(x), na.rm = TRUE),
      y_avg = mean(!!sym(y), na.rm = TRUE)
    )
  
  if (relative_to == "league"){
    mean_x <- league_avg_x
    mean_y <- league_avg_y
  } else if (relative_to == "position"){
    mean_x <- mean(data[[x]], na.rm = TRUE)
    mean_y <- mean(data[[y]], na.rm = TRUE)
  } else {
    mean_x <- mean(avg_data$x_avg, na.rm = TRUE)
    mean_y <- mean(avg_data$y_avg, na.rm = TRUE)
  }
  
  # Set a minimum threshold (e.g., 0.001) if the mean is 0
  mean_x <- ifelse(mean_x == 0 | is.na(mean_x), 0.001, mean_x)
  mean_y <- ifelse(mean_y == 0 | is.na(mean_y), 0.001, mean_y)
  
  axis_limits <- list(
    "goals_per90-xg_per90" = list(x = c(0, 2*mean_x), y = c(0, 2*mean_y)),
    "long_per90-long_pct" = list(x = c(0, 2*mean_x), y = c(0, 2*mean_y)),
    "duels_per90-duels_pct" = list(x = c(0, 2*mean_x), y = c(0, 2*mean_y)),
    "shots_per90-shots_pct" = list(x = c(0, 2*mean_x), y = c(0, 2*mean_y)),
    "loss_per90-rec_per90" = list(x = c(0, 2*mean_x), y = c(0, 2*mean_y)),
    "aerial_per90-aerial_pct" = list(x = c(0, 2*mean_x), y = c(0, 2*mean_y)),
    "pass_per90-pass_pct" = list(x = c(0, 2*mean_x), y = c(0, 2*mean_y)),
    "drib_per90-drib_pct" = list(x = c(0, 2*mean_x), y = c(0, 2*mean_y)),
    "actions_per90-actions_pct" = list(x = c(0, 2*mean_x), y = c(0, 2*mean_y)),
    "cross_per90-cross_pct" = list(x = c(0, 2*mean_x), y = c(0, 2*mean_y))
  )
  
  # Get axis limits for the variable pair
  var_key <- paste(x, y, sep = "-")
  limits <- axis_limits[[var_key]]
  x_min <- limits$x[1]
  x_max <- limits$x[2]
  y_min <- limits$y[1]
  y_max <- limits$y[2]
  
  # Create a fixed gradient grid (normalized)
  x_range <- seq(0, 1, length.out = 300)
  y_range <- seq(0, 1, length.out = 300)
  grid_data <- expand.grid(x_norm = x_range, y_norm = y_range)
  
  # Map the normalized grid back to plot coordinates
  grid_data$x_plot <- grid_data$x_norm * (x_max - x_min) + x_min
  grid_data$y_plot <- grid_data$y_norm * (y_max - y_min) + y_min
  
  # Define color gradient orientation
  if (var_key == "loss_per90-rec_per90") {
    # Flip gradient for this specific plot (red in bottom right, green in top left)
    grid_data$fill_value <- 1 - (grid_data$x_norm - grid_data$y_norm) / 2
  } else {
    grid_data$fill_value <- (grid_data$x_norm + grid_data$y_norm) / 2
  }
  
  # Compute aspect ratio based on axis limits
  aspect_ratio <- (x_max - x_min) / (y_max - y_min)
  
  # Plot
  ggplot() +
    geom_raster(data = grid_data, aes(x = x_plot, y = y_plot, fill = fill_value)) +
    scale_fill_gradientn(colors = c("red", "yellow", "green")) +
    geom_point(data = avg_data, aes(x = x_avg, y = y_avg), color = "black", size = 3) +
    geom_text_repel(data = avg_data, aes(x = x_avg, y = y_avg, label = player_name), 
                    size = 3, max.overlaps = Inf) + 
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 18),
      axis.title = element_text(size = 14)) +
    labs(
      title = plot_title,
      x = x_label,
      y = y_label
    ) +
    theme(legend.position = "none") +
    scale_x_continuous(limits = c(x_min, x_max), expand = c(0, 0), oob = squish) +
    scale_y_continuous(limits = c(y_min, y_max), expand = c(0, 0), oob = squish) +
    coord_fixed(ratio = aspect_ratio)
}
################################################################################





################################################################################
# Scatter Plot for GKs
scatter_plot_gk <- function(data, player_names, x, y){
  
  league_avg_x = mean(data[[x]], na.rm = TRUE)
  league_avg_y = mean(data[[y]], na.rm = TRUE)
  
  axis_labels <- list(
    "goals_conc_per90" = "Average Number of Goals Conceded per 90 Minutes",
    "xcg_per90" = "Average Expected Conceded Goals per 90 Minutes",
    "long_per90" = "Average number of long passes per 90 Minutes",
    "long_pct" = "Average Long Pass Accuracy (%)",
    "shortp_per90" = "Average Number of Short Passes per 90 Minutes",
    "shortp_pct" = "Average Short Pass Accuracy (%)",
    "saves_per90" = "Average Number of Saves per 90 Minutes",
    "save_pct" = "Average Save Percentage (%)",
    "exits_per90" = "Average Number of Exits per 90 Minutes"
  )
  
  # Lookup table for titles
  plot_titles <- list(
    "goals_conc_per90-xcg_per90" = "Goals Conceded vs Expected Goals Conceded",
    "long_per90-long_pct" = "Long Passing Ability",
    "shortp_per90-shortp_pct" = "Short Passing Ability",
    "saves_per90-save_pct" = "Saving Efficiency",
    "exits_per90-saves_per90" = "Distribution of Goaltending Actions"
  )
  
  # Get axis labels and title
  x_label <- axis_labels[[x]]
  y_label <- axis_labels[[y]]
  plot_title <- plot_titles[[paste(x, y, sep = "-")]]
  
  # Filter data for specified players and calculate averages for x and y
  avg_data <- data %>%
    filter(player_name %in% player_names) %>%
    group_by(player_name) %>%
    summarize(
      x_avg = mean(!!sym(x), na.rm = TRUE),
      y_avg = mean(!!sym(y), na.rm = TRUE)
    )
  
  axis_limits <- list(
    "goals_conc_per90-xcg_per90" = list(x = c(0, 2*league_avg_x), y = c(0, 2*league_avg_y)),
    "long_per90-long_pct" = list(x = c(0, 2*league_avg_x), y = c(0, 2*league_avg_y)),
    "shortp_per90-shortp_pct" = list(x = c(0, 2*league_avg_x), y = c(0, 2*league_avg_y)),
    "saves_per90-save_pct" = list(x = c(0, 2*league_avg_x), y = c(0, 2*league_avg_y)),
    "exits_per90-saves_per90" = list(x = c(0, 2*league_avg_x), y = c(0, 2*league_avg_y)))
  
  # Get axis limits for the variable pair
  var_key <- paste(x, y, sep = "-")
  limits <- axis_limits[[var_key]]
  x_min <- limits$x[1]
  x_max <- limits$x[2]
  y_min <- limits$y[1]
  y_max <- limits$y[2]
  
  # Create a fixed gradient grid (normalized)
  x_range <- seq(0, 1, length.out = 300)
  y_range <- seq(0, 1, length.out = 300)
  grid_data <- expand.grid(x_norm = x_range, y_norm = y_range)
  
  # Map the normalized grid back to plot coordinates
  grid_data$x_plot <- grid_data$x_norm * (x_max - x_min) + x_min
  grid_data$y_plot <- grid_data$y_norm * (y_max - y_min) + y_min
  
  # Define color gradient orientation
  if (var_key == "goals_conc_per90-xcg_per90") {
    # Flip gradient for this specific plot (green in bottom left, red in top right)
    grid_data$fill_value <- (1 - (grid_data$x_norm - grid_data$y_norm)) / 2
  } else {
    grid_data$fill_value <- (grid_data$x_norm + grid_data$y_norm) / 2
  }
  
  # Compute aspect ratio based on axis limits
  aspect_ratio <- (x_max - x_min) / (y_max - y_min)
  
  # Plot
  ggplot() +
    geom_raster(data = grid_data, aes(x = x_plot, y = y_plot, fill = fill_value)) +
    scale_fill_gradientn(colors = c("red", "yellow", "green")) +
    geom_point(data = avg_data, aes(x = x_avg, y = y_avg), color = "black", size = 3) +
    geom_text_repel(data = avg_data, aes(x = x_avg, y = y_avg, label = player_name), 
                    size = 3, max.overlaps = 10) + 
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 18),
      axis.title = element_text(size = 14)) +
    labs(
      title = plot_title,
      x = x_label,
      y = y_label
    ) +
    theme(legend.position = "none") +
    scale_x_continuous(limits = c(x_min, x_max), expand = c(0, 0), oob = squish) +
    scale_y_continuous(limits = c(y_min, y_max), expand = c(0, 0), oob = squish) +
    coord_fixed(ratio = aspect_ratio)
}
################################################################################





################################################################################
# Radar Plot for outside players
plot_radar <- function(df, player1, player2, playmaking_type = "pct") {
  
  # Compute average values for each player
  avg_data <- df %>%
    group_by(player_name) %>%
    summarize(
      goals_per90 = mean(goals_per90, na.rm = TRUE),
      shots_per90 = mean(shots_per90, na.rm = TRUE),
      xg_per90 = mean(xg_per90, na.rm = TRUE),
      ass_per90 = mean(ass_per90, na.rm = TRUE),
      interc_per90 = mean(interc_per90, na.rm = TRUE),
      aerial_pct = mean(aerial_pct, na.rm = TRUE),
      rec_opp_per90 = mean(rec_opp_per90, na.rm = TRUE),
      loss_own_per90 = mean(loss_own_per90, na.rm = TRUE),
      pass_per90 = mean(pass_per90, na.rm = TRUE),
      drib_per90 = mean(drib_per90, na.rm = TRUE),
      cross_per90 = mean(cross_per90, na.rm = TRUE),
      long_per90 = mean(long_per90, na.rm = TRUE),
      pass_pct = mean(pass_pct, na.rm = TRUE),
      drib_pct = mean(drib_pct, na.rm = TRUE),
      cross_pct = mean(cross_pct, na.rm = TRUE),
      long_pct = mean(long_pct, na.rm = TRUE)
    )
  
  # Compute min and max values for each variable
  min_vals <- avg_data %>%
    summarise(across(where(is.numeric), min, na.rm = TRUE)) %>%
    mutate(player_name = "Min")
  
  max_vals <- avg_data %>%
    summarise(across(where(is.numeric), max, na.rm = TRUE)) %>%
    mutate(player_name = "Max")
  
  # Bind min, max, and actual data
  radar_data <- bind_rows(max_vals, min_vals, avg_data)
  
  # Subset data for selected players
  radar_subset <- radar_data %>%
    filter(player_name %in% c("Max", "Min", player1))
  
  player2_data <- radar_data %>%
    filter(player_name == player2)
  
  radar_subset <- rbind(radar_subset, player2_data)
  
  # Variable labels
  var_labels <- c(
    "goals_per90" = "Goals", "ass_per90" = "Assists", "xg_per90" = "xG", "shots_per90" = "Shots",
    "interc_per90" = "Interceptions", "loss_own_per90" = "Losses Own Half", "rec_opp_per90" = "Recoveries Opp Half", "aerial_pct" = "Aerial Win %",
    "pass_pct" = "Pass Accuracy", "long_pct" = "Long Pass Accuracy", "cross_pct" = "Cross Accuracy", "drib_pct" = "Dribble Success %",
    "pass_per90"     = "Passes",
    "long_per90"     = "Long Passes",
    "cross_per90"    = "Crosses",
    "drib_per90"     = "Dribbles"
  )
  
  # Function to generate radar plot
  create_radar <- function(vars, title) {
    
    radar_values <- radar_subset %>%
      select(player_name, all_of(vars)) %>%
      rename_with(~ var_labels[.x], .cols = all_of(vars)) %>%
      column_to_rownames(var = "player_name")
    
    radarchart(as.data.frame(radar_values), axistype = 0,
               pty = 32,
               pcol = c("red", "blue"), pfcol = c(rgb(1,0,0,0.4), rgb(0,0,1,0.4)),
               plty = c(1, 1), plwd = 2, vlcex    = 1,      # up from .6, makes the axis labels larger
               cex.main = 1.1,      # bumps up the plot title size slightly
              # title    = title,    # radarchart passes this to main
               cglcol = "gray", cglty = 1, cglwd = 0.8)
    
    text(
      x      = 0,         # center
      y      = 1.5,        # 5% above the max radial circle
      labels = title,
      cex    = 1.2,         # bump up title size
      font   = 2            # bold
    )
  }
  
  # Create radar plots
  par(
    mfrow = c(1,3),
    mar   = c(0.5, 3, 0.5, 3),  
    oma   = c(0,   0,   2,   0),    
    xpd   = NA,
    font = 2
  )
  create_radar(c("goals_per90", "ass_per90", "xg_per90", "shots_per90"), "Attacking")
  create_radar(c("interc_per90", "loss_own_per90", "rec_opp_per90", "aerial_pct"), "Defending")
  if (playmaking_type == "per90"){
    create_radar(c("pass_per90", "long_per90", "cross_per90", "drib_per90"), "Playmaking (Volume)")
  } else {
    create_radar(c("pass_pct", "long_pct", "cross_pct", "drib_pct"), "Playmaking (Percentage)")
  }
  
  # Add legend
  legend(
    x      = 0.75,          # centered in device (data) coords
    y      = 1.05,          # high enough to live in oma[3]
    legend = c(player1, player2),
    col    = c("red","blue"),
    pch    = 15,
    pt.cex = 2,
    horiz  = FALSE,
    bty    = "n",
    cex    = 1.1,
    xpd    = NA
  )
}
################################################################################





################################################################################
# Radar Plot for GKs
plot_radar_gk <- function(gk, player1, player2) {
  
  # Compute average values for each goalkeeper
  avg_data_gk <- gk %>%
    group_by(player_name) %>%
    summarize(
      goals_conc_per90 = mean(goals_conc_per90, na.rm = TRUE),
      xcg_per90 = mean(xcg_per90, na.rm = TRUE),
      shots_against_per90 = mean(shots_against_per90, na.rm = TRUE),
      save_pct = mean(save_pct, na.rm = TRUE),
      saves_reflex_per90 = mean(saves_reflex_per90, na.rm = TRUE),
      exits_per90 = mean(exits_per90, na.rm = TRUE),
      long_per90 = mean(long_per90, na.rm = TRUE),
      long_pct = mean(long_pct, na.rm = TRUE),
      shortp_per90 = mean(shortp_per90, na.rm = TRUE),
      shortp_pct = mean(shortp_pct, na.rm = TRUE),
      short_gkick_per90 = mean(short_gkick_per90, na.rm = TRUE),
      long_gkick_per90 = mean(long_gkick_per90, na.rm = TRUE)
    )
  
  # Compute min and max values for each variable
  min_vals_gk <- avg_data_gk %>%
    summarise(across(where(is.numeric), min, na.rm = TRUE)) %>%
    mutate(player_name = "Min")
  
  max_vals_gk <- avg_data_gk %>%
    summarise(across(where(is.numeric), max, na.rm = TRUE)) %>%
    mutate(player_name = "Max")
  
  # Bind min, max, and actual data
  radar_data_gk <- bind_rows(max_vals_gk, min_vals_gk, avg_data_gk)
  
  # Subset data for selected players
  radar_subset <- radar_data_gk %>%
    filter(player_name %in% c("Max", "Min", player1))
           
  player2_data <- radar_data_gk %>%
    filter(player_name == player2)
  
  radar_subset <- rbind(radar_subset, player2_data)
  
  # Variable labels
  var_labels <- c(
    "goals_conc_per90" = "Goals Conceded", "xcg_per90" = "xG Conceded", "shots_against_per90" = "Shots Against",
    "save_pct" = "Save %", "saves_reflex_per90" = "Saves Reflexes", "exits_per90" = "Exits",
    "long_per90" = "Long Passes", "long_pct" = "Long Pass Accuracy",
    "shortp_per90" = "Short Passes", "shortp_pct" = "Short Pass Accuracy",
    "short_gkick_per90" = "Short Goal Kicks", "long_gkick_per90" = "Long Goal Kicks"
  )
  
  # Function to generate radar plot
  create_radar <- function(vars, title) {
    radar_values <- radar_subset %>%
      select(player_name, all_of(vars)) %>%
      rename_with(~ var_labels[.x], .cols = all_of(vars)) %>%
      column_to_rownames(var = "player_name")
    
    radarchart(as.data.frame(radar_values), axistype = 0,
               pty = 32,
               pcol = c("red", "blue"), pfcol = c(rgb(1,0,0,0.4), rgb(0,0,1,0.4)),
               plty = c(1, 1), plwd = 2,
               cglcol = "gray", cglty = 1, cglwd = 0.8,
               vlcex = .75)
    text(
      x      = 0,         
      y      = 1.5,        
      labels = title,
      cex    = 1.2,         
      font   = 2            
    )
  }
  
  # Create radar plots
  par(
    mfrow = c(1,2),
    mar   = c(0.5, 3, 0.5, 3),  
    oma   = c(0,   0,   2,   0),
    xpd   = NA,
    font = 2
  )
  
  create_radar(c("goals_conc_per90", "xcg_per90", "shots_against_per90", "save_pct", "saves_reflex_per90", "exits_per90"), "Goalkeeping")
  create_radar(c("long_per90", "long_pct", "shortp_per90", "shortp_pct", "short_gkick_per90", "long_gkick_per90"), "Possession")
  
  # Add legend
  legend(
    x      = 0.65,          # centered in device (data) coords
    y      = 1.25,          # high enough to live in oma[3]
    legend = c(player1, player2),
    col    = c("red","blue"),
    pch    = 15,
    pt.cex = 2,
    horiz  = FALSE,
    bty    = "n",
    cex    = 1.1,
    xpd    = NA
  )
}
################################################################################





################################################################################
# Bar Plot for teams
plot_bar_chart <- function(df, var, seasons = "2024", n_games = NULL) {
  
  # Create label dictionary for display
  var_labels <- c(
    "goals" = "Goals", 
    "xg" = "Expected Goals (xG)", 
    "shots_on_target" = "Shots on Target", 
    "pass_pct" = "Pass Percentage", 
    "poss_pct" = "Possession Percentage", 
    "losses_low" = "Losses in Defensive Third", 
    "rec_high" = "Recoveries in Attacking Third", 
    "duels_pct" = "Duels Won Percentage", 
    "opp.goals" = "Opponent Goals", 
    "opp.xg" = "Opponent xG", 
    "opp.shots_on_target" = "Opponent Shots on Target"
  )
  
  # Filter data based on season or recent n_games
  df_filtered <- df %>%
    filter(season == seasons)
  
  if (!is.null(n_games)) {
    df_filtered <- df_filtered %>%
      group_by(team) %>%
      top_n(n_games, wt = date) %>%
      ungroup()
  }
  
  # Compute team averages
  avg <- df_filtered %>%
    group_by(team) %>%
    summarise(avg = mean(.data[[var]], na.rm = TRUE), .groups = "drop")
  
  # Custom color scheme: Yale blue for Yale, grey for others
  teams <- unique(avg$team)
  fill_colors <- setNames(rep("grey", length(teams)), teams)
  fill_colors["Yale Bulldogs"] <- "#0f4d92"
  
  # Create custom labels
  avg <- avg %>%
    mutate(
      team_label = ifelse(team == "Yale Bulldogs",
                          "<span style='color:#0f4d92'><b>Yale Bulldogs</b></span>",
                          team),
      label_text = ifelse(team == "Yale Bulldogs",
                          paste0("<span style='color:#0f4d92'><b>", round(avg, 2), "</b></span>"),
                          as.character(round(avg, 2)))
    )
  
  # Plot
  ggplot(avg, aes(x = avg, y = reorder(team_label, avg), fill = team)) +
    geom_bar(stat = "identity") +
    ggtext::geom_richtext(
      aes(label = label_text),
      hjust = -0.1,
      size = 3,
      fill = NA, label.color = NA
    ) +
    labs(
      title = paste0("Average ", var_labels[[var]], " by Team"),
      x = var_labels[[var]],
      y = "Team"
    ) +
    scale_y_discrete(labels = function(x) x) +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.text.y = ggtext::element_markdown()
    ) +
    scale_fill_manual(values = fill_colors) +
    coord_cartesian(clip = "off")
}

################################################################################





################################################################################
# Bar Plot for players
plot_player_bar_chart <- function(df, var, players) {
  
  # Labels for nicer display
  var_labels <- c(
    "goals_per90" = "Goals", 
    "xg_per90" = "Expected Goals (xG)", 
    "shots_per90" = "Shots", 
    "pass_per90" = "Passes", 
    "ass_per90" = "Assists", 
    "crosses_per90" = "Crosses", 
    "drib_per90" = "Dribbles", 
    "long_per90" = "Long Passes"
  )
  
  # Averages -------------------------------------------------
  player_avg <- df %>%
    filter(player_name %in% players) %>%
    group_by(player_name) %>%
    summarise(avg = mean(.data[[var]], na.rm = TRUE), .groups = "drop")
  
  league_avg <- df %>%
    summarise(avg = mean(.data[[var]], na.rm = TRUE)) %>%
    pull(avg)
  
  team_avg <- df %>%
    filter(team == "yale") %>%
    summarise(avg = mean(.data[[var]], na.rm = TRUE)) %>%
    pull(avg)
  
  # Colors: Yale blue for Yale players, grey for others
  players_list <- unique(player_avg$player_name)
  fill_colors <- setNames(rep("grey", length(players_list)), players_list)
  fill_colors[player_avg$team == "yale"] <- "#0f4d92"
  
  # Labels with Yale highlight
  player_avg <- player_avg %>%
    mutate(
      player_label = ifelse(team == "yale",
                            paste0("<span style='color:#0f4d92'><b>", player_name, "</b></span>"),
                            player_name),
      label_text = ifelse(team == "yale",
                          paste0("<span style='color:#0f4d92'><b>", round(avg, 1), "</b></span>"),
                          as.character(round(avg, 1)))
    )
  
  # Plot ------------------------------------------------------
  ggplot(player_avg, aes(x = avg, y = reorder(player_label, avg), fill = player_name)) +
    geom_bar(stat = "identity") +
    # League average line
    geom_vline(xintercept = league_avg, linetype = "dashed", color = "red", linewidth = 0.8) +
    # Team average line (Yale)
    geom_vline(xintercept = team_avg, linetype = "dashed", color = "#0f4d92", linewidth = 0.8) +
    ggtext::geom_richtext(
      aes(label = label_text),
      hjust = -0.1,
      size = 3,
      fill = NA, label.color = NA
    ) +
    labs(
      title = paste0("Average ", var_labels[[var]], " by Player"),
      x = var_labels[[var]],
      y = "Player"
    ) +
    scale_y_discrete(labels = function(x) x) +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.text.y = ggtext::element_markdown()
    ) +
    scale_fill_manual(values = fill_colors) +
    coord_cartesian(clip = "off")
}

################################################################################





################################################################################
# Radar Plot for teams
plot_radar_team <- function(dt, team1, team2) {
  
  # Data for radar plot (average values for each team)
  avg_data_team <- dt %>%
    group_by(team) %>%
    summarise(goals = mean(goals, na.rm = TRUE),
              xg = mean(xg, na.rm = TRUE),
              shots_on_target = mean(shots_on_target, na.rm = TRUE),
              passes_accurate = mean(passes_accurate, na.rm = TRUE),
              poss_pct = mean(poss_pct, na.rm = TRUE),
              shots = mean(shots, na.rm = TRUE),
              losses_low = mean(losses_low, na.rm = TRUE),
              rec_high = mean(rec_high, na.rm = TRUE),
              duels_won = mean(duels_won, na.rm = TRUE),
              opp.goals = mean(opp.goals, na.rm = TRUE),
              opp.xg = mean(opp.xg, na.rm = TRUE),
              opp.shots_on_target = mean(opp.shots_on_target, na.rm = TRUE))
  
  # Compute GLOBAL min/max values (ensuring consistency across plots)
  global_min_vals <- avg_data_team %>%
    summarise(across(where(is.numeric), min, na.rm = TRUE)) %>%
    mutate(team = "Min")
  
  global_max_vals <- avg_data_team %>%
    summarise(across(where(is.numeric), max, na.rm = TRUE)) %>%
    mutate(team = "Max")
  
  # Variables to flip (low values are better)
  flip_vars <- c("losses_low", "opp.goals", "opp.xg", "opp.shots_on_target")
  
  # Normalize function
  normalize <- function(x, min_x, max_x) {
    (x - min_x) / (max_x - min_x)
  }
  
  # Normalize all values using GLOBAL min/max
  normalize_data <- function(df, min_vals, max_vals, flip_vars) {
    df_norm <- df
    for (col in names(df)[-1]) {  # Exclude "team" column
      df_norm[[col]] <- normalize(df[[col]], min_vals[[col]], max_vals[[col]])
      
      # Flip selected defensive variables
      if (col %in% flip_vars) {
        df_norm[[col]] <- 1 - df_norm[[col]]
      }
    }
    return(df_norm)
  }
  
  # Normalize data
  avg_data_team_norm <- normalize_data(avg_data_team, global_min_vals, global_max_vals, flip_vars)
  
  # Ensure min/max values are properly scaled
  global_min_vals[-ncol(global_min_vals)] <- 0  # Exclude "team" column
  global_max_vals[-ncol(global_min_vals)] <- 1
  
  # Bind datasets together
  radar_data_team <- rbind(global_max_vals, global_min_vals, avg_data_team_norm)
  
  radar_subset <- radar_data_team %>%
    filter(team %in% c("Max", "Min", team1))
  
  team2_data <- radar_data_team %>%
    filter(team == team2)
  
  radar_subset <- rbind(radar_subset, team2_data)
  
  # Rename variables for better readability
  var_labels <- c("goals" = "Goals", "xg" = "xG", "shots_on_target" = "Shots on Target", 
                  "passes_accurate" = "Accurate Passes", "poss_pct" = "Possession Percentage", 
                  "shots" = "Shots", "losses_low" = "Losses in Defensive Third", "rec_high" = "Recoveries in Attacking Third", 
                  "duels_won" = "Duels Won", "opp.goals" = "Opponent Goals", "opp.xg" = "Opponent xG", 
                  "opp.shots_on_target" = "Opponent Shots on Target")
  
  # Function to generate radar plots for a given category of variables
  create_radar <- function(vars, title) {
    radar_values <- radar_subset %>%
      select(team, all_of(vars)) %>%
      rename_with(~ var_labels[.x], .cols = all_of(vars)) %>%
      column_to_rownames(var = "team")
    
    # Convert to matrix format required by radarchart
    radar_matrix <- as.data.frame(radar_values)
    radarchart(radar_matrix, axistype = 0,
               pty = 32,
               pcol = c("red", "blue"), pfcol = c(rgb(1,0,0,0.4), rgb(0,0,1,0.4)),
               plty = c(1, 1),
               plwd = 2, title = title,
               cglcol = "gray", cglty = 1, cglwd = 0.8,  # Customize grid lines
               vlcex = NULL # Adjust label size
    )
  }
  
  # Create radar plots for attacking and defending variables
  par(mfrow = c(1, 2), mar = c(1, 2, 2, 2), oma = c(0, 1, 2, 1))
  create_radar(c("goals", "xg", "shots_on_target", "passes_accurate", "poss_pct", "shots"), "Attacking")
  create_radar(c("losses_low", "rec_high", "duels_won", "opp.goals", "opp.xg", "opp.shots_on_target"), "Defending")
  
  # Add legend
  legend("topright",
         legend = c(team1, team2),
         pch    = 15,
         col    = c("red","blue"),
         pt.cex = 2,
         bty    = "n")
}
################################################################################