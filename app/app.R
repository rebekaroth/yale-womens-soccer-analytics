library(shiny)
library(shinydashboard)
library(shinyjs)

source("functions.R")

df = readRDS("players.rds")
gk = readRDS("gks.rds")
dt = readRDS("teams.rds")

ui <- fluidPage(
  useShinyjs(),  # For shinyjs features
  tags$head(
    tags$style(HTML("
      .container-fluid {
        max-width: 900px;
        margin-left: 50px;
        margin-right: auto;
      }    
        
      body {
        background-color: white;
        color: #0A284B;  /* Yale Blue */
      }
      .navbar {
        background-color: #0A284B;  /* Yale Blue */
        color: white;
      }
      .navbar-nav > li > a {
        color: white;
      }
      .nav-tabs > li > a {
        color: #0A284B;  /* Yale Blue */
        background-color: white;
        border: none;
      }
      .nav-tabs > li > a:hover {
        background-color: #0A284B;  /* Yale Blue */
        color: white;
      }
      .nav-tabs > li.active > a {
        background-color: #0A284B;  /* Yale Blue */
        color: white;
      }
      .tab-content {
        background-color: white;
        padding: 15px;
        border: none;
        border-top: none;
      }
      .box {
        border-color: #0A284B;
      }
    "))
  ),
  
  # sidebarLayout(
  #   sidebarPanel(
  #     
  #     )
  #   ),
  
  mainPanel(
    titlePanel("Ivy League Soccer Analysis"),
    width = 9,
    tabsetPanel(
      id = "sub_tabs",
      tabPanel("Player", 
               tabsetPanel(
                 id = "player_tabs",
                 tabPanel("Scatter", 
                          tagList(
                            helpText("Step 1: Choose one or more players to include in the scatter plot. If you want, you can also select an entire team at once (or multiple teams). Alternatively, you can also pick all players for a given position(s). If both team(s) and position(s) are selected, the intersection of the two will be used. After this, you can still select any individual players, who will be added to the plot regardless of their team or position. Ex: yale and DEF will show all defenders from Yale."),
                            selectInput("selected_players", "Select Players:",
                                        choices = list(
                                          "── Teams ──" = setNames(unique(df$team), unique(df$team)),
                                          "── Positions ──" = setNames(unique(df$pos),  unique(df$pos)),
                                          "── Players ──" = setNames(unique(df$player_name), unique(df$player_name))  
                                        ),
                                        #selected = unique(df$player_name[df$team == "yale"]),
                                        multiple = TRUE)
                          ),
                          tagList(
                            helpText("Step 2: Choose the type of scatter plot to display. Each type uses different metrics."),
                            selectInput("xy_pair", "Select Plot Type:",
                                        choices = c("Finishing Efficiency" = "goals_per90-xg_per90",
                                                    "Long Passing Ability" = "long_per90-long_pct",
                                                    "Duel Efficiency" = "duels_per90-duels_pct",
                                                    "Shooting Efficiency" = "shots_per90-shots_pct",
                                                    "Possession Impact" = "loss_per90-rec_per90",
                                                    "Aerial Duel Performance" = "aerial_per90-aerial_pct",
                                                    "Passing Efficiency" = "pass_per90-pass_pct",
                                                    "Dribbling Effectiveness" = "drib_per90-drib_pct",
                                                    "Overall Action Efficiency" = "actions_per90-actions_pct",
                                                    "Crossing Ability" = "cross_per90-cross_pct")),
                          ),
                          tagList(
                            helpText("Step 3: Filter data by position. 'All' shows all data available for each player. A specified position will show data only for that position for each player. Specific positions only available if selected players share a position. Players might have different positions each game, therefore they might have data for more than one position available."),
                            selectInput("position", "Select Position:", choices = c("All" = "all"))
                          ),
                          tagList(
                            helpText("Step 4: Choose what to compare selected players against. This controls the background coloring. 'League' shows the league average, 'position' shows the average for the selected position, and 'selected players' shows the average of players on the plot."),
                            selectInput("relative_to", "Compare Relative To:", 
                                        choices = c("Selected Players" = "player", "League" = "league", "Position" = "position"),
                                        selected = "league")
                          ), 
                          plotOutput("scatterPlot")),
                 tabPanel("Radar",
                          selectInput("radar_player1", "Select First Player:",
                                      choices = unique(df$player_name),
                                      selected = "K. Hernes",
                                      multiple = FALSE),
                          selectInput("radar_player2", "Select Second Player:",
                                      choices = unique(df$player_name),
                                      selected = "V. Lundstedt",
                                      multiple = FALSE),
                          tagList(
                            helpText("Choose how to display Playmaking stats. Percentage refers to accuracy statistics (e.g., pass completion percentage), while Volume refers to the number of actions per 90 minutes (e.g., passes per 90)."),
                            selectInput(
                              "playmaking_type", 
                              "Playmaking Metric Type:", 
                              choices = c("Percentage" = "pct", "Volume" = "per90"),
                              selected = "pct"
                            )
                          ),
                          plotOutput("radarPlot"), width = "2000px"),
                 tabPanel("Bar Charts",
                          selectInput("selected_players_bar", "Select Players:",
                                      choices = list(
                                        "── Teams ──" = setNames(unique(df$team), unique(df$team)),
                                        "── Positions ──" = setNames(unique(df$pos),  unique(df$pos)),
                                        "── Players ──" = setNames(unique(df$player_name), unique(df$player_name))  
                                      ),
                                      multiple = TRUE),
                              selectInput("bar_var", "Variable", choices = c(
                                "Goals" = "goals_per90",
                                "Shots" = "shots_per90",
                                "Assists" = "ass_per90",
                                "xG" = "xg_per90",
                                "Passes" = "pass_per90",
                                "Crosses" = "cross_per90",
                                "Dribbles" = "drib_per90",
                                "Long Passes" = "long_per90"
                              ))
                            ),
                            mainPanel(
                              plotOutput("playerBarPlot")
                            )
               )
      ),
      tabPanel("Goalkeeper", 
               tabsetPanel(
                 id = "goalkeeper_tabs",
                 tabPanel("Scatter", 
                          selectInput("selected_gks", "Select Goalkeepers:",
                                      choices = unique(gk$player_name),
                                      selected = c("K. Holmes", "A. Shamgochian"),
                                      multiple = TRUE),
                          selectInput("xy_pair_gk", "Select Plot Type:",
                                      choices = c("Goals vs Expected Goals Conceded" =                                                                            "goals_conc_per90-xcg_per90",
                                                  "Long Passing Ability" = "long_per90-long_pct",
                                                  "Short Passing Ability" = "shortp_per90-shortp_pct",
                                                  "Saving Efficiency" = "saves_per90-save_pct",
                                                  "Distribution of Goaltending Actions" = "exits_per90-saves_per90")),
                          plotOutput("goalkeeperScatterPlot")),
                 tabPanel("Radar", 
                          selectInput("radar_gk1", "Select First Goalkeeper:",
                                      choices = unique(gk$player_name),
                                      selected = "K. Holmes",
                                      multiple = FALSE),
                          selectInput("radar_gk2", "Select Second Goalkeeper:",
                                      choices = unique(gk$player_name),
                                      selected = "A. Shamgochian",
                                      multiple = FALSE),
                          plotOutput("goalkeeperRadarPlot"), width = "2000px")
               )
      ),
      tabPanel("Team", 
               tabsetPanel(
                 id = "team_tabs",
                 tabPanel("Bar", 
                          selectInput("var", "Select a variable to plot:", 
                                      choices = c("Goals" = "goals", 
                                                  "Expected Goals" = "xg", 
                                                  "Passes Percentage" = "pass_pct", 
                                                  "Shots on Target" = "shots_on_target", 
                                                  "Posession Percentage" = "poss_pct", 
                                                  "Losses in Defensive Third" = "losses_low", 
                                                  "Recoveries in Attacking Third" = "rec_high", 
                                                  "Duels Won Percentage" = "duels_pct",
                                                  "Opponent Goals" = "opp.goals",
                                                  "Opponent xG" = "opp.xg",
                                                  "Opponent Shots on Target" = "opp.shots_on_target")),
                          selectInput("season", "Select a season:", 
                                      choices = c("2024", "2023", "2022", "2021"),
                                      selected = "2024",
                                      multiple = TRUE),
                          selectInput("n_games", "Select number of games:", 
                                      choices = c("All", "Last 5" = 5, "Last 10" = 10, "Last 15" = 15),
                                      selected = "All"),
                          plotOutput("teamBarPlot")),
                 tabPanel("Radar", 
                          selectInput("team1", "Select a team", 
                                      choices = c("Brown Bears", "Columbia Lions", "Cornell Big Red", "Dartmouth Big Green", "Harvard Crimson", "Penn Quakers", "Princeton Tigers", "Yale Bulldogs"),
                                      selected = "Yale Bulldogs"),
                          selectInput("team2", "Select another team", 
                                      choices = c("Brown Bears", "Columbia Lions", "Cornell Big Red", "Dartmouth Big Green", "Harvard Crimson", "Penn Quakers", "Princeton Tigers", "Yale Bulldogs"),
                                      selected = "Harvard Crimson"),
                          plotOutput("teamRadarPlot", width = "1000px"))
               )
      )
    )
  )
)


server <- function(input, output, session) {
  
  get_selected_players <- reactive({
    sel <- input$selected_players
    if (is.null(sel)) sel <- character(0)
    
    teams_sel <- intersect(sel, unique(df$team))
    pos_sel   <- intersect(sel, unique(df$pos))
    indiv_sel <- intersect(sel, unique(df$player_name))
    
    # Start from all players, then narrow down by team and/or position (intersection)
    pool <- unique(df$player_name)
    if (length(teams_sel) > 0) {
      pool <- intersect(pool, unique(df$player_name[df$team %in% teams_sel]))
    }
    if (length(pos_sel) > 0) {
      pool <- intersect(pool, unique(df$player_name[df$pos %in% pos_sel]))
    }
    
    # If neither a team nor a position shortcut was chosen, don't auto-include everyone
    base <- if ((length(teams_sel) + length(pos_sel)) > 0) pool else character(0)
    
    # Allow adding individuals on top of the filters
    unique(c(base, indiv_sel))
  })
  
  get_selected_players_bar <- reactive({
    sel <- input$selected_players_bar
    if (is.null(sel)) sel <- character(0)
    
    teams_sel <- intersect(sel, unique(df$team))
    pos_sel   <- intersect(sel, unique(df$pos))
    indiv_sel <- intersect(sel, unique(df$player_name))
    
    # Start from all players, then narrow down by team and/or position (intersection)
    pool <- unique(df$player_name)
    if (length(teams_sel) > 0) {
      pool <- intersect(pool, unique(df$player_name[df$team %in% teams_sel]))
    }
    if (length(pos_sel) > 0) {
      pool <- intersect(pool, unique(df$player_name[df$pos %in% pos_sel]))
    }
    
    # If neither a team nor a position shortcut was chosen, don't auto-include everyone
    base <- if ((length(teams_sel) + length(pos_sel)) > 0) pool else character(0)
    
    # Allow adding individuals on top of the filters
    unique(c(base, indiv_sel))
  })
  
  
  observeEvent(input$selected_players, {
    players <- get_selected_players()
    req(length(players) > 0)
    
    filtered_df <- df[df$player_name %in% players, ]
    
    # Positions per player (unique per player)
    pos_by_player <- tapply(filtered_df$pos, filtered_df$player_name, function(x) sort(unique(x)))
    
    # Find shared positions (intersection across selected players)
    shared_positions <- if (length(pos_by_player) == 1) {
      pos_by_player[[1]]
    } else {
      Reduce(intersect, pos_by_player)
    }
    
    # Build choices for the dropdown: "All" + the shared positions
    choices_pos <- c("All" = "all", shared_positions)
    
    # If a POS shortcut is selected and still shared, preselect it; else keep current or "all"
    pos_shortcuts <- intersect(input$selected_players, unique(df$pos))
    default_sel <- if (length(pos_shortcuts) == 1 && pos_shortcuts %in% shared_positions) {
      pos_shortcuts
    } else if (!is.null(input$position) && input$position %in% choices_pos) {
      input$position
    } else {
      "all"
    }
    
    updateSelectInput(session, "position",
                      choices  = choices_pos,
                      selected = default_sel
    )
  })
  
  
  observeEvent(input$radar_gk1, {
    updateSelectInput(session, "radar_gk2",
                      choices = setdiff(unique(gk$player_name), input$radar_gk1),
                      selected = if (input$radar_gk2 == input$radar_gk1) NULL else input$radar_gk2)
  })
  
  observeEvent(input$radar_gk2, {
    updateSelectInput(session, "radar_gk1",
                      choices = setdiff(unique(gk$player_name), input$radar_gk2),
                      selected = if (input$radar_gk1 == input$radar_gk2) NULL else input$radar_gk1)
  })
  
  observeEvent(input$radar_player1, {
    updateSelectInput(session, "radar_player2",
                      choices = setdiff(unique(df$player_name), input$radar_player1),
                      selected = if (input$radar_player2 == input$radar_player1) NULL else input$radar_player2)
  })
  
  observeEvent(input$radar_player2, {
    updateSelectInput(session, "radar_player1",
                      choices = setdiff(unique(df$player_name), input$radar_player2),
                      selected = if (input$radar_player1 == input$radar_player2) NULL else input$radar_player1)
  })
  
  observeEvent(input$team1, {
    updateSelectInput(session, "team2",
                      choices = setdiff(c("Brown Bears", "Columbia Lions", "Cornell Big Red", "Dartmouth Big Green", "Harvard Crimson", "Penn Quakers", "Princeton Tigers", "Yale Bulldogs"), input$team1),
                      selected = if (input$team2 == input$team1) NULL else input$team2)
  })
  
  observeEvent(input$team2, {
    updateSelectInput(session, "team1",
                      choices = setdiff(c("Brown Bears", "Columbia Lions", "Cornell Big Red", "Dartmouth Big Green", "Harvard Crimson", "Penn Quakers", "Princeton Tigers", "Yale Bulldogs"), input$team2),
                      selected = if (input$team1 == input$team2) NULL else input$team1)
  })
  
  
  # Placeholder for plots (no actual data for now)
  output$scatterPlot <- renderPlot({
    vars <- strsplit(input$xy_pair, "-")[[1]]  # Split the selected pair into X and Y
    scatter_plot_avg(df, get_selected_players(), vars[1], vars[2], input$position, input$relative_to)
  }, width = 500, height = 500)
  
  output$radarPlot <- renderPlot({ 
    plot_radar(df, input$radar_player1, input$radar_player2, input$playmaking_type) 
  }, width = 1000, height = 600)
  
  output$goalkeeperScatterPlot <- renderPlot({
    vars <- strsplit(input$xy_pair_gk, "-")[[1]]  # Split the selected pair into X and Y
    scatter_plot_gk(gk, input$selected_gks, vars[1], vars[2])
  })
  
  output$goalkeeperRadarPlot <- renderPlot({ 
    plot_radar_gk(gk, input$radar_gk1, input$radar_gk2)
  }, width = 1000, height = 600)
  
  output$teamBarPlot <- renderPlot({
    plot_bar_chart(dt, input$var, input$season, input$n_games)
  })
  
  output$teamRadarPlot <- renderPlot({
    plot_radar_team(dt, input$team1, input$team2)
  })
  
  #output$playerBarPlot <- renderPlot({
    #plot_player_bar_chart(df, input$bar_var, get_selected_players_bar())
  #})
}

shinyApp(ui, server)



