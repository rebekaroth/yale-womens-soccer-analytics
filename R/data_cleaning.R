library(dplyr)
library(readxl)

#------------------------------------------------------------------------------------------
# players including goalkeepers

# list of players for each team
teams = list(
  "brown" = c("A. Davis", "A. Lam", "A. Sahin", "A. Vargas", "A. Welch", "B. Birtwistle", "B. Schopp", "C. Gregory", "C. Robinson", "C. Silverman", "E. Weil", "G. De priest", "G. Toor", "H. Gholami", "H. Houston", "I. Chukwu", "J. Okonye", "K. Maguire", "K. Park", "K. Schlosser", "K. Treanor", "L. Gould", "L. Quinn", "L. Shell", "M. Grant-clavijo", "M. Templeton", "N. Cardoza", "N. Meite", "S. Gevelber"),
  "columbia" = c("A. Akanyirige", "A. Bahugudumbi", "A. Faraone", "A. Prussak", "A. Turner", "C. Berry", "C. Pinnie", "C. Ruedt", "D. Elder", "G. Chan", "I. Liu", "J. Bitzer", "L. Rodriguez", "M. Beltran", "M. Ojo", "M. Tabion", "N. Omorogbe", "N. Ramirez", "S. Ambrose", "S. Cavaliere", "S. Cohn", "S. Jung", "S. Mahoney", "S. Moncion", "S. Robbins", "S. Weiss", "T. Parameswaran"),
  "cornell" = c("A. Bishara", "A. Brotherton", "A. Colbert", "A. Jung", "A. Laden", "B. Brown", "C. Pokigo", "E. Fox", "E. Gibbons", "E. Koschineg", "G. Gonzalez", "I. Scott", "K. Ristianto", "L. DiPierri", "L. Ellingson", "L. Gallman", "M. Kessinger", "M. Kingsley", "M. Leroy", "N. Medugno", "P. Nichols", "R. Gabriel", "S. Allen", "S. Malaga", "T. Nelson"),
  "dartmouth" = c("A. Connors", "A. Marin", "A. Seales", "C. Retterer", "D. Burke", "D. Granholm", "D. Hase", "E. Davidson", "E. Hardy", "E. Russ", "F. Valverde", "G. Faulkner", "G. Martin", "H. Curtin", "H. Rorick", "J. Curry", "K. Sutton", "L. Lauterborn", "M. Lundregan", "S. Adams", "S. Brown", "S. Shelburne", "T. Williams"),
  "harvard" = c("A. Ahmadian", "A. Einde", "A. Francois", "Á. Gunnlaugsdóttir", "A. Hunter", "A. König", "A. Maechler", "A. Rayhill", "D. Tolson", "E. Gordon", "G. Maltby", "Í. Gonzalez", "J. Hasbo", "J. Leshnick", "J. Rose", "K. Mackey", "L. Muniz", "N. Golen", "Ó. Kristinsdóttir", "R. Stewart", "S. Farnham", "S. Lloyd", "S. Long", "V. Frelih"),
  "penn" = c("A. Austen", "A. Chapel", "A. Cook", "A. Okafor", "C. Robke", "E. Priest", "E. Veenema", "H. Adamsky", "I. Glass", "I. Zulli", "J. Stewart", "K. Conte", "K. Peroulas", "L. Finkelman", "L. White", "M. Capdevila", "M. Fuss", "M. Leschly", "M. Lucas", "M. Lusher", "M. Maltby", "T. Ferraro"),
  "princeton" = c("A. Barry", "A. Murphy", "B. Dawahare", "C. Cerone", "C. Kane", "D. Coomans", "D. Jovanovic", "E. Midura", "E. Rudell", "G. Rossner", "H. MacNab", "I. Garces", "K. Hamou", "K. Toomey", "K. Wong", "K. Wozniak", "L. Bryant", "P. Beaulieu", "P. Tordin", "R. Brown", "S. Kurisu", "S. Pierson", "T. McCamey", "Z. Markesini"),
  "yale" = c("A. Bray", "A. Bryant", "A. Butcher", "A. Chang", "A. Kirschner", "A. Miller", "A. Moos", "A. Solomon", "A. Thorvaldsdottir", "A. Shamgochian", "A. So", "B. Golden", "C. Laureano", "E. Rappole", "K. Hernes", "K. Holmes", "L. Booker", "L. Jacobs", "M. Akins", "M. Phillips", "N. Yang", "P. Ryan", "R. Lundstedt", "R. Róth", "R. Exley", "T. Cahalan", "T. Teik", "V. Lundstedt")
)

# create an empty data frame for outfield players and a separate one for gk
df <- data.frame()
gk <- data.frame()

# list for new column names
cols = c("match", "comp", "date", "pos", "min", "actions", "actions_succ", "goals", "ass", "shots", "shots_target", "xg", "pass", "pass_succ", "long", "long_succ", "cross", "cross_succ", "drib", "drib_succ", "duels", "duels_won", "aerial", "aerial_won", "interc", "loss", "loss_own", "rec", "rec_opp", "yellows", "reds")

cols_gk = c("match", "comp", "date", "pos", "min", "goals_conc", "xcg", "shots_against", "saves", "saves_reflex", "exits", "long", "long_succ", "shortp", "shortp_succ", "gkicks", "short_gkick", "long_gkick")

# loop through all the files
for (team in names(teams)){
  print(team)
  for (player in teams[[team]]) {
    print(player)
    # read in data from xlsx
    file_path <- paste0("/Users/rothrebeka/Documents/OneDrive - Yale University/Spring 2025/senior project/data/", team,  " players/Player stats ", player, ".xlsx")
    player_df <- read_excel(file_path)
    
    # check is position column contains 'GK' or not
    if (any(player_df$Position == 'GK')){
      colnames(player_df) = cols_gk
    } else {
      colnames(player_df) <- cols
    }
    
    player_df$player_name = player
    player_df$team = team
    
    if (any(player_df$pos == 'GK')){
      gk = rbind(gk, player_df)
    } else {
      df = rbind(df, player_df)
    }
    
  }
}

# remove rows (games) where minutes played is less than 15
df = df %>%
  filter(min >= 15)

gk = gk %>%
  group_by(player_name) %>%
  filter(min >= 15)

# removes players who don't have at least a total of 90 minutes played
df = df %>%
  group_by(player_name) %>%
  filter(sum(min) >= 90)

gk = gk %>%
  group_by(player_name) %>%
  filter(sum(min) >= 90)

# Position Mapping
position_map <- list(
  GK = "GK",
  CB = "DEF", LCB = "DEF", RCB = "DEF", RB = "DEF", LB = "DEF",
  LWB = "MID", RWB = "MID",
  DMF = "MID", AMF = "MID", RCMF = "MID", LCMF = "MID",
  RDMF = "MID", LDMF = "MID", LAMF = "MID", RAMF = "MID",
  CF = "ATT", RWF = "ATT", LWF = "ATT", RW = "ATT", LW = "ATT"
)

# Function to map positions
convert_position <- function(pos) {
  pos_list <- unlist(strsplit(pos, ",\\s*"))  # Split multiple positions
  first_pos <- pos_list[1]  # Take the first listed position
  return(ifelse(first_pos %in% names(position_map), position_map[[first_pos]], NA))
}

df$pos <- sapply(df$pos, convert_position)

# Assign most common position for missing values
for (player in unique(df$player_name)) {
  most_common <- names(which.max(table(df$pos[df$player_name == player])))
  df$pos[df$player_name == player & is.na(df$pos)] <- most_common
}

# Create per 90 statistics
# like goals_per90, xg_per90, etc..
df = df %>%
  mutate(
    goals_per90 = goals/min*90,
    shots_per90 = shots/min*90,
    xg_per90 = xg/min*90,
    long_per90 = long/min*90,
    duels_per90 = duels/min*90,
    aerial_per90 = aerial/min*90,
    interc_per90 = interc/min*90,
    loss_per90 = loss/min*90,
    rec_per90 = rec/min*90,
    pass_per90 = pass/min*90,
    drib_per90 = drib/min*90,
    actions_per90 = actions/min*90,
    cross_per90 = cross/min*90,
    ass_per90 = ass/min*90,
    loss_own_per90 = loss_own/min*90,
    rec_opp_per90 = rec_opp/min*90,
    shots_target_per90 = shots_target/min*90,
    long_succ_per90 = long_succ/min*90,
    duels_won_per90 = duels_won/min*90,
    aerial_won_per90 = aerial_won/min*90,
    pass_succ_per90 = pass_succ/min*90,
    drib_succ_per90 = drib_succ/min*90,
    actions_succ_per90 = actions_succ/min*90,
    cross_succ_per90 = cross_succ/min*90
  )

gk = gk %>%
  mutate(
    goals_conc_per90 = goals_conc/min*90,
    xcg_per90 = xcg/min*90,
    shots_against_per90 = shots_against/min*90,
    saves_per90 = saves/min*90,
    saves_reflex_per90 = saves_reflex/min*90,
    exits_per90 = exits/min*90,
    long_per90 = long/min*90,
    shortp_per90 = shortp/min*90,
    gkicks_per90 = gkicks/min*90,
    short_gkick_per90 = short_gkick/min*90,
    long_gkick_per90 = long_gkick/min*90,
    shortp_succ_per90 = shortp_succ/min*90,
    long_succ_per90 = long_succ/min*90
  )

# create pct for shots, long, duels, aerial, pass, drib, actions, cross

df = df %>%
  mutate(
    shots_pct = shots_target_per90/shots_per90,
    long_pct = long_succ_per90/long_per90,
    duels_pct = duels_won_per90/duels_per90,
    aerial_pct = aerial_won_per90/aerial_per90,
    pass_pct = pass_succ_per90/pass_per90,
    drib_pct = drib_succ_per90/drib_per90,
    actions_pct = actions_succ_per90/actions_per90,
    cross_pct = cross_succ_per90/cross_per90
  )

gk = gk %>%
  mutate(
    save_pct = saves_per90/shots_against_per90,
    shortp_pct = shortp_succ_per90/shortp_per90,
    long_pct = long_succ_per90/long_per90,
    short_gkick_pct = short_gkick_per90/gkicks_per90,
    long_gkick_pct = long_gkick_per90/gkicks_per90
  )

#------------------------------------------------------------------------------------------
# team

# list of teams and seasons
teams = c("Brown Bears", "Columbia Lions", "Cornell Big Red", "Dartmouth Big Green", "Harvard Crimson", "Penn Quakers", "Princeton Tigers", "Yale Bulldogs")

seasons = c("2021", "2022", "2023", "2024")

dt = data.frame()
col_names = c("date", "match", "comp", "duration", "team", "scheme", "goals", "xg", "shots", "shots_on_target", "shots_pct", "passes", "passes_accurate", "pass_pct", "poss_pct", "losses", "losses_low", "losses_med", "losses_high", "rec", "rec_low", "rec_med", "rec_high", "duels", "duels_won", "duels_pct")

# example file location and name: /Users/rothrebeka/Documents/OneDrive - Yale University/Spring 2025/senior project/data/team stats/Team Stats Brown Bears 2021.xlsx

for (team in teams) {
  for (season in seasons) {
    file = paste0("/Users/rothrebeka/Documents/OneDrive - Yale University/Spring 2025/senior project/data/team stats/Team Stats ", team, " ", season, ".xlsx")
    data = read_excel(file)
    
    # get rid of first two rows
    data = data[-c(1, 2), ]
    
    # apply column names
    colnames(data) = col_names
    data$season = season
    dt = rbind(dt, data)
  }
}

# Remove duplicates
dt = dt %>% unique()

# Create new columns (opp.goals, opp.xg, etc)
# Make them be empty
# Take odd and even rows and fill them with appropaite values such as df[odd.rows, opp.cols] = df[even.rows, team.cols]

team_cols = c("goals", "xg", "shots", "shots_on_target", "shots_pct", "passes", "passes_accurate", "pass_pct", "poss_pct", "losses", "losses_low", "losses_med", "losses_high", "rec", "rec_low", "rec_med", "rec_high", "duels", "duels_won", "duels_pct")
opp_cols = paste0("opp.", team_cols)

dt[opp_cols] = NA

odd_rows = seq(1, nrow(dt), by = 2)
even_rows = seq(2, nrow(dt), by = 2)

dt[odd_rows, opp_cols] = dt[even_rows, team_cols]
dt[even_rows, opp_cols] = dt[odd_rows, team_cols]

# Create new columns for home team and away team
# split on hyphen in match column
# get rid of result (everything after last space)
# team before hyphen is home team, team after hyphen is away team

dt = dt %>%
  mutate(match = str_remove(match, "\\s\\d+:\\d+$")) %>% # Remove result
  separate(match, into = c("home_team", "away_team"), sep = " - ")

# filter for Ivy Teams
dt = dt %>%
  filter(team %in% teams)




