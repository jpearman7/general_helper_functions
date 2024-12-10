library(psych)
library(tidyverse)
library(CTT)
library(readxl)
library(statnet)
library(network)
library(sna)

library(here)

# Set working directory and load data
here("03_analysis.R")

df <- read.csv('output/msu_ari_data.csv') %>% 
  select(c(Team_Id, Role_ID, Role, 486:551)) %>% 
  mutate(Role = recode(Role, Leader = 1, Navigator = 2, Engineer = 3, Intel = 4, Security= 5)) %>% 
  distinct(Role_ID, .keep_all = T) %>% 
  group_by(Team_Id, Role) %>% 
  arrange(Team_Id, Role) %>% 
  ungroup()

# Within each team, calculate network metrics
# Note on Network Items:
# 1 = Leader
# 2 = Navigator
# 3 = Engineer
# 4 = Intelligence
# 5 = Security

ha_info_network <- df %>% select(c(Team_Id,Role, 25:29))
ha_info_network <- df %>% select(c(Team_Id,Role, 30:34))
ha_info_network <- df %>% select(c(Team_Id,Role, 35:39))

sr_comp_network <- df %>% select(c(Team_Id,Role, 40:44))
sr_work_network <- df %>% select(c(Team_Id,Role, 45:49))
sr_info_network <- df %>% select(c(Team_Id,Role, 50:54))

es_comp_network <- df %>% select(c(Team_Id,Role, 55:59))
es_work_network <- df %>% select(c(Team_Id,Role, 60:64))
es_info_network <- df %>% select(c(Team_Id,Role, 65:69))
# ha_comp_network_mat <- as.matrix(ha_comp_network)

# Network analysis code for one matrix. 
# Adapt this so it applies across all networks, append network names to metrics, then examine
# perform a left_join and examine how networks change over time.
# HA Networks
## Competence Network

# Get unique Team_Ids
team_ids <- unique(ha_comp_network$Team_Id)

# Create an empty list to store network objects
network_objects <- list()

for (team in team_ids) {
  # Filter data for current team
  team_data <- ha_comp_network %>% 
    filter(Team_Id == team) %>%
    select(-Team_Id)  # Remove Team_Id column
  
  # Create matrix for the team
  # Assuming the remaining columns (except Role) represent the network data
  network_matrix <- as.matrix(team_data %>% select(-Role))
  
  # Set row and column names to Role
  rownames(network_matrix) <- team_data$Role
  colnames(network_matrix) <- team_data$Role
  
  # Create network object
  net_object <- network(network_matrix, matrix.type = "adjacency", directed = TRUE)
  
  # Add Role as vertex attribute
  set.vertex.attribute(net_object, "role", team_data$Role)
  
  # Store the network object in the list with a name that starts with Team_Id
  network_objects[[paste0(team, "_ha_comp_network")]] <- net_object
  
  # Print confirmation
  # print(paste("Created network object for Team", team))
}

# Function to calculate network metrics
calculate_network_metrics <- function(net) {
  list(
    density = network.density(net),
    avg_degree = mean(degree(net, gmode = "graph")),
    centralization = centralization(net, degree, normalize = TRUE),
    avg_path_length = mean(geodist(net)$gdist[geodist(net)$gdist != Inf]),
    avg_betweenness = mean(betweenness(net)),
    avg_closeness = mean(closeness(net))
  )
}

# Initialize an empty list to store results
results_list <- list()

# Loop through each network object
for (team_name in names(network_objects)) {
  net <- network_objects[[team_name]]
  
  # Split the name into Team_Id and network type
  name_parts <- str_split(team_name, "_", n = 2)[[1]]
  team_id <- name_parts[1]
  network_type <- name_parts[2]
  
  # Calculate metrics
  metrics <- calculate_network_metrics(net)
  
  # Add Team_Id and network type to metrics
  metrics$Team_Id <- team_id
  metrics$network_type <- network_type
  
  # Add to results list
  results_list[[team_name]] <- metrics
}

# Convert list of results to a dataframe
ha_comp_network_metrics_df <- bind_rows(results_list)

# Reorder columns to have Team_Id first
ha_comp_network_metrics_df <- ha_comp_network_metrics_df %>%
  select(Team_Id, everything()) %>% 
  rename_with( ~ paste0("ha_comp_", .x))

## Working Together Network
# Get unique Team_Ids
team_ids <- unique(ha_work_network$Team_Id)

# Create an empty list to store network objects
network_objects <- list()

for (team in team_ids) {
  # Filter data for current team
  team_data <- ha_work_network %>% 
    filter(Team_Id == team) %>%
    select(-Team_Id)  # Remove Team_Id column
  
  # Create matrix for the team
  # Assuming the remaining columns (except Role) represent the network data
  network_matrix <- as.matrix(team_data %>% select(-Role))
  
  # Set row and column names to Role
  rownames(network_matrix) <- team_data$Role
  colnames(network_matrix) <- team_data$Role
  
  # Create network object
  net_object <- network(network_matrix, matrix.type = "adjacency", directed = TRUE)
  
  # Add Role as vertex attribute
  set.vertex.attribute(net_object, "role", team_data$Role)
  
  # Store the network object in the list with a name that starts with Team_Id
  network_objects[[paste0(team, "_ha_work_network")]] <- net_object
  
  # Print confirmation
  # print(paste("Created network object for Team", team))
}

# Function to calculate network metrics
calculate_network_metrics <- function(net) {
  list(
    density = network.density(net),
    avg_degree = mean(degree(net, gmode = "graph")),
    centralization = centralization(net, degree, normalize = TRUE),
    avg_path_length = mean(geodist(net)$gdist[geodist(net)$gdist != Inf]),
    avg_betweenness = mean(betweenness(net)),
    avg_closeness = mean(closeness(net))
  )
}

# Initialize an empty list to store results
results_list <- list()

# Loop through each network object
for (team_name in names(network_objects)) {
  net <- network_objects[[team_name]]
  
  # Split the name into Team_Id and network type
  name_parts <- str_split(team_name, "_", n = 2)[[1]]
  team_id <- name_parts[1]
  network_type <- name_parts[2]
  
  # Calculate metrics
  metrics <- calculate_network_metrics(net)
  
  # Add Team_Id and network type to metrics
  metrics$Team_Id <- team_id
  metrics$network_type <- network_type
  
  # Add to results list
  results_list[[team_name]] <- metrics
}

# Convert list of results to a dataframe
ha_work_network_metrics_df <- bind_rows(results_list)

# Reorder columns to have Team_Id first
ha_work_network_metrics_df <- ha_work_network_metrics_df %>%
  select(Team_Id, everything()) %>% 
  rename_with( ~ paste0("ha_work_", .x))

## Information Sharing Network

team_ids <- unique(ha_info_network$Team_Id)

# Create an empty list to store network objects
network_objects <- list()

for (team in team_ids) {
  # Filter data for current team
  team_data <- ha_info_network %>% 
    filter(Team_Id == team) %>%
    select(-Team_Id)  # Remove Team_Id column
  
  # Create matrix for the team
  # Assuming the remaining columns (except Role) represent the network data
  network_matrix <- as.matrix(team_data %>% select(-Role))
  
  # Set row and column names to Role
  rownames(network_matrix) <- team_data$Role
  colnames(network_matrix) <- team_data$Role
  
  # Create network object
  net_object <- network(network_matrix, matrix.type = "adjacency", directed = TRUE)
  
  # Add Role as vertex attribute
  set.vertex.attribute(net_object, "role", team_data$Role)
  
  # Store the network object in the list with a name that starts with Team_Id
  network_objects[[paste0(team, "_ha_info_network")]] <- net_object
  
  # Print confirmation
  # print(paste("Created network object for Team", team))
}

# Function to calculate network metrics
calculate_network_metrics <- function(net) {
  list(
    density = network.density(net),
    avg_degree = mean(degree(net, gmode = "graph")),
    centralization = centralization(net, degree, normalize = TRUE),
    avg_path_length = mean(geodist(net)$gdist[geodist(net)$gdist != Inf]),
    avg_betweenness = mean(betweenness(net)),
    avg_closeness = mean(closeness(net))
  )
}

# Initialize an empty list to store results
results_list <- list()

# Loop through each network object
for (team_name in names(network_objects)) {
  net <- network_objects[[team_name]]
  
  # Split the name into Team_Id and network type
  name_parts <- str_split(team_name, "_", n = 2)[[1]]
  team_id <- name_parts[1]
  network_type <- name_parts[2]
  
  # Calculate metrics
  metrics <- calculate_network_metrics(net)
  
  # Add Team_Id and network type to metrics
  metrics$Team_Id <- team_id
  metrics$network_type <- network_type
  
  # Add to results list
  results_list[[team_name]] <- metrics
}

# Convert list of results to a dataframe
ha_info_network_metrics_df <- bind_rows(results_list)

# Reorder columns to have Team_Id first
ha_info_network_metrics_df <- ha_info_network_metrics_df %>%
  select(Team_Id, everything()) %>% 
  rename_with( ~ paste0("ha_info_", .x))

# SR Networks
## Competence Network

# Get unique Team_Ids
team_ids <- unique(sr_comp_network$Team_Id)

# Create an empty list to store network objects
network_objects <- list()

for (team in team_ids) {
  # Filter data for current team
  team_data <- sr_comp_network %>% 
    filter(Team_Id == team) %>%
    select(-Team_Id)  # Remove Team_Id column
  
  # Create matrix for the team
  # Assuming the remaining columns (except Role) represent the network data
  network_matrix <- as.matrix(team_data %>% select(-Role))
  
  # Set row and column names to Role
  rownames(network_matrix) <- team_data$Role
  colnames(network_matrix) <- team_data$Role
  
  # Create network object
  net_object <- network(network_matrix, matrix.type = "adjacency", directed = TRUE)
  
  # Add Role as vertex attribute
  set.vertex.attribute(net_object, "role", team_data$Role)
  
  # Store the network object in the list with a name that starts with Team_Id
  network_objects[[paste0(team, "_sr_comp_network")]] <- net_object
  
  # Print confirmation
  # print(paste("Created network object for Team", team))
}

# Function to calculate network metrics
calculate_network_metrics <- function(net) {
  list(
    density = network.density(net),
    avg_degree = mean(degree(net, gmode = "graph")),
    centralization = centralization(net, degree, normalize = TRUE),
    avg_path_length = mean(geodist(net)$gdist[geodist(net)$gdist != Inf]),
    avg_betweenness = mean(betweenness(net)),
    avg_closeness = mean(closeness(net))
  )
}

# Initialize an empty list to store results
results_list <- list()

# Loop through each network object
for (team_name in names(network_objects)) {
  net <- network_objects[[team_name]]
  
  # Split the name into Team_Id and network type
  name_parts <- str_split(team_name, "_", n = 2)[[1]]
  team_id <- name_parts[1]
  network_type <- name_parts[2]
  
  # Calculate metrics
  metrics <- calculate_network_metrics(net)
  
  # Add Team_Id and network type to metrics
  metrics$Team_Id <- team_id
  metrics$network_type <- network_type
  
  # Add to results list
  results_list[[team_name]] <- metrics
}

# Convert list of results to a dataframe
sr_comp_network_metrics_df <- bind_rows(results_list)

# Reorder columns to have Team_Id first
sr_comp_network_metrics_df <- sr_comp_network_metrics_df %>%
  select(Team_Id, everything()) %>% 
  rename_with( ~ paste0("sr_comp_", .x))

## Working Together Network
# Get unique Team_Ids
team_ids <- unique(sr_work_network$Team_Id)

# Create an empty list to store network objects
network_objects <- list()

for (team in team_ids) {
  # Filter data for current team
  team_data <- sr_work_network %>% 
    filter(Team_Id == team) %>%
    select(-Team_Id)  # Remove Team_Id column
  
  # Create matrix for the team
  # Assuming the remaining columns (except Role) represent the network data
  network_matrix <- as.matrix(team_data %>% select(-Role))
  
  # Set row and column names to Role
  rownames(network_matrix) <- team_data$Role
  colnames(network_matrix) <- team_data$Role
  
  # Create network object
  net_object <- network(network_matrix, matrix.type = "adjacency", directed = TRUE)
  
  # Add Role as vertex attribute
  set.vertex.attribute(net_object, "role", team_data$Role)
  
  # Store the network object in the list with a name that starts with Team_Id
  network_objects[[paste0(team, "_sr_work_network")]] <- net_object
  
  # Print confirmation
  # print(paste("Created network object for Team", team))
}

# Function to calculate network metrics
calculate_network_metrics <- function(net) {
  list(
    density = network.density(net),
    avg_degree = mean(degree(net, gmode = "graph")),
    centralization = centralization(net, degree, normalize = TRUE),
    avg_path_length = mean(geodist(net)$gdist[geodist(net)$gdist != Inf]),
    avg_betweenness = mean(betweenness(net)),
    avg_closeness = mean(closeness(net))
  )
}

# Initialize an empty list to store results
results_list <- list()

# Loop through each network object
for (team_name in names(network_objects)) {
  net <- network_objects[[team_name]]
  
  # Split the name into Team_Id and network type
  name_parts <- str_split(team_name, "_", n = 2)[[1]]
  team_id <- name_parts[1]
  network_type <- name_parts[2]
  
  # Calculate metrics
  metrics <- calculate_network_metrics(net)
  
  # Add Team_Id and network type to metrics
  metrics$Team_Id <- team_id
  metrics$network_type <- network_type
  
  # Add to results list
  results_list[[team_name]] <- metrics
}

# Convert list of results to a dataframe
sr_work_network_metrics_df <- bind_rows(results_list)

# Reorder columns to have Team_Id first
sr_work_network_metrics_df <- sr_work_network_metrics_df %>%
  select(Team_Id, everything()) %>% 
  rename_with( ~ paste0("sr_work_", .x))

## Information Sharing Network

team_ids <- unique(sr_info_network$Team_Id)

# Create an empty list to store network objects
network_objects <- list()

for (team in team_ids) {
  # Filter data for current team
  team_data <- sr_info_network %>% 
    filter(Team_Id == team) %>%
    select(-Team_Id)  # Remove Team_Id column
  
  # Create matrix for the team
  # Assuming the remaining columns (except Role) represent the network data
  network_matrix <- as.matrix(team_data %>% select(-Role))
  
  # Set row and column names to Role
  rownames(network_matrix) <- team_data$Role
  colnames(network_matrix) <- team_data$Role
  
  # Create network object
  net_object <- network(network_matrix, matrix.type = "adjacency", directed = TRUE)
  
  # Add Role as vertex attribute
  set.vertex.attribute(net_object, "role", team_data$Role)
  
  # Store the network object in the list with a name that starts with Team_Id
  network_objects[[paste0(team, "_sr_info_network")]] <- net_object
  
  # Print confirmation
  # print(paste("Created network object for Team", team))
}

# Function to calculate network metrics
calculate_network_metrics <- function(net) {
  list(
    density = network.density(net),
    avg_degree = mean(degree(net, gmode = "graph")),
    centralization = centralization(net, degree, normalize = TRUE),
    avg_path_length = mean(geodist(net)$gdist[geodist(net)$gdist != Inf]),
    avg_betweenness = mean(betweenness(net)),
    avg_closeness = mean(closeness(net))
  )
}

# Initialize an empty list to store results
results_list <- list()

# Loop through each network object
for (team_name in names(network_objects)) {
  net <- network_objects[[team_name]]
  
  # Split the name into Team_Id and network type
  name_parts <- str_split(team_name, "_", n = 2)[[1]]
  team_id <- name_parts[1]
  network_type <- name_parts[2]
  
  # Calculate metrics
  metrics <- calculate_network_metrics(net)
  
  # Add Team_Id and network type to metrics
  metrics$Team_Id <- team_id
  metrics$network_type <- network_type
  
  # Add to results list
  results_list[[team_name]] <- metrics
}

# Convert list of results to a dataframe
sr_info_network_metrics_df <- bind_rows(results_list)

# Reorder columns to have Team_Id first
sr_info_network_metrics_df <- sr_info_network_metrics_df %>%
  select(Team_Id, everything()) %>% 
  rename_with( ~ paste0("sr_info_", .x))

# ES Networks
## Competence Network

# Get unique Team_Ids
team_ids <- unique(es_comp_network$Team_Id)

# Create an empty list to store network objects
network_objects <- list()

for (team in team_ids) {
  # Filter data for current team
  team_data <- es_comp_network %>% 
    filter(Team_Id == team) %>%
    select(-Team_Id)  # Remove Team_Id column
  
  # Create matrix for the team
  # Assuming the remaining columns (except Role) represent the network data
  network_matrix <- as.matrix(team_data %>% select(-Role))
  
  # Set row and column names to Role
  rownames(network_matrix) <- team_data$Role
  colnames(network_matrix) <- team_data$Role
  
  # Create network object
  net_object <- network(network_matrix, matrix.type = "adjacency", directed = TRUE)
  
  # Add Role as vertex attribute
  set.vertex.attribute(net_object, "role", team_data$Role)
  
  # Store the network object in the list with a name that starts with Team_Id
  network_objects[[paste0(team, "_es_comp_network")]] <- net_object
  
  # Print confirmation
  # print(paste("Created network object for Team", team))
}

# Function to calculate network metrics
calculate_network_metrics <- function(net) {
  list(
    density = network.density(net),
    avg_degree = mean(degree(net, gmode = "graph")),
    centralization = centralization(net, degree, normalize = TRUE),
    avg_path_length = mean(geodist(net)$gdist[geodist(net)$gdist != Inf]),
    avg_betweenness = mean(betweenness(net)),
    avg_closeness = mean(closeness(net))
  )
}

# Initialize an empty list to store results
results_list <- list()

# Loop through each network object
for (team_name in names(network_objects)) {
  net <- network_objects[[team_name]]
  
  # Split the name into Team_Id and network type
  name_parts <- str_split(team_name, "_", n = 2)[[1]]
  team_id <- name_parts[1]
  network_type <- name_parts[2]
  
  # Calculate metrics
  metrics <- calculate_network_metrics(net)
  
  # Add Team_Id and network type to metrics
  metrics$Team_Id <- team_id
  metrics$network_type <- network_type
  
  # Add to results list
  results_list[[team_name]] <- metrics
}

# Convert list of results to a dataframe
es_comp_network_metrics_df <- bind_rows(results_list)

# Reorder columns to have Team_Id first
es_comp_network_metrics_df <- es_comp_network_metrics_df %>%
  select(Team_Id, everything()) %>% 
  rename_with( ~ paste0("es_comp_", .x))

## Working Together Network
# Get unique Team_Ids
team_ids <- unique(es_work_network$Team_Id)

# Create an empty list to store network objects
network_objects <- list()

for (team in team_ids) {
  # Filter data for current team
  team_data <- es_work_network %>% 
    filter(Team_Id == team) %>%
    select(-Team_Id)  # Remove Team_Id column
  
  # Create matrix for the team
  # Assuming the remaining columns (except Role) represent the network data
  network_matrix <- as.matrix(team_data %>% select(-Role))
  
  # Set row and column names to Role
  rownames(network_matrix) <- team_data$Role
  colnames(network_matrix) <- team_data$Role
  
  # Create network object
  net_object <- network(network_matrix, matrix.type = "adjacency", directed = TRUE)
  
  # Add Role as vertex attribute
  set.vertex.attribute(net_object, "role", team_data$Role)
  
  # Store the network object in the list with a name that starts with Team_Id
  network_objects[[paste0(team, "_es_work_network")]] <- net_object
  
  # Print confirmation
  # print(paste("Created network object for Team", team))
}

# Function to calculate network metrics
calculate_network_metrics <- function(net) {
  list(
    density = network.density(net),
    avg_degree = mean(degree(net, gmode = "graph")),
    centralization = centralization(net, degree, normalize = TRUE),
    avg_path_length = mean(geodist(net)$gdist[geodist(net)$gdist != Inf]),
    avg_betweenness = mean(betweenness(net)),
    avg_closeness = mean(closeness(net))
  )
}

# Initialize an empty list to store results
results_list <- list()

# Loop through each network object
for (team_name in names(network_objects)) {
  net <- network_objects[[team_name]]
  
  # Split the name into Team_Id and network type
  name_parts <- str_split(team_name, "_", n = 2)[[1]]
  team_id <- name_parts[1]
  network_type <- name_parts[2]
  
  # Calculate metrics
  metrics <- calculate_network_metrics(net)
  
  # Add Team_Id and network type to metrics
  metrics$Team_Id <- team_id
  metrics$network_type <- network_type
  
  # Add to results list
  results_list[[team_name]] <- metrics
}

# Convert list of results to a dataframe
es_work_network_metrics_df <- bind_rows(results_list)

# Reorder columns to have Team_Id first
es_work_network_metrics_df <- es_work_network_metrics_df %>%
  select(Team_Id, everything()) %>% 
  rename_with( ~ paste0("es_work_", .x))

## Information Sharing Network

team_ids <- unique(es_info_network$Team_Id)

# Create an empty list to store network objects
network_objects <- list()

for (team in team_ids) {
  # Filter data for current team
  team_data <- es_info_network %>% 
    filter(Team_Id == team) %>%
    select(-Team_Id)  # Remove Team_Id column
  
  # Create matrix for the team
  # Assuming the remaining columns (except Role) represent the network data
  network_matrix <- as.matrix(team_data %>% select(-Role))
  
  # Set row and column names to Role
  rownames(network_matrix) <- team_data$Role
  colnames(network_matrix) <- team_data$Role
  
  # Create network object
  net_object <- network(network_matrix, matrix.type = "adjacency", directed = TRUE)
  
  # Add Role as vertex attribute
  set.vertex.attribute(net_object, "role", team_data$Role)
  
  # Store the network object in the list with a name that starts with Team_Id
  network_objects[[paste0(team, "_es_info_network")]] <- net_object
  
  # Print confirmation
  # print(paste("Created network object for Team", team))
}

# Function to calculate network metrics
calculate_network_metrics <- function(net) {
  list(
    density = network.density(net),
    avg_degree = mean(degree(net, gmode = "graph")),
    centralization = centralization(net, degree, normalize = TRUE),
    avg_path_length = mean(geodist(net)$gdist[geodist(net)$gdist != Inf]),
    avg_betweenness = mean(betweenness(net)),
    avg_closeness = mean(closeness(net))
  )
}

# Initialize an empty list to store results
results_list <- list()

# Loop through each network object
for (team_name in names(network_objects)) {
  net <- network_objects[[team_name]]
  
  # Split the name into Team_Id and network type
  name_parts <- str_split(team_name, "_", n = 2)[[1]]
  team_id <- name_parts[1]
  network_type <- name_parts[2]
  
  # Calculate metrics
  metrics <- calculate_network_metrics(net)
  
  # Add Team_Id and network type to metrics
  metrics$Team_Id <- team_id
  metrics$network_type <- network_type
  
  # Add to results list
  results_list[[team_name]] <- metrics
}

# Convert list of results to a dataframe
es_info_network_metrics_df <- bind_rows(results_list)

# Reorder columns to have Team_Id first
es_info_network_metrics_df <- es_info_network_metrics_df %>%
  select(Team_Id, everything()) %>% 
  rename_with( ~ paste0("es_info_", .x))


# Combine network metrics
full_networks <- cbind(ha_comp_network_metrics_df, 
                       ha_info_network_metrics_df, 
                       ha_work_network_metrics_df,
                       sr_comp_network_metrics_df, 
                       sr_info_network_metrics_df, 
                       sr_work_network_metrics_df,
                       es_comp_network_metrics_df, 
                       es_info_network_metrics_df, 
                       es_work_network_metrics_df)

# Merge network metrics with df
full_networks <- full_networks %>% rename("Team_Id" = ha_comp_Team_Id)
df_merged <- left_join(df, full_networks, by = "Team_Id") %>% 
  select(-ends_with('network_type'))

write.csv(df_merged, "output/msu_ari_networks_performance.csv", row.names = F)
