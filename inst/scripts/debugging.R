

ids <- c(23820523, 23820523)
runs <- c(130390319, 130390319)
result <- await_all(scripts_get_r_runs, .x = ids, .y = runs)
print(result)
