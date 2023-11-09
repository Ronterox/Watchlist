data_str <- readLines("watchlist.txt")

headers <- c()
num_list <- list()

for (line in data_str) {
  if (line == "") {
    next
  }

  line_vec <- strsplit(line, " ")[[1]]

  if (is.na(suppressWarnings(as.numeric(line_vec[1])))) {
    headers <- c(headers, line)
  } else {
    nums <- as.numeric(line_vec)
    num_list <- c(num_list, list(nums))
  }
}

df <- data.frame(
  headers = rep(headers, lengths(num_list)),
  nums = unlist(num_list)
)

head(df)
# Create dataframe of only headers Videos
df_videos <- df[grepl("Videos", df$headers), ]

vids <- sort(df$nums)
frecuency <- table(vids)

actual_times <- c()

for (i in seq_along(frecuency)) {
  frecuency_times_value <- as.numeric(names(frecuency)[i]) * frecuency[[i]]
  actual_times <- c(actual_times, frecuency_times_value)
}

print_total_time <- function(time_list, start = 0, end = Inf) {
  total_minutes <- sum(time_list)
  total_hours <- total_minutes / 60
  total_days <- total_hours / 24

  print_formatted <- function(number, name) {
    print(paste0(format(number, digits = 2), " ", name))
  }

  print_formatted(total_minutes, "minutes")
  print_formatted(total_hours, "hours")
  print_formatted(total_days, "days")
  print(paste0("Total categories: ", length(time_list)))
  print(paste0("Total videos: ", length(vids[vids < end & vids >= start])))
}

print_total_time(actual_times)

df <- data.frame(
  minute = as.numeric(names(frecuency)),
  frecuency = as.numeric(frecuency),
  total_minutes = actual_times
)

see_range <- function(start = 0, end = Inf) {
  print_total_time(df[df$minute < end & df$minute >= start, 3], start, end)
}

see_range(0, 10)
see_range(10, 20)
see_range(30, 40)
see_range(60, 120)
see_range(120)
