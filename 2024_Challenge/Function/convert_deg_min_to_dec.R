convert_deg_min_to_dec <- function(coord) {
  # Return NA for missing or empty values
  ifelse(
    coord == "" | is.na(coord),
    NA,
    # Split the coordinate into parts
    sapply(coord, function(x) {
      parts <- unlist(strsplit(x, " "))
      
      if (length(parts) == 2) {
        # Handle degrees and minutes (e.g., "25 38")
        degrees <- as.numeric(parts[1])
        minutes <- as.numeric(parts[2])
        return(degrees + (minutes / 60))
      } else if (length(parts) == 3) {
        # Handle degrees, minutes, and seconds (e.g., "25 38 15")
        degrees <- as.numeric(parts[1])
        minutes <- as.numeric(parts[2])
        seconds <- as.numeric(parts[3])
        return(degrees + (minutes / 60) + (seconds / 3600))
      } else {
        return(NA)  # Handle unexpected formats
      }
    })
  )
}
