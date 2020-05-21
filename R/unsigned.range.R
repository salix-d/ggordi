# Function to get the range:
unsigned.range = function(x) c(-abs(min(x, na.rm = TRUE)),
                               abs(max(x, na.rm = TRUE)))
