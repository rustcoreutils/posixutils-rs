BEGIN { s = "aéb"; gsub(/a*/, "X", s); print s }
