# Ambil item kuesioner
item <- data[, c("P1","P2","P3","P4","P5",
                 "P6","P7","P8","P9","P10")]

# RESPONDEN LAKI-LAKI
laki <- data[data$`Jenis Kelamin` == "Laki - Laki", 
             c("P1","P2","P3","P4","P5",
               "P6","P7","P8","P9","P10")]

# Hitung proporsi puas laki-laki
puas_laki <- sum(laki == 4 | laki == 5)
total_laki <- length(as.matrix(laki))

p_laki <- puas_laki / total_laki

# RESPONDEN PEREMPUAN
perempuan <- data[data$`Jenis Kelamin` == "Perempuan", 
                  c("P1","P2","P3","P4","P5",
                    "P6","P7","P8","P9","P10")]
# Hitung proporsi puas perempuan
puas_perempuan <- sum(perempuan == 4 | perempuan == 5)
total_perempuan <- length(as.matrix(perempuan))

p_perempuan <- puas_perempuan / total_perempuan

# WEIGHTED ESTIMATE
weighted <- (0.15 * p_laki) + (0.85 * p_perempuan)

# Tampilkan hasil
weighted