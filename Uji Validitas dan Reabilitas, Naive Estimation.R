library(readxl)
library(psych)

# IMPORT DATA
data <- read_excel("C:/Users/Yani Ofa Salika/Documents/Akademik/Semester 4/Tugas/Teknik Sampling/Data Survey buat R.xlsx")
View(data)
# Melihat nama variabel
names(data)
# Melihat data
View(data)


# UJI VALIDITAS
# Ambil item kuesioner
item <- data[, c("P1","P2","P3","P4","P5","P6","P7","P8","P9","P10")]
# Uji validitas
corr.test(item)
# Skor total
total <- rowSums(item)
# Korelasi item-total
cor(item, total)

# UJI REABILITAS
# Uji reliabilitas Cronbach Alpha
alpha(item)$total
# Menampilkan hasil cronbach Alphanya saja
alpha(item)$total$raw_alpha

# Tampilkan Presentase 
lapply(item, function(x) prop.table(table(x))*100)

# Hitung jumlah jawaban puas (4 dan 5)
puas <- sum(item == 4 | item == 5)

# Hitung total seluruh jawaban
total_jawaban <- length(as.matrix(item))

# Naive Estimate
naive <- puas / total_jawaban

# Tampilkan hasil
naive