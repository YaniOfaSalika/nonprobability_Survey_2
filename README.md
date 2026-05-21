# nonprobability-survey
# Analisis Kepuasan Mahasiswa pada SIAKAD di Prodi Statistika Universitas Mataram

## Latar Belakang 
Perkembangan teknologi informasi mendorong perguruan tinggi untuk meningkatkan kualitas layanan akademik melalui sistem digital, salah satunya Sistem Informasi Akademik (SIAKAD). Sistem ini digunakan untuk mempermudah berbagai aktivitas akademik seperti pengisian KRS, akses nilai, dan penyampaian informasi perkuliahan. Keberadaan SIAKAD diharapkan mampu memberikan layanan yang cepat, akurat, dan efisien bagi mahasiswa. Namun, efektivitas penggunaan SIAKAD sangat bergantung pada tingkat kepuasan pengguna. Oleh karena itu, perlu dilakukan evaluasi untuk mengetahui sejauh mana sistem ini telah memenuhi kebutuhan mahasiswa. Penelitian ini bertujuan untuk menganalisis tingkat kepuasan mahasiswa terhadap SIAKAD di Universitas Mataram menggunakan pendekatan deskriptif serta metode naive estimation dan weighted estimation.

## Tujuan
Penelitian ini bertujuan untuk mengetahui tingkat kepuasan mahasiswa terhadap penggunaan Sistem Informasi Akademik (SIAKAD) di Universitas Mataram, menganalisis data menggunakan pendekatan deskriptif, serta membandingkan hasil estimasi kepuasan melalui metode naive estimation dan weighted estimation.

## Metode Penelitian
Penelitian ini menggunakan pendekatan kuantitatif deskriptif dengan populasi sebanyak 154 mahasiswa Program Studi Statistika FMIPA Universitas Mataram. Penentuan sampel menggunakan rumus Slovin dengan tingkat kesalahan 10% sehingga diperoleh 62 responden. Teknik pengambilan sampel yang digunakan adalah non-probability sampling dengan metode convenience sampling. Data dikumpulkan melalui kuesioner online dan dianalisis menggunakan analisis deskriptif, uji validitas, uji reliabilitas, serta metode naive estimation dan weighted estimation.

## Variabel Penelitian
### Variabel yang digunakan dalam penelitian ini adalah:
#### Variabel X:
Aspek-aspek kualitas SIAKAD, yaitu:
- P1 (Kemudahan penggunaan)
- P2 (Tampilan sistem)
- P3 (Kemudahan akses menu)
- P4 (Kecepatan sistem)
- P5 (Kestabilan sistem)
- P6 (Kelancaran KRS dan akses nilai)
- P7 (Keakuratan informasi)
- P8 (Ketepatan waktu informasi)
- P9 (Manfaat sistem)
#### Variabel Y:
- P10 (Kepuasan mahasiswa terhadap penggunaan SIAKAD)

## Tahapan Analisis Data
### 1. Penentuan Ukuran Sampel dengan Rumus Slovin
Ukuran sampel dihitung menggunakan rumus Slovin dengan tingkat kesalahan 10%.

```r
# Populasi
N <- 154
# Tingkat kesalahan
e <- 0.1
# Rumus Slovin
n <- N / (1 + N * e^2)
# Hasil
n
# Pembulatan
round(n)
```

### 2. Import Data
Data hasil survei diimpor ke dalam R menggunakan package readxl.
library(readxl)

```r
library(readxl)
data <- read_excel("C:/Users/Yani Ofa Salika/Documents/Akademik/Semester 4/Tugas/Teknik Sampling/Data Survey buat R.xlsx")
```

### 3. Uji Validitas 
Uji validitas dilakukan untuk mengetahui apakah setiap item pertanyaan mampu mengukur variabel penelitian dengan baik.

```r
library(psych)
item <- data[, c("P1","P2","P3","P4","P5",
                 "P6","P7","P8","P9","P10")]
corr.test(item)
total <- rowSums(item)
cor(item, total)
```

### 4. Uji Reabilitas
Uji reliabilitas dilakukan menggunakan Cronbach Alpha untuk mengetahui konsistensi instrumen penelitian.
```r
alpha(item)$total
alpha(item)$total$raw_alpha
```

### 5. Analisis Deskriptif
Analisis deskriptif digunakan untuk mengetahui distribusi jawaban responden.
```r
lapply(item, function(x)
  prop.table(table(x))*100)
```

### 6. Distribusi Responden Berdasarkan Gender
```r
table(data$`Jenis Kelamin`)
```
#### Diagram Batang
```r
barplot(
  table(data$`Jenis Kelamin`),
  main = "Distribusi Responden Berdasarkan Gender",
  xlab = "Gender",
  ylab = "Frekuensi",
  col = "lightblue"
)
```
#### Pie Chart
```r
pie(
  table(data$`Jenis Kelamin`),
  main = "Distribusi Responden Berdasarkan Gender",
  col = c("lightblue","pink")
)
```

### 7. Naive Estimation
Naive estimation digunakan untuk menghitung estimasi tingkat kepuasan mahasiswa tanpa pembobotan.
Responden dianggap puas apabila memberikan jawaban kategori 4 dan 5.
```r
puas <- sum(item == 4 | item == 5)
total_jawaban <- length(as.matrix(item))
naive <- puas / total_jawaban
naive
naive * 100
```

### 8. Weighted Estimation
Weighted estimation dilakukan dengan mempertimbangkan proporsi gender dalam populasi.
```r
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
```

## Hasil dan Pembahasan 
### Uji Validitas  

| No | Item | r Hitung | r Tabel (n=31, α=0.05) | Keterangan |
|:--:|:----:|:--------:|:----------------------:|:----------:|
| 1  | P1   | 0.662    | 0.250                  | Valid      |
| 2  | P2   | 0.747    | 0.250                  | Valid      |
| 3  | P3   | 0.788    | 0.250                  | Valid      |
| 4  | P4   | 0.773    | 0.250                  | Valid      |
| 5  | P5   | 0.831    | 0.250                  | Valid      |
| 6  | P6   | 0.737    | 0.250                  | Valid      |
| 7  | P7   | 0.811    | 0.250                  | Valid      |
| 8  | P8   | 0.773    | 0.250                  | Valid      |
| 9  | P9   | 0.796    | 0.250                  | Valid      |
| 10 | P10  | 0.793    | 0.250                  | Valid      |

Berdasarkan hasil uji validitas, seluruh item pertanyaan (P1–P10) memiliki nilai r hitung yang lebih besar dari r tabel (0,250). Hal ini menunjukkan bahwa setiap item mampu mengukur variabel kepuasan dengan baik, sehingga seluruh instrumen dinyatakan valid dan layak digunakan dalam penelitian.

### Uji Reabilitas 
| Cronbach’s Alpha | Jumlah Item | Keterangan        |
|:----------------:|:-----------:|:------------------:|
| 0.924            | 10          | Sangat Reliabel   |

Hasil uji reliabilitas menunjukkan nilai Cronbach’s Alpha sebesar 0,924, yang berada di atas batas minimum 0,7. Hal ini menunjukkan bahwa instrumen memiliki tingkat konsistensi yang sangat tinggi, sehingga dapat dikatakan sangat reliabel dan mampu memberikan hasil yang stabil.

### Analisis Deskriptif
Analisis deskriptif digunakan untuk menggambarkan distribusi jawaban responden melalui tabel frekuensi, persentase, dan grafik.
### Tabel Frekuensi Responden  
| Item | Sangat Tidak Puas | Tidak Puas | Netral | Puas | Sangat Puas |
|------|------------------|------------|--------|------|-------------|
| P1   | 0 | 0 | 16 | 38 | 8 |
| P2   | 0 | 3 | 16 | 33 | 10 |
| P3   | 0 | 1 | 14 | 35 | 12 |
| P4   | 0 | 1 | 21 | 30 | 10 |
| P5   | 0 | 2 | 21 | 29 | 10 |
| P6   | 0 | 1 | 13 | 34 | 14 |
| P7   | 0 | 0 | 12 | 37 | 13 |
| P8   | 0 | 2 | 11 | 39 | 10 |
| P9   | 0 | 1 | 12 | 34 | 15 |
| P10  | 0 | 1 | 11 | 37 | 13 |

Berdasarkan tabel frekuensi, kategori *puas* dan *sangat puas* mendominasi seluruh item pertanyaan (P1–P10). Kategori *netral* masih muncul namun lebih kecil, sedangkan *tidak puas* dan *sangat tidak puas* sangat minim.  Hal ini menunjukkan bahwa tingkat kepuasan mahasiswa terhadap SIAKAD tergolong tinggi.

### Tabel Persentase Tingjat Kepuasan Per Item 
| Item | Sangat Tidak Puas (%) | Tidak Puas (%) | Netral (%) | Puas (%) | Sangat Puas (%) |
|------|----------------------|----------------|------------|----------|-----------------|
| P1   | 0.0 | 0.0 | 25.8 | 61.3 | 12.9 |
| P2   | 0.0 | 4.8 | 25.8 | 53.2 | 16.7 |
| P3   | 0.0 | 1.6 | 22.6 | 56.5 | 19.4 |
| P4   | 0.0 | 1.6 | 33.9 | 48.4 | 16.7 |
| P5   | 0.0 | 0.0 | 33.9 | 48.4 | 16.7 |
| P6   | 0.0 | 0.0 | 21.0 | 54.8 | 22.6 |
| P7   | 0.0 | 0.0 | 19.4 | 62.9 | 16.1 |
| P8   | 0.0 | 3.2 | 17.7 | 62.9 | 16.1 |
| P9   | 0.0 | 1.6 | 19.4 | 54.8 | 24.2 |
| P10  | 0.0 | 1.6 | 17.7 | 59.7 | 21.0 |

Berdasarkan tabel persentase, kategori *puas* merupakan yang paling dominan pada seluruh item (P1–P10), diikuti oleh kategori *sangat puas*. Kategori *netral* masih muncul dalam jumlah sedang, sedangkan *tidak puas* dan *sangat tidak puas* memiliki persentase yang sangat kecil. Hal ini menunjukkan bahwa tingkat kepuasan mahasiswa terhadap SIAKAD berada pada kategori tinggi.

## Naive Estimation  
### Tabel Estimasi Naive  

| Metode Estimasi | Nilai Estimasi | Persentase | Keterangan |
|----------------|---------------|------------|------------|
| Naive Estimate | 0.743 | 74.35% | Mahasiswa cenderung puas |

Berdasarkan hasil naive estimation, diperoleh nilai sebesar **0,743** atau **74,35%**. Hal ini menunjukkan bahwa sebagian besar mahasiswa cenderung merasa puas terhadap penggunaan Sistem Informasi Akademik (SIAKAD) di Universitas Mataram.  Namun, nilai tersebut masih merupakan estimasi sederhana yang belum mempertimbangkan struktur populasi secara proporsional, sehingga masih terdapat potensi bias akibat penggunaan data sampel langsung tanpa pembobotan.

### Weighted estimation
### Diketahui  
Komposisi populasi terdiri dari:  
- Laki-laki = 30 (19.5%)  
- Perempuan = 124 (80.5%)  
Jumlah sampel:  
- n = 62  
- Laki-laki = 9  
- Perempuan = 53  
Kriteria kepuasan:  
- Skor 4 dan 5 = Puas dan Sangat Puas  

### Tabel Pembobotan Berdasarkan Gender  

| Gender     | Proporsi Kepuasan (4–5) | Proporsi Populasi | Weighted Contribution |
|------------|------------------------|-------------------|----------------------|
| Laki-laki  | 0.78                   | 0.195             | 0.152                |
| Perempuan  | 0.74                   | 0.805             | 0.596                |
| **Total Weighted Estimate** | - | - | **0.748 (74.8%)** | 

Berdasarkan hasil weighted estimation, tingkat kepuasan mahasiswa terhadap SIAKAD adalah sebesar **0,748 atau 74,8%**.  

Hasil ini diperoleh dengan mempertimbangkan perbedaan proporsi gender antara sampel dan populasi serta hanya menggunakan respon dengan kategori **puas (4)** dan **sangat puas (5)**.  

Kontribusi terbesar berasal dari responden perempuan sebesar 0,596, sedangkan responden laki-laki memberikan kontribusi sebesar 0,152 terhadap estimasi keseluruhan.

## Tabel Perbandingan Naive dan Weighted Estimation  

| Metode Estimasi     | Nilai Estimasi | Persentase |
|---------------------|----------------|------------|
| Naive Estimate      | 0,7435         | 74,35%     |
| Weighted Estimate   | 0,7480         | 74,8%      |

Berdasarkan hasil perbandingan, nilai weighted estimate sebesar 74,8% sedikit lebih tinggi dibandingkan naive estimate sebesar 74,35%.  

Hal ini menunjukkan bahwa setelah dilakukan pembobotan berdasarkan proporsi gender, tingkat kepuasan mahasiswa sedikit meningkat dan menjadi lebih representatif terhadap populasi. Perbedaan kedua nilai tersebut sangat kecil sehingga dapat disimpulkan bahwa hasil estimasi relatif stabil.

## Distribusi Responden Berdasarkan Gender
| Gender | Frekuensi | Persentase |
|--------|------------|-------------|
| Laki-laki | 9 | 14.5% |
| Perempuan | 53 | 85.5% |
| Total | 62 | 100% |

Berdasarkan hasil survei, mayoritas responden adalah perempuan sebanyak 53 orang (85,5%), sedangkan responden laki-laki sebanyak 9 orang (14,5%).

### Diagram Batang Distribusi Gender

Grafik batang menunjukkan bahwa jumlah responden perempuan lebih banyak dibandingkan responden laki-laki.

---

### Pie Chart Distribusi Gender

<img width="500" alt="Pie Chart Gender" src="LINK GAMBAR PIECHART" />

Pie chart menunjukkan bahwa proporsi responden perempuan mendominasi dibandingkan responden laki-laki.

## Kesimpulan

Berdasarkan hasil analisis yang telah dilakukan, diperoleh nilai naive estimate sebesar 74,35% dan weighted estimate sebesar 74,8%. Kedua hasil tersebut menunjukkan bahwa tingkat kepuasan mahasiswa terhadap kondisi lingkungan kampus FMIPA Universitas Mataram berada pada kategori tinggi.
Perbedaan antara naive estimate dan weighted estimate sangat kecil, namun weighted estimate dianggap lebih representatif karena telah mempertimbangkan proporsi gender dalam populasi. Dengan demikian, dapat disimpulkan bahwa mahasiswa cenderung puas terhadap kondisi lingkungan kampus.
