install.packages("dplyr")
library(readr)
library(psych)
library(dplyr)
library(ggpubr)
library(rstatix)
library(ggplot2)
library(skimr)
library(tidyr)
library(gmodels)

# input data
data <- read.csv("C:/Users/Muhammad Naufal Dipo/Documents/02 Kampus/01 Kuliah/06 Semester 6/06 APS/Projek UAS/Analisis Statistik/Data Primer_SMAN 21_Intensitas Siswa.csv", stringsAsFactors = FALSE)
head(data,5)
colnames(data)
str(data)

# Melihat ringkasan data yang lebih komprehensif dan ramah pengguna dengan skimr
cat("\nComprehensive summary with skimr::skim():\n")
skim(data)

# Melihat dimensi data (jumlah baris dan kolom)
cat(paste0("\nDataset dimensions: ", nrow(data), " rows, ", ncol(data), " columns.\n"))

# --- Definisikan Nama Kolom yang Telah Disanitasi ---
## Dimensi 1
kolom_timestamp <- "Timestamp"
kolom_email <- "Email.Address"
kolom_nama <- "Nama.Lengkap"
kolom_jk <- "Jenis.Kelamin" # Asumsi: "Jenis Kelamin" -> "Jenis.Kelamin"
kolom_kelas <- "Jenjang.Pendidikan..Kelas." # Asumsi: "Jenjang Pendidikan (Kelas)" -> "Jenjang.Pendidikan..Kelas."
kolom_hp_nomor <- "Nomor.Handphone"

## Dimensi 2
kolom_thp <- "Berapa.lama..dalam.jam..rata.rata.Anda.menggunakan.HP.setiap.hari."
kolom_aktvhp <- "Aktivitas.apa.yang.paling.sering.Anda.lakukan.saat.menggunakan.HP."
Kolom_bljrhp <- "Seberapa.sering.Anda.menggunakan.HP.untuk.keperluan.belajar.mencari.informasi.pendidikan."
kolom_penghp <- "Menurut.Anda..apakah.penggunaan.HP.lebih.banyak.mendukung.atau.mengganggu.aktivitas.belajar.Anda..." # Perhatikan elipsis di akhir

## Dimensi 3
kolom_tbk <- "Berapa.lama.rata.rata.Anda.membaca.buku.cetak.atau.digital..selain.komik.dan.konten.media.sosial..dalam.sehari."
kolom_jnsbk <- "Jenis.buku.apa.yang.paling.sering.Anda.baca."
kolom_mntbc <- "Seberapa.besar.minat.Anda.dalam.membaca.buku.di.luar.tugas.sekolah."
kolom_lingbc <- "Apakah.lingkungan.sekolah.mendorong.Anda.untuk.rajin.membaca.buku."

## Dimensi 4
kolom_pgrhhp_pdbk <- "Saya.merasa.penggunaan.HP.memiliki.pengaruh.terhadap.waktu.saya.untuk.membaca.buku."
kolom_fksbc <- "Saya.merasa.membaca.melalui.buku.cetak.membuat.saya.lebih.fokus.dan.mendalam.dalam.belajar.dibandingkan.dengan.membaca.melalui.digital.seperti.hp."
kolom_mnjmt <- "Saya.mampu.mengatur.waktu.antara.penggunaan.HP.dan.membaca.buku.dengan.baik."

item_names <- c(
  "Berapa.lama..dalam.jam..rata.rata.Anda.menggunakan.HP.setiap.hari.",
  "Berapa.lama.rata.rata.Anda.membaca.buku.cetak.atau.digital..selain.komik.dan.konten.media.sosial..dalam.sehari.")

## Mengubah kolom Penggunaan HP dan buku ke skala Likert 1-5
data_analisis_likert <- data %>%
  mutate(
    HP_Skor_Likert = case_when(
      !!sym(kolom_thp) == "< 1 jam" ~ 1,
      !!sym(kolom_thp) == "1 - 2 jam" ~ 2,
      !!sym(kolom_thp) == "2 - 3 jam" ~ 3,
      !!sym(kolom_thp) == "3 - 4 jam" ~ 4,
      !!sym(kolom_thp) == "> 4 jam" ~ 5,
      TRUE ~ NA_real_ # Mengelola nilai yang tidak cocok sebagai NA
    ),
    # Mengubah kolom Membaca Buku ke skala Likert 1-5
    Buku_Skor_Likert = case_when(
      !!sym(kolom_tbk) == "Tidak pernah" ~ 1,
      !!sym(kolom_tbk) == "< 15 menit" ~ 2,
      !!sym(kolom_tbk) == "15 – 30 menit" ~ 3,
      !!sym(kolom_tbk) == "30 – 60 menit" ~ 4,
      !!sym(kolom_tbk) == "> 1 jam" ~ 5,
      TRUE ~ NA_real_
    )
  ) %>%
  # Hapus baris di mana salah satu skor Likert adalah NA
  drop_na(HP_Skor_Likert, Buku_Skor_Likert) %>%
  # Hitung selisih skor
  mutate(Selisih_Skor = HP_Skor_Likert - Buku_Skor_Likert)

# --- Pemeriksaan Normalitas untuk HP_Skor_Likert ---
cat("--- Pemeriksaan Normalitas: HP_Skor_Likert ---\n")

# Menghitung Q-Q Plot
qq_hp <- ggqqplot(data_analisis_likert, x = "HP_Skor_Likert",
                  color = "steelblue",
                  title = "Q-Q Plot Skor Likert Penggunaan HP")
print(qq_hp)

# Uji Shapiro-Wilk
shapiro_hp <- shapiro_test(data_analisis_likert$HP_Skor_Likert)
print(shapiro_hp)
cat("\n\n")

# --- Pemeriksaan Normalitas untuk Buku_Skor_Likert ---
cat("--- Pemeriksaan Normalitas: Buku_Skor_Likert ---\n")

# Q-Q Plot
qq_buku <- ggqqplot(data_analisis_likert, x = "Buku_Skor_Likert",
                    color = "darkgreen",
                    title = "Q-Q Plot Skor Likert Membaca Buku")
print(qq_buku)

# Uji Shapiro-Wilk
shapiro_buku <- shapiro_test(data_analisis_likert$Buku_Skor_Likert)
print(shapiro_buku)
cat("\n\n")

# --- Pemeriksaan Normalitas untuk Selisih_Skor ---
cat("--- Pemeriksaan Normalitas: Selisih_Skor (HP - Buku) ---\n")

# Q-Q Plot
qq_selisih <- ggqqplot(data_analisis_likert, x = "Selisih_Skor",
                       color = "purple",
                       title = "Q-Q Plot Selisih Skor Likert (HP - Buku)")
print(qq_selisih)

# Uji Shapiro-Wilk
shapiro_selisih <- shapiro_test(data_analisis_likert$Selisih_Skor)
print(shapiro_selisih)

## Uji Wilcoxon Bertanda 
cat("\nHasil Uji Wilcoxon Bertanda (berpasangan) untuk Skor Likert:\n")
uji_wilcoxon_likert <- wilcox.test(data_analisis_likert$HP_Skor_Likert,
                                   data_analisis_likert$Buku_Skor_Likert,
                                   paired = TRUE)
print(uji_wilcoxon_likert)

# Mengumpulkan semua nama kolom yang perlu diubah menjadi faktor
faktor_kolom <- c(
  kolom_jk, kolom_kelas, 
  kolom_thp, kolom_aktvhp, Kolom_bljrhp, kolom_penghp,
  kolom_tbk, kolom_jnsbk, kolom_mntbc, kolom_lingbc,
  kolom_pgrhhp_pdbk, kolom_fksbc, kolom_mnjmt)

data_factor <- data %>%
  mutate(across(all_of(faktor_kolom), as.factor))

data_df_faktor <- data_factor %>%
  left_join(data_analisis_likert %>% select(Timestamp, HP_Skor_Likert, Buku_Skor_Likert, Selisih_Skor), by = "Timestamp")

print(head(data_df_faktor %>% select(HP_Skor_Likert, Buku_Skor_Likert, Selisih_Skor, !!sym(kolom_jk), !!sym(kolom_kelas))))
print(str(data_df_faktor %>% select(HP_Skor_Likert, Buku_Skor_Likert, Selisih_Skor, !!sym(kolom_jk), !!sym(kolom_kelas))))

cat("--- Statistik Deskriptif untuk Seluruh Dataset ---\n\n")

# Statistik Deskriptif untuk Kolom Numerik (Skor Likert yang telah ditransformasi)
cat(" Statistik Deskriptif untuk Skor Likert Numerik:\n")
data_analisis_likert %>%
  select(HP_Skor_Likert, Buku_Skor_Likert, Selisih_Skor) %>%
  get_summary_stats(type = "full") # Menggunakan type = "full" untuk detail lebih lanjut

# Statistik deskriptif untuk kolom kategorik
cat("Statistik Deskriptif untuk Kolom Kategorik :\n")
for (col_name in names(data_factor)) {
  # Memeriksa apakah kolom tersebut ada dalam daftar kolom_untuk_faktor
  # atau jika R secara otomatis membacanya sebagai faktor/karakter
  if (col_name %in% faktor_kolom || is.factor(data_factor[[col_name]]) || is.character(data_factor[[col_name]])) {
    cat(paste0("\nKolom: ", col_name, "\n"))
    print(table(data_factor[[col_name]]))
    cat("----------------------------------------\n")
  }
}

# Ringkasan Kolom Pengenal Unik/ID
cat("Ringkasan Kolom Pengenal Unik:\n")
cat(paste0("Jumlah total responden: ", nrow(data), "\n"))
cat(paste0("Jumlah responden setelah NA dihapus untuk skor Likert: ", nrow(data_analisis_likert), "\n"))
cat(paste0("Jumlah Email Unik: ", n_distinct(data$`Email Address`), "\n"))
cat(paste0("Jumlah Nama Unik: ", n_distinct(data$`Nama Lengkap`), "\n"))
cat(paste0("Jumlah Nomor HP Unik: ", n_distinct(data$`Nomor Handphone`), "\n"))

# --- Eksplorasi Data ---

# Eksplorasi Missing value
na_counts <- colSums(is.na(data_df_faktor))
print(na_counts[na_counts > 0]) # Hanya tampilkan kolom dengan NA
if (all(na_counts == 0)) {
  cat("Tidak ada nilai NA yang signifikan di dataset setelah transformasi dan penghapusan baris yang relevan.\n")
} else {
  cat("\nBeberapa kolom masih memiliki NA setelah pemrosesan. Pertimbangkan strategi penanganan NA lebih lanjut.\n")
}

# Histogram Buku
hist_buku <- gghistogram(data_analisis_likert, x = "Buku_Skor_Likert",
                         add = "mean", rug = TRUE,
                         color = "darkgreen", fill = "lightgreen",
                         title = "Distribusi Skor Likert Membaca Buku",
                         xlab = "Skor Likert Buku", ylab = "Frekuensi",
                         binwidth = 1)+
  scale_x_continuous(breaks = 1:5)
print(hist_buku)

# Histogram hp
hist_hp <- gghistogram(data_analisis_likert, x = "HP_Skor_Likert",
                       add = "mean", rug = TRUE,
                       color = "blue", fill = "lightblue",
                       title = "Distribusi Skor Likert Penggunaan HP",
                       xlab = "Skor Likert HP", ylab = "Frekuensi",
                       binwidth = 1)+
  scale_x_continuous(breaks = 1:5)
print(hist_hp)

# Histogram
hist_selisih <- gghistogram(data_analisis_likert, x = "Selisih_Skor",
                            add = "mean", rug = TRUE,
                            color = "purple", fill = "plum",
                            title = "Distribusi Selisih Skor Likert (HP - Buku)",
                            xlab = "Selisih Skor", ylab = "Frekuensi",
                            binwidth = 1)+
  scale_x_continuous(breaks = -4:4)
print(hist_selisih)

# --- Bar Plot untuk Variabel Kategorik ---

# Bar Plot: Jenis Kelamin
plot_bar_gender <- data_df_faktor %>% 
  ggplot(aes(x = Jenis.Kelamin, fill = Jenis.Kelamin)) + 
  geom_bar(stat = "count") +
  labs(title = "Distribusi Responden Berdasarkan Jenis Kelamin",
       x = "Jenis Kelamin", y = "Jumlah Responden") +
  theme_minimal() +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5)
print(plot_bar_gender)

# Bar Plot: Jenjang Pendidikan (Kelas)
plot_bar_kelas <- data_df_faktor %>% 
  ggplot(aes(x = Jenjang.Pendidikan..Kelas., fill = Jenjang.Pendidikan..Kelas.)) + 
  geom_bar(stat = "count") +
  labs(title = "Distribusi Responden Berdasarkan Jenjang Pendidikan",
       x = "Jenjang Pendidikan (Kelas)", y = "Jumlah Responden") +
  theme_minimal() +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5)
print(plot_bar_kelas)

# Bar Plot: Aktivitas HP yang Paling Sering Dilakukan
plot_bar_aktivitas_hp <- data_df_faktor %>% 
  ggplot(aes(x = Aktivitas.apa.yang.paling.sering.Anda.lakukan.saat.menggunakan.HP.,
             fill = Aktivitas.apa.yang.paling.sering.Anda.lakukan.saat.menggunakan.HP.)) + 
  geom_bar(stat = "count") +
  labs(title = "Distribusi Aktivitas Penggunaan HP",
       x = "Aktivitas", y = "Jumlah Responden") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5)
print(plot_bar_aktivitas_hp)

# Bar Plot: Jenis Buku yang Paling Sering Dibaca
plot_bar_jenis_buku <- data_df_faktor %>% # Menggunakan data_df_faktor
  ggplot(aes(x = Jenis.buku.apa.yang.paling.sering.Anda.baca.,
             fill = Jenis.buku.apa.yang.paling.sering.Anda.baca.)) + # ASUMSI: Nama dengan titik
  geom_bar(stat = "count") +
  labs(title = "Distribusi Jenis Buku yang Paling Sering Dibaca",
       x = "Jenis Buku", y = "Jumlah Responden") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5)
print(plot_bar_jenis_buku)

# Density Plot untuk Skor Likert Numerik (lebih halus dari Histogram)
plot_list_density <- list()
for (col_num in c("HP_Skor_Likert", "Buku_Skor_Likert", "Selisih_Skor")) {
  p_density <- ggdensity(data_df_faktor, x = col_num, add = "mean", rug = TRUE,
                         color = "darkred", fill = "salmon",
                         title = paste("Density Plot", gsub("_", " ", col_num)),
                         xlab = gsub("_", " ", col_num)) +
    scale_x_continuous(breaks = if(col_num == "Selisih_Skor") -4:4 else 1:5)
  print(p_density)
  plot_list_density[[col_num]] <- p_density # Simpan plot jika ingin menampilkan grid nanti
}

# Bar Plot untuk Semua Variabel Kategorik (Iterasi untuk semua kolom di faktor_kolom)
plot_list_all_bars <- list()
for (col_cat in faktor_kolom) {
  if (col_cat %in% names(data_df_faktor)) {
    p_bar <- data_df_faktor %>%
      ggplot(aes(x = !!sym(col_cat), fill = !!sym(col_cat))) +
      geom_bar(stat = "count") +
      labs(title = paste("Distribusi Responden Berdasarkan", gsub("\\.", " ", col_cat)),
           x = gsub("\\.", " ", col_cat), y = "Jumlah Responden") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5)
    print(p_bar)
    plot_list_all_bars[[col_cat]] <- p_bar
  }
}

# Hubungan antar dimensi
cat("Hubungan Antar Variabel Kategorik (Tabel Silang & Visualisasi):\n")

# Definisikan pasangan variabel kategorik untuk tabel silang dan plot
# Pilih beberapa pasangan yang paling relevan untuk eksplorasi Anda
categorical_pairs <- list(
  c(kolom_jk, kolom_kelas),
  c(kolom_aktvhp, kolom_mntbc),
  c(kolom_penghp, kolom_lingbc), # Contoh lain
  c(Kolom_bljrhp, kolom_pgrhhp_pdbk) # Contoh lain
)

for (pair in categorical_pairs) {
  var1_name <- pair[1]
  var2_name <- pair[2]
  
  if (var1_name %in% names(data_df_faktor) && var2_name %in% names(data_df_faktor)) {
    cat(paste0("\n--- Tabel Silang: ", gsub("\\.", " ", var1_name), " vs. ", gsub("\\.", " ", var2_name), " ---\n"))
    CrossTable(data_df_faktor[[var1_name]], data_df_faktor[[var2_name]],
               prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE)
    
    # Bar Plot Bertumpuk (Stacked Bar Chart - menunjukkan proporsi)
    plot_stacked_bar <- data_df_faktor %>%
      ggplot(aes(x = !!sym(var1_name), fill = !!sym(var2_name))) +
      geom_bar(position = "fill") +
      labs(title = paste("Proporsi", gsub("\\.", " ", var2_name), "Berdasarkan", gsub("\\.", " ", var1_name)),
           x = gsub("\\.", " ", var1_name), y = "Proporsi", fill = gsub("\\.", " ", var2_name)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    print(plot_stacked_bar)
    
    # Bar Plot Berdampingan (Dodged Bar Chart - menunjukkan jumlah)
    plot_dodged_bar <- data_df_faktor %>%
      ggplot(aes(x = !!sym(var1_name), fill = !!sym(var2_name))) +
      geom_bar(position = "dodge") +
      labs(title = paste("Jumlah Responden", gsub("\\.", " ", var2_name), "Berdasarkan", gsub("\\.", " ", var1_name)),
           x = gsub("\\.", " ", var1_name), y = "Jumlah Responden", fill = gsub("\\.", " ", var2_name)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    print(plot_dodged_bar)
  }
}
