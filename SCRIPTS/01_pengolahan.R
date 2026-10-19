# ==========================================
# KABUPATEN MAGELANG
# Daffina Putri Affandi 5016241101
# Arthandia Pratma T.R 5016241105
# ==========================================

# LANGKAH 1 - MEMANGGIL PACKAGE
# ==============================
library(terra)
library(sf)
library(dplyr)
library(ggplot2)
library(exactextractr)

# LANGKAH 2 - MEMERIKSA DAN MEMBACA SHAPEFILE
# ============================================

# Memeriksa folder SHP
list.files("DATA/SHP")

kabupaten <- st_read(
  "DATA/SHP/Kabupaten.shp"
)

kecamatan <- st_read(
  "DATA/SHP/Kecamatan.shp"
)

# LANGKAH 3 - MEMERIKSA DATA SENTINEL-2
# ============================================

# Melihat isi folder Sentinel
list.files("DATA/SENTINEL")

# Mencari file band Sentinel-2
semua_file <- list.files(
  "DATA/SENTINEL",
  pattern = "\\.jp2$",
  recursive = TRUE,
  full.names = TRUE
)

# Menampilkan semua file JP2
print(semua_file)

# Band yang dibutuhkan
band_dicari <- c(
  "B02",
  "B03",
  "B04",
  "B05",
  "B08",
  "B11"
)

# Memeriksa keberadaan band
for (b in band_dicari) {
  
  jumlah <- sum(grepl(
    paste0("_", b, "_"),
    basename(semua_file)
  ))
  
  cat(b, ":", jumlah, "file\n")
}

# LANGKAH 4 - MEMBUAT FUNGSI PEMBACA BAND
# =========================================

# Fungsi membaca band Sentinel-2
ambil_band <- function(kode, resolusi) {
  
  pola <- paste0(
    "_", kode, "_",
    resolusi, "m\\.jp2$"
  )
  
  file <- grep(
    pola,
    semua_file,
    value = TRUE
  )
  
  if (length(file) != 1) {
    stop(paste(
      "Periksa band", kode,
      "- ditemukan", length(file), "file"
    ))
  }
  
  return(rast(file))
}

# LANGKAH 5 - MEMBACA BAND SENTINEL-2
# =====================================

# Membaca band resolusi 10 meter
B02 <- ambil_band("B02", 10)
B03 <- ambil_band("B03", 10)
B04 <- ambil_band("B04", 10)
B08 <- ambil_band("B08", 10)

# Membaca band resolusi 20 meter
B05 <- ambil_band("B05", 20)
B11 <- ambil_band("B11", 20)

# LANGKAH 6 - MEMERIKSA PETA, CRS, RESOLUSI
# ===========================================

plot(B04)

# Melihat koordinat Sentinel-2
crs(B04)

# Melihat koordinat SHP Kabupaten
st_crs(kabupaten)

# Melihat koordinat SHP Kecamatan
st_crs(kecamatan)

# Memeriksa informasi raster Sentinel-2
B04

# Memeriksa koordinat raster
terra::crs(B04)

# Memeriksa batas koordinat
terra::ext(B04)

# Memeriksa resolusi
terra::res(B04)

# LANGKAH 7 - MEMPERBAIKI CRS SENTINEL-2
# =========================================

# Menetapkan CRS sesuai metadata Sentinel-2
terra::crs(B02) <- "EPSG:32749"
terra::crs(B03) <- "EPSG:32749"
terra::crs(B04) <- "EPSG:32749"
terra::crs(B05) <- "EPSG:32749"
terra::crs(B08) <- "EPSG:32749"
terra::crs(B11) <- "EPSG:32749"

# Memeriksa CRS
terra::crs(B04, describe = TRUE)

# SHP sudah memiliki CRS yang sesuai
kabupaten_utm <- kabupaten
kecamatan_utm <- kecamatan

# Menampilkan citra Sentinel-2
plot(B04)

# Menampilkan batas Kabupaten Magelang
plot(
  sf::st_geometry(kabupaten_utm),
  add = TRUE,
  border = "red",
  lwd = 2
)
# Memeriksa jumlah wilayah dalam SHP
nrow(kabupaten_utm)

# Melihat nama kolom SHP
names(kabupaten_utm)

# Melihat batas koordinat Kabupaten
sf::st_bbox(kabupaten_utm)

# Melihat batas koordinat Kecamatan
sf::st_bbox(kecamatan_utm)

# Memeriksa nama kabupaten
kabupaten_utm$WADMKK

# Memeriksa nama kecamatan
kecamatan_utm$WADMKC

# LANGKAH 8 - MEMILIH 21 KECAMATAN MAGELANG
# ==========================================

# Daftar 21 kecamatan Kabupaten Magelang
daftar_kecamatan <- c(
  "Salaman",
  "Borobudur",
  "Ngluwar",
  "Salam",
  "Srumbung",
  "Dukun",
  "Muntilan",
  "Mungkid",
  "Sawangan",
  "Candimulyo",
  "Mertoyudan",
  "Tempuran",
  "Kajoran",
  "Kaliangkrik",
  "Bandongan",
  "Windusari",
  "Secang",
  "Tegalrejo",
  "Pakis",
  "Grabag",
  "Ngablak"
)

# Memilih hanya kecamatan Kabupaten Magelang
kecamatan_magelang <- kecamatan_utm[
  kecamatan_utm$WADMKC %in% daftar_kecamatan,
]

# Memeriksa jumlah kecamatan
nrow(kecamatan_magelang)

# Menampilkan nama kecamatan
kecamatan_magelang$WADMKC

# LANGKAH 9 - PETA KECAMATAN MAGELANG
# ==========================================

# Menampilkan batas Kabupaten Magelang
plot(
  sf::st_geometry(kabupaten_utm),
  col = "lightyellow",
  border = "black",
  main = "Peta 21 Kecamatan Kabupaten Magelang"
)

# Menambahkan batas 21 kecamatan
plot(
  sf::st_geometry(kecamatan_magelang),
  add = TRUE,
  border = "red",
  lwd = 1
)

# LANGKAH 10 - CEK CAKUPAN CITRA
# ==========================================

# Mengubah batas raster menjadi poligon
batas_citra <- sf::st_as_sf(
  terra::as.polygons(terra::ext(B04), crs = terra::crs(B04))
)

# Menghitung luas Kabupaten Magelang
luas_kabupaten <- as.numeric(
  sf::st_area(kabupaten_utm)
)

# Menyamakan CRS batas citra dengan SHP Kabupaten
sf::st_crs(batas_citra) <- sf::st_crs(kabupaten_utm)

# Menghitung bagian kabupaten yang tercakup citra
wilayah_tercakup <- sf::st_intersection(
  kabupaten_utm,
  batas_citra
)

luas_tercakup <- sum(
  as.numeric(sf::st_area(wilayah_tercakup))
)

# Menghitung persentase cakupan
persentase_cakupan <- luas_tercakup / sum(luas_kabupaten) * 100

# Menampilkan hasil
round(persentase_cakupan, 2)

# Menghitung luas wilayah yang tercakup citra
luas_tercakup <- sum(
  as.numeric(sf::st_area(wilayah_tercakup))
)

# Menghitung persentase cakupan citra
persentase_cakupan <- luas_tercakup / sum(luas_kabupaten) * 100

# Menampilkan hasil
round(persentase_cakupan, 2)

# LANGKAH 11 - MEMERIKSA DUA TILE SENTINEL
# ==========================================

# Mencari semua file band B04 resolusi 10 meter
file_B04 <- list.files(
  "DATA/SENTINEL",
  pattern = "_B04_10m\\.jp2$",
  recursive = TRUE,
  full.names = TRUE
)

# Menampilkan jumlah file B04
length(file_B04)

# Menampilkan lokasi file
file_B04

# LANGKAH 12 - MEMBACA DUA TILE SENTINEL-2
# ==========================================

# Mencari file B04 dari kedua tile
file_B04 <- list.files(
  "DATA/SENTINEL",
  pattern = "_B04_10m\\.jp2$",
  recursive = TRUE,
  full.names = TRUE
)

# Membaca citra bagian barat (T49MCM)
B04_barat <- terra::rast(
  file_B04[grepl("T49MCM", file_B04)]
)

# Membaca citra bagian timur (T49MDM)
B04_timur <- terra::rast(
  file_B04[grepl("T49MDM", file_B04)]
)

# Menetapkan CRS sesuai metadata
terra::crs(B04_barat) <- "EPSG:32749"
terra::crs(B04_timur) <- "EPSG:32749"

# Memeriksa informasi kedua citra
B04_barat
B04_timur

# LANGKAH 13 - MEMPERBAIKI CRS DUA CITRA
# ==========================================

# Menetapkan CRS kedua citra
terra::crs(B04_barat) <- "EPSG:32749"
terra::crs(B04_timur) <- "EPSG:32749"

# Memeriksa hasil
terra::crs(B04_barat, describe = TRUE)
terra::crs(B04_timur, describe = TRUE)

# ==========================================
# PENGATURAN PROJ UNTUK RSTUDIO
# ==========================================

lokasi_proj <- file.path(
  system.file(package = "terra"),
  "proj"
)

Sys.setenv(
  PROJ_LIB = lokasi_proj,
  PROJ_DATA = lokasi_proj
)

# LANGKAH 14 - MOSAIC CITRA B04
# ==========================================

# Membuat batas area Kabupaten Magelang
batas_kab <- terra::vect(kabupaten_utm)

# Memotong citra barat berdasarkan area kabupaten
B04_barat_crop <- terra::crop(B04_barat,batas_kab)

# Memotong citra timur berdasarkan area kabupaten
B04_timur_crop <- terra::crop(B04_timur,batas_kab)

# Menggabungkan kedua citra
B04_mosaic <- terra::mosaic(B04_barat_crop,B04_timur_crop,fun = "mean")

# Menampilkan hasil mosaic
plot(
  B04_mosaic,
  main = "Mosaic Sentinel-2 B04 Kabupaten Magelang"
)

# Menambahkan batas Kabupaten Magelang
plot(
  sf::st_geometry(kabupaten_utm),
  add = TRUE,
  border = "red",
  lwd = 2
)

library(sf)

kabupaten <- sf::st_read("DATA/SHP/Kabupaten.shp")
kabupaten_utm <- kabupaten

# LANGKAH 15 - MEMERIKSA HASIL MOSAIC
# ==========================================

# Menampilkan informasi mosaic
B04_mosaic

# Menampilkan peta hasil mosaic
plot(
  B04_mosaic,
  main = "Mosaic B04 Kabupaten Magelang"
)

# Menambahkan batas kabupaten
plot(
  sf::st_geometry(kabupaten_utm),
  add = TRUE,
  border = "red",
  lwd = 2
)

# LANGKAH 16 - MASKING CITRA B04
# ==========================================

# Memotong mosaic mengikuti bentuk kabupaten
B04_final <- terra::mask(B04_mosaic,batas_kab)

# Menampilkan hasil masking
plot(B04_final,main = "Sentinel-2 B04 Kabupaten Magelang")

# Menambahkan garis batas kabupaten
plot(
  batas_kab,
  add = TRUE,
  border = "red",
  lwd = 2
)

# Membuat folder hasil
dir.create(
  "HASIL",
  showWarnings = FALSE
)

# Menyimpan raster B04
terra::writeRaster(
  B04_final,
  "HASIL/B04_Magelang.tif",
  overwrite = TRUE
)


# LANGKAH 17 - MEMBACA, MENGOLAH, & HASIL BAND B02
# =================================================

# Mencari file B02 resolusi 10 meter
file_B02 <- list.files(
  "DATA/SENTINEL",
  pattern = "_B02_10m\\.jp2$",
  recursive = TRUE,
  full.names = TRUE
)

# Memeriksa file yang ditemukan
file_B02

# Membaca citra B02 barat
B02_barat <- terra::rast(
  file_B02[grepl("T49MCM", file_B02)]
)

# Membaca citra B02 timur
B02_timur <- terra::rast(
  file_B02[grepl("T49MDM", file_B02)]
)

# Menetapkan CRS kedua citra
terra::crs(B02_barat) <- "EPSG:32749"
terra::crs(B02_timur) <- "EPSG:32749"

# Memeriksa informasi kedua citra
B02_barat
B02_timur

# 1. Crop citra B02 barat
B02_barat_crop <- terra::crop(
  B02_barat,
  batas_kab
)

# 2. Crop citra B02 timur
B02_timur_crop <- terra::crop(
  B02_timur,
  batas_kab
)

# 3. Mosaic kedua citra B02
B02_mosaic <- terra::mosaic(
  B02_barat_crop,
  B02_timur_crop,
  fun = "mean"
)

# 4. Masking mengikuti batas Kabupaten Magelang
B02_final <- terra::mask(
  B02_mosaic,
  batas_kab
)

# 5. Menampilkan hasil B02
plot(
  B02_final,
  main = "Sentinel-2 B02 Kabupaten Magelang"
)

# 6. Menambahkan batas kabupaten
plot(
  batas_kab,
  add = TRUE,
  border = "red",
  lwd = 2
)

terra::writeRaster(
  B02_final,
  "HASIL/B02_Magelang.tif",
  overwrite = TRUE
)

# LANGKAH 18 - MEMBACA, MENGOLAH, & HASIL BAND B03
# =================================================

# Mencari file B03 resolusi 10 meter
file_B03 <- list.files(
  "DATA/SENTINEL",
  pattern = "_B03_10m\\.jp2$",
  recursive = TRUE,
  full.names = TRUE
)

# Memeriksa file yang ditemukan
file_B03

# Membaca citra B03 barat
B03_barat <- terra::rast(
  file_B03[grepl("T49MCM", file_B03)]
)

# Membaca citra B03 timur
B03_timur <- terra::rast(
  file_B03[grepl("T49MDM", file_B03)]
)

# Menetapkan CRS
terra::crs(B03_barat) <- "EPSG:32749"
terra::crs(B03_timur) <- "EPSG:32749"

# Memeriksa hasil
B03_barat
B03_timur


# Crop B03 barat
B03_barat_crop <- terra::crop(
  B03_barat,
  batas_kab
)

# Crop B03 timur
B03_timur_crop <- terra::crop(
  B03_timur,
  batas_kab
)


# Mosaic kedua citra
B03_mosaic <- terra::mosaic(
  B03_barat_crop,
  B03_timur_crop,
  fun = "mean"
)


# Masking mengikuti batas kabupaten
B03_final <- terra::mask(
  B03_mosaic,
  batas_kab
)

# Menampilkan hasil
plot(
  B03_final,
  main = "Sentinel-2 B03 Kabupaten Magelang"
)

# Menambahkan batas kabupaten
plot(
  batas_kab,
  add = TRUE,
  border = "red",
  lwd = 2
)


terra::writeRaster(
  B03_final,
  "HASIL/B03_Magelang.tif",
  overwrite = TRUE
)

# Memeriksa apakah file berhasil disimpan
file.exists("HASIL/B03_Magelang.tif")


# LANGKAH 19 - MEMBACA, MENGOLAH, DAN HASIL BAND B05
# ===================================================

# Mencari band B05 resolusi 20 meter
file_B05 <- list.files(
  "DATA/SENTINEL",
  pattern = "_B05_20m\\.jp2$",
  recursive = TRUE,
  full.names = TRUE
)

# Memeriksa file yang ditemukan
file_B05


# Membaca citra B05 barat
B05_barat <- terra::rast(
  file_B05[grepl("T49MCM", file_B05)]
)

# Membaca citra B05 timur
B05_timur <- terra::rast(
  file_B05[grepl("T49MDM", file_B05)]
)

# Menetapkan CRS
terra::crs(B05_barat) <- "EPSG:32749"
terra::crs(B05_timur) <- "EPSG:32749"

# Memeriksa informasi raster
B05_barat
B05_timur


# Crop citra B05 barat
B05_barat_crop <- terra::crop(
  B05_barat,
  batas_kab
)

# Crop citra B05 timur
B05_timur_crop <- terra::crop(
  B05_timur,
  batas_kab
)

# Menggabungkan kedua citra B05
B05_mosaic <- terra::mosaic(
  B05_barat_crop,
  B05_timur_crop,
  fun = "mean"
)


# Masking mengikuti batas Kabupaten Magelang
B05_mask <- terra::mask(
  B05_mosaic,
  batas_kab
)

# Menampilkan hasil
plot(
  B05_mask,
  main = "Sentinel-2 B05 Kabupaten Magelang (20m)"
)

# Menambahkan batas kabupaten
plot(
  batas_kab,
  add = TRUE,
  border = "red",
  lwd = 2
)


# Menyesuaikan B05 dengan resolusi dan grid B04
B05_final <- terra::resample(
  B05_mask,
  B04_final,
  method = "bilinear"
)

# Memeriksa hasil
B05_final

# Memeriksa resolusi
terra::res(B05_final)


# LANGKAH 20 - MEMBACA, MENGOLAH, & HASIL BAND B08
# =================================================

file_B08 <- list.files(
  "DATA/SENTINEL",
  pattern = "_B08_10m\\.jp2$",
  recursive = TRUE,
  full.names = TRUE
)

# Memeriksa file yang ditemukan
file_B08


# Membaca B08 barat
B08_barat <- terra::rast(
  file_B08[grepl("T49MCM", file_B08)]
)

# Membaca B08 timur
B08_timur <- terra::rast(
  file_B08[grepl("T49MDM", file_B08)]
)

# Menetapkan CRS
terra::crs(B08_barat) <- "EPSG:32749"
terra::crs(B08_timur) <- "EPSG:32749"

# Memeriksa hasil
B08_barat
B08_timur


# Crop citra B08 barat
B08_barat_crop <- terra::crop(
  B08_barat,
  batas_kab
)

# Crop citra B08 timur
B08_timur_crop <- terra::crop(
  B08_timur,
  batas_kab
)

# Mosaic kedua citra B08
B08_mosaic <- terra::mosaic(
  B08_barat_crop,
  B08_timur_crop,
  fun = "mean"
)


# Masking mengikuti batas kabupaten
B08_final <- terra::mask(
  B08_mosaic,
  batas_kab
)

# Menampilkan hasil
plot(
  B08_final,
  main = "Sentinel-2 B08 Kabupaten Magelang"
)

# Menambahkan batas kabupaten
plot(
  batas_kab,
  add = TRUE,
  border = "red",
  lwd = 2
)


terra::writeRaster(
  B08_final,
  "HASIL/B08_Magelang.tif",
  overwrite = TRUE
)

# Memeriksa file
file.exists("HASIL/B08_Magelang.tif")

# Memeriksa resolusi
terra::res(B08_final)

# Memastikan grid B08 sesuai B04
terra::compareGeom(
  B08_final,
  B04_final,
  stopOnError = FALSE
)


# LANGKAH 21 - MEMBACA, MENGOLAH, & HASIL BAND B11
# ===================================================

# Mencari band B11 resolusi 20 meter
file_B11 <- list.files(
  "DATA/SENTINEL",
  pattern = "_B11_20m\\.jp2$",
  recursive = TRUE,
  full.names = TRUE
)

# Memeriksa file yang ditemukan
file_B11


# Membaca B11 barat
B11_barat <- terra::rast(
  file_B11[grepl("T49MCM", file_B11)]
)

# Membaca B11 timur
B11_timur <- terra::rast(
  file_B11[grepl("T49MDM", file_B11)]
)

# Menetapkan CRS kedua citra
terra::crs(B11_barat) <- "EPSG:32749"
terra::crs(B11_timur) <- "EPSG:32749"

# Memeriksa informasi raster
B11_barat
B11_timur


# Crop citra B11 barat
B11_barat_crop <- terra::crop(
  B11_barat,
  batas_kab
)

# Crop citra B11 timur
B11_timur_crop <- terra::crop(
  B11_timur,
  batas_kab
)

# Menggabungkan kedua citra B11
B11_mosaic <- terra::mosaic(
  B11_barat_crop,
  B11_timur_crop,
  fun = "mean"
)


# Masking sesuai batas kabupaten
B11_mask <- terra::mask(
  B11_mosaic,
  batas_kab
)

# Menampilkan hasil masking
plot(
  B11_mask,
  main = "Sentinel-2 B11 Kabupaten Magelang (20m)"
)

# Menambahkan batas kabupaten
plot(
  batas_kab,
  add = TRUE,
  border = "red",
  lwd = 2
)


# Menyesuaikan B11 dengan grid B04
B11_final <- terra::resample(
  B11_mask,
  B04_final,
  method = "bilinear"
)

# Memeriksa hasil resampling
B11_final

# Memeriksa resolusi
terra::res(B11_final)


# Menampilkan hasil B11
plot(
  B11_final,
  main = "Sentinel-2 B11 Kabupaten Magelang (10m)"
)

# Menambahkan batas kabupaten
plot(
  batas_kab,
  add = TRUE,
  border = "red",
  lwd = 2
)

# Menyimpan raster B11
terra::writeRaster(
  B11_final,
  "HASIL/B11_Magelang.tif",
  overwrite = TRUE
)

# Memeriksa apakah file tersimpan
file.exists("HASIL/B11_Magelang.tif")

# Memeriksa kesesuaian grid dengan B04
terra::compareGeom(
  B11_final,
  B04_final,
  stopOnError = FALSE
)


# LANGKAH 21 - MENGHITUNG NDVI
# ==========================================

# Menghitung NDVI
NDVI <- terra::ifel(
  (B08_final + B04_final) == 0,
  NA,
  (B08_final - B04_final) /
    (B08_final + B04_final)
)

# Menampilkan peta NDVI
plot(
  NDVI,
  main = "NDVI Kabupaten Magelang"
)

# Menyimpan hasil NDVI
terra::writeRaster(
  NDVI,
  "HASIL/NDVI_Magelang.tif",
  overwrite = TRUE
)

# Memeriksa hasil penyimpanan
file.exists("HASIL/NDVI_Magelang.tif")


# LANGKAH 22 - MENGHITUNG NDBI
# ==========================================

# Membaca band B08 dan B11
B08_final <- terra::rast("HASIL/B08_Magelang.tif")

B11_final <- terra::rast("HASIL/B11_Magelang.tif")

# Memeriksa kesesuaian band

terra::compareGeom(
  B08_final,
  B11_final
)

# Menghitung indeks kawasan terbangun
NDBI <- terra::ifel(
  (B11_final + B08_final) == 0,
  NA,
  (B11_final - B08_final) /
    (B11_final + B08_final)
)

# Menampilkan peta NDBI
plot(
  NDBI,
  main = "NDBI Kabupaten Magelang"
)

# Menyimpan NDBI
terra::writeRaster(
  NDBI,
  "HASIL/NDBI_Magelang.tif",
  overwrite = TRUE
)

# Memeriksa apakah file berhasil disimpan
file.exists("HASIL/NDBI_Magelang.tif")


# LANGKAH 24 - MENGHITUNG NDWI
# ==========================================

# Membaca band Green dan NIR
B03_final <- terra::rast("HASIL/B03_Magelang.tif")

B08_final <- terra::rast("HASIL/B08_Magelang.tif")

# Memeriksa kesesuaian band
terra::compareGeom(
  B03_final,
  B08_final
)

# Perhitungan NDWI
NDWI <- terra::ifel(
  (B03_final + B08_final) == 0,
  NA,
  (B03_final - B08_final) /
    (B03_final + B08_final)
)

# Menampilkan peta NDWI
plot(
  NDWI,
  main = "NDWI Kabupaten Magelang",
  col = hcl.colors(100, "Blues")
)

# Menyimpan NDWI
terra::writeRaster(
  NDWI,
  "HASIL/NDWI_Magelang.tif",
  overwrite = TRUE
)

# Memeriksa file hasil NDWI
file.exists("HASIL/NDWI_Magelang.tif")


# LANGKAH 25 - MENGHITUNG NDCI
# ===============================

# Membaca band Red Edge dan Red
B05_final <- terra::rast("HASIL/B05_Magelang.tif")

B04_final <- terra::rast("HASIL/B04_Magelang.tif")

# Memeriksa kesesuaian band B05 dan B04
terra::compareGeom(
  B05_final,
  B04_final
)

# Perhitungan NDCI
NDCI <- terra::ifel(
  (B05_final + B04_final) == 0,
  NA,
  (B05_final - B04_final) /
    (B05_final + B04_final)
)

# Menampilkan peta NDCI
plot(
  NDCI,
  main = "NDCI Kabupaten Magelang",
  col = hcl.colors(100, "YlGn")
)

# Menyimpan NDCI
terra::writeRaster(
  NDCI,
  "HASIL/NDCI_Magelang.tif",
  overwrite = TRUE
)

# Memeriksa hasil penyimpanan NDCI
file.exists("HASIL/NDCI_Magelang.tif")


# LANGKAH 26 - PERSIAPAN KLASIFIKASI CITRA
# ==========================================

# Membaca hasil keempat indeks
NDVI <- terra::rast("HASIL/NDVI_Magelang.tif")

NDBI <- terra::rast("HASIL/NDBI_Magelang.tif")

NDWI <- terra::rast("HASIL/NDWI_Magelang.tif")

NDCI <- terra::rast("HASIL/NDCI_Magelang.tif")

# Memeriksa kesesuaian geometri indeks
terra::compareGeom(
  NDVI,
  NDBI,
  NDWI,
  NDCI
)

# Menggabungkan keempat indeks
indeks_gabungan <- c(
  NDVI,
  NDBI,
  NDWI,
  NDCI
)

# Memberikan nama layer
names(indeks_gabungan) <- c(
  "NDVI",
  "NDBI",
  "NDWI",
  "NDCI"
)

# Memeriksa hasil
indeks_gabungan

# Menampilkan keempat indeks
plot(
  indeks_gabungan,
  main = c(
    "NDVI Magelang",
    "NDBI Magelang",
    "NDWI Magelang",
    "NDCI Magelang"
  )
)

# Melihat nilai minimum dan maksimum
terra::minmax(indeks_gabungan)

# Melihat ringkasan nilai indeks
terra::global(
  indeks_gabungan,
  fun = c("mean", "sd"),
  na.rm = TRUE
)

# Menyimpan indeks gabungan
terra::writeRaster(
  indeks_gabungan,
  "HASIL/Indeks_Gabungan_Magelang.tif",
  overwrite = TRUE
)

# Memeriksa apakah file tersimpan
file.exists("HASIL/Indeks_Gabungan_Magelang.tif")


# LANGKAH 27 - PERSIAPAN KLASIFIKASI
# ==========================================

# Mencari file SCL pada folder Sentinel-2
file_SCL <- list.files(
  "DATA/SENTINEL",
  pattern = "SCL.*\\.jp2$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

# Menampilkan file SCL yang ditemukan
file_SCL

# Menghitung jumlah file SCL
length(file_SCL)

# Menampilkan nama file SCL
basename(file_SCL)

# Menampilkan folder asal masing-masing file
dirname(file_SCL)


# LANGKAH 28 - MEMILIH FILE SCL 20 METER
# ==========================================

# Memilih SCL resolusi 20 meter
SCL_20m <- file_SCL[
  grepl("SCL_20m.jp2$", file_SCL)
]

# Menampilkan file yang terpilih
basename(SCL_20m)

# Memastikan jumlah file
length(SCL_20m)


# LANGKAH 29 - MEMBACA RASTER SCL
# ==========================================

# Membaca SCL tile pertama
SCL_barat <- terra::rast(
  SCL_20m[grepl("T49MCM", SCL_20m)]
)

# Membaca SCL tile kedua
SCL_timur <- terra::rast(
  SCL_20m[grepl("T49MDM", SCL_20m)]
)

# Memeriksa informasi raster
SCL_barat
SCL_timur


# LANGKAH 30 - MEMERIKSA CRS SCL
# ==========================================

terra::crs(SCL_barat, describe = TRUE)

terra::crs(SCL_timur, describe = TRUE)

terra::same.crs(
  SCL_barat,
  SCL_timur
)


# LANGKAH 31 - MENAMPILKAN SCL
# ==========================================

# Menampilkan SCL tile barat
plot(
  SCL_barat,
  main = "SCL Sentinel-2 Tile Barat"
)

# Menampilkan SCL tile timur
plot(
  SCL_timur,
  main = "SCL Sentinel-2 Tile Timur"
)


# LANGKAH 32 - MEMERIKSA CRS SCL
# ==========================================

terra::same.crs(SCL_barat, SCL_timur)


# Menggabungkan dua tile SCL

SCL_gabungan <- terra::mosaic(
  SCL_barat,
  SCL_timur,
  fun = "first"
)

# Memeriksa hasil gabungan
SCL_gabungan

# Menampilkan hasil gabungan SCL
plot(
  SCL_gabungan,
  main = "SCL Gabungan Sentinel-2 Magelang"
)


# LANGKAH 33 - MENYAMAKAN GRID SCL
# ==========================================

# Membaca raster acuan
NDVI_acuan <- terra::rast(
  "HASIL/NDVI_Magelang.tif"
)

# Menyamakan SCL dengan grid NDVI
SCL_sesuai <- terra::project(
  SCL_gabungan,
  NDVI_acuan,
  method = "near"
)

# Memeriksa kesesuaian geometri
terra::compareGeom(
  SCL_sesuai,
  NDVI_acuan
)


# LANGKAH 34 - MASKING SCL MAGELANG
# ==========================================

# Mengikuti area valid raster NDVI
SCL_Magelang <- terra::ifel(
  !is.na(NDVI_acuan),
  SCL_sesuai,
  NA
)

# Menampilkan SCL Kabupaten Magelang
plot(
  SCL_Magelang,
  main = "SCL Kabupaten Magelang"
)


# LANGKAH 35 - MENYIMPAN HASIL SCL
# ==========================================

terra::writeRaster(
  SCL_Magelang,
  "HASIL/SCL_Magelang.tif",
  datatype = "INT1U",
  overwrite = TRUE
)

# Memeriksa apakah file tersimpan
file.exists("HASIL/SCL_Magelang.tif")

# MEMBUAT RASTER PERSEBARAN AWAN

library(terra)

SCL <- rast("HASIL/SCL_Magelang.tif")

awan <- ifel(
  SCL %in% c(8, 9, 10),
  1,
  NA
)

plot(
  awan,
  col = "lightgray",
  main = "Persebaran Awan Kabupaten Magelang"
)

writeRaster(
  awan,
  "HASIL/Awan_Magelang.tif",
  overwrite = TRUE
)


# LANGKAH 36 - KLASIFIKASI TUTUPAN LAHAN
# ==========================================

# Membaca raster SCL
SCL <- terra::rast("HASIL/SCL_Magelang.tif")

# Membaca indeks NDBI
NDBI <- terra::rast("HASIL/NDBI_Magelang.tif")

# Memeriksa kesesuaian raster
terra::compareGeom(SCL, NDBI)

# Membuat Klasifikasi Awal

# 1 = Vegetasi
# 2 = Permukaan tidak bervegetasi
# 3 = Badan air
# 4 = Awan

klasifikasi_awal <- terra::ifel(
  SCL == 4, 1,
  terra::ifel(
    SCL == 5, 2,
    terra::ifel(
      SCL == 6, 3,
      terra::ifel(
        SCL %in% c(8, 9, 10), 4,
        NA
      )
    )
  )
)

names(klasifikasi_awal) <- "Klasifikasi_Awal"

# Menampilkan peta klasifikasi awal

warna_kelas <- c(
  "forestgreen",
  "orange",
  "dodgerblue",
  "white"
)

plot(
  klasifikasi_awal,
  col = warna_kelas,
  breaks = seq(0.5, 4.5, by = 1),
  main = "Klasifikasi Awal Kabupaten Magelang",
  plg = list(
    title = "Kelas",
    labels = c(
      "Vegetasi",
      "Nonvegetasi",
      "Badan Air",
      "Awan"
    )
  )
)

# Menghitung jumlah piksel per kelas
terra::freq(klasifikasi_awal)

# Menyimpan klasifikasi awal

terra::writeRaster(
  klasifikasi_awal,
  "HASIL/Klasifikasi_Awal_Magelang.tif",
  datatype = "INT1U",
  overwrite = TRUE
)

# Memeriksa file
file.exists("HASIL/Klasifikasi_Awal_Magelang.tif")


# LANGKAH 36 - EVALUASI KAWASAN TERBANGUN
# ==========================================

# Membaca klasifikasi awal
klasifikasi_awal <- terra::rast(
  "HASIL/Klasifikasi_Awal_Magelang.tif"
)

# Membaca indeks NDBI dan NDVI
NDBI <- terra::rast("HASIL/NDBI_Magelang.tif")
NDVI <- terra::rast("HASIL/NDVI_Magelang.tif")

# Memeriksa kesesuaian raster
terra::compareGeom(
  klasifikasi_awal,
  NDBI,
  NDVI
)


# MEMERIKSA NDBI PADA KELAS NONVEGETASI
# ==========================================

# Mengambil nilai NDBI pada kelas 2
NDBI_nonvegetasi <- terra::ifel(
  klasifikasi_awal == 2,
  NDBI,
  NA
)

# Melihat ringkasan nilai
terra::global(
  NDBI_nonvegetasi,
  fun = c("mean", "sd", "min", "max"),
  na.rm = TRUE
)


# MEMBUAT AREA TERBANGUN
# ==========================================

NDWI <- terra::rast("HASIL/NDWI_Magelang.tif")

area_terbangun <- terra::ifel(
  (klasifikasi_awal == 2) &
    (NDVI < 0.25) &
    (NDBI > -0.10) &
    (NDWI < 0),
  1,
  NA
)

plot(
  area_terbangun,
  col = "orange",
  main = "Area Terbangun Magelang"
)

terra::freq(area_terbangun)

terra::writeRaster(
  area_terbangun,
  "HASIL/Area_Terbangun_Magelang.tif",
  datatype = "INT1U",
  overwrite = TRUE
)

# Memeriksa file hasil
file.exists("HASIL/Area_Terbangun_Magelang.tif")


# LANGKAH 37 - PETA KLASIFIKASI GABUNGAN
# ==========================================

# Membaca raster SCL
SCL <- terra::rast("HASIL/SCL_Magelang.tif")

# Membaca area kawasan terbangun
calon_terbangun <- terra::rast(
  "HASIL/Area_Terbangun_Magelang.tif"
)

# Memeriksa kesesuaian raster
terra::compareGeom(SCL, calon_terbangun)

# Membuat klasifikasi gabungan
klasifikasi_gabungan <- terra::ifel(
  SCL == 4, 1,
  terra::ifel(
    !is.na(calon_terbangun), 2,
    terra::ifel(
      SCL == 6, 3,
      terra::ifel(
        SCL %in% c(8, 9, 10), 4,
        NA
      )
    )
  )
)

names(klasifikasi_gabungan) <- "Kelas_Tutupan_Lahan"

# Menampilkan peta klasifikasi gabungan
warna_klasifikasi <- c(
  "forestgreen",
  "orange",
  "dodgerblue",
  "gray70"
)

plot(
  klasifikasi_gabungan,
  col = warna_klasifikasi,
  breaks = seq(0.5, 4.5, by = 1),
  main = "Klasifikasi Tutupan Lahan Magelang"
)

# Menghitung jumlah piksel tiap kelas
terra::freq(klasifikasi_gabungan)

# Menyimpan klasifikasi gabungan
terra::writeRaster(
  klasifikasi_gabungan,
  "HASIL/Klasifikasi_Gabungan_Magelang.tif",
  datatype = "INT1U",
  overwrite = TRUE
)

# Memeriksa apakah file tersimpan
file.exists("HASIL/Klasifikasi_Gabungan_Magelang.tif")


# LANGKAH 38 - EVALUASI KLASIFIKASI
# ==========================================

library(terra)

# Membaca raster klasifikasi
klasifikasi <- terra::rast(
  "HASIL/Klasifikasi_Gabungan_Magelang.tif"
)

# Membaca SCL sebagai acuan area
SCL <- terra::rast(
  "HASIL/SCL_Magelang.tif"
)

# Memeriksa kesesuaian raster
terra::compareGeom(klasifikasi, SCL)

# Menghitung jumlah piksel tiap kelas
frekuensi_kelas <- terra::freq(
  klasifikasi
)

# Menampilkan hasil
frekuensi_kelas


# Peta klasifikasi
# ==========================================

# Warna setiap kelas
warna_kelas <- c(
  "forestgreen",
  "orange",
  "dodgerblue",
  "gray70"
)

# Menampilkan peta tanpa legenda otomatis
plot(
  klasifikasi,
  col = warna_kelas,
  breaks = seq(0.5, 4.5, by = 1),
  legend = FALSE,
  main = "Klasifikasi Tutupan Lahan Magelang"
)

# Menambahkan legenda dengan nama kelas
legend(
  "topright",
  legend = c(
    "Vegetasi",
    "Area terbangun",
    "Badan air",
    "Awan"
  ),
  fill = warna_kelas,
  cex = 0.7,
  bg = "white"
)


# Menyimpan peta klasifikasi
# =============================

png(
  "HASIL/Peta_Klasifikasi_Magelang.png",
  width = 1600,
  height = 1200,
  res = 150
)

plot(
  klasifikasi,
  col = warna_kelas,
  breaks = seq(0.5, 4.5, by = 1),
  legend = FALSE,
  main = "Klasifikasi Tutupan Lahan Magelang"
)

legend(
  "topright",
  legend = c(
    "Vegetasi",
    "Calon kawasan terbangun",
    "Badan air",
    "Awan"
  ),
  fill = warna_kelas,
  cex = 0.7,
  bg = "white"
)

dev.off()

# Memeriksa hasil penyimpanan
file.exists("HASIL/Peta_Klasifikasi_Magelang.png")


# LANGKAH 39 - ANALISIS PER KECAMATAN
# ==========================================

library(sf)
library(terra)

# Membaca SHP kecamatan
kecamatan_awal <- sf::st_read(
  "DATA/SHP/Kecamatan.shp"
)

# Melihat nama kolom
names(kecamatan_awal)

# Melihat jumlah kecamatan dalam SHP
nrow(kecamatan_awal)

# Melihat beberapa baris data
head(kecamatan_awal)


# LANGKAH 40 - MEMILIH KECAMATAN MAGELANG
# ==========================================

# Daftar 21 kecamatan Kabupaten Magelang
daftar_kecamatan <- c(
  "Salaman",
  "Borobudur",
  "Ngluwar",
  "Salam",
  "Srumbung",
  "Dukun",
  "Muntilan",
  "Mungkid",
  "Sawangan",
  "Candimulyo",
  "Mertoyudan",
  "Tempuran",
  "Kajoran",
  "Kaliangkrik",
  "Bandongan",
  "Windusari",
  "Secang",
  "Tegalrejo",
  "Pakis",
  "Grabag",
  "Ngablak"
)

# Memilih 21 kecamatan
kecamatan_magelang <- kecamatan_awal[
  kecamatan_awal$WADMKK == "Magelang" &
    kecamatan_awal$WADMKC %in% daftar_kecamatan,
]

# Memeriksa jumlah kecamatan
nrow(kecamatan_magelang)

# Menampilkan nama kecamatan
kecamatan_magelang$WADMKC

# Memeriksa kecamatan yang belum ditemukan
setdiff(
  daftar_kecamatan,
  kecamatan_magelang$WADMKC
)

# Menampilkan batas 21 kecamatan
plot(
  sf::st_geometry(kecamatan_magelang),
  col = "lightblue",
  border = "black",
  main = "21 Kecamatan Kabupaten Magelang"
)

# Menyimpan SHP 21 kecamatan
sf::st_write(
  kecamatan_magelang,
  "HASIL/Kecamatan_Magelang.shp",
  delete_layer = TRUE
)

# Memeriksa file
file.exists("HASIL/Kecamatan_Magelang.shp")


# LANGKAH 41 - STATISTIK PER KECAMATAN
# ==========================================

library(terra)
library(sf)

# Membaca raster indeks
NDVI <- terra::rast("HASIL/NDVI_Magelang.tif")
NDBI <- terra::rast("HASIL/NDBI_Magelang.tif")
NDWI <- terra::rast("HASIL/NDWI_Magelang.tif")

# Membaca SCL yang sudah diolah
SCL <- terra::rast("HASIL/SCL_Magelang.tif")

# Mengubah kelas SCL menjadi persentase awan
# Awan = 100, bukan awan = 0
# Nama NDCI tetap digunakan untuk dashboard
NDCI <- terra::ifel(
  is.na(SCL) | SCL == 0,
  NA,
  terra::ifel(SCL %in% c(8, 9, 10), 100, 0)
)

# Menyamakan grid dengan NDVI jika berbeda
if (!terra::compareGeom(NDVI, NDCI, stopOnError = FALSE)) {
  NDCI <- terra::project(NDCI, NDVI, method = "near")
}


# Memeriksa kesesuaian raster
terra::compareGeom(NDVI, NDBI, NDWI, NDCI)

# Membaca SHP 21 kecamatan
kecamatan_magelang <- sf::st_read(
  "HASIL/Kecamatan_Magelang.shp"
)

# Memeriksa jumlah kecamatan
nrow(kecamatan_magelang)

# Memeriksa kesesuaian CRS
sf::st_crs(kecamatan_magelang)$epsg
terra::crs(NDVI, describe = TRUE)

# Menggabungkan empat indeks
indeks_zonal <- c(NDVI, NDBI, NDWI, NDCI)

# Memberikan nama layer
names(indeks_zonal) <- c(
  "NDVI",
  "NDBI",
  "NDWI",
  "NDCI"
)

# Memeriksa raster gabungan
indeks_zonal

# Mengubah SHP menjadi SpatVector
kecamatan_vect <- terra::vect(kecamatan_magelang)

# Menyamakan CRS kecamatan dengan raster
kecamatan_vect <- terra::project(
  kecamatan_vect,
  terra::crs(indeks_zonal)
)


# LANGKAH 42 - STATISTIK ZONAL
# ==========================================

statistik_rata <- terra::extract(
  indeks_zonal,
  kecamatan_vect,
  fun = mean,
  na.rm = TRUE,
  exact = FALSE
)

# Menampilkan hasil
head(statistik_rata)

# Mengambil nama kecamatan sesuai urutan SHP
nama_kecamatan <- kecamatan_magelang$WADMKC

# Menggabungkan nama kecamatan dengan statistik
hasil_zonal <- data.frame(
  Kecamatan = nama_kecamatan,
  statistik_rata[, c("NDVI", "NDBI", "NDWI", "NDCI")]
)

# Menampilkan hasil
print(hasil_zonal)

# Memeriksa jumlah kecamatan
nrow(hasil_zonal)

# Menyimpan tabel statistik ke CSV
write.csv(
  hasil_zonal,
  "HASIL/Statistik_Zonal_Magelang.csv",
  row.names = FALSE
)

# Memastikan file berhasil disimpan
file.exists("HASIL/Statistik_Zonal_Magelang.csv")


# LANGKAH 43 - GABUNGKAN STATISTIK DAN SHP
# ==========================================

# Memastikan jumlah baris sama
nrow(kecamatan_magelang)
nrow(hasil_zonal)

# Menggabungkan data berdasarkan nama kecamatan
kecamatan_statistik <- merge(
  kecamatan_magelang,
  hasil_zonal,
  by.x = "WADMKC",
  by.y = "Kecamatan",
  all.x = TRUE,
  sort = FALSE
)

# Memeriksa jumlah kecamatan
nrow(kecamatan_statistik)

# Memeriksa nama kolom
names(kecamatan_statistik)

# Menampilkan contoh hasil
head(
  sf::st_drop_geometry(kecamatan_statistik)[,
                                            c("WADMKC", "NDVI", "NDBI", "NDWI", "NDCI")
  ]
)

# Memeriksa apakah ada nilai indeks yang kosong
colSums(
  is.na(
    sf::st_drop_geometry(kecamatan_statistik)[,
                                              c("NDVI", "NDBI", "NDWI", "NDCI")
    ]
  )
)


# Menyimpan SHP kecamatan beserta hasil statistik
sf::st_write(
  kecamatan_statistik,
  "HASIL/Kecamatan_Statistik_Magelang.shp",
  delete_layer = TRUE
)

# Memeriksa apakah file berhasil tersimpan
file.exists("HASIL/Kecamatan_Statistik_Magelang.shp")

# Membaca kembali SHP yang telah disimpan
shp_statistik <- sf::st_read(
  "HASIL/Kecamatan_Statistik_Magelang.shp"
)

# Memeriksa jumlah kecamatan
nrow(shp_statistik)

# Memeriksa nama kolom
names(shp_statistik)