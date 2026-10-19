# ==========================================
# MAGELANG GEOAPP
# DASHBOARD SHINY LEAFLET
# Daffina Putri Affandi 5016241101
# Arthandia Pratma T.R 5016241105
# ==========================================

# 1. MEMANGGIL PACKAGE
library(shiny)
library(shinydashboard)
library(leaflet)
library(sf)
library(terra)

# 2. MEMBACA SHAPEFILE
magelang <- sf::st_read(
  "HASIL/Kecamatan_Statistik_Magelang.shp",
  quiet = TRUE
)

magelang <- sf::st_make_valid(magelang)
magelang <- sf::st_transform(magelang, 4326)

magelang$id_peta <- as.character(
  seq_len(nrow(magelang))
)

# 3. MEMBACA RASTER RINGAN UNTUK WEBSITE

daftar_raster <- list(
  NDVI = raster::raster("HASIL/ringan_NDVI_Magelang.tif"),
  NDBI = raster::raster("HASIL/ringan_NDBI_Magelang.tif"),
  NDWI = raster::raster("HASIL/ringan_NDWI_Magelang.tif"),
  NDCI = raster::raster("HASIL/ringan_Awan_Magelang.tif")
)

# 4. PALET WARNA INDEKS
warna_indeks <- list(
  
  # Vegetasi
  NDVI = c(
    "#8B4513",
    "#D9A441",
    "#FFFF99",
    "#78C850",
    "#006400"
  ),
  
  # Area terbangun
  NDBI = c(
    "#26734D",
    "#A9D18E",
    "#FFF2CC",
    "#E99B59",
    "#A93226"
  ),
  
  # Perairan
  NDWI = c(
    "#986633",
    "#D9C6A5",
    "#F0F5F5",
    "#68BDE1",
    "#08519C"
  ),
  
  
  # Awan
  NDCI = c(
    "#FFFFFF",
    "#E1E5EB",
    "#AAB8C8",
    "#627A98",
    "#263B59"
  )
  
)

# 5. MEMBUAT SKALA WARNA
buat_skala <- function(r, warna) {
  
  nilai <- raster::getValues(r)
  nilai <- nilai[is.finite(nilai)]
  
  if (length(nilai) == 0) {
    stop("Raster tidak memiliki nilai valid.")
  }
  
  batas <- as.numeric(
    stats::quantile(
      nilai,
      probs = c(0.02, 0.98),
      na.rm = TRUE
    )
  )
  
  if (batas[1] == batas[2]) {
    batas <- range(nilai)
    
    if (batas[1] == batas[2]) {
      batas <- batas + c(-0.001, 0.001)
    }
  }
  
  r_tampil <- raster::clamp(
    r,
    lower = batas[1],
    upper = batas[2],
    useValues = TRUE
  )
  
  pal <- leaflet::colorNumeric(
    palette = warna,
    domain = batas,
    na.color = "transparent"
  )
  
  list(
    raster = r_tampil,
    palette = pal,
    batas = batas
  )
}

skala_indeks <- lapply(
  names(daftar_raster),
  function(nama) {
    buat_skala(
      daftar_raster[[nama]],
      warna_indeks[[nama]]
    )
  }
)

names(skala_indeks) <- names(daftar_raster)

# 6. FUNGSI POPUP
format_nilai <- function(x) {
  
  if (length(x) == 0 || is.na(x)) {
    return("Tidak tersedia")
  }
  
  formatC(
    as.numeric(x),
    format = "f",
    digits = 4
  )
}

buat_popup <- function(data_kecamatan, indeks) {
  
  nama <- htmltools::htmlEscape(
    as.character(data_kecamatan$WADMKC[1])
  )
  
  nilai <- data_kecamatan[[indeks]][1]
  
  paste0(
    "<div style='min-width:190px;",
    "font-family:Arial;padding:5px;'>",
    "<b>Kecamatan ", nama, "</b>",
    "<hr style='margin:8px 0;'>",
    "<b>", if (indeks == "NDCI") {
      "NDCI (Tutupan Awan)"
    } else {
      indeks
    }, ":</b> ",
    format_nilai(nilai),
    if (indeks == "NDCI") "%" else "",
    "</div>"
  )
}

# 7. FUNGSI KARTU INFORMASI INDEKS
kartu_indeks <- function(judul, objek, deskripsi,
                         warna, keterangan) {
  
  box(
    title = paste(judul, "-", objek),
    width = 6,
    status = "primary",
    solidHeader = TRUE,
    
    p(deskripsi),
    
    tags$div(
      style = paste0(
        "height:24px;",
        "border-radius:5px;",
        "margin:15px 0 5px 0;",
        "background:linear-gradient(to right,",
        paste(warna, collapse = ","),
        ");"
      )
    ),
    
    fluidRow(
      column(
        6,
        tags$small("Nilai rendah")
      ),
      column(
        6,
        tags$small(
          style = "float:right;",
          "Nilai tinggi"
        )
      )
    ),
    
    br(),
    
    p(
      tags$b("Interpretasi warna: "),
      keterangan
    )
  )
}

# 8. USER INTERFACE
ui <- dashboardPage(
  
  # HEADER
  dashboardHeader(
    title = "Magelang GeoApp"
  ),
  
  # SIDEBAR
  dashboardSidebar(
    
    sidebarMenu(
      id = "menu_utama",
      
      menuItem(
        "Peta Interaktif",
        tabName = "peta",
        icon = icon("map")
      ),
      
      menuItem(
        "Informasi Indeks",
        tabName = "informasi",
        icon = icon("book")
      ),
      
      menuItem(
        "Statistik Kecamatan",
        tabName = "statistik",
        icon = icon("bar-chart")
      ),
      
      menuItem(
        "Tentang Proyek",
        tabName = "tentang",
        icon = icon("info-circle")
      )
    )
  ),
  
  # BODY
  dashboardBody(
    
    # Mengurangi jarak judul dan isi kotak hanya di halaman Tentang Proyek
    tags$head(
      tags$style(HTML("
        #shiny-tab-tentang .box-header {
          padding-bottom: 2px !important;
        }
        #shiny-tab-tentang .box-body {
          padding-top: 2px !important;
          padding-bottom: 10px !important;
        }
        #shiny-tab-tentang .box-body p,
        #shiny-tab-tentang .box-body ul {
          margin-top: 0 !important;
          margin-bottom: 3px !important;
        }
        #shiny-tab-tentang .box-body ul {
          padding-left: 22px;
        }
      "))
    ),
    
    tabItems(
      
      # ====================================
      # MENU 1 - PETA INTERAKTIF
      # ====================================
      
      tabItem(
        tabName = "peta",
        
        h3("Peta Interaktif Kabupaten Magelang"),
        
        p(
          "Visualisasi indeks Sentinel-2.",
          "Pilih indeks dan klik kecamatan",
          "untuk melihat nilainya."
        ),
        
        fluidRow(
          
          column(
            width = 8,
            
            leafletOutput(
              "peta_magelang",
              height = 620
            )
          ),
          
          column(
            width = 4,
            
            box(
              title = "Pengaturan Peta",
              width = 12,
              status = "primary",
              solidHeader = TRUE,
              
              radioButtons(
                inputId = "pilih_indeks",
                label = "Pilih indeks:",
                choices = c(
                  "NDVI",
                  "NDBI",
                  "NDWI",
                  "NDCI"
                ),
                selected = "NDVI"
              ),
              
              checkboxInput(
                inputId = "batas_kecamatan",
                label = "Tampilkan Batas Kecamatan",
                value = TRUE
              ),
              
              hr(),
              
              strong("Indeks aktif:"),
              
              textOutput("indeks_aktif"),
              
              br(),
              
              p(
                "Klik wilayah kecamatan",
                "untuk menampilkan nilai indeks."
              )
            )
          )
        )
      ),
      
      # ====================================
      # MENU 2 - INFORMASI INDEKS
      # ====================================
      
      tabItem(
        tabName = "informasi",
        
        h3("Informasi Indeks Sentinel-2"),
        
        p(
          "Halaman ini menjelaskan tujuan",
          "dan visualisasi warna setiap indeks."
        ),
        
        fluidRow(
          
          kartu_indeks(
            judul = "NDVI",
            objek = "Vegetasi",
            
            deskripsi = paste(
              "Normalized Difference Vegetation",
              "Index digunakan untuk mengamati",
              "kehijauan vegetasi."
            ),
            
            warna = warna_indeks$NDVI,
            
            keterangan = paste(
              "Cokelat menunjukkan nilai rendah,",
              "sedangkan hijau tua menunjukkan",
              "nilai vegetasi yang lebih tinggi."
            )
          ),
          
          kartu_indeks(
            judul = "NDBI",
            objek = "Area Terbangun",
            
            deskripsi = paste(
              "Normalized Difference Built-up",
              "Index digunakan untuk mengidentifikasi",
              "kawasan terbangun."
            ),
            
            warna = warna_indeks$NDBI,
            
            keterangan = paste(
              "Hijau menunjukkan nilai rendah,",
              "sedangkan merah bata menunjukkan",
              "nilai NDBI yang lebih tinggi."
            )
          )
        ),
        
        fluidRow(
          
          kartu_indeks(
            judul = "NDWI",
            objek = "Perairan",
            
            
            deskripsi = paste(
              "Normalized Difference Water",
              "Index digunakan untuk mengidentifikasi",
              "keberadaan air dan wilayah perairan."
            ),
            
            
            warna = warna_indeks$NDWI,
            
            keterangan = paste(
              "Cokelat menunjukkan nilai rendah,",
              "sedangkan biru tua menunjukkan",
              "nilai NDWI yang lebih tinggi."
            )
          ),
          
          kartu_indeks(
            judul = "NDCI",
            objek = "Awan",
            
            deskripsi = paste(
              "Normalized Difference Chlorophyll Index",
              "digunakan untuk mengidentifikasi",
              "keberadaan awan pada citra Sentinel-2."
            ),
            
            warna = warna_indeks$NDCI,
            
            keterangan = paste(
              "Warna terang menunjukkan nilai rendah,",
              "sedangkan abu-abu gelap menunjukkan",
              "nilai tutupan awan yang lebih tinggi."
            )
          )
        )
      ),
      
      # ====================================
      # MENU 3 - STATISTIK KECAMATAN
      # ====================================
      
      tabItem(
        tabName = "statistik",
        
        h3("Statistik Indeks per Kecamatan"),
        
        p(
          "Tabel berikut menampilkan nilai",
          "indeks hasil pengolahan spasial",
          "untuk setiap kecamatan."
        ),
        
        fluidRow(
          
          valueBoxOutput(
            "jumlah_kecamatan",
            width = 6
          ),
          
          valueBoxOutput(
            "jumlah_indeks",
            width = 6
          )
        ),
        
        fluidRow(
          
          box(
            title = "Tabel Statistik Kecamatan",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            
            div(
              style = "overflow-x:auto;",
              tableOutput("tabel_statistik")
            )
          )
        )
      ),
      
      
      # ====================================
      # MENU 4 - TENTANG PROYEK
      # ====================================
      
      tabItem(
        tabName = "tentang",
        
        h3("Tentang Magelang GeoApp"),
        
        box(
          title = "Deskripsi Proyek",
          width = 12,
          status = "primary",
          
          p(
            "Magelang GeoApp merupakan",
            "dashboard interaktif untuk",
            "menampilkan hasil analisis",
            "indeks citra Sentinel-2",
            "di Kabupaten Magelang."
          )
        ),
        
        box(
          title = "Tujuan Analisis",
          width = 12,
          
          tags$ul(
            tags$li("NDVI: analisis vegetasi"),
            tags$li("NDBI: analisis area terbangun"),
            tags$li("NDWI: analisis perairan"),
            tags$li("NDCI: analisis awan")
          )
        ),
        
        box(
          title = "Data dan Teknologi",
          width = 12,
          
          tags$ul(
            tags$li("Data citra Sentinel-2"),
            tags$li("Batas administrasi kecamatan Kabupaten Magelang"),
            tags$li("RStudio"),
            tags$li("Shiny dan Shinydashboard"),
            tags$li("Leaflet untuk peta interaktif"),
            tags$li("Paket sf dan terra untuk pengolahan data spasial")
          )
        )
      )  # Penutup tabItem tentang
    )    # Penutup tabItems
  )      # Penutup dashboardBody
)        # Penutup dashboardPage


# 9. SERVER
server <- function(input, output, session) {
  
  # INDEKS AKTIF
  output$indeks_aktif <- renderText({
    input$pilih_indeks
  })
  
  
  # FUNGSI MENAMPILKAN RASTER
  tampilkan_indeks <- function(peta, indeks) {
    
    info <- skala_indeks[[indeks]]
    
    if (indeks == "NDCI") {
      info$raster <- daftar_raster$NDCI
    }
    
    if (indeks == "NDCI") {
      
      # Menampilkan raster awan
      peta %>%
        addRasterImage(
          info$raster,
          colors = colorNumeric(
            palette = c("#B0BEC5", "#B0BEC5"),
            domain = c(0, 1),
            na.color = "transparent"
          ),
          opacity = 0.85,
          project = TRUE,
          group = "Indeks",
          maxBytes = 8 * 1024^2
        ) %>%
        addLegend(
          position = "bottomright",
          colors = "#B0BEC5",
          labels = "Awan",
          title = "Legenda Awan",
          opacity = 1
        )
      
    } else {
      
      # Menampilkan NDVI, NDBI, dan NDWI
      peta %>%
        addRasterImage(
          info$raster,
          colors = info$palette,
          opacity = 0.85,
          project = TRUE,
          group = "Indeks",
          maxBytes = 8 * 1024^2
        ) %>%
        addLegend(
          position = "bottomright",
          pal = info$palette,
          values = info$batas,
          title = paste("Legenda", indeks),
          opacity = 1,
          labFormat = labelFormat(digits = 3)
        )
    }
  }
  
  
  # PETA AWAL
  output$peta_magelang <- renderLeaflet({
    
    peta <- leaflet() %>%
      
      addProviderTiles(providers$Esri.WorldTopoMap) %>%      
      fitBounds(
        lng1 = 110.03,
        lat1 = -7.71,
        lng2 = 110.45,
        lat2 = -7.32
      )
    
    peta <- tampilkan_indeks(
      peta,
      "NDVI"
    )
    
    peta %>%
      
      addPolygons(
        data = magelang,
        
        layerId = ~id_peta,
        
        color = "blue",
        weight = 1.5,
        
        fillColor = "lightblue",
        fillOpacity = 0.08,
        
        label = ~WADMKC,
        
        highlightOptions = highlightOptions(
          weight = 3,
          color = "red",
          fillOpacity = 0.2,
          bringToFront = TRUE
        ),
        
        group = "Batas Kecamatan"
      )
  })
  
  # MENGGANTI INDEKS
  observeEvent(input$pilih_indeks, {
    
    indeks <- input$pilih_indeks
    
    proxy <- leafletProxy(
      "peta_magelang"
    )
    
    proxy <- proxy %>%
      clearGroup("Indeks") %>%
      clearControls() %>%
      clearPopups()
    
    tampilkan_indeks(
      proxy,
      indeks
    )
    
  }, ignoreInit = TRUE)
  

  # MENAMPILKAN BATAS KECAMATAN
  observeEvent(input$batas_kecamatan, {
    
    proxy <- leafletProxy(
      "peta_magelang"
    )
    
    if (isTRUE(input$batas_kecamatan)) {
      
      proxy %>%
        showGroup("Batas Kecamatan")
      
    } else {
      
      proxy %>%
        hideGroup("Batas Kecamatan") %>%
        clearPopups()
    }
    
  }, ignoreInit = TRUE)
  

  # POPUP KECAMATAN
  observeEvent(
    input$peta_magelang_shape_click,
    {
      
      klik <- input$peta_magelang_shape_click
      
      req(klik$id)
      
      baris <- which(
        magelang$id_peta ==
          as.character(klik$id)
      )
      
      req(length(baris) == 1)
      
      data_kecamatan <- magelang[baris, ]
      
      indeks <- input$pilih_indeks
      
      isi_popup <- buat_popup(
        data_kecamatan,
        indeks
      )
      
      leafletProxy("peta_magelang") %>%
        
        clearPopups() %>%
        
        addPopups(
          lng = klik$lng,
          lat = klik$lat,
          popup = isi_popup
        )
    }
  )
  

  # STATISTIK KECAMATAN
  output$jumlah_kecamatan <- renderValueBox({
    
    valueBox(
      value = nrow(magelang),
      subtitle = "Jumlah Kecamatan",
      icon = icon("map-marker"),
      color = "aqua"
    )
  })
  
  output$jumlah_indeks <- renderValueBox({
    
    valueBox(
      value = length(daftar_raster),
      subtitle = "Jumlah Indeks",
      icon = icon("bar-chart"),
      color = "green"
    )
  })
  
  output$tabel_statistik <- renderTable({
    
    kolom <- c(
      "WADMKC",
      "NDVI",
      "NDBI",
      "NDWI",
      "NDCI"
    )
    
    data_tabel <- sf::st_drop_geometry(
      magelang
    )
    
    data_tabel <- data_tabel[, kolom]
    
    names(data_tabel)[1] <- "Kecamatan"
    
    for (nama in c(
      "NDVI", "NDBI", "NDWI", "NDCI"
    )) {
      data_tabel[[nama]] <- round(
        as.numeric(data_tabel[[nama]]),
        4
      )
    }
    
    data_tabel
    
  }, striped = TRUE,
  bordered = TRUE,
  hover = TRUE,
  spacing = "s")
}

# 10. MENJALANKAN APLIKASI
shinyApp(ui, server)
