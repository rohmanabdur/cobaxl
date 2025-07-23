#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

fpaket <- function(paket1, ...) {
  nama_paket <- c(paket1, ...)
  for (k in seq_along(nama_paket)) {
    if (isFALSE(suppressWarnings(require(nama_paket[k],
                                         character.only = TRUE
    )))) {
      install.packages(nama_paket[k],
                       repos = "https://cran.usk.ac.id/"
      )
      library(nama_paket[k], character.only = TRUE)
    }
  }
}

fpaket("readxl", "shiny")

fbaca_sheets <- function(link_gsheets) {
  if (is.null(link_gsheets) || !grepl("http", link_gsheets)) {
    showModal(modalDialog("Kolom input di sini tidak boleh kosong dan harus
                              diisi link Google Sheets berisi
                              daftar mata kuliah dan pengampunya.",
                          title = "Salah Input"
    ))
  } else {
    google_id <- gsub(".*https://docs.google.com/spreadsheets/d/(.*)/edit.*",
                      "\\1",
                      x = link_gsheets, perl = TRUE
    )
    download.file(paste0("https://drive.google.com/uc?export=download&id=", 
                         google_id),
                  destfile = paste0(getwd(), "/plotting_bobot.xlsx"), mode = "wb"
    )
    nama_sheets <- excel_sheets(paste0(getwd(), "/plotting_bobot.xlsx"))
    dataplot <- lapply(nama_sheets, function(x) {
      read_xlsx(paste0(getwd(), "/plotting_bobot.xlsx"), sheet = x)
    })
    names(dataplot) <- nama_sheets
    return(dataplot)
  }
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"), 
           tableOutput("tabelku")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
    output$tabelku <- renderTable({
      dat <- fbaca_sheets("https://docs.google.com/spreadsheets/d/15srhlRfcojnlfoG0qy94kqQv5GJtMJy_/edit?gid=1681762421#gid=1681762421")
      dat$daftar_dosen
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
