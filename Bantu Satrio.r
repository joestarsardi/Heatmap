library(tidyverse)
library(hrbrthemes)
library(ggExtra)
library(lubridate)

erosi <- read_csv("Erosivitas Hujan.csv")
head(erosi)
erosi$Month <-month(erosi$Month)
erosi$Month <- month.abb[erosi$Month]
erosi$Month <- month.name[match(erosi$Month, month.abb)]
head(erosi)

#erosi$Month <- as.character(erosi$Month)
Month_factor <- factor(erosi$Month, levels = rev(unique(erosi$Month)))
erosi$Rainfall_Erosivity <- as.numeric(erosi$Rainfall_Erosivity)
erosi$Rainfall_Erosivity <- round(erosi$Rainfall_Erosivity)
erosi <- as.tibble(erosi)
heatmaperosi <- ggplot(data = erosi, aes(x = Rain_Station, y = Month_factor, fill = Rainfall_Erosivity)) +
  geom_tile(
    color = "white",
    lwd = 0,
    linetype = 0,
  ) +
  labs(
    y = "Month",
    x = "Rain Station"
  ) +
  geom_text(aes(label = Rainfall_Erosivity), color = "#041540", size = 5, fontface = "bold", alpha = 1) +
  scale_fill_gradient(low = "#f4f5eb", high = "#576ab3", labels = scales::number_format(scale = 1, accuracy = 1)) +
  guides(fill = guide_colourbar(barwidth = 1, title.position = "top", title.vjust = 2, title.hjust = -15.5, label.position = "right",
                                barheight = 8, vjust = 75,
                                title = "Rainfall Erosivity   
                        ")) +
   coord_fixed(ratio = 0.3) +
   #geom_hline(yintercept = 1:12, inherit.aes = FALSE, color = "black") +  # Garis horizontal (hline)
   #geom_vline(xintercept = 1:4, inherit.aes = FALSE, color = "black")  +
   theme_void() +
   theme(axis.text.x = element_text(size = 14, color = "#041540", vjust = 62), 
                         axis.text.y = element_text(size = 14, color = "#041540", hjust = 1),
                         axis.title.x = element_text(size = 15, face = "bold", vjust = 139, color = "#041540"), 
                         axis.title.y = element_text(size = 15, face = "bold", angle = 90, color = "#041540"))
heatmaperosi

# Menyimpan plot dengan ukuran yang diinginkan
#ggsave("heatmap erosi.jpg", plot = heatmaperosi, width = 8, height = 8)