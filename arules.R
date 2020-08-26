#install jika belum
#install.packages("Matrix")
#install.packages("arules")
#install.packages("arulesViz")
#install.packages("grid")
#install.packages("RColorBrewer")
#install.packages("tidyr")


library(Matrix)
library(arules)
library(arulesViz)
library(grid)
library(RColorBrewer)
library(tidyr)

#load data
data1 <- read.csv("520_KODE.csv")
View(data1)

#merubah struktur data
data2 <- gather(data1, "penyakit_ke", "nama_penyakit", -1)
#mengurutkan data berdasarkan ID PASIEN
data_olah <-data2[order(data2$ID.PASIEN),]
head(data_olah)

#menyimpan data yang sudah diubah strukturnya ke excel csv
#data yang kosong saya hapus manual 
#write.csv(df, "data_olah.csv")

#import data yang sudah di simpan
#data_olah <- read.csv("data_olah.csv")
#head(data_olah)

#convert data ke format transaksi
trans <- as(split(data_olah[,"nama_penyakit"], data_olah[,"ID.PASIEN"]), "transactions")
trans
#deskripsi data
summary(trans)

# grafiknya jelek karena nama penyakitnya terlalu panjang
itemFrequencyPlot(trans, topN=10, type="absolute",col=brewer.pal(8,'Pastel2'), main="Absolute Item Frequency Plot")

#membuat tabel tabulasi 
tabel <- crossTable(trans, sort=T)
tabel_lift_rasio <- crossTable(trans, measure="lift",sort=T)
tabel_chi <- crossTable(trans, measure="chi", sort=T) #chi square dependensi antarbarang
write.csv(tabel, "tabulasi_silang.csv")
write.csv(tabel_lift_rasio, "tabulasi_lift.csv")
write.csv(tabel_chi, "tabulasi_chi.csv")

#membuat rules, paramter support dan confidence silahkan menyesuaikan kebutuhan
rules <- apriori(trans, parameter = list(supp=0.3, conf=0.5))
rules
summary(rules)
inspect_rules <- inspect(sort(rules))

#plot graph
plot(sort(rules, by="lift"), method="graph", control=list("items"))

#misal hanya ingin melihat lhs nya untuk penyakit tertentu, contoh penyakit dengan kode P15
rules2 <- apriori(trans, parameter = list(supp=0.2, conf=0.2),
                  appearance = list(default="rhs", lhs="P15"))
rules2
summary(rules2)
inspect_rules2 <- inspect(sort(rules2))

#plot graph rules2
plot(sort(rules2, by="lift"), method="graph", control=list("items"))
