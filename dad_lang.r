library(readxl)

dad_lang <- read_excel("dad_lang.xlsx") 

View(dad_lang)                                            #open and view                 

library(cluster)

data.matrix(dad_lang) #make data numerical

head(dad_lang)

d=daisy(dad_lang[,5:23], metric="gower") #calculate dissimilarity matrix

d

dadresults=agnes(d, method="ward") #agglomerative clustering 

dadresults

plot(dadresults) #notice 4 main clusters

dadclusters = cutree(dadresults, k=4) #make 4 cluster therefore k=4

dad_lang=data.frame(dad_lang,dadclusters) #add clusters to main data frame

summary(subset(dad_lang, dadclusters ==1)) #cycle through all clusters to analyse

library("ggplot2")

library("reshape2")

library("purrr")

library("dplyr")

library("dendextend")

dendro <- as.dendrogram(dadresults)

dad_lang$Together[order.dendrogram(as.dendrogram(dadresults))] #make labels (this stumped me)

 dendro.col <- dendro %>%

  set("branches_k_color", k = 4, value =   c("darkslategray", "darkcyan", "cyan3", "gold3")) %>%

    set("branches_lwd", 2) %>%

     set("labels_colors", 

      value = c("darkslategray")) %>% 

set("labels_cex", 0.6)%>%

 set("labels", c ( "apa - Cha'palaa"  ,  "apu - Hungarian" ,   "apa-g - Aguaruna"   ,  "appa - Korean",  "appa - Tamil" ,  "athair - Irish"  ,  "ayah - Indonesian",   "ai - Hoti"  ,  "aita - Basque"            ,       "abá - North Slavey"    ,         

"ab - Arabic"          ,           "aabe - Somali"            ,      

 "abbat - Amharic"         ,        "uba - Tupinambá"      ,          

 "uba - Hausa"               ,      "u-bä'-bä - Zulu"          ,      

"paba - Sirionó"            ,      "pa'a - Movima"           ,       

 "paapa - Maasai"        ,          "père - French"         ,         

"Thai - phaw"               ,      "Chibcha- pǎʼ-bä"       ,        

 "Portuguese- pai"        ,        "Catalan- pare"     ,            

 "Māori -pāpara"           ,      "Spanish - padre"      ,          

 "Italian - padre"            ,     "Tajik - padar"             ,     

 "Hindi - pitā"            , "Gujarati - pitā"            ,    

 "Jamaican Creole English - fadā" , "Icelandic - fadir",       

"Swedish- fa"            ,  "Norwegian Bokmål - far"         ,

 "Dutch - vader"         ,          "Afrikaans - vader"      ,        

"English - dad"         ,          "Malagasy - dada"         ,       

 "Mungaka - bàba"      ,            "Cupeño -na"          ,          

 "Plains Cree - nohˑ-täʼ-we"     ,  "Igbo - nna"         ,            

 "Northern Ojibwa - nidada"    ,    "Assiniboine - ah-daʼ"    ,       

"Farsi - pedær"          ,         "Dari - pedar"        ,           

 "Marathi - bāpa"       ,           "Malay - bapak"    ,              

 "Urdu - bāp"               ,       "Vietnamese - bố"      ,          

 "Lebanese - baba"        ,         "Turkish - baba"    ,             

 "Mandarin - baba"           ,      "Bengali - bābā"        ,         

 "Moroccan - baba"          ,       "Swahili - baba"        ,         

 "Panjabi - bapa"            ,      "Cantonese - bàh"      ,          

 "Polci - baa"             ,        "Fula - baaba"         ,          

"SionaTetete - taita"      ,      "Tepehuan - taat"   ,

"ta-ši - Wayuu"            ,       "taata - Ding"       ,            

 "tad - Welsh"       ,              "táé - Yaka"       ,              

"tuba - LG Paulista"  ,  "tatko - Macedonian"   ,          

"tä-taʼ - Hidatsa"        ,        "tata - Páez"   ,                 

 "tata - Mosetén-Chimané"    ,      "tatǎ - Romanian"         ,       

 "tata - Serbian-Croatian-Bosnian", "tato - Ukrainian"   ,            

 "tatta - Sinhala"         ,        "tatana - Tsonga"      ,          

 "tata - Lunda"             ,       "tata - Seki"      ,              

"tate - Ngulu"             ,       "vati - German"      ,            

 "pateras - Greek"         ,        "tatay - Tagalog"  ))

ggd1 <- as.ggdend(dendro.col)



//old

ggplot(ggd1, theme = theme_minimal()) +

   labs(x = "Num. observations", y = "Height", title = "Dendrogram, k = 4")

ggplot(ggd1, labels = T) + 

  scale_y_reverse(expand = c(0.2, 0)) +

     coord_polar(theta="x")



library(circlize) #pull in circilize 



//new

circlize_dendrogram(dendro.col, dend_track_height = 0.8)