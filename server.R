


tweets=fread("DATA/tweets_afganistan.csv", fill = FALSE,encoding="UTF-8")
tweets=tweets %>% mutate(status_id=as.character(status_id))

corpus=fread("DATA/corpus_limpio.csv", fill = FALSE,encoding="UTF-8")

sentimientos_all=fread("DATA/sentimientos_all.csv", fill = FALSE,encoding="UTF-8")


shinyServer(function(input, output) {
    
    observe({
        if(input$sidebarID == "retwitteados") {
            output$tweets_retwitteados <- renderUI({
                mas_retwitteados=tweets %>% 
                    arrange(-retweet_count) %>%
                    top_n(input$slider_tweets, retweet_count) %>% 
                    select(screen_name, status_id)
                
                v=list()
                for(i in 1:nrow(mas_retwitteados)){
                    v[[i]]=tags$iframe(
                        id = gsub(" ", "", paste("tweet_r",i)),
                        frameborder=0, width=400,
                        src = paste("https://twitframe.com/show?url=",tweet_url(mas_retwitteados[i,1],mas_retwitteados[i,2]),sep = "")
                    )
                }
                tagList(
                    tags$div(class = "container-tweets",v),
                    tags$script(src = "myscript.js")
                )
            })
        } else if(input$sidebarID == "favoritos"){
            output$tweets_favoritos <- renderUI({
                mas_likes=tweets %>% 
                    arrange(-favorite_count) %>%
                    top_n(input$slider_tweets2, favorite_count) %>% 
                    select(screen_name, status_id)
                
                v=list()
                for(i in 1:nrow(mas_likes)){
                    v[[i]]=tags$iframe(
                        id = gsub(" ", "", paste("tweet_f",i)),
                        frameborder=0, width=400,
                        src = paste("https://twitframe.com/show?url=",tweet_url(mas_likes[i,1],mas_likes[i,2]),sep = "")
                    )
                }
                tagList(
                    tags$div(class = "container-tweets",v),
                    tags$script(src = "myscript.js")
                )
            })
        }
    })
    
    
    output$grafico_frecuencias <- renderPlotly({
        validate(
            need(input$rango[1] <= input$rango[2], "La fecha de inicio debe ser inferior o igual a la fecha final")
        )
        #grafico de frecuencias de los tweets
        p=tweets %>%
            filter(created_at >= paste(input$rango[1],"00:00:00") , created_at <= paste(input$rango[2],"23:59:59")) %>% 
            ts_plot("hours",colour="#E69F00",size=1)+
            labs(x ="Tiempo del Tweet", y = "Tweets",
                 title = "Frecuencia de tweets sobre afganistán ")+
            theme(axis.title = element_text(face = "bold"),
                   title = element_text(face = "bold") )
        
        ggplotly(p)
        
    })
    
    output$grafico_ciudades <- renderPlotly({
        validate(
            need(input$rango2[1] <= input$rango2[2], "La fecha de inicio debe ser inferior o igual a la fecha final")
        )
        #Grafico de ciudades con mayor numero de tweets
        p=tweets %>% 
            filter(!is.na(place_full_name)) %>% 
            filter(created_at >= paste(input$rango2[1],"00:00:00") , created_at <= paste(input$rango2[2],"23:59:59")) %>% 
            count(place_full_name, sort = TRUE) %>% 
            top_n(input$slider) %>% 
            mutate(place_full_name=fct_reorder(place_full_name,n)) %>% 
            ggplot(aes(x=n, y=place_full_name,fill=place_full_name,
                       text=paste('<b>Ciudad:</b>', place_full_name, '<br>', '<b>tweets:</b>', n))) +
            geom_bar(stat="identity")+
            geom_text(aes(label=n), hjust=3, color="black", size=3.5)+
            theme_minimal()+
            labs(x ="Número de tweets", y = "Ciudades",fill="Ciudades",
                 title = "Ciudades con mayor número de tweets sobre afganistán ")+
            theme(text=element_text(size=6), 
                  axis.text=element_text(size=6), 
                  axis.title=element_text(size=10,face = "bold"),
                  plot.title=element_text(size=12,face = "bold"),
                  legend.text=element_text(size=6),
                  legend.title=element_text(size=10))
        
        ggplotly(p,tooltip = "text")%>% style(textposition = "left")
        
    })
    
    output$grafico_usuarios <- renderPlotly({
        validate(
            need(input$rango3[1] <= input$rango3[2], "La fecha de inicio debe ser inferior o igual a la fecha final")
        )
        #Grafico de usuarios con mayor numero de tweets
        p=tweets %>% 
            filter(created_at >= paste(input$rango3[1],"00:00:00") , created_at <= paste(input$rango3[2],"23:59:59")) %>% 
            count(screen_name, sort = TRUE) %>% 
            top_n(input$slider2) %>% 
            mutate(screen_name = paste0("@", screen_name),
                   screen_name=fct_reorder(screen_name,n)) %>% 
            ggplot(aes(x=n, y=screen_name,fill=screen_name,
                       text=paste('<b>Usuario:</b>', screen_name , '<br>', '<b>tweets:</b>', n))) +
            geom_bar(stat="identity")+
            geom_text(aes(label=n), hjust=3, color="black", size=3.5)+
            theme_minimal()+
            labs(x ="Número de tweets", y = "Usuarios",fill="Usuarios",
                 title = "Usuarios con mayor número de tweets sobre afganistán ")+
            theme(text=element_text(size=6), 
                  axis.text=element_text(size=6), 
                  axis.title=element_text(size=10,face = "bold"),
                  plot.title=element_text(size=12,face = "bold"),
                  legend.text=element_text(size=6),
                  legend.title=element_text(size=10))
        
        ggplotly(p,tooltip = "text")%>% style(textposition = "left")
        
    })
    
    output$grafico_hashtags <- renderPlotly({
        validate(
            need(input$rango4[1] <= input$rango4[2], "La fecha de inicio debe ser inferior o igual a la fecha final")
        )
        #Grafico de hashtags mas utilizados en los tweets
        p=tweets %>% 
            filter(!is.na(hashtags)) %>%
            filter(created_at >= paste(input$rango4[1],"00:00:00") , created_at <= paste(input$rango4[2],"23:59:59")) %>% 
            unnest_tokens(hashtags,hashtags, to_lower = FALSE) %>%
            count(hashtags, sort = TRUE) %>% 
            top_n(input$slider3) %>% 
            mutate(hashtags = paste0("#", hashtags),
                   hashtags=fct_reorder(hashtags,n)) %>% 
            ggplot(aes(x=n, y=hashtags,fill=hashtags,
                       text=paste('<b>Hashtag:</b>', hashtags , '<br>', '<b>Apariciones:</b>', n))) +
            geom_bar(stat="identity")+
            geom_text(aes(label=n), hjust=3, color="black", size=3.5)+
            theme_minimal()+
            labs(x ="Número de apariciones", y = "Hashtags",fill="Hashtags",
                 title = "Hastaghs más utilizados en tweets sobre afganistán ")+
            theme(text=element_text(size=6), 
                  axis.text=element_text(size=6), 
                  axis.title=element_text(size=10,face = "bold"),
                  plot.title=element_text(size=12,face = "bold"),
                  legend.text=element_text(size=6),
                  legend.title=element_text(size=10))
        
        ggplotly(p,tooltip = "text")%>% style(textposition = "left")
        
    })
    
    output$grafico_menciones <- renderPlotly({
        validate(
            need(input$rango5[1] <= input$rango5[2], "La fecha de inicio debe ser inferior o igual a la fecha final")
        )
        #Grafico de menciones mas utilizados en los tweets
        p=tweets %>% 
            filter(!is.na(mentions_screen_name)) %>%
            filter(created_at >= paste(input$rango5[1],"00:00:00") , created_at <= paste(input$rango5[2],"23:59:59")) %>% 
            unnest_tokens(mentions_screen_name,mentions_screen_name, to_lower = FALSE) %>%
            count(mentions_screen_name, sort = TRUE) %>%
            top_n(input$slider4) %>% 
            mutate(mentions_screen_name = paste0("@", mentions_screen_name),
                   mentions_screen_name=fct_reorder(mentions_screen_name,n)) %>% 
            ggplot(aes(x=n, y=mentions_screen_name,fill=mentions_screen_name,
                       text=paste('<b>Mención:</b>', mentions_screen_name , '<br>', '<b>Apariciones:</b>', n))) +
            geom_bar(stat="identity")+
            geom_text(aes(label=n), hjust=3, color="black", size=3.5)+
            theme_minimal()+
            labs(x ="Número de apariciones", y = "Menciones",fill="Menciones",
                 title = "menciones más utilizadas en tweets sobre afganistán ")+
            theme(text=element_text(size=6), 
                  axis.text=element_text(size=6), 
                  axis.title=element_text(size=10,face = "bold"),
                  plot.title=element_text(size=12,face = "bold"),
                  legend.text=element_text(size=6),
                  legend.title=element_text(size=10))
        
        ggplotly(p,tooltip = "text")%>% style(textposition = "left")
        
    })
    
    output$grafico_emojis <- renderPlotly({
        validate(
            need(input$rango6[1] <= input$rango6[2], "La fecha de inicio debe ser inferior o igual a la fecha final")
        )
        #Grafico de emojis mas utilizados en los tweets
        p=tweets %>% 
            filter(created_at >= paste(input$rango6[1],"00:00:00") , created_at <= paste(input$rango6[2],"23:59:59")) %>% 
            mutate(emoji = ji_extract_all(text)) %>%
            unnest(cols = c(emoji)) %>%
            count(emoji, sort = TRUE) %>%
            top_n(input$slider5) %>% 
            mutate(emoji=fct_reorder(emoji,n)) %>% 
            ggplot(aes(x=n, y=emoji,fill=emoji,
                       text=paste('<br>', '<b>Apariciones:</b>', n))) +
            geom_bar(stat="identity")+
            geom_text(aes(label=n), hjust=3, color="black", size=3.5)+
            theme_minimal()+
            labs(x ="Número de apariciones", y = "Emojis",fill="Emojis",
                 title = "Emojis más utilizadas en tweets sobre afganistán ")+
            theme(axis.title = element_text(face = "bold"),
                  title = element_text(face = "bold"))
        
        ggplotly(p,tooltip = "text")%>% style(textposition = "left")
        
    })
    
    output$grafico_nube_palabras <- renderWordcloud2({
        validate(
            need(input$rango7[1] <= input$rango7[2], "La fecha de inicio debe ser inferior o igual a la fecha final")
        )
        #Graficacion de nube de las palabras mas frecuentes
        corpus_limpio=corpus %>% 
            filter(tweets.created_at >= paste(input$rango7[1],"00:00:00") , tweets.created_at <= paste(input$rango7[2],"23:59:59"))
        #convertimos a corpus
        corpus_limpio=VCorpus(VectorSource(select(corpus_limpio,character.0.)),readerControl = list(language = "es"))
        
        matriz_doc_term <- DocumentTermMatrix(corpus_limpio)
        matriz_doc_term <- removeSparseTerms(matriz_doc_term, 0.2)
        matrizDT <- as.matrix(matriz_doc_term)
        
        frecuencia <- colSums(as.matrix(matrizDT))
        tabla_frecuencia <- as.data.frame(cbind(palabra=names(frecuencia), frecuencia=frecuencia))
        tabla_frecuencia <- tabla_frecuencia %>% mutate(frecuencia=as.numeric(frecuencia))
        
        freq <- colSums(as.matrix(matrizDT))
        freq <- freq[order(freq, decreasing = TRUE)][1:input$slider6]
        freq=na.omit(freq)
        freq=data.frame(names(freq),freq)
        freq=freq %>% mutate(word=names(freq))
        
        wordcloud2(freq, size = 0.8,color = "random-dark")
        
    })
    
    output$g_barra_sentimientos <- renderPlotly({
        validate(
            need(input$rango8[1] <= input$rango8[2], "La fecha de inicio debe ser inferior o igual a la fecha final")
        )
        
        sentimientos_all2=sentimientos_all %>%
            filter(created_at >= paste(input$rango8[1],"00:00:00") , created_at <= paste(input$rango8[2],"23:59:59"))
        
        #gráfico de barras dependiendo del tipo seleccionado
        if(input$tipo_grafico==1){
            
            #cambio el titulO dinamicamente
            output$title_pg_sentimiento <- renderUI({
                h3("Gráfico de sentimientos de los tweets ",align = "center")
            })
            
            #en el for cuento la cantidad de tweets que tiene cada sentimiento
            sentimientos=data.frame()
            for (i in 1:8) {
                sentimientos=rbind(sentimientos,count(filter(sentimientos_all2,sentimientos_all2[[i]]>0)))
            }
            sentimientos=cbind(names(sentimientos_all2[,1:8]),sentimientos)
            colnames(sentimientos)=c("tipo","cantidad")
            
            #grafico de la cantidad de tweets que tiene cada sentimiento
            p=sentimientos %>% 
                mutate(tipo=fct_reorder(tipo,cantidad)) %>% 
                ggplot(aes(x=tipo,y=cantidad,fill=tipo,
                           text=paste('<b>',cantidad,'</b>tweets representan', tipo)))+
                geom_bar(stat = "identity")+
                theme_minimal()+
                labs(x ="Sentimientos", y = "Cantidad",fill="Sentimientos",
                     title = "Sentimientos que representan los tweets ")+
                coord_flip()+
                theme(axis.title = element_text(face = "bold"),title = element_text(face = "bold"))
            
            ggplotly(p,tooltip = "text")
        }else{
            
            #cambio el titulO dinamicamente
            output$title_pg_sentimiento <- renderUI({
                h3("Gráfico de polaridad de los tweets ",align = "center")
            })
            
            #grafico de polaridad de los tweets
            positivo=cbind("positivo",
                           count(sentimientos_all2 %>% filter(positivo>0)))
            
            negativo=cbind("negativo",
                           count(sentimientos_all2 %>% filter(negativo>0)))
            
            neutro=cbind("neutro",
                         count(sentimientos_all2 %>% filter(positivo==0 & negativo==0)))
            
            polaridad=as.data.frame(rbind(positivo,negativo,neutro))
            colnames(polaridad)=c("tipo","cantidad")
            
            p=polaridad %>% 
                mutate(tipo=fct_reorder(tipo,cantidad)) %>% 
                ggplot(aes(x=tipo,y=cantidad,fill=tipo,
                           text=paste('<b>',cantidad,'</b>tienen polaridad', tipo)))+
                geom_bar(stat = "identity")+
                theme_minimal()+
                labs(x ="Polaridad", y = "Cantidad",fill="Polaridad",
                     title = "Polaridad de los tweets ")+
                theme(axis.title = element_text(face = "bold"),title = element_text(face = "bold"))
            
            ggplotly(p,tooltip = "text")
        }
        
    })

    output$g_nube_sentimientos <- renderWordcloud2({
        validate(
            need(input$rango9[1] <= input$rango9[2], "La fecha de inicio debe ser inferior o igual a la fecha final")
        )
        #Graficacion de nube de las palabras mas frecuentes dependiendo del sentimiento seleccionado
        
        palabras=sentimientos_all %>%
            filter(created_at >= paste(input$rango9[1],"00:00:00") , created_at <= paste(input$rango9[2],"23:59:59"))
        
        if(input$select_sentimiento==1){
            palabras=sentimientos_all %>%
                filter(alegria>0) %>% 
                select(text)
            
        }else if(input$select_sentimiento==2){
            palabras=sentimientos_all %>%
                filter(anticipacion>0) %>% 
                select(text)
            
        }else if(input$select_sentimiento==3){
            palabras=sentimientos_all %>%
                filter(confianza>0) %>% 
                select(text)
            
        }else if(input$select_sentimiento==4){
            palabras=sentimientos_all %>%
                filter(disgusto>0) %>% 
                select(text)
            
        }else if(input$select_sentimiento==5){
            palabras=sentimientos_all %>%
                filter(ira>0) %>% 
                select(text)
            
        }else if(input$select_sentimiento==6){
            palabras=sentimientos_all %>%
                filter(miedo>0) %>% 
                select(text)
            
        }else if(input$select_sentimiento==7){
            palabras=sentimientos_all %>%
                filter(sorpresa>0) %>% 
                select(text)
            
        }else if(input$select_sentimiento==8){
            palabras=sentimientos_all %>%
                filter(tristeza>0) %>% 
                select(text)
        }
        
        palabras=syuzhet::get_tokens(palabras$text)
        
        palabras=as.data.frame(palabras)
        
        palabras=palabras %>%
            group_by(palabras) %>% 
            summarise(n = n())
        
        palabras <- palabras[order(palabras$n,decreasing = TRUE), ]
        
        wordcloud2(palabras[1:input$slider7,], size = 0.8,color = "random-dark")
    })
    
    output$g_mapa_paises <- renderPlotly({
        validate(
            need(input$rango10[1] <= input$rango10[2], "La fecha de inicio debe ser inferior o igual a la fecha final")
        )
        #Mapa de paises con sus tweets
        paises_coords=fread("DATA/world_country.csv", fill = FALSE,encoding="UTF-8")
        paises_coords=paises_coords %>% select(1:4)
        
        paises_tweets=tweets %>%
            filter(created_at >= paste(input$rango10[1],"00:00:00") , created_at <= paste(input$rango10[2],"23:59:59")) %>% 
            select(country_code,country) %>% 
            filter(!is.na(country_code)) %>% count(country_code)
        
        paises_coords=paises_coords %>% inner_join(paises_tweets,by="country_code")
        
        Sys.setenv(MAPBOX_API_TOKEN = "pk.eyJ1Ijoiam9zZWdiMjAwMCIsImEiOiJja3N3dWN2ajExMG9qMm9xdTZvNW1ibDc3In0.1EHt2M9F_NB7YTKYMoNeog")
        
        map <- paises_coords 
        map <- map %>%
            plot_ly(
                lat = ~latitude,
                lon = ~longitude,
                marker = list(color = "blue"),
                type = 'scattermapbox',
                mode   = 'markers',
                size = ~n,
                text=paste('<b>País: </b>',map$country,' (',map$country_code,')','<br>','<b>Tweets: </b>', map$n,'<br><br>',sep = ""),
                hoverinfo="text")
        map <- map %>% 
            layout(
                mapbox = list(
                    style = input$select_mapa,
                    zoom =2,
                    center=list(lon= "-78.183406", lat="-1.831239")
                ),
                autosize = TRUE
            )
        map <- map %>%
            config(mapboxAccessToken = Sys.getenv("MAPBOX_API_TOKEN"))
        
        map
    })
})

##################### OBTENCION DE DATOS CON RTWEET #########################
#
# consumer_key <- 'abcdefghijklmnñopqrstuvwxyz1234567890'
# consumer_secret <- 'abcdefghijklmnñopqrstuvwxyz1234567890'
# access_token <- 'abcdefghijklmnñopqrstuvwxyz1234567890'
# access_secret <- 'abcdefghijklmnñopqrstuvwxyz1234567890'
# 
# token <- create_token(
#   consumer_key = consumer_key,
#   consumer_secret = consumer_secret,
#   access_token = access_token,
#   access_secret = access_secret)
# get_token()
# 
# #retryonratelimit = TRUE nos permite extraer mas de 18000 resultados que son los que permite la API
# tweets <- search_tweets(q = "afganistan -filter:replies",
#                         n = 100000,
#                         include_rts = FALSE,
#                         lang = "es",
#                         type="mixed",
#                         since = "2021-08-17",
#                         until = "2021-08-20",
#                         retryonratelimit = TRUE,
#                         )

# tweets=tweets %>% select("status_id","created_at","screen_name","text","favorite_count","retweet_count","hashtags","place_full_name","country_code","country","urls_expanded_url","mentions_screen_name")
# tweets=tweets %>% mutate(status_id=as.character(status_id))
# tweets=tweets %>% group_by(status_id) %>% slice(1)
# tweets=as.data.frame(tweets)

# readr::write_csv(tweets, file="DATA/tweets_afganistan.csv")
####################################################################################



################################# CORPUS LIMPIO ####################################
#
# Para el proceso de analisis de sentimientos y para graficar la nube 
# de las palabras frecuentes, necesitamos crear un corpus y limpiarlo,
# debido a que estre poceso es un poco tardado decidimos crear un archivo
# llamado corpus_limpio que contendra el texto de cada tweet limpio y ademas
# la fecha de cada tweet para poder filtrar los graficos
# a continuacion se encuentra el codigo para crear al archivo corpus_limpio.csv
# 
# tweets=fread("DATA/tweets_afganistan.csv", fill = FALSE,encoding="UTF-8")
# 
# docs=tweets %>% mutate(
#   text=replace_hash(text),#eliminar hashtagas por ejemplo(#afganistan)
#   text=replace_tag(text),#eliminar menciones por ejemplo(@cnn)
#   text=replace_url(text),#elminar enlaces
#   text=replace_symbol(text),#eliminar simbolos
#   text=ji_replace_all(text,"")#eliminar emojis
#   )
# 
# set.seed(1987)
# docs=VCorpus(VectorSource(select(docs,text)),readerControl = list(language = "es"))
# docs=tm_map(docs,removePunctuation)
# docs=tm_map(docs, removeNumbers)
# docs=tm_map(docs, tolower)
# docs=tm_map(docs, removeWords, stopwords("spanish"))
# docs=tm_map(docs, PlainTextDocument)
# docs=tm_map(docs, removeWords,
#                c("lunes", "martes", "miércoles", "jueves", "viernes", "sabado", "domingo","miercoles"))
# docs=tm_map(docs, removeWords,
#                c("enero", "febrero", "marzo","abril", "mayo", "junio",
#                  "julio","agosto","septiembre","octubre","noviembre","diciembre"))
# docs <- tm_map(docs, removeWords,
#                c("video", "audio", "publicación", "rt", "retweet", "retweets","tweet",
#                  "tweets","afganistan","afganistán","-"))
# docs=tm_map(docs, stripWhitespace)
# 
# corpus_limpio=data.frame(convert.tm.to.character(docs),tweets$created_at)
# 
# readr::write_csv(corpus_limpio, file="DATA/corpus_limpio.csv")
####################################################################################



#################### RECOPILACION DE LOS SENTIMIENTOS DE CADA DIA ####################
# Tuvimos que crear un archivos con los sentimientos y su día, ya que este proceso
# es demorado y entonces realentizaria mucho hacerlo en la shiny app
# 
# corpus=fread("DATA/corpus_limpio.csv", fill = FALSE,encoding="UTF-8")
# 
# corpus_limpio=corpus %>%
#   filter(tweets.created_at >= "2021-08-17 00:00:00", tweets.created_at <="2021-08-17 23:59:59")
# 
# sentimientos_17=get_nrc_sentiment(corpus_limpio$character.0., lang="spanish")
# 
# 
# 
# corpus_limpio=corpus %>%
#   filter(tweets.created_at >= "2021-08-18 00:00:00", tweets.created_at <="2021-08-18 23:59:59")
# 
# sentimientos_18=get_nrc_sentiment(corpus_limpio$character.0., lang="spanish")
# 
# 
# 
# corpus_limpio=corpus %>%
#   filter(tweets.created_at >= "2021-08-19 00:00:00", tweets.created_at <="2021-08-19 23:59:59")
# 
# sentimientos_19=get_nrc_sentiment(corpus_limpio$character.0., lang="spanish")
# 
# sentimientos_19$dia=19
# 
# 
# corpus_limpio=corpus %>%
#   filter(tweets.created_at >= "2021-08-20 00:00:00", tweets.created_at <="2021-08-20 23:59:59")
# 
# sentimientos_20=get_nrc_sentiment(corpus_limpio$character.0., lang="spanish")
# 
# 
# #unimos los sentimientos de los 4 días en un DATAframe para luego escribir un csv
# sentimientos_all=bind_rows(sentimientos_17, sentimientos_18, sentimientos_19, sentimientos_20)
# 
# #agregamos la fecha de creacion para luego poder filtrar por fecha
# sentimientos_all$created_at=corpus$tweets.created_at
# 
# #agregamos  el texto de cada tweet
# sentimientos_all$text=corpus$character.0.
#
# sentimientos_all=sentimientos_all %>% rename(
#                                         ira=anger,
#                                         anticipacion=anticipation,
#                                         disgusto=disgust,
#                                         miedo=fear,
#                                         alegria=joy,
#                                         tristeza=sadness,
#                                         sorpresa=surprise,
#                                         confianza=trust,
#                                         negativo=negative,
#                                         positivo=positive
#                                         )
# readr::write_csv(sentimientos_all, file="DATA/sentimientos_all.csv")
# 
##############################################################################



#El archivo que contiene los paises con sus coordenas fue descargado del siguiente enlace, lo renombramos para trabajar más comodamente
#https://www.kaggle.com/paultimothymooney/latitude-and-longitude-for-every-country-and-state