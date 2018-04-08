
rm(list = ls())

library(shiny)
library(ggplot2)
library(xts)
library(data.table)
library(tm)

XVAR__model_base        <<- 1
XVAR__model_advance     <<- 2
# disableActionButton <- function(id,session) {
#         session$sendCustomMessage(type="jsCode",
#                                   list(code= paste("$('#",id,"').prop('disabled',true)"
#                                                    ,sep="")))
# }

# shinyServer(function(input, output,session) {
#         
#         observe({
#                 if(input$btn1 == 0) return()
#                 disableActionButton("btn2",session)
#         })
# })

shinyServer(function(input, output, session) {
        cleanInput      <- function( instr ) {
                
                fclean                  <- function(x) {gsub("[^\x20-\x7E]", "", x)}
                cleantext               <- unlist(lapply(instr, FUN=fclean))
                textCorpus      <- Corpus(VectorSource(cleantext)) 
                textCorpus      <- tm_map(textCorpus, content_transformer(tolower))
                textCorpus      <- tm_map(textCorpus, content_transformer(removePunctuation))
                textCorpus      <- tm_map(textCorpus, content_transformer(removeNumbers))
                textCorpus      <- tm_map(textCorpus, removeWords, stopwords("english"))
                textCorpus      <- tm_map(textCorpus, stemDocument, language="english")
                textCorpus      <- tm_map(textCorpus, removeWords, profws )
                alnewtext       <- unlist(lapply(textCorpus, function(x) x ))
                return ( alnewtext )
        }
        updateUserTextEntry     <- function( inword ) {
                #
                #       update entered text with the top pick
                newPhrase       <<- paste0(newPhrase, " ", cleanInput(inword))
                newPhrase       <<- unlist((strsplit(gsub("\\s+"," ",trimws(newPhrase)), " ", fixed=T))[[1]])
                #       keep last 5 words for display scroll window
                newPhrase     <<- paste((tail(newPhrase,5)),collapse=' ')
                # cat(paste0("textInClean: [", textInClean), "]\n")
                updateTextInput(session, "enteredPhrase",
                                label = paste("Enter Phrase:", input$controller),
                                value = paste(newPhrase, input$controller))
                
        }
        
        loadModelBase           <<- 1                   # load base model
        loadModelAdvance        <<- 1                   # load advanced model
        initflag                <<- 0                   # restart prediction process
        pick1                   <<- 0
        pick2                   <<- 0
        pick3                   <<- 0
        mustPredictAgain        <<- FALSE
        newPhrase               <<- ""
        cntLoadModelBase        <<- 0
        cntLoadModelAdvance     <<- 0
        cntPredict              <<- 0
        textInClean             <<- ""
        profws                  <<- c()
        n1gram                  <<- NA
        n2gram                  <<- NA
        n3gram                  <<- NA
        top3picks               <<- c()
        XVAR__model_selected    <<- XVAR__model_base
        cat(" ******************************* shinyServer executing ...\n")
        
        
        output$spacetext <- renderUI({
                HTML(paste(" ", " ", sep="<br/>"))
        })
        observeEvent(input$do, {
                cat("testing CHANGE click value of ", toString(initflag), "\n" )
                initflag                        <<- input$do
                output$textEntered              <- renderText({ paste0("text entered: [", newPhrase, "]")})
                textInClean                     <<- cleanInput( newPhrase )
                output$textCleaned              <- renderText({ paste0("text cleaned: [", textInClean, "]")})
                # updateTextInput(session, "enteredPhrase",
                #                 label = paste("Enter Phrase:", input$controller),
                #                 value = paste(newPhrase, input$controller))
                cat("testing REACTIVE click value of ", toString(initflag), "\n" )
        })
        observeEvent(input$pick1, {
                cat("testing CHANGE click value of ", toString(pick1), "\n" )
                pick1                           <<- input$pick1
                if (!mustPredictAgain) updateUserTextEntry( top3picks[1] )
                mustPredictAgain        <<- TRUE; disableActionButton("pick1",session)
                cat("testing REACTIVE click value of ", toString(pick1), "\n" )
        })
        observeEvent(input$pick2, {
                cat("testing CHANGE click value of ", toString(pick2), "\n" )
                pick2                           <<- input$pick2
                if (!mustPredictAgain) updateUserTextEntry( top3picks[2] )
                mustPredictAgain        <<- TRUE
                cat("testing REACTIVE click value of ", toString(pick2), "\n" )
        })
        observeEvent(input$pick3, {
                cat("testing CHANGE click value of ", toString(pick3), "\n" )
                pick3                           <<- input$pick3
                if (!mustPredictAgain) updateUserTextEntry( top3picks[3] )
                mustPredictAgain        <<- TRUE
                cat("testing REACTIVE click value of ", toString(pick3), "\n" )
        })
        observeEvent(input$loadBase, {
                cat("testing CHANGE click value of ", toString(loadModelBase), "\n" )
                loadModelBase           <<- input$loadBase
                XVAR__model_selected    <<- XVAR__model_base
                cat("testing REACTIVE click value of ", toString(loadModelBase), "\n" )
        })
        observeEvent(input$loadAdvance, {
                cat("testing CHANGE click value of ", toString(loadModelAdvance), "\n" )
                loadModelAdvance        <<- input$loadAdvance
                XVAR__model_selected    <<- XVAR__model_advance
                output$modelLoading     <- renderText({ paste0("reloading advanced model ...")})
                cat("testing REACTIVE click value of ", toString(loadModelAdvance), "\n" )
        })
        observeEvent(input$zoomStart, {
                plotZoomStart         <- input$zoomStart
                #cat("testing CHANGE click value of ", toString(initflag), "\n" )
                initflag <<- 1
                #cat("testing REACTIVE click value of ", toString(initflag), "\n" )
        })
        observeEvent(input$zoomEnd, {
                plotZoomEnd         <- input$zoomEnd
                #cat("testing CHANGE click value of ", toString(initflag), "\n" )
                initflag <<- 1
                #cat("testing REACTIVE click value of ", toString(initflag), "\n" )
        })
        observeEvent(input$enteredPhrase, {
                newPhrase                       <<- input$enteredPhrase
                #output$textEntered              <- renderText({ paste0("text entered: [", newPhrase, "]")})
                #cat("testing CHANGE click value of ", toString(initflag), "\n" )
                #initflag <<- 1
                #cat("testing REACTIVE click value of ", toString(initflag), "\n" )
        })
        calcReturn      <- function( df ) { (df[1,"AdjClose"] - df[nrow(df),"AdjClose"]) / df[nrow(df),"AdjClose"] * 100.0 }
        output$distPlot <- renderPlot({
                loadModelBase           <<- input$loadBase
                loadModelAdvance        <<- input$loadAdvance
                cntPredict              <<- input$do
                cat(paste0("\n renderPlot activated ... cntLoadModelBase ", toString(cntLoadModelBase)
                    , ",  cntLoadModelAdvance ", toString(cntLoadModelAdvance)), " \n")
                loadModelBase           <<- input$loadBase
                loadModelAdvance        <<- input$loadAdvance
                cntPredict              <<- input$do
                cat(paste0("\n renderPlot activated ... cntLoadModelBase ", toString(cntLoadModelBase)
                           , ",  cntLoadModelAdvance ", toString(cntLoadModelAdvance)), " \n")
                if ( XVAR__model_selected == XVAR__model_base ) {
                        if ( cntLoadModelBase < 1 ) {
                                withProgress(message = paste0('loading base model in progress'),
                                     detail = 'This may take a while...', value = 0, {
                                             ld1             <- load(file="n1gramGT1.tm", verbose=T);   n1gram      <<- eval(as.symbol(ld1))
                                             incProgress(0.1, detail = paste("loading part", 2))
                                             ld2             <- load(file="n2gramGT1split.tm", verbose=T);   n2gram      <<- eval(as.symbol(ld2))         #  24 MB
                                             incProgress(0.5, detail = paste("loading part", 3))
                                             ld3             <- load(file="n3gramGT1split.tm", verbose=T);   n3gram      <<- eval(as.symbol(ld3))         #  20 MB
                                             ld4             <- load(file="profws.tm", verbose=T);      profws      <<- eval(as.symbol(ld4))        #  small
                                             Sys.time()
                                             nrow(n1gram); nrow(n2gram); nrow(n3gram)
                                             setkey(n1gram, col1)
                                             setkey(n2gram, w1)
                                             setkey(n3gram, w1, w2)
                                             output$n1grams_items       <- renderText({ paste0( toString(nrow(n1gram)), " 1-ngrams") })
                                             #Sys.sleep(100)
                                             output$n2grams_items       <- renderText({ paste0( toString(nrow(n2gram)), " 2-ngrams") })
                                             output$n3grams_items       <- renderText({ paste0( toString(nrow(n3gram)), " 3-ngrams") })
                                             output$profws_words        <- renderText({ paste0( toString(length(profws)), " profws") })
                                             loadModelBase              <<- 0
                                             output$modelLoading        <- renderText({ paste0(" ")})
                                             cntLoadModelBase           <<- 1
                                     })
                        } else {
                                output$modelLoading        <- renderText({ paste0("selected base model")})
                                cat("selected base model \n")
                        }
                }
                if ( XVAR__model_selected == XVAR__model_advance ) {
                        if ( cntLoadModelAdvance < 1 ) { 
                                withProgress(message = paste0('loading advance model in progress'),
                                             detail = 'This may take a while...', value = 0, {
                                                     ld1             <- load(file="n1gramGT1.tm", verbose=T);        n1gram      <<- eval(as.symbol(ld1))
                                                     incProgress(0.1, detail = paste("loading part", 2))
                                                     ld2             <- load(file="n2gramGT1split.tm", verbose=T);        n2gram      <<- eval(as.symbol(ld2))         #  24 MB
                                                     incProgress(0.5, detail = paste("loading part", 3))
                                                     ld3             <- load(file="n3gramGT1split.tm", verbose=T);        n3gram      <<- eval(as.symbol(ld3))         #  20 MB
                                                     Sys.time()
                                                     nrow(n1gram); nrow(n2gram); nrow(n3gram)
                                                     setkey(n1gram, col1)
                                                     setkey(n2gram, w1)
                                                     setkey(n3gram, w1, w2)
                                                     output$n1grams_items       <- renderText({ paste0( toString(nrow(n1gram)), " 1-ngrams") })
                                                     output$n2grams_items       <- renderText({ paste0( toString(nrow(n2gram)), " 2-ngrams") })
                                                     output$n3grams_items       <- renderText({ paste0( toString(nrow(n3gram)), " 3-ngrams") })
                                                     loadModelAdvance           <<- 0
                                                     output$modelLoading        <- renderText({ paste0(" ")})
                                                     cntLoadModelAdvance        <<- 1
                                             })
                        } else {
                                output$modelLoading        <- renderText({ paste0("selected advanced model")})
                                cat("selected advance model \n")
                        }
                }
                if ( initflag ) {
                        #       predict words
                        cat("\n Prediction model activated for cleaned phrase: [", textInClean, "] ...\n")
                        allwords        <- unlist((strsplit(gsub("\\s+"," ",trimws(textInClean)), " ", fixed=T))[[1]])
                        #       only use last 2 words since we only got 2-grams and 3-grams
                        last2w          <- allwords[max(1,(length(allwords)-1)):length(allwords)]
                        cat(paste0("last2: ", last2w, "\n"))
                        #       need top 3 picks - s3 then s2s in that order
                        top3picks       <<- c()
                        if ( length(last2w) >  1 ) {
                                df3res          <- n3gram[w1==last2w[1] & w2==last2w[2]]
                                if ( nrow(df3res) > 0 ) {
                                        s3              <- df3res[order(df3res$n, decreasing=T), w3 ] 
                                        top3picks       <<- c(top3picks, s3)
                                        for (i in 1:min(3,length(s3))) { cat(paste0(" s3[", toString(i), "] ", s3[i], "\n"))}
                                }
                                w2compare       <- last2w[2]
                        } else {
                                w2compare       <- last2w[1]
                        }
                        df2res          <- n2gram[w1==w2compare]
                        if ( nrow(df2res) > 0 ) {
                                s2              <- df2res[order(df2res$n, decreasing=T), w2 ]
                                top3picks       <<- c(top3picks, s2)
                                for (i in 1:min(3,length(s2))) { cat(paste0(" s2[", toString(i), "] ", s2[i], "\n"))}
                        } else {
                                #       no matches, figure out what to display
                                cat("No direct predictions found\n")
                        }
                        #       pad top3picks to three if needed to have defaults indicating no picks
                        i <- length(top3picks)+1
                        while ( i <= 3 ) {
                                top3picks       <<- c(top3picks, "NA")
                                i <- i + 1
                        }
                        updateActionButton(session, "pick1", label = top3picks[1])
                        updateActionButton(session, "pick2", label = top3picks[2])
                        updateActionButton(session, "pick3", label = top3picks[3])
                        mustPredictAgain        <<- FALSE
                        #updateUserTextEntry( top3picks[1] )
                        cat("done with predictions\n")
                }

                if ( initflag >=1 ) {
                        initflag    <<- 0



                }
   })
})


