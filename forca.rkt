#lang racket

; Carregamento do banco de palavras

(define (carregar-palavras linha entrada)  ; linha é a linha a ser lida no arquivo entrada e entrada é o arquivo de palavras               
  (define palavra (read-line entrada)) ; le as palavras do arquivo de palavras (entrada) 

  (if (= linha 0)  ; se estivermos na linha correspondente a var n
    palavra  ; retorna a palavra
    (carregar-palavras (sub1 linha) entrada)  ; joga palavra no lixo e vai pra próxima linha
  )
)
  

; função para trocar letras acentuadas por normais
(define (troca-acento caracter)
  (define com-acento
    (member caracter '(#\á #\ã #\â #\ê #\é #\í #\ó #\ô #\õ #\ú #\ç))
  )

  (if com-acento
    (cond
      [(member (first com-acento) `(#\á #\ã #\â)) #\a]
      [(member (first com-acento) `(#\ê #\é)) #\e]
      [(member (first com-acento) `(#\í)) #\i]
      [(member (first com-acento) `(#\ó #\ô #\õ)) #\o]
      [(member (first com-acento) `(#\ú)) #\u]
      [(member (first com-acento) `(#\ç)) #\c]
    )
    caracter
  )
)

; Display das letras encontradas
(define (progresso resposta letras-adivinhadas aux-letras-adivinhadas) 
  (cond
    [(empty? resposta) empty]
    [(empty? aux-letras-adivinhadas)
       (cons #\_ (progresso(rest resposta) letras-adivinhadas letras-adivinhadas))
    ]
    [
     (equal? (troca-acento (first resposta)) (first aux-letras-adivinhadas))
               (cons (first resposta) (progresso (rest resposta) letras-adivinhadas letras-adivinhadas))
    ]
    [
     (not (equal? (troca-acento (first resposta)) (first aux-letras-adivinhadas)))
                    (progresso resposta letras-adivinhadas (rest aux-letras-adivinhadas))
    ]
  )
)

(define (print-progresso letras-adivinhadas resposta)  ; usa a função de cima pra printar
  (define itemResposta (string->list resposta))  ; transforma a string resposta em lista

  (displayln (list->string (progresso itemResposta letras-adivinhadas letras-adivinhadas)))  ; pega o retorno de progresso e transforma a lista em string pra retornar
)

; Checagem de condição de vitória
(define (checar-vitoria resposta letras-adivinhadas)
  (cond
    [(empty? resposta) #t]
    [(member (first resposta) letras-adivinhadas)
               (checar-vitoria (rest resposta) letras-adivinhadas)
    ]
    [else #f]
  )
)



;vidas
;comparar letras-advinhadas com resposta
; se a letra estiver no array, nao fazer nada
; se a letra nao estiver no array, fazer vidas - 1

(define (checar-vidas vidas letra resposta)
  (cond
    [(empty? resposta) (sub1 vidas)]
    [(equal? (troca-acento (first resposta)) letra) vidas]
    [else (checar-vidas vidas letra (rest resposta))]
  )
)

(define gerar-letra (list "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"))

(define (print-vitoria resposta letras-adivinhadas)
        (cond [(checar-vitoria (string->list resposta) letras-adivinhadas) (displayln "\n\nParabéns, você ganhou!\n       ___________      \n      '._==_==_=_.'     \n      .-\\:      /-.    \n     | (|:.     |) |    \n      '-|:.     |-'     \n        \\::.    /      \n         '::. .'        \n           ) (          \n         _.' '._        \n        '-------'       ")]
))

(define (print-derrota vidas)
        (cond [(equal? vidas 0) (displayln "\n\nQue pena, você perdeu!!\n
                     ⢀⣀⡤⠔⠒⠒⠚⠹⣄⣀⣀⠤⢄⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⡴⠋⠉⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠉⠙⠓⠒⣄⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⢀⡤⠴⠒⠒⠒⠒⠒⠒⠋⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⣧⠤⠒⠒⠚⠛⠓⠒⠒⠤⣀⠀⠀
⠀⠀⠀⠀⡤⠚⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠳⡄
⠀⠀⢀⡞⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⡠⠼
⠀⢀⡜⢀⣀⣀⣀⣀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣠⠞⠀⠀⠀⠀⠀⠀⠘⢦⣀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣀⠴⠒⠊⠉⠀⠀
⠀⠈⠉⠉⠀⠀⠀⠀⠉⠉⢘⠇⠀⠀⠀⠀⠀⠀⢀⣠⠴⠚⠉⠙⢲⡀⠀⠀⠀⠀⢠⠎⠉⠉⠑⢦⡀⠀⠀⠀⠀⠀⢾⡁⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⢠⠏⠀⠀⠀⠀⠀⠀⢠⠞⢀⣠⣤⣄⠀⠀⣷⠀⠀⠀⠀⡼⠀⣰⣶⣦⡄⠙⣆⠀⠀⠀⠀⠀⠹⡀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⡞⠀⠀⠀⠀⠀⠀⢠⠏⠀⣾⣿⣯⣽⡆⠀⡏⠀⠀⠀⠀⣇⠀⣿⣿⣾⣷⠀⠘⡆⠀⠀⠀⠀⢸⠁⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⣸⠁⠀⠀⠀⠀⠀⠀⢯⠀⠀⠹⣿⣿⡿⠃⡸⠁⠀⠀⠀⠀⠸⡄⠙⠿⠿⠃⠀⠀⢻⠀⠀⠀⠀⢸⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⡇⠀⠀⠀⠀⠀⠀⠀⠈⢧⠀⠀⠀⠀⠀⡴⠁⠀⠀⠀⠀⠀⠀⠙⢆⡀⠀⠀⠀⢀⡞⠀⠀⠀⠀⢸⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⣧⠀⠀⠀⠀⠀⠀⠀⠀⠈⠓⠤⠤⠴⠊⠀⠀⠀⠀⣠⡆⠀⠀⠀⠀⠙⠲⠤⠴⠊⠀⠀⠀⠀⠀⢸⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠸⡄⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠴⠯⠷⠆⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡼⠀⠀⠀⠀⠀⠀
⠈⠀⠀⠉⠉⠁⠒⠉⠉⠹⡍⠉⠉⠉⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠓⠒⠒⠒⠒⠦⣴⠗⠒⠤⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠹⣄⠀⢠⠆⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣤⠽⢤⠀⠀⠀⠀⠀⠀⠲⠄⣀⡀⠀⠀⡴⠃⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡠⠼⢶⡋⠀⠀⠀⢀⡀⠀⣀⣀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠙⣲⠞⠓⠲⠶⢄⡀⢀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠖⠊⠑⠋⠀⠀⠀⠙⢦⣀⡴⠋⠉⡟⠉⠉⡷⠚⢳⠀⠀⠀⠀⠀⣠⠖⢲⠖⠒⣤⠖⢢⠞⠁⠀⠀⠀⠀⠀⠈⠀⠈⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸⠀⠀⣰⠃⠀⣰⠃⠀⣸⠒⠒⠢⠤⢤⡇⠀⡎⠀⣸⠁⠀⢸⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠦⠖⠙⠤⠴⠛⠦⠴⠃⠀⠀⠀⠀⠀⠣⣤⣧⣠⣧⣀⡠⠋⠀⠀⠀⠀⠀⠀⠀⠀
                                           
                                           ")]
)
)

(define (print-forca vidas)
  (cond
    [(equal? vidas 6) (displayln "
                            ___________
                        | /        | 
                        |/        ( )
                        |         /|\\
                        |          |
                        |         / \\
                        |
                       ")
      ]
    [(equal? vidas 5) (displayln "
                            ___________
                        | /        | 
                        |/        ( )
                        |         /|\\
                        |          | 
                        |         / 
                        |
                       ")
      ]
     [(equal? vidas 4)
                 (displayln "
                            ___________
                        | /        | 
                        |/        ( )
                        |         /|\\
                        |          | 
                        |          
                        |
                       ")
     ]
     [(equal? vidas 3)
                 (displayln "
                            ___________
                        | /        | 
                        |/        ( )
                        |         /|
                        |          | 
                        |          
                        |
                       ")
     ]
     [(equal? vidas 2)
                 (displayln "
                            ___________
                        | /        | 
                        |/        ( )
                        |          |
                        |          | 
                        |          
                        |
                       ")
     ]
     [(equal? vidas 1)
                 (displayln "
                            ___________
                        | /        | 
                        |/        ( )
                        |          
                        |           
                        |          
                        |
                       ")
     ]
     [(equal? vidas 0)
                 (displayln "
                            ___________
                        | /        | 
                        |/       
                        |          
                        |           
                        |          
                        |
                       ")
     ]
  )
)

; Lógica principal
(define (loop letras-adivinhadas resposta vidas)
  (displayln (string-append "\n***************************************Você tem " (~r vidas) " vidas\n"))
  (print-forca vidas)
  (displayln (string-append "A resposta tem " (~r (length (string->list resposta))) " letras"))

  (displayln "\nLetras adivinhadas: ")
  (void (map display letras-adivinhadas))
  (displayln "\n")

  (print-progresso letras-adivinhadas resposta)

  (cond
    [(checar-vitoria (string->list resposta) letras-adivinhadas) (print-vitoria resposta letras-adivinhadas)]
    [(equal? vidas 0) (print-derrota vidas)]
    [else
      (define letra (first (string->list (read-line))))
      (displayln (string-append "Letra digitada: " (make-string 1 letra) "\n"))
      (loop (cons letra letras-adivinhadas) resposta (checar-vidas vidas letra (string->list resposta))
      )
    ]
  )
 )

; Automatico - lógica principal


(define (forca-automatica letras-adivinhadas resposta vidas)
  (define letra-random (random 26))
  (define letra-gerada (list-ref gerar-letra letra-random))
  
  (displayln (string-append "\n***************************************Você tem " (~r vidas) " vidas\n"))
  (print-forca vidas)
  
  (displayln (string-append "Letra gerada: " letra-gerada "\n"))
  
  (displayln "\nLetras adivinhadas: ")
  (void (map display letras-adivinhadas))
  (displayln "\n")

  (print-progresso letras-adivinhadas resposta)

  (cond
    [(checar-vitoria (string->list resposta) letras-adivinhadas) (print-vitoria resposta letras-adivinhadas)]
    [(equal? vidas 0) (print-derrota vidas)]
    [else
       (define letra (first (string->list letra-gerada)))
       (forca-automatica
          (cons letra letras-adivinhadas) resposta (checar-vidas vidas letra (string->list resposta))
      )
    ]
  )
 )

; Execução propriamente dita
"Forca!"


(define resposta (carregar-palavras (random 36172) (open-input-file "palavras.txt")))
(println (string-append "A resposta é " resposta))
(forca-automatica '() resposta 6)


(displayln "\n\n\n\n######################################Agora é sua vez############################################\n\n")
(define newResposta (carregar-palavras (random 36172) (open-input-file "palavras.txt")))
(loop '() newResposta  6) 
(println (string-append "A resposta é: " newResposta))
