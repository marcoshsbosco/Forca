#lang racket

(define gerar-letra (list "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"))

; Int Entrada -> String
; carregamento do banco de palavras
(define (carregar-palavras linha entrada)  ; linha é a linha a ser lida no arquivo entrada e entrada é o arquivo de palavras
  (define palavra (read-line entrada)) ; lê uma palavra do arquivo da entrada

  (if (= linha 0)  ; se estivermos na linha esperada
    palavra  ; retorna a palavra
    (carregar-palavras (sub1 linha) entrada)  ; joga palavra no lixo e vai pra próxima linha
  )
)


; Char -> Char
; função para trocar uma letra acentuada pela equivalente sem acento
(define (troca-acento caracter)
  (define com-acento
    (member caracter '(#\á #\ã #\â #\ê #\é #\í #\ó #\ô #\õ #\ú #\ç))
  )

  (if com-acento  ; se caracter for acentuado, com-acento é a lista contendo caracter como primeiro elemento
    (cond  ; retorna caracter não acentuado
      [(member (first com-acento) `(#\á #\ã #\â)) #\a]
      [(member (first com-acento) `(#\ê #\é)) #\e]
      [(member (first com-acento) `(#\í)) #\i]
      [(member (first com-acento) `(#\ó #\ô #\õ)) #\o]
      [(member (first com-acento) `(#\ú)) #\u]
      [(member (first com-acento) `(#\ç)) #\c]
    )
    caracter  ; retorna caracter sem modificações porque já não tem acento
  )
)


; List List List -> String
; display das letras encontradas
(define (progresso resposta letras-adivinhadas aux-letras-adivinhadas) 
  (cond
    [(empty? resposta) empty]  ; todas as letras de resposta foram printadas (ou _ no lugar)
    [(empty? aux-letras-adivinhadas)  ; para a letra de resposta atual, sua correspondência com todas as letras adivinhadas foi checada e não encontrada
      (cons #\_ (progresso(rest resposta) letras-adivinhadas letras-adivinhadas))  ; retorna _ e analisa o resto das letras de resposta, resetando a lista de letras adivinhadas à original
    ]
    [(equal? (troca-acento (first resposta)) (first aux-letras-adivinhadas))  ; se a letra atual de resposta for igual à atual das letras adivinhadas
      (cons (first resposta) (progresso (rest resposta) letras-adivinhadas letras-adivinhadas))  ; printa a letra atual e analisa o resto das letras de resposta com a lista original de letras adivinhadas
    ]
    [(not (equal? (troca-acento (first resposta)) (first aux-letras-adivinhadas)))  ; se forem diferentes a letra atual da resposta e das letras adivinhadas
      (progresso resposta letras-adivinhadas (rest aux-letras-adivinhadas))  ; analisa a próxima das letras adivinhadas com a letra da resposta atual
    ]
  )
)


; List String -> Void
; chama a função de cima para printar as letras
(define (print-progresso letras-adivinhadas resposta)
  (define item-resposta (string->list resposta))  ; transforma a string resposta em lista

  (displayln (list->string (progresso item-resposta letras-adivinhadas letras-adivinhadas)))  ; pega o retorno de progresso e transforma a lista em string para fazer o display
)


; List List -> Bool
; checagem de condição de vitória
(define (checar-vitoria resposta letras-adivinhadas)
  (cond
    [(empty? resposta) #t]  ; fim da recursão, todas as letras analisadas - vitória
    [(member (first resposta) letras-adivinhadas)  ; se a letra atual de resposta estiver contida em letras-adivinhadas
      (checar-vitoria (rest resposta) letras-adivinhadas)  ; checar o resto das letras de resposta
    ]
    [else #f]  ; fim da recursão, letra atual não foi adivinhada ainda - sem vitória
  )
)


; Int Char List -> Int
; retorna a quantidade de vidas restantes
(define (checar-vidas vidas letra resposta)
  (cond
    [(empty? resposta) (sub1 vidas)]  ; já percorreu toda a lista e não achou a letra - subtrai uma vida
    [(equal? (troca-acento (first resposta)) letra) vidas]  ; a letra atual de resposta é igual à letra digitada - retorna a mesma quantia de vidas
    [else (checar-vidas vidas letra (rest resposta))]  ; letra atual de resposta não é a adivinhada, checar resto das letras de resposta
  )
)


; String List -> Void
; printa o desenho de troféu quando a pessoa ganha o jogo
(define (print-vitoria resposta letras-adivinhadas)
  (cond
    [(checar-vitoria (string->list resposta) letras-adivinhadas)
      (displayln "\n\nParabéns, você ganhou!\n       ___________      \n      '._==_==_=_.'     \n      .-\\:      /-.    \n     | (|:.     |) |    \n      '-|:.     |-'     \n        \\::.    /      \n         '::. .'        \n           ) (          \n         _.' '._        \n        '-------'       ")
    ]
  )
)


; Int -> Void
; printa o desenho do gatinho triste quando a pessoa perder o jogo
(define (print-derrota vidas)
  (cond
    [(equal? vidas 0)
      (displayln "\n\nQue pena, você perdeu!!\n
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
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠦⠖⠙⠤⠴⠛⠦⠴⠃⠀⠀⠀⠀⠀⠣⣤⣧⣠⣧⣀⡠⠋⠀⠀⠀⠀⠀⠀⠀⠀")
    ]
  )
)


; Int -> Void
; printa o desenho do homenzinho de acordo com as vidas que vai perdendo
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
    [(equal? vidas 4) (displayln "
          ___________
      | /        |
      |/        ( )
      |         /|\\
      |          |
      |
      |
      ")
    ]
    [(equal? vidas 3) (displayln "
          ___________
      | /        |
      |/        ( )
      |         /|
      |          |
      |
      |
      ")
    ]
    [(equal? vidas 2) (displayln "
          ___________
      | /        |
      |/        ( )
      |          |
      |          |
      |
      |
      ")
    ]
    [(equal? vidas 1) (displayln "
          ___________
      | /        |
      |/        ( )
      |
      |
      |
      |
      ")
    ]
    [(equal? vidas 0) (displayln "
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


; List String Int -> Void
; lógica principal
(define (loop letras-adivinhadas resposta vidas)
  (displayln "---------------------------------------------------")
  (displayln (string-append "\nVocê tem " (~r vidas) " vidas\n"))
  (print-forca vidas)
  (displayln (string-append "A resposta tem " (~r (length (string->list resposta))) " letras"))

  (displayln "\nLetras adivinhadas: ")
  (void (map display letras-adivinhadas))
  (displayln "\n")

  (print-progresso letras-adivinhadas resposta)

  (cond  ; checagem de vitória, derrota ou continuação do jogo
    [(checar-vitoria (string->list resposta) letras-adivinhadas) (print-vitoria resposta letras-adivinhadas)]
    [(equal? vidas 0) (print-derrota vidas)]
    [else
      (define letra (first (string->list (read-line))))  ; usuário adivinha uma letra
      (displayln (string-append "Letra digitada: " (make-string 1 letra) "\n"))
      (loop (cons letra letras-adivinhadas) resposta (checar-vidas vidas letra (string->list resposta))  ; chama-se loop recursivamente com a letra recém adivinhada e com quantia de vidas atualizada
      )
    ]
  )
)


; List String Int -> Void
; resolução automática do jogo
(define (forca-automatica letras-adivinhadas resposta vidas)
  (define letra-random (random 26))  ; número aleatório para escolha de letra
  (define letra-gerada (list-ref gerar-letra letra-random))  ; letra escolhida aleatoriamente

  (displayln "---------------------------------------------------")
  (displayln (string-append "\nVocê tem " (~r vidas) " vidas\n"))
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

(define resposta (carregar-palavras (random 36172) (open-input-file "substantivo.txt")))
(println (string-append "A resposta é " resposta))
(forca-automatica '() resposta 6)

(displayln "\n\n\n\n######################################Agora é sua vez############################################\n\n")

(define (classe-palavras)
   (displayln "\nDigite uma classe de palavra:\n Adjetivo \n Substantivo \n Verbo\n ")

   (define classe-escolhida (read-line))
   (displayln (string-append "\n\tClasse escolhida: " classe-escolhida))

   (cond
     [(equal? classe-escolhida "Substantivo")
                (carregar-palavras (random 36172) (open-input-file "substantivo.txt"))
     ]
     [(equal? classe-escolhida "Adjetivo")
                (carregar-palavras (random 24142) (open-input-file "adjetivo.txt"))
     ]
     [(equal? classe-escolhida "Verbo")
                (carregar-palavras (random 14279) (open-input-file "verbo.txt"))
     ]
    
   )
)

(define new-resposta (classe-palavras))

(loop '() new-resposta  6)

(println (string-append "A resposta é: " new-resposta))  ; printa a resposta no fim do jogo
