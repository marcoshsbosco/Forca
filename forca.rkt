#lang racket

; Carregamento do banco de palavras
(define n (random 36172))  ; var n de 0 a 36172 exclusivo

(define entrada (open-input-file "palavras.txt"))

(define (carregar-palavras n)  ; var n é a linha a ser lida
  (define palavra (read-line entrada))

  (if (= n 0)  ; se estivermos na linha correspondente a var n
    palavra  ; retorna a palavra
    (carregar-palavras (sub1 n))  ; joga palavra no lixo e vai pra próxima linha
  )
)

; função para trocar letras acentuadas por normais
(define (troca caracter)
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
(define (progresso r letras-adivinhadas la)  ; retorna uma lista com as letras e underlines dependendo das letras adivinhadas
  (cond
    [(empty? r) empty]
    [(empty? la) (cons #\_ (progresso (rest r) letras-adivinhadas letras-adivinhadas))]
    [(equal? (troca (first r)) (first la)) (cons (first r) (progresso (rest r) letras-adivinhadas letras-adivinhadas))]
    [(not (equal? (troca (first r)) (first la))) (progresso r letras-adivinhadas (rest la))]
  )
)

(define (print-progresso letras-adivinhadas resposta)  ; usa a função de cima pra printar
  (define r (string->list resposta))  ; transforma a string resposta em lista

  (println (list->string (progresso r letras-adivinhadas letras-adivinhadas)))  ; pega o retorno de progresso e transforma a lista em string pra retornar
)

; Checagem de condição de vitória
(define (checar-vitoria resposta letras-adivinhadas)
  (cond
    [(empty? resposta) #t]
    [(member (first resposta) letras-adivinhadas) (checar-vitoria (rest resposta) letras-adivinhadas)]
    [else #f]
  )
)



;vidas
;comparar letras-advinhadas com resposta
; se a letra estiver no array, nao fazer nada
; se a letra nao estiver no array, fazer vidas - 1

(define (checar-vidas v l r)
  (cond
    [(empty? r) (sub1 v)]
    [(equal? (troca (first r)) l) v]
    [else (checar-vidas v l (rest r))]
  )
)



; Lógica principal
(define (loop letras-adivinhadas resposta vidas)
  (println vidas)
  (print-progresso letras-adivinhadas resposta)

  (cond
    [(checar-vitoria (string->list resposta) letras-adivinhadas) (println "Vitória!")]
    [(equal? vidas 0) (println "Que pena, você perdeu!")]
    [else
      (define letra (first (string->list (read-line))))
      (loop (cons letra letras-adivinhadas) resposta (checar-vidas vidas letra (string->list resposta)))
    ]
  )
 )

; Execução propriamente dita
"Forca!"
(define resposta (carregar-palavras n))
(println (string-append "DEBUG: a resposta é " resposta))

(loop '() resposta 5)

