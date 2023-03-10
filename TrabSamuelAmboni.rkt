#lang racket

(require racket/format)
(require examples)

;; Exercício 1:
;; Desenvolva um programa que verifica se duas reservas podem ser feitas.
;; Uma reserva não pode ser feita se sua duração entra em conflito com outra reserva em andamento.

(examples
 (check-equal? (disponibilidade (reserva "09:30" "12:00") (reserva "12:30" "13:30")) "A reserva é possível.")
 (check-equal? (disponibilidade (reserva "10:30" "12:00") (reserva "11:30" "13:00")) "A reserva não é possível.")
 (check-equal? (disponibilidade (reserva "11:30" "14:00") (reserva "14:00" "15:30")) "A reserva é possível.")
 (check-equal? (disponibilidade (reserva "14:30" "16:00") (reserva "11:30" "15:00")) "A reserva não é possível."))

(struct reserva (horario-inicio horario-final) #:transparent)
;; Representa uma reserva à ser feita
;; horario-inicio: String, marca o horário de início da reserva, dentro de "08:00" e "18:00". Exemplo "14:30"
;; horario-final: String, marca o horário de término da reserva, dentro de "08:00" e "18:00". Exemplo "16:00"

;; reserva1 e reserva2 são do tipo:
;; - (reserva Inteiro Inteiro Inteiro)  A sala está reservada para tal horário
;;
;; disponibilidade retorna uma mensagem que diz se a reserva é possível.
(define (disponibilidade reserva1 reserva2)
  (cond [(string>=? (reserva-horario-inicio reserva2) (reserva-horario-final reserva1)) "A reserva é possível."]
        [(string>=? (reserva-horario-inicio reserva1) (reserva-horario-final reserva2)) "A reserva é possível."]
        [else "A reserva não é possível."]))

;; Exercício 2:
;; Desenvolva um programa que movimenta um personagem num tabuleiro 10x10.
;; Este personagem pode somente virar para esquerda e direita.

(examples
 (check-equal? (gira-personagem (personagem "n" 1 1) "f") "Direção invalida.")
 (check-equal? (gira-personagem (personagem "n" 1 1) "e") (personagem "o" 1 1))
 (check-equal? (gira-personagem (personagem "o" 1 1) "e") (personagem "s" 1 1))
 (check-equal? (gira-personagem (personagem "s" 1 1) "e") (personagem "l" 1 1))
 (check-equal? (gira-personagem (personagem "l" 1 1) "e") (personagem "n" 1 1))
 (check-equal? (gira-personagem (personagem "n" 1 1) "d") (personagem "l" 1 1))
 (check-equal? (gira-personagem (personagem "o" 1 1) "d") (personagem "n" 1 1))
 (check-equal? (gira-personagem (personagem "s" 1 1) "d") (personagem "o" 1 1))
 (check-equal? (gira-personagem (personagem "l" 1 1) "d") (personagem "s" 1 1)))

(struct personagem (direcao pos-linha pos-coluna) #:transparent)
;; Representa o personagem.
;; direcao: String, marca a direção que o personagem aponta. Sendo elas: "n" = Norte, "s" = Sul, "l" = Leste, "o" = Oeste.
;; pos-linha: Inteiro, marca a linha em que o personagem se encontra.
;; pos-coluna: Inteiro, marca a coluna em que o personagem se encontra.

;; direcao:
;; - String: "e" = Esquerda, gira o personagem para a esquerda, "d" = Direita, gira o personagem para a direita.
;; personagem:
;; - Struct definida acima.
;; (personagem, String) -> personagem | String
;; gira-personagem retorna a nova direção do personagem
;; ou uma mensagem de erro.

(define (gira-personagem personagem direcao)
  (cond [(string=? direcao "e") (gira-esquerda personagem)]
        [(string=? direcao "d") (gira-direita personagem)]
        [else "Direção invalida."]))

;; personagem -> personagem
;; gira-esquerda retorna o personagem na nova direção
;; o mesmo se faz para a função abaixo, só que para a direita.
(define (gira-esquerda personagem-aux)
  (define direcao (personagem-direcao personagem-aux))
  (define pos-linha (personagem-pos-linha personagem-aux))
  (define pos-coluna (personagem-pos-coluna personagem-aux))
  (cond [(string=? direcao "n") (cria-personagem "o" pos-linha pos-coluna)]
        [(string=? direcao "o") (cria-personagem "s" pos-linha pos-coluna)]
        [(string=? direcao "s") (cria-personagem "l" pos-linha pos-coluna)]
        [(string=? direcao "l") (cria-personagem "n" pos-linha pos-coluna)]))

(define (gira-direita personagem-aux)
  (define direcao (personagem-direcao personagem-aux))
  (define pos-linha (personagem-pos-linha personagem-aux))
  (define pos-coluna (personagem-pos-coluna personagem-aux))
  (cond [(string=? direcao "n") (cria-personagem "l" pos-linha pos-coluna)]
        [(string=? direcao "l") (cria-personagem "s" pos-linha pos-coluna)]
        [(string=? direcao "s") (cria-personagem "o" pos-linha pos-coluna)]
        [(string=? direcao "o") (cria-personagem "n" pos-linha pos-coluna)]))

;; (String, Inteiro, Inteiro) -> personagem
;; cria-personagem retorna um struct personagem.
(define (cria-personagem direcao pos-linha pos-coluna)
  (personagem direcao pos-linha pos-coluna))

;; Este personagem só poderá também andar em linha reta, na direção em que está apontado.
(examples
 (check-equal? (move-personagem (personagem "n" 1 1) 7) (personagem "n" 8 1))
 (check-equal? (move-personagem (personagem "s" 10 1) 7) (personagem "s" 3 1))
 (check-equal? (move-personagem (personagem "l" 1 1) 7) (personagem "l" 1 8))
 (check-equal? (move-personagem (personagem "o" 1 10) 7) (personagem "o" 1 3))
 (check-equal? (move-personagem (personagem "n" 10 1) 1) "Erro: Fora do tabuleiro.")
 (check-equal? (move-personagem (personagem "s" 1 1) 1) "Erro: Fora do tabuleiro.")
 (check-equal? (move-personagem (personagem "l" 1 10) 1) "Erro: Fora do tabuleiro.")
 (check-equal? (move-personagem (personagem "o" 1 1) 1) "Erro: Fora do tabuleiro."))

;; passos:
;; - Inteiro: A quantidade de casas que o personagem irá andar.
;; personagem:
;; - Struct definida acima.
;; (personagem, Inteiro) -> personagem
;; move-personagem retorna o personagem na sua nova posição no tabuleiro.
;; ou uma mensagem de erro.
(define (move-personagem personagem passos)
  (define direcao (personagem-direcao personagem))
  (cond [(string=? direcao "n") (move-norte personagem passos)]
        [(string=? direcao "s") (move-sul personagem passos)]
        [(string=? direcao "l") (move-leste personagem passos)]
        [(string=? direcao "o") (move-oeste personagem passos)]))

;; (personagem, Inteiro) -> personagem
;; recebe um struct personagem e um inteiro, retorna um personagem na nova posição
;; o mesmo se faz para as funções à baixo.
(define (move-norte personagem passos)
  (define nova-pos (+ (personagem-pos-linha personagem) passos))
  (define pos-coluna (personagem-pos-coluna personagem))
  (if (> nova-pos 10) "Erro: Fora do tabuleiro." (cria-personagem "n" nova-pos pos-coluna)))

(define (move-sul personagem passos)
  (define nova-pos (- (personagem-pos-linha personagem) passos))
  (define pos-coluna (personagem-pos-coluna personagem))
  (if (< nova-pos 1) "Erro: Fora do tabuleiro." (cria-personagem "s" nova-pos pos-coluna)))

(define (move-leste personagem passos)
  (define nova-pos (+ (personagem-pos-coluna personagem) passos))
  (define pos-linha (personagem-pos-linha personagem))
  (if (> nova-pos 10) "Erro: Fora do tabuleiro." (cria-personagem "l" pos-linha nova-pos )))

(define (move-oeste personagem passos)
  (define nova-pos (- (personagem-pos-coluna personagem) passos))
  (define pos-linha (personagem-pos-linha personagem))
  (if (< nova-pos 1) "Erro: Fora do tabuleiro." (cria-personagem "o" pos-linha nova-pos)))

;; Exercício 3:
;; Desenvolva um programa que recebe uma lista de números e produza uma lista de strings
;; Cada string nessa lista é um número convertido
;; Todas as strings possuem o mesmo tamanho

(examples
 (check-equal? (converte-lista empty) empty)
 (check-equal? (converte-lista '(1 10 100)) '("  1" " 10" "100"))
 (check-equal? (converte-lista '(1321 321 5346526 12)) '("   1321" "    321" "5346526" "     12"))
 (check-equal? (converte-lista '(1000 100 10 1)) '("1000" " 100" "  10" "   1")))

;; Lista -> Lista
;; converte-lista recebe uma lista de números e retorna uma lista de strings
(define (converte-lista lista)
  (cond [(empty? lista) empty]
        [else
         (define lista-convertida (converte-num->string lista))
         (define tamanho-max (maior-string lista-convertida))
         (ajeita-string lista-convertida tamanho-max)]))

;; Lista -> Lista
;; converte-lista-aux faz a conversão dos números da lista para strings
(define (converte-num->string lista)
  (cond [(empty? lista) empty]
        [else (cons (~a (first lista)) (converte-num->string (rest lista)))]))

;; Lista -> Inteiro
;; encontra o tamanho da maior string da lista
(define (maior-string lista)
  (cond [(empty? lista) 0]
        [else (max (string-length (first lista)) (maior-string (rest lista)))]))

;; (Lista, Inteiro) -> Lista
;; recebe uma lista de strings e um inteiro, retorna uma lista de strings
;; refatora as strings para ajeitá-las ao tamanho da maior
(define (ajeita-string lista tamanho-max)
  (cond [(empty? lista) empty]
        [else
         (define diferenca (- tamanho-max (string-length (first lista))))
         (define primeiro-convertido (string-append (make-string diferenca #\space) (first lista)))
         (cons primeiro-convertido (ajeita-string (rest lista) tamanho-max))]))

;; Exercício 4:
;; Desenvolva um programa que recebe uma string e confere se ela é um palíndromo

(examples
  (check-equal? (palindromo? "hello world!") #f)
  (check-equal? (palindromo? "") #t)
  (check-equal? (palindromo? "a") #t)
  (check-equal? (palindromo? "abc") #f)
  (check-equal? (palindromo? "a. b/a") #t)
  (check-equal? (palindromo? "mãe é âm") #t)
  (check-equal? (palindromo? "12345") #f)
  (check-equal? (palindromo? "123321") #t)
  (check-equal? (palindromo? "a123b321a") #t)
  (check-equal? (palindromo? "1 Casa vérde, e drev as ac 1") #t))

;; String -> Boolean
;; palindromo? recebe uma string texto e retorna um boolean
;; #t caso palíndromo #f caso contrário
(define (palindromo? texto)
  (define string-limpa (limpa-string texto))
  (aux-palindromo? string-limpa))

;; String -> Boolean
;; Recebe uma string limpa e retorna um booleano #t se for palíndromo, #f caso contrário.
(define (aux-palindromo? texto)
  (define ultimo-indice (- (string-length texto) 1))
  (cond [(<= (string-length texto) 1) #t]
        [else (cond [(char=? (string-ref texto 0) (string-ref texto ultimo-indice)) (aux-palindromo? (substring texto 1 ultimo-indice))]
                    [else #f])]))

;; String -> String
;; Recebe uma string qualquer e retorna uma string "limpa", sem diacriticos, espaços ou caracteres especiais.
(define (limpa-string texto)
  (define caracteres (string-split (string-downcase texto) ""))
  (junta-string (remove-especiais caracteres)))

;; Lista -> Lista
;; Recebe uma lista de "caracter" (string de tamanho 1) e retorna uma lista de "caracter" sem diacriticos, caracteres especiais ou espaços.
(define (remove-especiais lista-caracter)
  (define a-diacritico '("ã" "á" "à" "â"))
  (define e-diacritico '("é" "ê"))
  (define i-diacritico '("í" "ì"))
  (define o-diacritico '("ó" "õ" "ô"))
  (define u-diacritico '("ú" "ü"))
  (cond [(empty? lista-caracter) empty]
        [else (cond [(contem? a-diacritico (first lista-caracter)) (cons "a" (remove-especiais (rest lista-caracter)))]
                    [(contem? e-diacritico (first lista-caracter)) (cons "e" (remove-especiais (rest lista-caracter)))]
                    [(contem? i-diacritico (first lista-caracter)) (cons "i" (remove-especiais (rest lista-caracter)))]
                    [(contem? o-diacritico (first lista-caracter)) (cons "o" (remove-especiais (rest lista-caracter)))]
                    [(contem? u-diacritico (first lista-caracter)) (cons "u" (remove-especiais (rest lista-caracter)))]
                    [(letra-ou-num? (first lista-caracter)) (cons (first lista-caracter) (remove-especiais (rest lista-caracter)))]
                    [else (remove-especiais (rest lista-caracter))])]))

;; (Lista, String) -> Boolean
;; Recebe uma lista e um "caracter" (string de tamanho 1) letra e retorna um booleano, #t caso lista contenha letra, #f caso contrário.
(define (contem? lista letra)
  (if (empty? lista) #f
  (cond [(string=? (first lista) letra) #t]
        [else (contem? (rest lista) letra)])))

;; String -> Boolean
;; Recebe um "caracter" (string de tamanho 1) e retorna um booleano, #t caso seja uma letra, #f caso contrário.
(define (letra-ou-num? caracter)
  (if (zero? (string-length caracter)) #f
      (or (char-alphabetic? (string-ref caracter 0)) (char-numeric? (string-ref caracter 0)))))

;; Lista -> String
;; Recebe uma lista de "caracteres" (strings de tamanho 1) e retorna uma string.
(define (junta-string lista-caracter)
  (cond [(empty? lista-caracter) ""]
        [else (string-append (first lista-caracter) (junta-string (rest lista-caracter)))]))

;; Exercício 5
;; Desenvolva um programa que recebe uma lista de números e retorna uma lista de strings
;; Cada string nessa lista é um número convertido
;; Todas as strings possuem o mesmo tamanho

(examples
 (check-equal? (converte-estiloso empty) empty)
 (check-equal? (converte-estiloso '(1 10 100)) '("  1" " 10" "100"))
 (check-equal? (converte-estiloso '(1321 321 5346526 12)) '("   1321" "    321" "5346526" "     12"))
 (check-equal? (converte-estiloso '(1000 100 10 1)) '("1000" " 100" "  10" "   1")))

;; Lista -> Lista
;; Recebe uma lista de números e retorna uma lista de strings
(define (converte-estiloso lista)
  (define lista-convertida (converte-num->string lista))
  (define tamanho (tam-maior-string lista-convertida))
  (ajeita-string lista-convertida tamanho))

;; Lista -> Inteiro
;; Recebe uma lista de strings e retorna um inteiro igual ao tamanho da maior string na lista
(define (tam-maior-string lst)
  (define (tam-aux tam lst)
    (if (empty? lst) tam
      (cond [(>= (string-length (first lst)) tam)
             (tam-aux (string-length (first lst)) (rest lst))]
            [else (tam-aux tam (rest lst))])))
  (tam-aux 0 lst))

;; Exercício 6
;; Desenvolva um programa que recebe uma string e verifica se ela é um palíndromo
;; Utilizando map, filter e foldl pelo menos uma vez cada.

(examples
  (check-equal? (palindromo-estiloso? "hello world!") #f)
  (check-equal? (palindromo-estiloso? "") #t)
  (check-equal? (palindromo-estiloso? "a") #t)
  (check-equal? (palindromo-estiloso? "abc") #f)
  (check-equal? (palindromo-estiloso? "a. b/a") #t)
  (check-equal? (palindromo-estiloso? "mãe é âm") #t)
  (check-equal? (palindromo-estiloso? "12345") #f)
  (check-equal? (palindromo-estiloso? "123321") #t)
  (check-equal? (palindromo-estiloso? "a123b321a") #t)
  (check-equal? (palindromo-estiloso? "1 Casa vérde, e drev as ac 1") #t))

;; String -> Boolean
;; Recebe uma string e retorna um booleano. #t caso a string seja um palíndromo, #f caso contrário.
(define (palindromo-estiloso? texto)
  (cond [(<= (string-length texto) 1) #t]
        [else
         (define lista-caracter (map limpa-letra (string->list(string-downcase texto))))
         (define lista-filtrada (filter alfanumerico? lista-caracter))
         (define listaf-invertida (foldl cons empty lista-filtrada))
         (if (string=? (list->string lista-filtrada) (list->string listaf-invertida)) #t #f)]))

;; Caracter -> Boolean
;; Recebe um caracter e retorna um booleano. #t caso seja alfanumérico (Alfabético ou Numérico), #f caso contrário.
(define (alfanumerico? char)
  (or (char-alphabetic? char) (char-numeric? char)))

;; Caracter -> Caracter
;; Recebe um caracter e retorna um caracter sem diacríticos.
(define (limpa-letra letra)
  (define a-diacritico '(#\á #\à #\ã #\â))
  (define e-diacritico '(#\é #\ê))
  (define i-diacritico '(#\í))
  (define o-diacritico '(#\ó #\õ #\ô))
  (define u-diacritico '(#\ú #\ü))
  (define (contem-char? lista letra)
    (foldr (lambda (x y) (or y (char=? x letra))) #f lista))
  (cond [(contem-char? a-diacritico letra) #\a]
        [(contem-char? e-diacritico letra) #\e]
        [(contem-char? i-diacritico letra) #\i]
        [(contem-char? o-diacritico letra) #\o]
        [(contem-char? u-diacritico letra) #\u]
        [else letra]))
