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
;; gira-personagem retorna a nova direção do personagem
;; ou uma mensagem de erro.

(define (gira-personagem personagem direcao)
  (cond [(string=? direcao "e") (gira-esquerda personagem)]
        [(string=? direcao "d") (gira-direita personagem)]
        [else "Direção invalida."]))

;; gira-esquerda retorna o personagem na nova direção
(define (gira-esquerda personagem-aux)
  (define direcao (personagem-direcao personagem-aux))
  (define pos-linha (personagem-pos-linha personagem-aux))
  (define pos-coluna (personagem-pos-coluna personagem-aux))
  (cond [(string=? direcao "n") (cria-personagem "o" pos-linha pos-coluna)]
        [(string=? direcao "o") (cria-personagem "s" pos-linha pos-coluna)]
        [(string=? direcao "s") (cria-personagem "l" pos-linha pos-coluna)]
        [(string=? direcao "l") (cria-personagem "n" pos-linha pos-coluna)]))

;; gira-direita retorna o personagem na nova direção
(define (gira-direita personagem-aux)
  (define direcao (personagem-direcao personagem-aux))
  (define pos-linha (personagem-pos-linha personagem-aux))
  (define pos-coluna (personagem-pos-coluna personagem-aux))
  (cond [(string=? direcao "n") (cria-personagem "l" pos-linha pos-coluna)]
        [(string=? direcao "l") (cria-personagem "s" pos-linha pos-coluna)]
        [(string=? direcao "s") (cria-personagem "o" pos-linha pos-coluna)]
        [(string=? direcao "o") (cria-personagem "n" pos-linha pos-coluna)]))

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
;; move-personagem retorna o personagem na sua nova posição no tabuleiro.
;; ou uma mensagem de erro.
(define (move-personagem personagem passos)
  (define direcao (personagem-direcao personagem))
  (cond [(string=? direcao "n") (move-norte personagem passos)]
        [(string=? direcao "s") (move-sul personagem passos)]
        [(string=? direcao "l") (move-leste personagem passos)]
        [(string=? direcao "o") (move-oeste personagem passos)]))

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

;; converte-lista recebe uma lista de números e retorna uma lista de strings
(define (converte-lista lista)
  (cond [(empty? lista) empty]
        [else
         (define lista-convertida (converte-lista-aux lista))
         (define tamanho-max (maior-string lista-convertida))
         (ajeita-string lista-convertida tamanho-max)]))

;; converte-lista-aux faz a conversão dos números da lista para strings
(define (converte-lista-aux lista)
  (cond [(empty? lista) empty]
        [else (cons (~a (first lista)) (converte-lista-aux (rest lista)))]))

;; encontra o tamanho da maior string da lista
(define (maior-string lista)
  (cond [(empty? lista) 0]
        [else (max (string-length (first lista)) (maior-string (rest lista)))]))

;; refatora as strings para ajeitá-las ao tamanho da maior
(define (ajeita-string lista tamanho-max)
  (cond [(empty? lista) empty]
        [else
         (define diferenca (- tamanho-max (string-length (first lista))))
         (define primeiro-convertido (string-append (make-string diferenca #\space) (first lista)))
         (cons primeiro-convertido (ajeita-string (rest lista) tamanho-max))]))
