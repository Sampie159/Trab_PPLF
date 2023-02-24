#lang racket

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
;; Este personagem pode somente andar em linha reta.
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
  (cond [(string=? direcao "e") gira-esquerda personagem]
        [(string=? direcao "d") gira-direita personagem]
        [else "Direção invalida."]))

;; gira-esquerda retorna o personagem na nova direção
(define (gira-esquerda personagem-aux)
  (define direcao (personagem-direcao personagem-aux))
  (define pos-linha (personagem-pos-linha personagem-aux))
  (define pos-coluna (personagem-pos-coluna personagem-aux))
  (cond [(string=? direcao "n") (personagem "o" pos-linha pos-coluna)]
        [(string=? direcao "o") (personagem "s" pos-linha pos-coluna)]
        [(string=? direcao "s") (personagem "l" pos-linha pos-coluna)]
        [(string=? direcao "l") (personagem "n" pos-linha pos-coluna)]))

;; gira-direita retorna o personagem na nova direção
(define (gira-direita personagem-aux)
  (define direcao (personagem-direcao personagem-aux))
  (define pos-linha (personagem-pos-linha personagem-aux))
  (define pos-coluna (personagem-pos-coluna personagem-aux))
  (cond [(string=? direcao "n") (personagem "l" pos-linha pos-coluna)]
        [(string=? direcao "l") (personagem "s" pos-linha pos-coluna)]
        [(string=? direcao "s") (personagem "o" pos-linha pos-coluna)]
        [(string=? direcao "o") (personagem "n" pos-linha pos-coluna)]))
