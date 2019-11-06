;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Esercizio 3 laboratorio|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define bintodec
  (lambda (n)
  (let ((punto (point-position n 0)))                                                                           ; punto = posizione del punto nella stringa, se esiste
    (cond ((string=? n "") 0)                                                                                
          ((number? punto) (+ (positivebin (substring n 0 punto)) (negativebin (substring n (+ punto 1)))))     ; se punto è un numero significa che esiste, altrimenti riceverei "false", in questo caso sommo la parte intera e quella frazionaria, convertite separatamente
          (else (positivebin n))                                                                                ; se non trovo un punto eseguo la normale conversione della parte intera
    ))))

(define positivebin
  (lambda (n)
    (cond ((string=? n "") 0)
          (else
             (+ (* (expt 2 (- (string-length n) 1))
                   (string->number (substring n 0 1)))
                (positivebin (substring n 1))
           )))))

(define negativebin
  (lambda (n)
    (cond ((string=? n "") 0)
          (else
             (+ (* (expt 2 (-(string-length n)))
                   (string->number (substring n (- (string-length n) 1))))
                (negativebin (substring n 0 (- (string-length n) 1)))
              )))))

(define point-position                                                                     ; Funziona che si occupa di trovare il punto all'interno della stringa e la sua posizione
  (lambda (str pos)                                                                        ; Stringa str in ingresso contenente il numero binario, pos è una variabile settata a 0 per tenere conto della posizione del punto
    (cond ((string=? str "") #f)                                                           ; se la stringa risulta vuota termino la funzione con un "false"
          ((string=? (substring str 0 1) ".") pos)                                         ; verifico se il primo carattere della stringa corrisponde al punto
          (else (point-position (substring str 1) (add1 pos)))                             ; se non trovo il punto eseguo una ricorsione alla stessa funzione rimuovendo il primo carattere e aggiungendo 1 alla posizione
    )))

(define converti                                                                           ; Funziona che si occupa di analizzare la stringa in ingresso e verificare se contiene segni
  (lambda (str)                                                                            ; stringa str
    (let ((primacifra (substring str 0 1)) (conversione (bintodec (substring str 1))))     ; primacifra = primo carattere della stringa , conversione = manda al convertitore la stringa col segno rimosso (se presente)
    (cond  ((string=? primacifra "-") (* conversione -1))                                  ; se trovo un "-" nel primo carattere lo rimuovo e moltiplico il numero per -1 così da renderlo negativo
           ((string=? primacifra "+") conversione)                                         ; se trovo un "+" nel primo carattere lo rimuovo 
           (else (bintodec str))                                                           ; se non trovo segni invio la stringa non modificata al convertitore
    ))))



