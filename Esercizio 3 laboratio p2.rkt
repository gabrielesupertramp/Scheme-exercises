;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Esercizio 3 laboratio p2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define converti                                                                                   ; Funzione che si occupa di analizzare la stringa in ingresso e verificare se contiene segni
  (lambda (strbase str)                                                                            ; stringa str = ciò che voglio convertire, strbase = indica la base utilizzata dalla stringa che voglio convertire in decimale
    (let ((primacifra (substring str 0 1)) (conversione (basetodec strbase (substring str 1))))    ; primacifra = primo carattere della stringa , conversione = manda al convertitore la stringa col segno rimosso (se presente)
    (cond  ((string=? primacifra "-") (* conversione -1))                                          ; se trovo un "-" nel primo carattere lo rimuovo e moltiplico il numero per -1 così da renderlo negativo
           ((string=? primacifra "+") conversione)                                                 ; se trovo un "+" nel primo carattere lo rimuovo 
           (else (basetodec strbase str))                                                          ; se non trovo segni invio la stringa non modificata al convertitore
    ))))

(define basetodec                                                                                                            ; questa funzione si occupa di separare la parte intera da quella frazionaria del numero, trovando il punto
  (lambda (strbase str)
  (let ((punto (find-position str 0 ".")))                                                                                   ; punto = posizione del punto nella stringa, se esiste
    (cond ((string=? str "") 0)                                                                                
          ((number? punto) (+ (positive strbase (substring str 0 punto)) (fractional strbase (substring str (+ punto 1)))))  ; se punto è un numero significa che esiste, altrimenti riceverei "false", in questo caso sommo la parte intera e quella frazionaria, convertite separatamente
          (else (positive strbase str))                                                                                      ; se non trovo un punto eseguo la normale conversione della parte intera
    ))))

(define positive                                                              ; Converte la parte intera in decimale
  (lambda (strbase str)
    (cond ((string=? str "") 0)
          (else
             (+ (* (expt (lunghezzabase strbase) (- (string-length str) 1))   ; Come nella conversione binaria, elevo la base alla posizione della stringa in cui mi trovo, se ho base 5 e mi trovo a pos=0 avrò 5^0
                   (find-position strbase 0 (substring str 0 1)))             ; la strbase contiene tutti i caratteri/numeri utilizzati dalla base in ordine crescente, mi basta cercare il carattere trovato in str in strbase per conoscerne il valore
                (positive strbase (substring str 1))
           )))))

(define fractional
  (lambda (strbase str)
    (cond ((string=? str "") 0)
          (else
             (+ (* (expt (lunghezzabase strbase) (-(string-length str)))
                   (find-position strbase 0 (substring str (- (string-length str) 1))))
                (fractional strbase (substring str 0 (- (string-length str) 1)))
              )))))

(define find-position                                                                     ; Funzione che si occupa di trovare un carattere all'interno della stringa e la sua posizione
  (lambda (str pos tofind)                                                                ; Stringa str da analizzare, pos = posizione di ciò che voglio trovare, tofind = carattere che sto cercando
    (cond ((string=? str "") #f)                                                          ; se la stringa risulta vuota termino la funzione con un "false"
          ((string=? (substring str 0 1) tofind) pos)                                     ; verifico se il primo carattere della stringa corrisponde al carattere che cerco
          (else (find-position (substring str 1) (add1 pos) tofind))                      ; se non trovo il carattere eseguo una ricorsione alla stessa funzione rimuovendo il primo carattere e aggiungendo 1 alla posizione
    )))

(define lunghezzabase                                                                     ; Trova la lunghezza della stringa che mi indica la base, sarà il numero che userò nella funzione expt per ricavare il numero decimale corrispondente
  (lambda (strbase)
    (string-length strbase)
    ))
  


