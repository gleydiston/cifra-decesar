;O procedimento string-map recebe um procedimento proc que transforma
;caracteres,uma string, e retorna uma nova string onde os caracteres
;são o resultado da aplicação de proc.

(define string-map  
    (lambda (proc s)
        (let ((l (string->list s)))
            (let ((new-list (map proc l)))
                (list->string new-list)))))

;string-upcase trata todas os caracteres de entrada para caixa alta.

(define string-upcase
    (lambda (s)
        (string-map char-upcase s)))

;com o procedimento char->integer podemos transformar as 26 letras do
;alfabeto em inteiros de o a 25.

(define char->cesar-idx
    (lambda (x)
        (let ((a (char->integer #\A)))
            (- (char->integer x) a ))))

;assim como podemos passar inteiros para caracteres com a mesma lógica.

(define cesar-idx->char (lambda (x)
    (let ((a (char->integer #\A)))
        (let ((b (+ x a)))
            (integer->char b)))))

;A função cesar-traslate-char é a responsável por fazer
;o deslocamento dos caracteres.
;Caraacteres especiais e o espaço não são deslocados, ou seja
;eles permanecem imutaveis.

(define cesar-traslate-char (lambda (c k)
    (if (char-alphabetic? c)
        (cesar-idx->char (modulo (+ (char->cesar-idx c)
            k)
                26))
        c)))


(define cesar-encrypt (lambda (s)
    (let ((cesar-enc-10 (lambda (c)
        (cesar-traslate-char c 10))))
    (string-map cesar-enc-10
        (string-upcase s)))))

 (define cesar-decrypt
    (lambda (s)
        (let ((cesar-dec-10 (lambda (c)
            (cesar-traslate-char c -10))))
                (string-map cesar-dec-10
                    (string-upcase s)))))

