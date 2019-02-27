#lang racket/base

(require racket/string racket/match)

(provide get-completions)

(require srfi/2)

(define (parse-path str)
  (define-values (base sub dir?) (split-path str))
  (cond
    [(or dir? (memq sub '(same up))) (values str #f)]
    [(eq? base 'relative) (values "." sub)]
    [(eq? base #f) (values "/" sub)]
    [else (values base sub)]))

(define (get-completions str)
  (cond
    [(string=? str "") (map path->string (directory-list))]
    [else
     (define-values (b s) (parse-path str))
     (cond
       [(not s)
        (if (directory-exists? str)
            (map (λ (f)
                   (string-append
                    (path->string (path->directory-path b))
                    (path->string f)))
                 (directory-list str))
            '())]
       [else
        (map (λ (f)
               (path->string (build-path b f)))
             (filter (λ (f) (string-prefix? (path->string f) (path->string s)))
                     (directory-list b)))])]))
