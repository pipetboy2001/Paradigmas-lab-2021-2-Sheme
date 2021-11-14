#lang racket
(provide (all-defined-out))
(require "Registro.rkt")
(require "Create.rkt")
(require "anadir.rkt")

;restoreVersion elementos
(define (endFuntion id) 
  null
  )

(define (restore number doc)
  (lambda (currentDoc) ;creacion currentDoc nuevo
              (actualParadoc(endSesion)
                 (Paradoc->users doc) ;Cadr del doc
                 (DocAct(Paradoc->Doc doc) (Doc->id currentDoc) (Doc->author currentDoc) ;creacion de nueva lista 
                                   (- 1 (Doc->numAdd currentDoc)) ;bajar a 1
                                   (Doc->content currentDoc));llevar al contenido
                 (endSesion);cerramos sesion
                 )))
  
  