#lang racket
(provide (all-defined-out))
(require "Registro_20575068_FernandezHernandez.rkt")
(require "Create_20575068_FernandezHernandez.rkt")
(require "anadir_20575068_FernandezHernandez.rkt")

;restoreVersion elementos
;contructor
;definir final funcion por id vacio
(define (endFuntion id) 
  null
  )
;Modificador
;restorar 
(define (restore number doc)
  (lambda (currentDoc) ;creacion currentDoc nuevo
              (actualParadoc(endSesion)
                 (Paradoc->users doc) ;Cadr del doc
                 (DocAct(Paradoc->Doc doc) (Doc->id currentDoc) (Doc->author currentDoc) ;creacion de nueva lista 
                                   (- 1 (Doc->numAdd currentDoc)) ;bajar a 1
                                   (Doc->content currentDoc));llevar al contenido
                 (endSesion);cerramos sesion
                 )))
  
  