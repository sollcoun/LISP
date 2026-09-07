;;; ============================================================
;;; DIMMLINE — размер М-линии у полилинии
;;; Если текст длиннее М-линии — вынос мультивыноской
;;; Команда: DIMMLINE
;;; ============================================================

(vl-load-com)

(defun dm:fmt (val / abs-val int-part frac-part)
  (setq abs-val (abs val))
  (setq int-part  (fix abs-val))
  (setq frac-part (fix (+ 0.5 (* (- abs-val int-part) 100))))
  (if (>= frac-part 100)
    (progn (setq int-part (1+ int-part)) (setq frac-part 0))
  )
  (strcat (itoa int-part) "."
          (if (< frac-part 10) (strcat "0" (itoa frac-part)) (itoa frac-part)))
)

(defun dm:mline-verts (ename / obj coords verts n k pair)
  (setq verts '())
  (setq obj (vlax-ename->vla-object ename))
  (setq coords (vl-catch-all-apply 'vlax-get (list obj 'Coordinates)))
  (if (and (not (vl-catch-all-error-p coords))
           (listp coords)
           (numberp (car coords)))
    (progn
      (setq n (length coords) k 0)
      (while (< k n)
        (setq verts (cons (list (nth k coords) (nth (1+ k) coords) 0.0) verts))
        (setq k (+ k 3))
      )
      (setq verts (reverse verts))
    )
  )
  (if (or (null verts) (< (length verts) 2))
    (progn
      (setq verts '())
      (foreach pair (entget ename)
        (if (= (car pair) 11)
          (setq verts (cons (list (cadr pair) (caddr pair) 0.0) verts))
        )
      )
      (setq verts (reverse verts))
    )
  )
  verts
)

(defun dm:poly-len (verts / i len)
  (setq len 0.0 i 0)
  (repeat (max 0 (1- (length verts)))
    (setq len (+ len (distance (nth i verts) (nth (1+ i) verts))))
    (setq i (1+ i))
  )
  len
)

(defun dm:dist-pt-poly (pt verts / i va vb ax ay bx by px py abx aby ab2 param qx qy d mind)
  (setq mind 1e99 px (car pt) py (cadr pt) i 0)
  (repeat (max 0 (1- (length verts)))
    (setq va (nth i verts) vb (nth (1+ i) verts)
          ax (car va) ay (cadr va) bx (car vb) by (cadr vb)
          abx (- bx ax) aby (- by ay)
          ab2 (+ (* abx abx) (* aby aby)))
    (if (< ab2 1e-18)
      (setq d (distance pt va))
      (progn
        (setq param (/ (+ (* (- px ax) abx) (* (- py ay) aby)) ab2))
        (if (< param 0.0) (setq param 0.0))
        (if (> param 1.0) (setq param 1.0))
        (setq qx (+ ax (* param abx)) qy (+ ay (* param aby)))
        (setq d (distance pt (list qx qy 0.0)))
      )
    )
    (if (< d mind) (setq mind d))
    (setq i (1+ i))
  )
  mind
)

(defun dm:pl-verts (ename / etype vlist v)
  (setq etype (cdr (assoc 0 (entget ename))) vlist '())
  (cond
    ((= etype "LWPOLYLINE")
     (foreach pair (entget ename)
       (if (= (car pair) 10)
         (setq vlist (append vlist (list (list (cadr pair) (caddr pair) 0.0))))
       )
     )
    )
    ((or (= etype "POLYLINE") (= etype "3DPOLYLINE"))
     (setq v (entnext ename))
     (while (and v (= (cdr (assoc 0 (entget v))) "VERTEX"))
       (setq vlist (append vlist (list (list (cadr (assoc 10 (entget v)))
                                             (caddr (assoc 10 (entget v))) 0.0))))
       (setq v (entnext v))
     )
    )
  )
  vlist
)

;;; --- Простая мультивыноска: острие → текст ---
(defun dm:add-mleader (pt-arrow pt-text str / mspace pts ml arr)
  (setq mspace (vla-get-ModelSpace
                 (vla-get-ActiveDocument (vlax-get-acad-object))))
  (setq pts (vlax-make-safearray vlax-vbDouble '(0 . 5)))
  (vlax-safearray-fill pts
    (list (car pt-arrow) (cadr pt-arrow) 0.0
          (car pt-text)  (cadr pt-text)  0.0))
  (setq ml (vl-catch-all-apply 'vla-AddMLeader (list mspace pts 0)))
  (if (vl-catch-all-error-p ml)
    (progn
      (princ (strcat "\n  Не удалось создать MLeader: "
                     (vl-catch-all-error-message ml)))
      nil
    )
    (progn
      (vl-catch-all-apply '(lambda () (vla-put-TextString ml str)))
      (vl-catch-all-apply '(lambda () (vla-put-TextHeight ml 1.0)))
      ml
    )
  )
)

;;; --- Главная команда ---
(defun c:DIMMLINE (/ ss pl-en pl-verts mid search-dist
                    ss-ml j ml-en ml-verts d found-list
                    len p1 p2 mid-ml mspace dim-obj txt
                    txt-h need-w off old-cmdecho)

  (vl-load-com)
  (setq old-cmdecho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)

  (princ "\n=== DIMMLINE: размер М-линии у полилинии ===")
  (princ "\nВыберите полилинию:")
  (setq ss (ssget '((0 . "LWPOLYLINE,POLYLINE,3DPOLYLINE"))))
  (if (null ss)
    (princ "\nНичего не выбрано.")
    (progn
      (setq search-dist 5.0)
      (setq txt-h 1.0)          ; высота текста для оценки ширины
      (setq found-list '())

      (setq pl-en (ssname ss 0))
      (setq pl-verts (dm:pl-verts pl-en))
      (if (< (length pl-verts) 2)
        (princ "\nНе удалось прочитать вершины полилинии.")
        (progn
          (setq ss-ml (ssget "_X" '((0 . "MLINE"))))
          (if (null ss-ml)
            (princ "\nВ чертеже нет М-линий.")
            (progn
              (setq j 0)
              (repeat (sslength ss-ml)
                (setq ml-en (ssname ss-ml j))
                (setq ml-verts (dm:mline-verts ml-en))
                (if (>= (length ml-verts) 2)
                  (progn
                    (setq d 1e99)
                    (foreach p ml-verts
                      (setq d (min d (dm:dist-pt-poly p pl-verts)))
                    )
                    (setq d (min d
                      (dm:dist-pt-poly
                        (nth (/ (length ml-verts) 2) ml-verts)
                        pl-verts)))
                    (if (<= d search-dist)
                      (setq found-list (cons (list ml-en ml-verts d) found-list))
                    )
                  )
                )
                (setq j (1+ j))
              )

              (if (null found-list)
                (princ (strcat "\nРядом с полилинией М-линий нет (допуск "
                               (rtos search-dist 2 1) ")."))
                (progn
                  (setq mspace (vla-get-ModelSpace
                                 (vla-get-ActiveDocument (vlax-get-acad-object))))
                  (foreach item found-list
                    (setq ml-en    (car item)
                          ml-verts (cadr item)
                          d        (caddr item)
                          len      (dm:poly-len ml-verts)
                          p1       (car ml-verts)
                          p2       (last ml-verts)
                          txt      (strcat "L=" (dm:fmt len))
                          mid-ml   (list (/ (+ (car p1) (car p2)) 2.0)
                                         (/ (+ (cadr p1) (cadr p2)) 2.0)
                                         0.0)
                          off      1.0)

                    ;; нужная ширина текста ≈ 0.6 * высота * число символов
                    (setq need-w (* (strlen txt) txt-h 0.6))

                    (setq dim-obj
                      (vla-AddDimAligned
                        mspace
                        (vlax-3d-point p1)
                        (vlax-3d-point p2)
                        (vlax-3d-point
                          (list (car mid-ml) (- (cadr mid-ml) off) 0.0))
                      )
                    )

                    (if dim-obj
                      (if (>= len need-w)
                        ;; текст помещается на линию размера
                        (progn
                          (vl-catch-all-apply
                            '(lambda () (vla-put-TextOverride dim-obj txt)))
                          (princ (strcat "\n  MLINE dist=" (rtos d 2 2)
                                         "  " txt "  (на размере)"))
                        )
                        ;; текст длиннее М-линии — вынос мультивыноской
                        (progn
                          (vl-catch-all-apply
                            '(lambda () (vla-put-TextOverride dim-obj " ")))
                          (dm:add-mleader
                            mid-ml
                            (list (+ (car mid-ml) (* txt-h 3.0))
                                  (+ (cadr mid-ml) (* txt-h 2.0))
                                  0.0)
                            txt
                          )
                          (princ (strcat "\n  MLINE dist=" (rtos d 2 2)
                                         "  " txt "  (мультивыноска, сегмент короткий)"))
                        )
                      )
                    )
                  )
                  (princ (strcat "\nГотово. Размеров: "
                                 (itoa (length found-list))))
                )
              )
            )
          )
        )
      )
    )
  )
  (setvar "CMDECHO" old-cmdecho)
  (princ)
)

(princ "\nDIMMLINE загружен. Команда: DIMMLINE")
(princ)