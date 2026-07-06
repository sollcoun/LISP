;;; ===============================================================
;;; PLREPORT.LSP
;;; Команда: PLREPORT
;;;
;;; Что делает:
;;;   1. Берёт выделенную (или предлагает выбрать) полилинию LWPOLYLINE
;;;   2. Проставляет расстояния между всеми вершинами полилинии
;;;   3. Подписывает координаты X и Y у каждой вершины
;;;   4. Предлагает выгрузить таблицу координат в Excel, Word или CSV
;;;
;;; Загрузка: APPLOAD -> выбрать этот файл -> Load
;;; Запуск:   в командной строке AutoCAD ввести PLREPORT
;;; ===============================================================

(vl-load-com)

;; --------------------------------------------------------------
;; Создание однострочного текста с выравниванием по центру
;; --------------------------------------------------------------
(defun mk-text (pt height str ang / )
  (entmake
    (list
      '(0 . "TEXT")
      (cons 8 (getvar "CLAYER"))
      (cons 10 pt)
      (cons 11 pt)
      (cons 40 height)
      (cons 50 ang)
      (cons 72 1)   ; горизонтальное выравнивание - Center
      (cons 73 2)   ; вертикальное выравнивание - Middle
      (cons 1 str)
    )
  )
)

;; --------------------------------------------------------------
;; Получение списка координат вершин LWPOLYLINE
;; --------------------------------------------------------------
(defun get-lwpoly-pts (ent / edata pts)
  (setq edata (entget ent))
  (setq pts '())
  (foreach pair edata
    (if (= (car pair) 10)
      (setq pts (append pts (list (cdr pair))))
    )
  )
  pts
)

;; --------------------------------------------------------------
;; Приведение угла текста так, чтобы он не был "вверх ногами"
;; --------------------------------------------------------------
(defun normalize-ang (ang / halfpi threehalfpi)
  (setq halfpi (/ pi 2.0))
  (setq threehalfpi (* pi 1.5))
  (if (and (> ang halfpi) (< ang threehalfpi))
    (+ ang pi)
    ang
  )
)

;; --------------------------------------------------------------
;; Экспорт таблицы координат в Excel через COM-автоматизацию
;; data - список вида ((N X Y) (N X Y) ...)
;; --------------------------------------------------------------
(defun export-excel (data / xlApp xlBooks xlBook xlSheet row rec)
  (setq xlApp (vlax-get-or-create-object "Excel.Application"))
  (vlax-put-property xlApp 'Visible :vlax-true)
  (setq xlBooks (vlax-get-property xlApp 'Workbooks))
  (setq xlBook (vlax-invoke-property xlBooks 'Add))
  (setq xlSheet (vlax-get-property xlBook 'ActiveSheet))
  (vlax-put-property (vlax-invoke-property xlSheet 'Cells 1 1) 'Value2 "№ точки")
  (vlax-put-property (vlax-invoke-property xlSheet 'Cells 1 2) 'Value2 "X")
  (vlax-put-property (vlax-invoke-property xlSheet 'Cells 1 3) 'Value2 "Y")
  (setq row 2)
  (foreach rec data
    (vlax-put-property (vlax-invoke-property xlSheet 'Cells row 1) 'Value2 (nth 0 rec))
    (vlax-put-property (vlax-invoke-property xlSheet 'Cells row 2) 'Value2 (nth 1 rec))
    (vlax-put-property (vlax-invoke-property xlSheet 'Cells row 3) 'Value2 (nth 2 rec))
    (setq row (1+ row))
  )
  ;; автоподбор ширины столбцов
  (vlax-invoke-property (vlax-get-property xlSheet 'Columns) 'AutoFit)
  (vlax-release-object xlSheet)
  (vlax-release-object xlBook)
  (vlax-release-object xlBooks)
  (vlax-release-object xlApp)
  (princ "\nТаблица координат выгружена в Excel.")
  (princ)
)

;; --------------------------------------------------------------
;; Экспорт таблицы координат в Word через COM-автоматизацию
;; --------------------------------------------------------------
(defun export-word (data / wdApp wdDocs wdDoc rng tbl rows cols row rec cellObj)
  (setq rows (1+ (length data)))
  (setq cols 3)
  (setq wdApp (vlax-get-or-create-object "Word.Application"))
  (vlax-put-property wdApp 'Visible :vlax-true)
  (setq wdDocs (vlax-get-property wdApp 'Documents))
  (setq wdDoc (vlax-invoke-property wdDocs 'Add))
  (setq rng (vlax-get-property wdDoc 'Content))
  (setq tbl (vlax-invoke-property (vlax-get-property wdDoc 'Tables) 'Add rng rows cols))

  ;; заголовок таблицы
  (setq cellObj (vlax-invoke-property tbl 'Cell 1 1))
  (vlax-put-property (vlax-get-property cellObj 'Range) 'Text "№ точки")
  (setq cellObj (vlax-invoke-property tbl 'Cell 1 2))
  (vlax-put-property (vlax-get-property cellObj 'Range) 'Text "X")
  (setq cellObj (vlax-invoke-property tbl 'Cell 1 3))
  (vlax-put-property (vlax-get-property cellObj 'Range) 'Text "Y")

  ;; данные
  (setq row 2)
  (foreach rec data
    (setq cellObj (vlax-invoke-property tbl 'Cell row 1))
    (vlax-put-property (vlax-get-property cellObj 'Range) 'Text (itoa (nth 0 rec)))
    (setq cellObj (vlax-invoke-property tbl 'Cell row 2))
    (vlax-put-property (vlax-get-property cellObj 'Range) 'Text (rtos (nth 1 rec) 2 3))
    (setq cellObj (vlax-invoke-property tbl 'Cell row 3))
    (vlax-put-property (vlax-get-property cellObj 'Range) 'Text (rtos (nth 2 rec) 2 3))
    (setq row (1+ row))
  )
  (princ "\nТаблица координат выгружена в Word.")
  (princ)
)

;; --------------------------------------------------------------
;; Экспорт таблицы координат в CSV файл (открывается Excel/Word)
;; --------------------------------------------------------------
(defun export-csv (data fname / f rec)
  (setq f (open fname "w"))
  (write-line "№ точки;X;Y" f)
  (foreach rec data
    (write-line
      (strcat (itoa (nth 0 rec)) ";" (rtos (nth 1 rec) 2 3) ";" (rtos (nth 2 rec) 2 3))
      f
    )
  )
  (close f)
  (princ (strcat "\nCSV файл сохранён: " fname))
  (princ)
)

;; --------------------------------------------------------------
;; Создание текста с принудительным использованием стиля Д-431
;; --------------------------------------------------------------
;; 1. Проверка и создание стиля "Д-431"
(defun ensure-style-exists (styleName fontFile /)
  (if (null (tblsearch "STYLE" styleName))
    (progn
      (entmake
        (list
          '(0 . "STYLE")
          '(100 . "AcDbSymbolTableRecord")
          '(100 . "AcDbTextStyleTableRecord")
          (cons 2 styleName)
          (cons 70 0)
          (cons 40 0.0)
          (cons 41 1.0)
          (cons 3 fontFile)
        )
      )
      (princ (strcat "\nСтиль " styleName " создан успешно."))
    )
  )
)

;; 2. Создание текста (с использованием стиля Д-431)
(defun mk-text (pt height str ang / )
  (entmake
    (list
      '(0 . "TEXT")
      (cons 8 (getvar "CLAYER"))
      (cons 7 "Д-431")                ;; Принудительный стиль
      (cons 10 pt)
      (cons 11 pt)
      (cons 40 height)
      (cons 50 ang)
      (cons 72 1)
      (cons 73 2)
      (cons 1 str)
    )
  )
)

;; --------------------------------------------------------------
;; Основная команда
;; --------------------------------------------------------------
(defun c:PLREPORT (/ ss ent edata pts n i p1 p2 dist mid ang txtht
                      closedFlag tableData choice fname)
  (vl-load-com)

  ;; если объект уже выделен на экране до запуска команды - используем его
  (setq ss (ssget "_I"))
  (if ss
    (setq ent (ssname ss 0))
    (setq ent (car (entsel "\nВыберите полилинию: ")))
  )

  (if (null ent)
    (princ "\nОбъект не выбран.")
    (progn
      (setq edata (entget ent))
      (if (/= (cdr (assoc 0 edata)) "LWPOLYLINE")
        (princ "\nВыбранный объект должен быть полилинией (LWPOLYLINE).")
        (progn
          (setq pts (get-lwpoly-pts ent))
          (setq n (length pts))
          (setq closedFlag (= (logand (cdr (assoc 70 edata)) 1) 1))
          (setq txtht (getvar "DIMTXT"))
          (if (or (null txtht) (= txtht 0)) (setq txtht 2.5))
          (setq tableData '())
          (setq i 0)

          (while (< i n)
            (setq p1 (nth i pts))

            ;; подпись координат вершины
            (mk-text
              (list (+ (car p1) txtht) (+ (cadr p1) txtht) 0.0)
              txtht
              (strcat "X=" (rtos (car p1) 2 3) " Y=" (rtos (cadr p1) 2 3))
              0.0
            )
            (setq tableData (append tableData (list (list (1+ i) (car p1) (cadr p1)))))

            ;; расстояние до следующей вершины (или до первой, если контур замкнут)
            (cond
              ((< (1+ i) n) (setq p2 (nth (1+ i) pts)))
              (closedFlag (setq p2 (nth 0 pts)))
              (t (setq p2 nil))
            )

            (if p2
              (progn
                (setq dist (distance p1 p2))
                (setq mid (list (/ (+ (car p1) (car p2)) 2.0)
                                 (/ (+ (cadr p1) (cadr p2)) 2.0) 0.0))
                (setq ang (normalize-ang (angle p1 p2)))
                (mk-text mid txtht (rtos dist 2 2) ang)
              )
            )
            (setq i (1+ i))
          )

          (princ (strcat "\nОбработано вершин: " (itoa n)))

          (initget "Excel Word CSV Нет")
          (setq choice (getkword
            "\nВывести координаты в таблицу? [Excel/Word/CSV/Нет] <Excel>: "))
          (if (null choice) (setq choice "Excel"))

          (cond
            ((= choice "Excel") (export-excel tableData))
            ((= choice "Word") (export-word tableData))
            ((= choice "CSV")
             (setq fname (getfiled "Сохранить как CSV" "" "csv" 1))
             (if fname (export-csv tableData fname))
            )
          )
        )
      )
    )
  )
  (princ)
)

(princ "\nКоманда PLREPORT загружена. Введите PLREPORT для запуска.")
(princ)