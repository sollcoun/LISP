;;; ============================================================
;;; vertex_leaders.lsp
;;; AutoLISP-скрипт для простановки координатных выносок
;;; по вершинам полилинии с использованием мультивыноски MLEADER
;;;
;;; Стиль мультивыноски: «Координаты»
;;; Текстовый стиль (для координат и номеров вершин): «Д-431»
;;;
;;; Автоматическое создание стиля «Координаты»:
;;;   Прямое создание объекта MLEADERSTYLE через entmake ненадёжно
;;;   (обязательные внутренние DXF-группы недокументированы и
;;;   различаются между версиями AutoCAD). Поэтому стиль
;;;   импортируется из отдельного чертежа-шаблона (++РАМКА++.dwg)
;;;   путём вставки его как временного блока — при этом AutoCAD
;;;   автоматически копирует все использованные в шаблоне стили
;;;   в текущий чертёж. Временный блок сразу удаляется.
;;;
;;;   Если стиль импортировать не удалось (шаблон не найден и т.п.),
;;;   скрипт не падает, а работает на базе стиля "Standard",
;;;   принудительно назначая все нужные параметры оформления
;;;   (без стрелки, горизонтальная полка, текстовый стиль «Д-431»,
;;;   маска фона) напрямую каждому созданному объекту.
;;; ============================================================


;;; ------------------------------------------------------------
;;; Вспомогательная функция: форматирование числа с обязательным
;;; знаком (+/-) и ровно двумя знаками после запятой.
;;; Пример: 1234.5 -> "+1234.50",  -789.1 -> "-0789.10"
;;; ------------------------------------------------------------
(defun vl:fmt-coord (val / sign abs-val int-part frac-part int-str frac-str)
  (setq sign (if (< val 0) "-" "+"))
  (setq abs-val (abs val))
  (setq int-part  (fix abs-val))
  (setq frac-part (fix (+ 0.5 (* (- abs-val int-part) 100))))
  (if (>= frac-part 100)
    (progn
      (setq int-part  (1+ int-part))
      (setq frac-part 0)
    )
  )
  (setq int-str  (itoa int-part))
  (setq frac-str (if (< frac-part 10)
                   (strcat "0" (itoa frac-part))
                   (itoa frac-part)))
  (strcat sign int-str "." frac-str)
)


;;; ------------------------------------------------------------
;;; Проверяет, существует ли стиль мультивыноски с данным именем
;;; в текущем чертеже. Возвращает T или nil.
;;; ------------------------------------------------------------
(defun vl:mleader-style-exists-p (style-name / dict)
  (and
    (setq dict (dictsearch (namedobjdict) "ACAD_MLEADERSTYLE"))
    (dictsearch (cdr (assoc -1 dict)) style-name)
  )
)


;;; ------------------------------------------------------------
;;; Проверяет/создаёт текстовый стиль (обычный STYLE) с заданным
;;; именем. Возвращает имя стиля (строку).
;;; ------------------------------------------------------------
(defun vl:ensure-text-style (style-name / doc styles ts result)
  (vl-load-com)
  (setq doc    (vla-get-activedocument (vlax-get-acad-object)))
  (setq styles (vla-get-textstyles doc))
  (setq result (vl-catch-all-apply 'vla-item (list styles style-name)))
  (if (vl-catch-all-error-p result)
    (progn
      (princ (strcat "\n  Текстовый стиль «" style-name "» не найден. Создаю..."))
      (setq ts (vla-add styles style-name))
      ;; Шрифт по умолчанию — при необходимости замените на нужный
      (vl-catch-all-apply '(lambda () (vla-put-fontfile ts "arial.ttf")))
      (princ (strcat "\n  Текстовый стиль «" style-name "» создан."))
    )
    (princ (strcat "\n  Текстовый стиль «" style-name "» найден."))
  )
  style-name
)


;;; ------------------------------------------------------------
;;; Автоматически подтягивает стиль мультивыноски (и все объекты,
;;; которые он использует) из чертежа-шаблона, если стиля нет
;;; в текущем чертеже. Работает через вставку чертежа-шаблона
;;; как блока — AutoCAD при этом копирует все использованные
;;; в шаблоне именованные объекты (стили, слои, типы линий)
;;; в текущий чертёж. После вставки временный блок удаляется.
;;;
;;; Аргументы:
;;;   template-path — полный путь к файлу ++РАМКА++.dwg
;;;   mleader-style — ожидаемое имя стиля мультивыноски
;;;
;;; Возвращает T, если стиль есть в чертеже (был или появился),
;;; и nil, если импортировать не удалось.
;;; ------------------------------------------------------------
(defun vl:ensure-mleader-style-from-template (template-path mleader-style
                                               / old-filedia old-cmdecho old-osmode)
  ;; Если стиль уже есть — ничего делать не нужно
  (if (vl:mleader-style-exists-p mleader-style)
    (progn
      (princ (strcat "\n  Стиль «" mleader-style "» уже есть в чертеже."))
      T
    )
    ;; Стиля нет — пробуем импортировать из шаблона
    (if (not (findfile template-path))
      (progn
        (princ (strcat "\n  ПРЕДУПРЕЖДЕНИЕ: файл шаблона не найден: " template-path))
        (princ "\n  Создайте ++РАМКА++.dwg с настроенным стилем «Координаты»")
        (princ "\n  (см. инструкцию) и укажите верный путь в переменной template-path.")
        nil
      )
      (progn
        (princ (strcat "\n  Импортирую стиль «" mleader-style "» из шаблона..."))
        (setq old-filedia (getvar "FILEDIA"))
        (setq old-cmdecho (getvar "CMDECHO"))
        (setq old-osmode  (getvar "OSMODE"))
        (setvar "FILEDIA" 0)   ; отключаем диалоги выбора файлов
        (setvar "CMDECHO" 0)
        (setvar "OSMODE"  0)

        ;; Вставляем чертёж-шаблон как блок в точку (0,0,0).
        ;; Это подтягивает используемые в нём стили в текущий чертёж.
        (command "_.-INSERT" template-path "_S" 1.0 "" "0,0,0" 0.0)

        ;; Удаляем последний созданный объект (вставленный блок) —
        ;; нужные стили уже скопированы и останутся в чертеже.
        (command "_.ERASE" "_L" "")

        ;; Восстанавливаем системные переменные
        (setvar "FILEDIA" old-filedia)
        (setvar "CMDECHO" old-cmdecho)
        (setvar "OSMODE"  old-osmode)

        ;; Проверяем результат
        (if (vl:mleader-style-exists-p mleader-style)
          (progn
            (princ (strcat "\n  Стиль «" mleader-style "» успешно импортирован."))
            T
          )
          (progn
            (princ (strcat "\n  ПРЕДУПРЕЖДЕНИЕ: импорт не удался, стиль «"
                           mleader-style "» не появился в чертеже."))
            nil
          )
        )
      )
    )
  )
)


;;; ------------------------------------------------------------
;;; Создаёт MLEADER (мультивыноску) через ActiveX (vla-объекты).
;;; Все ключевые параметры (без стрелки, горизонтальная полка,
;;; текст, маска фона) назначаются напрямую — это гарантирует
;;; правильный вид независимо от того, какой базовый стиль
;;; фактически используется (импортированный "Координаты" или
;;; резервный "Standard").
;;;
;;; Аргументы:
;;;   pt-arrow   — точка «острия» выноски (вершина полилинии)
;;;   pt-land    — точка начала горизонтальной полки
;;;   pt-text    — точка привязки текста (конец полки)
;;;   mtext-str  — строка содержимого (координаты x, y)
;;;   style-name — имя базового стиля MLEADER
;;;
;;; Возвращает: entity name созданного MLEADER или nil
;;; ------------------------------------------------------------
(defun vl:create-mleader (pt-arrow pt-land pt-text mtext-str style-name
                           / acadobj mspace mleader pts-variant)
  (vl-load-com)
  (setq acadobj (vlax-get-acad-object))
  (setq mspace  (vla-get-modelspace (vla-get-activedocument acadobj)))

  (setq pts-variant
    (vlax-make-safearray vlax-vbdouble '(0 . 5))
  )
  (vlax-safearray-put-element pts-variant 0 (car   pt-arrow))
  (vlax-safearray-put-element pts-variant 1 (cadr  pt-arrow))
  (vlax-safearray-put-element pts-variant 2 (caddr pt-arrow))
  (vlax-safearray-put-element pts-variant 3 (car   pt-land))
  (vlax-safearray-put-element pts-variant 4 (cadr  pt-land))
  (vlax-safearray-put-element pts-variant 5 (caddr pt-land))

  (setq mleader (vla-AddMLeader mspace pts-variant 0))

  (if (null mleader)
    (progn (princ "\n  ОШИБКА: не удалось создать MLEADER.") nil)
    (progn
      ;; Назначаем базовый стиль
      (vl-catch-all-apply
        '(lambda () (vla-put-StyleName mleader style-name))
      )
      ;; Отключаем стрелку: 20 = None (_None)
      (vl-catch-all-apply
        '(lambda () (vla-put-ArrowheadType mleader 20))
      )
      ;; Включаем горизонтальную полку
      (vl-catch-all-apply
        '(lambda () (vla-put-EnableLanding mleader :vlax-true))
      )
      ;; Тип выноски: 1 = прямая линия
      (vl-catch-all-apply
        '(lambda () (vla-put-LeaderLineType mleader 1))
      )
      ;; Тип содержимого: 1 = MTEXT
      (vl-catch-all-apply
        '(lambda () (vla-put-ContentType mleader 1))
      )
      ;; Текст
      (vl-catch-all-apply
        '(lambda () (vla-put-TextString mleader mtext-str))
      )
      ;; Выравнивание текста: по левому краю
      (vl-catch-all-apply
        '(lambda () (vla-put-TextAttachmentType mleader 1 1))
      )
      (vl-catch-all-apply
        '(lambda () (vla-put-TextAngle mleader 0.0))
      )
      ;; Маска фона для текста мультивыноски
      (vl-catch-all-apply
        '(lambda ()
           (vla-put-TextBackgroundFill mleader :vlax-true)
           (vla-put-TextBackgroundScaleFactor mleader 1.5)
        )
      )
      (vlax-vla-object->ename mleader)
    )
  )
)


;;; ------------------------------------------------------------
;;; Создаёт MTEXT с включённой маской фона (background mask)
;;; и заданным текстовым стилем.
;;;
;;; Аргументы:
;;;   ins-pt     — точка вставки текста
;;;   txt-str    — строка текста
;;;   height     — высота текста
;;;   width      — ширина текстового поля (0 = авто)
;;;   just       — выравнивание (AttachmentPoint)
;;;   style-name — имя текстового стиля (например, "Д-431")
;;; ------------------------------------------------------------
(defun vl:create-mtext-masked (ins-pt txt-str height width just style-name
                                / mspace doc mt)
  (vl-load-com)
  (setq doc    (vla-get-activedocument (vlax-get-acad-object)))
  (setq mspace (vla-get-modelspace doc))

  (setq mt
    (vla-AddMText mspace (vlax-3d-point ins-pt) (float width) txt-str)
  )
  (if mt
    (progn
      (vla-put-Height mt (float height))
      ;; Текстовый стиль (Д-431)
      (vl-catch-all-apply
        '(lambda () (vla-put-StyleName mt style-name))
      )
      (vla-put-AttachmentPoint mt just)
      (vl-catch-all-apply
        '(lambda ()
           (vla-put-BackgroundFill mt :vlax-true)
           (vla-put-BackgroundScaleFactor mt 1.5)
        )
      )
      (vlax-vla-object->ename mt)
    )
    nil
  )
)


;;; ------------------------------------------------------------
;;; Извлекает список вершин из полилинии (LWPOLYLINE или
;;; 2D/3D POLYLINE). Возвращает список точек в формате (x y z).
;;; ------------------------------------------------------------
(defun vl:get-polyline-vertices (ename / etype vlist vertex-pt)
  (setq etype (cdr (assoc 0 (entget ename))))
  (cond
    ((= etype "LWPOLYLINE")
     (setq vlist '())
     (foreach pair (entget ename)
       (if (= (car pair) 10)
         (setq vlist (append vlist (list (list (cadr pair) (caddr pair) 0.0))))
       )
     )
     vlist
    )
    ((or (= etype "POLYLINE") (= etype "3DPOLYLINE"))
     (setq vlist '())
     (setq vertex-pt (entnext ename))
     (while (and vertex-pt
                 (= (cdr (assoc 0 (entget vertex-pt))) "VERTEX"))
       (setq vlist
         (append vlist (list (cdr (assoc 10 (entget vertex-pt)))))
       )
       (setq vertex-pt (entnext vertex-pt))
     )
     vlist
    )
    (T
     (princ (strcat "\n  ОШИБКА: объект не является полилинией (тип: " etype ")."))
     nil
    )
  )
)


;;; ------------------------------------------------------------
;;; Возвращает высоту текста из системной переменной TEXTSIZE.
;;; Если равна 0 — возвращает разумный дефолт 2.5.
;;; ------------------------------------------------------------
(defun vl:get-text-height (/ h)
  (setq h (getvar "TEXTSIZE"))
  (if (or (null h) (<= h 0.0))
    2.5
    h
  )
)


;;; ============================================================
;;; ГЛАВНАЯ КОМАНДА: VERTEXLEADERS
;;; Запускается через команду AutoCAD: VERTEXLEADERS
;;; ============================================================
(defun c:VERTEXLEADERS (/ sel ename vertices pt-count idx pt
                          x-coord y-coord coord-text
                          mleader-style-name text-style-name actual-mleader-style
                          template-path
                          txt-height land-length land-offset
                          pt-land pt-text
                          ml-ename num-ename
                          num-text num-ins-pt
                          old-osmode old-cmdecho)

  ;; --- Инициализация ---
  (vl-load-com)
  (setq mleader-style-name "Координаты")   ; желаемое имя стиля MLEADER
  (setq text-style-name    "Д-431")        ; имя текстового стиля

  ;; ВАЖНО: укажите здесь реальный путь к вашему файлу-шаблону
  ;; ++РАМКА++.dwg, в котором один раз настроен стиль «Координаты».
  ;; Путь можно сделать относительным к папке самого lisp-файла —
  ;; проще всего положить ++РАМКА++.dwg в ту же папку и
  ;; прописать полный путь вручную ниже.
  (setq template-path "C:/Users/User/Desktop/LISP/++РАМКА++.dwg")

  (setq old-osmode  (getvar "OSMODE"))
  (setq old-cmdecho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (setvar "OSMODE"  0)  ; отключаем привязки на время работы скрипта

  ;; --- Блок обработки ошибок ---
  (defun *error* (msg)
    (setvar "OSMODE"  old-osmode)
    (setvar "CMDECHO" old-cmdecho)
    (if (not (wcmatch (strcase msg) "*CANCEL*,*EXIT*,*QUIT*"))
      (princ (strcat "\n  ОШИБКА: " msg))
    )
    (princ "\n  Команда отменена.")
    (princ)
  )

  (princ "\n=== Простановка координатных выносок по вершинам полилинии ===")
  (princ "\n  Команда: VERTEXLEADERS")

  ;; --- Проверяем текстовый стиль ---
  (vl:ensure-text-style text-style-name)

  ;; --- Проверяем / импортируем стиль мультивыноски из шаблона ---
  (if (vl:ensure-mleader-style-from-template template-path mleader-style-name)
    (setq actual-mleader-style mleader-style-name)
    (progn
      (princ "\n  Использую «Standard» как базу — все параметры оформления")
      (princ "\n  (без стрелки, полка, текст «Д-431», маска) назначаются вручную.")
      (setq actual-mleader-style "Standard")
    )
  )

  ;; --- Запрашиваем выбор полилинии ---
  (setq sel (entsel "\nВыберите полилинию: "))

  (if (null sel)
    (progn
      (princ "\n  Выбор отменён пользователем.")
      (setvar "OSMODE"  old-osmode)
      (setvar "CMDECHO" old-cmdecho)
      (princ)
      (exit)
    )
  )

  (setq ename (car sel))

  ;; Проверяем, что выбранный объект — полилиния
  (if (not (member (cdr (assoc 0 (entget ename)))
                   '("LWPOLYLINE" "POLYLINE" "3DPOLYLINE")))
    (progn
      (princ (strcat "\n  ОШИБКА: выбранный объект не является полилинией (тип: "
                     (cdr (assoc 0 (entget ename))) ")."))
      (setvar "OSMODE"  old-osmode)
      (setvar "CMDECHO" old-cmdecho)
      (princ)
      (exit)
    )
  )

  ;; --- Получаем вершины полилинии ---
  (setq vertices (vl:get-polyline-vertices ename))
  (if (null vertices)
    (progn
      (princ "\n  ОШИБКА: не удалось извлечь вершины полилинии.")
      (setvar "OSMODE"  old-osmode)
      (setvar "CMDECHO" old-cmdecho)
      (princ)
      (exit)
    )
  )

  (setq pt-count (length vertices))
  (princ (strcat "\n  Найдено вершин: " (itoa pt-count)))

  ;; --- Параметры оформления ---
  (setq txt-height  (vl:get-text-height))
  (setq land-length (* txt-height 5.0))
  (setq land-offset (* txt-height 1.0))

  ;; --- Цикл по вершинам ---
  (setq idx 1)
  (foreach pt vertices

    (princ (strcat "\n  Обработка вершины №" (itoa idx) "..."))

    (setq x-coord (car  pt))
    (setq y-coord (cadr pt))

    ;; \P — перевод строки в MTEXT
    (setq coord-text
      (strcat
        "x = " (vl:fmt-coord x-coord)
        "\\Py = " (vl:fmt-coord y-coord)
      )
    )

    ;; Точка начала полки — правее и выше вершины
    (setq pt-land
      (list
        (+ x-coord land-length)
        (+ y-coord (* txt-height 3.0))
        0.0
      )
    )
    ;; Точка конца полки (точка привязки текста)
    (setq pt-text
      (list
        (+ (car pt-land) land-length)
        (cadr pt-land)                   ; та же высота — полка горизонтальна
        0.0
      )
    )

    ;; --- Создаём мультивыноску (используем actual-mleader-style) ---
    (setq ml-ename
      (vl:create-mleader
        (list x-coord y-coord 0.0)
        pt-land
        pt-text
        coord-text
        actual-mleader-style
      )
    )

    (if ml-ename
      (princ (strcat "  Мультивыноска №" (itoa idx) " создана."))
      (princ (strcat "  ПРЕДУПРЕЖДЕНИЕ: мультивыноска №" (itoa idx) " не создана!"))
    )

    ;; --- Создаём номер вершины как отдельный MTEXT ---
    (setq num-text (itoa idx))
    (setq num-ins-pt
      (list
        (+ (car pt-text) land-offset)
        (cadr pt-text)
        0.0
      )
    )

    (setq num-ename
      (vl:create-mtext-masked
        num-ins-pt
        num-text
        txt-height
        0.0
        7                                ; AttachmentPoint 7 = Middle Left
        text-style-name                  ; Текстовый стиль «Д-431»
      )
    )

    (if num-ename
      (princ (strcat "  Номер вершины №" (itoa idx) " создан."))
      (princ (strcat "  ПРЕДУПРЕЖДЕНИЕ: номер вершины №" (itoa idx) " не создан!"))
    )

    (setq idx (1+ idx))
  ) ; конец foreach

  (princ (strcat "\n  Готово! Создано выносок: " (itoa (1- idx))))

  (setvar "OSMODE"  old-osmode)
  (setvar "CMDECHO" old-cmdecho)

  (command "_.REGEN")

  (princ "\n=== VERTEXLEADERS завершена ===")
  (princ)
)


;;; ============================================================
;;; ДОПОЛНИТЕЛЬНАЯ УТИЛИТА: CHECKSTYLE
;;; Проверяет наличие стиля мультивыноски «Координаты» (импортируя
;;; его из шаблона при необходимости) и текстового стиля «Д-431».
;;; Запуск: CHECKSTYLE
;;; ============================================================
(defun c:CHECKSTYLE (/ mleader-style-name text-style-name template-path)
  (setq mleader-style-name "Координаты")
  (setq text-style-name    "Д-431")

  ;; ВАЖНО: тот же путь к шаблону, что и в c:VERTEXLEADERS
  (setq template-path "C:/CAD_Styles/++РАМКА++.dwg")

  (vl-load-com)
  (vl:ensure-text-style text-style-name)
  (vl:ensure-mleader-style-from-template template-path mleader-style-name)
  (princ)
)


;;; ============================================================
;;; Сообщение об успешной загрузке
;;; ============================================================
(princ "\n+---------------------------------------------------+")
(princ "\n|  vertex_leaders.lsp успешно загружен.              |")
(princ "\n|  Доступные команды:                                |")
(princ "\n|    VERTEXLEADERS — создать выноски по вершинам     |")
(princ "\n|    CHECKSTYLE    — проверить/импортировать стили   |")
(princ "\n|                    (Координаты / Д-431)            |")
(princ "\n+---------------------------------------------------+")
(princ)

;;; ============================================================
;;; Конец файла vertex_leaders.lsp
;;; ============================================================