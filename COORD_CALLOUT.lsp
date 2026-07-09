;;; ============================================================
;;; vertex_leaders.lsp
;;; AutoLISP-скрипт для простановки координатных выносок
;;; по вершинам полилинии + параллельных размеров по её сегментам.
;;;
;;; Стиль мультивыноски: «Координаты»
;;; Текстовый стиль (для координат, номеров и размеров): «Д-431»
;;;
;;; Автоматическое создание стиля «Координаты»:
;;;   Прямое создание объекта MLEADERSTYLE через entmake ненадёжно.
;;;   Стиль импортируется из чертежа-шаблона (style_template.dwg)
;;;   путём вставки его как временного блока. Если импортировать
;;;   не удалось — скрипт работает на базе стиля "Standard",
;;;   принудительно назначая все нужные параметры оформления
;;;   (без стрелки, горизонтальная полка, текстовый стиль «Д-431»,
;;;   маска фона) напрямую каждому созданному объекту.
;;;
;;; Способ вывода координат по вершинам полилинии — на выбор:
;;;   - Мультивыноска (MLEADER) со строками "X= .." / "Y= ..";
;;;   - Вхождение атрибутивного блока «XY», вставляемое точкой
;;;     вставки прямо в вершину полилинии (без дополнительной
;;;     линии-соединителя). Атрибуты X/Y блока заполняются строкой
;;;     вида "+123.45"/"-67.89" (обязательный знак), высота текста
;;;     атрибутов принудительно приравнивается к высоте текста
;;;     мультивыноски (txt-height), независимо от масштаба вставки
;;;     самого блока.
;;;
;;; Параллельные размеры по сегментам полилинии:
;;;   Для каждого сегмента (между соседними вершинами) создаётся
;;;   параллельный (aligned) размер, смещённый ПОД полилинию на
;;;   заданное расстояние. Встроенный текст размера скрывается,
;;;   а значение длины выводится отдельным MTEXT с маской фона —
;;;   либо прямо на линии размера (если сегмент достаточно длинный
;;;   для текста), либо выносится мультивыноской в сторону, если
;;;   сегмент короче, чем требуется для отображения цифр.
;;;
;;; Экспорт таблицы координат:
;;;   После построения выносок скрипт предлагает экспортировать
;;;   таблицу координат в CSV (Excel) и/или RTF (Word).
;;; ============================================================


;;; ------------------------------------------------------------
;;; Глобальный счётчик сквозной нумерации вершин.
;;;
;;; Хранит номер ПОСЛЕДНЕЙ использованной вершины между вызовами
;;; команды VERTEXLEADERS в рамках одного сеанса AutoCAD (пока
;;; чертёж/AutoCAD не закрыты). При первой обработке полилинии в
;;; сеансе значение = nil, поэтому нумерация начинается с 1 БЕЗ
;;; лишних вопросов. При обработке второй и последующих полилиний
;;; пользователю задаётся вопрос: продолжить нумерацию дальше
;;; (например, с №4, если предыдущая полилиния закончилась на №3)
;;; или начать заново с №1.
;;;
;;; Инициализация выполняется только если переменная ещё НЕ была
;;; объявлена — это защищает счётчик от сброса, если пользователь
;;; повторно загрузит (APPLOAD) этот файл в течение того же сеанса.
;;; ------------------------------------------------------------
(if (not (boundp '*VL:LAST-VERTEX-NUM*)) (setq *VL:LAST-VERTEX-NUM* nil))
(if (not (boundp '*VL:ALL-EXPORT-DATA*)) (setq *VL:ALL-EXPORT-DATA* '()))
(if (not (boundp '*VL:LAST-POLY-ENAME*)) (setq *VL:LAST-POLY-ENAME* nil))


;;; ------------------------------------------------------------
;;; Форматирование координаты: обязательный знак (+/-) и ровно
;;; два знака после запятой. Пример: 1234.5 -> "+1234.50"
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
;;; Форматирование длины сегмента: без знака, ровно два знака
;;; после запятой. Пример: 1234.5 -> "1234.50"
;;; ------------------------------------------------------------
(defun vl:fmt-length (val / abs-val int-part frac-part int-str frac-str)
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
  (strcat int-str "." frac-str)
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
;;; Проверяет, существует ли размерный стиль (DIMSTYLE) с данным
;;; именем в текущем чертеже. Возвращает T или nil.
;;; ------------------------------------------------------------
(defun vl:dimstyle-exists-p (style-name / doc styles result)
  (vl-load-com)
  (setq doc    (vla-get-activedocument (vlax-get-acad-object)))
  (setq styles (vla-get-dimstyles doc))
  (setq result (vl-catch-all-apply 'vla-item (list styles style-name)))
  (not (vl-catch-all-error-p result))
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
      (vl-catch-all-apply '(lambda () (vla-put-fontfile ts "arial.ttf")))
      (princ (strcat "\n  Текстовый стиль «" style-name "» создан."))
    )
    (princ (strcat "\n  Текстовый стиль «" style-name "» найден."))
  )
  style-name
)

;;; ------------------------------------------------------------
;;; Проверяет, является ли полилиния замкнутой.
;;; Работает через ActiveX-свойство Closed (LWPOLYLINE / POLYLINE).
;;;
;;; ВАЖНО: vla-get-Closed возвращает не T/nil, а символы
;;; :vlax-true / :vlax-false. :vlax-false НЕ равен nil, поэтому
;;; его нельзя использовать в (if ...) напрямую — нужно явное
;;; сравнение с :vlax-true.
;;; ------------------------------------------------------------
(defun vl:polyline-closed-p (ename / obj result)
  (vl-load-com)
  (setq obj (vlax-ename->vla-object ename))
  (setq result (vl-catch-all-apply 'vla-get-Closed (list obj)))
  (if (vl-catch-all-error-p result)
    nil
    (= result :vlax-true)
  )
)

;;; ------------------------------------------------------------
;;; Автоматически подтягивает стиль мультивыноски из чертежа-
;;; шаблона, если стиля нет в текущем чертеже (через вставку
;;; шаблона как временного блока с последующим удалением).
;;;
;;; Возвращает T, если стиль есть в чертеже, и nil в противном случае.
;;; ------------------------------------------------------------
(defun vl:ensure-mleader-style-from-template (template-path mleader-style
                                               / old-filedia old-cmdecho old-osmode)
  (if (vl:mleader-style-exists-p mleader-style)
    (progn
      (princ (strcat "\n  Стиль «" mleader-style "» уже есть в чертеже."))
      T
    )
    (if (not (findfile template-path))
      (progn
        (princ (strcat "\n  ПРЕДУПРЕЖДЕНИЕ: файл шаблона не найден: " template-path))
        (princ "\n  Создайте style_template.dwg с настроенным стилем «Координаты»")
        (princ "\n  и укажите верный путь в переменной template-path.")
        nil
      )
      (progn
        (princ (strcat "\n  Импортирую стиль «" mleader-style "» из шаблона..."))
        (setq old-filedia (getvar "FILEDIA"))
        (setq old-cmdecho (getvar "CMDECHO"))
        (setq old-osmode  (getvar "OSMODE"))
        (setvar "FILEDIA" 0)
        (setvar "CMDECHO" 0)
        (setvar "OSMODE"  0)

        (command "_.-INSERT" template-path "_S" 1.0 "0,0,0" 0.0)
        (command "_.ERASE" "_L" "")

        (setvar "FILEDIA" old-filedia)
        (setvar "CMDECHO" old-cmdecho)
        (setvar "OSMODE"  old-osmode)

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
;;; Автоматически подтягивает размерный стиль (DIMSTYLE) из чертежа-
;;; шаблона, если стиля нет в текущем чертеже — тем же способом,
;;; что и стиль мультивыноски: вставка шаблона как временного блока
;;; с последующим удалением.
;;;
;;; Возвращает T, если стиль есть в чертеже, и nil в противном случае.
;;; ------------------------------------------------------------
(defun vl:ensure-dimstyle-from-template (template-path dim-style
                                          / old-filedia old-cmdecho old-osmode)
  (if (vl:dimstyle-exists-p dim-style)
    (progn
      (princ (strcat "\n  Размерный стиль «" dim-style "» уже есть в чертеже."))
      T
    )
    (if (not (findfile template-path))
      (progn
        (princ (strcat "\n  ПРЕДУПРЕЖДЕНИЕ: файл шаблона не найден: " template-path))
        nil
      )
      (progn
        (princ (strcat "\n  Импортирую размерный стиль «" dim-style "» из шаблона..."))
        (setq old-filedia (getvar "FILEDIA"))
        (setq old-cmdecho (getvar "CMDECHO"))
        (setq old-osmode  (getvar "OSMODE"))
        (setvar "FILEDIA" 0)
        (setvar "CMDECHO" 0)
        (setvar "OSMODE"  0)

        (command "_.-INSERT" template-path "_S" 1.0 "0,0,0" 0.0)
        (command "_.ERASE" "_L" "")

        (setvar "FILEDIA" old-filedia)
        (setvar "CMDECHO" old-cmdecho)
        (setvar "OSMODE"  old-osmode)

        (if (vl:dimstyle-exists-p dim-style)
          (progn
            (princ (strcat "\n  Размерный стиль «" dim-style "» успешно импортирован."))
            T
          )
          (progn
            (princ (strcat "\n  ПРЕДУПРЕЖДЕНИЕ: импорт не удался, размерный стиль «"
                           dim-style "» не появился в чертеже."))
            nil
          )
        )
      )
    )
  )
)

;;; ------------------------------------------------------------
;;; Проверяет / импортирует атрибутивный блок coord-block-name из
;;; чертежа-шаблона (если его ещё нет в текущем чертеже).
;;; Вызывается ОДИН РАЗ до начала цикла по вершинам — не путать
;;; с вставкой самого блока в вершину (это делает
;;; vl:insert-coord-block отдельно для каждой вершины).
;;;
;;; Возвращает T, если блок в итоге есть в чертеже, иначе nil.
;;; ------------------------------------------------------------
(defun vl:ensure-coord-block-from-template (template-path block-name)
  (if (tblsearch "BLOCK" block-name)
    (progn
      (princ (strcat "\n  Блок «" block-name "» уже есть в чертеже."))
      T
    )
    (if (not (findfile template-path))
      (progn
        (princ (strcat "\n  ПРЕДУПРЕЖДЕНИЕ: файл шаблона не найден: " template-path))
        nil
      )
      (progn
        (princ (strcat "\n  Импортирую блок «" block-name "» из шаблона..."))
        (setvar "FILEDIA" 0)
        ;; Синтаксис "путь_к_dwg=имя_блока" — INSERT берёт из внешнего
        ;; DWG именно вложенный блок с таким именем, а не весь чертёж.
        (command "_.-INSERT" (strcat template-path "=" block-name) "_S" 1.0 "0,0,0" 0.0)
        (command "_.ERASE" "_L" "")
        (if (tblsearch "BLOCK" block-name)
          (progn
            (princ (strcat "\n  Блок «" block-name "» успешно импортирован."))
            T
          )
          (progn
            (princ (strcat "\n  ОШИБКА: блок «" block-name
                           "» не найден ни в чертеже, ни в шаблоне."))
            nil
          )
        )
      )
    )
  )
)

;;; ------------------------------------------------------------
;;; Создаёт MLEADER (мультивыноску) через ActiveX (vla-объекты).
;;; Все ключевые параметры назначаются напрямую каждому объекту —
;;; это гарантирует правильный вид независимо от базового стиля.
;;;
;;; Аргументы:
;;;   pt-arrow   — точка «острия» выноски
;;;   pt-land    — точка начала горизонтальной полки
;;;   pt-text    — точка привязки текста (справочно, для геометрии
;;;                самой линии выноски; фактическая длина полки
;;;                задаётся отдельно параметром dogleg-len)
;;;   mtext-str  — строка содержимого
;;;   style-name — имя базового стиля MLEADER
;;;   dogleg-len — желаемая длина горизонтальной полки
;;;
;;; Возвращает: entity name созданного MLEADER или nil
;;; ------------------------------------------------------------
(defun vl:create-mleader (pt-arrow pt-land pt-text mtext-str style-name dogleg-len
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
      ;; --- Включаем горизонтальную полку (правильное имя свойства!) ---
      (vl-catch-all-apply
        '(lambda () (vla-put-EnableDogleg mleader :vlax-true))
      )
      ;; --- Реальная длина полки ---
      (vl-catch-all-apply
        '(lambda () (vla-put-DoglegLength mleader dogleg-len))
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
      ;; --- Выравнивание текста: по левому краю (один аргумент!) ---
      (vl-catch-all-apply
        '(lambda () (vla-put-TextAttachmentType mleader 1))
      )
      (vl-catch-all-apply
        '(lambda () (vla-put-TextAngle mleader 0.0))
      )
      ;; --- Маска фона для текста мультивыноски (с явным цветом) ---
      (vl-catch-all-apply
        '(lambda ()
           (vla-put-TextBackgroundFill mleader :vlax-true)
           (vla-put-TextBackgroundScaleFactor mleader 1.05)
           (vla-put-TextBackgroundColor mleader 254)
        )
      )
      (vlax-vla-object->ename mleader)
    )
  )
)

;;; ------------------------------------------------------------
;;; Создаёт MTEXT с включённой маской фона и заданным текстовым
;;; стилем.
;;;
;;; Аргументы:
;;;   ins-pt     — точка вставки текста
;;;   txt-str    — строка текста
;;;   height     — высота текста
;;;   width      — ширина текстового поля (0 = авто)
;;;   just       — выравнивание (AttachmentPoint)
;;;   style-name — имя текстового стиля
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
      (vl-catch-all-apply
        '(lambda () (vla-put-StyleName mt style-name))
      )
      (vla-put-AttachmentPoint mt just)
      (vl-catch-all-apply
        '(lambda ()
           (vla-put-BackgroundFill mt :vlax-true)
           (vla-put-BackgroundScaleFactor mt 1.05)
           (vla-put-BackgroundColor mt 254)
        )
      )
      (vlax-vla-object->ename mt)
    )
    nil
  )
)

;;; ------------------------------------------------------------
;;; Вставляет блок с атрибутами координат — альтернатива MLEADER.
;;; Блок должен быть атрибутивным, с тегами атрибутов "X" и "Y".
;;; Блок вставляется точкой вставки прямо в вершину полилинии —
;;; никакой дополнительной "выносной" геометрии не создаётся.
;;;
;;; Строки x-str/y-str должны уже содержать знак +/- (см.
;;; vl:fmt-coord) — они попадают в атрибуты как есть, без
;;; дополнительных префиксов "X="/"Y=" (эти префиксы уже
;;; нарисованы статикой внутри самого блока).
;;;
;;; Высота текста атрибутов X/Y принудительно выставляется равной
;;; txt-height — той же высоте, что используется для мультивыносок
;;; и подписей номеров вершин. Это гарантирует одинаковый размер
;;; текста независимо от масштаба, в котором нарисован сам блок.
;;;
;;; Аргументы:
;;;   pt-vertex  — точка вершины полилинии (она же точка вставки блока)
;;;   block-name — имя блока в чертеже (блок должен уже существовать —
;;;                см. vl:ensure-coord-block-from-template)
;;;   x-str,y-str— отформатированные строки координат (со знаком +/-)
;;;   scale      — масштаб вставки блока (геометрия/пропорции блока,
;;;                НЕ высота текста — та задаётся отдельно)
;;;   txt-height — требуемая высота текста атрибутов (как у мультивыноски)
;;;
;;; Возвращает: entity name вставленного блока или nil
;;; ------------------------------------------------------------
(defun vl:insert-coord-block (pt-vertex block-name x-str y-str scale txt-height
                               / acadobj mspace blk-ref atts att tag readback)
  (vl-load-com)
  (setq acadobj (vlax-get-acad-object))
  (setq mspace  (vla-get-modelspace (vla-get-activedocument acadobj)))

  ;; --- Вставка блока прямо в точку вершины полилинии ---
  (setq blk-ref
    (vl-catch-all-apply
      'vla-InsertBlock
      (list mspace (vlax-3d-point pt-vertex) block-name scale scale scale 0.0)
    )
  )

  (if (vl-catch-all-error-p blk-ref)
    (progn
      (princ (strcat "\n  ОШИБКА: не удалось вставить блок «" block-name "»: "
                     (vl-catch-all-error-message blk-ref)))
      nil
    )
    (progn
      ;; --- КРИТИЧЕСКИ ВАЖНО: обновляем объект сразу после вставки.
      ;; Без этого HasAttributes/GetAttributes у только что созданного
      ;; блока может ложно возвращать "нет атрибутов", даже если в
      ;; определении блока атрибуты реально есть. ---
      (vl-catch-all-apply '(lambda () (vla-Update blk-ref)))

      ;; --- Пытаемся получить атрибуты через ActiveX. Не полагаемся
      ;; только на HasAttributes — иногда оно врёт даже после Update,
      ;; поэтому пробуем GetAttributes напрямую и ловим ошибку. ---
      (setq atts (vl-catch-all-apply 'vlax-invoke (list blk-ref 'GetAttributes)))
      (if (vl-catch-all-error-p atts) (setq atts nil))

      (if (and atts (> (length atts) 0))
        (progn
          (foreach att atts
            (setq tag (strcase (vla-get-TagString att)))
                        (cond
              ;; Латинские X/Y
              ((= tag "X") (vla-put-TextString att (strcat "X= " x-str)))
              ((= tag "Y") (vla-put-TextString att (strcat "Y= " y-str)))
              ;; Кириллические омоглифы Х/У (часто встречаются в русских
              ;; шаблонах и визуально неотличимы от латинских X/Y)
              ((= tag "Х") (vla-put-TextString att (strcat "X= " x-str)))   ; кириллица Х (U+0425)
              ((= tag "У") (vla-put-TextString att (strcat "Y= " y-str)))   ; кириллица У (U+0423)
              (T
               (princ (strcat "\n    [диагностика] тег «" tag
                              "» не распознан как X или Y — атрибут не обновлён.")))
            )
            (if (> scale 1e-9)
              (vl-catch-all-apply
                '(lambda () (vla-put-Height att (/ txt-height scale)))
              )
            )
            ;; --- Проверяем, "прилипло" ли наше значение (не Field ли это) ---
            (setq readback (vl-catch-all-apply 'vla-get-TextString (list att)))
            (if (and (not (vl-catch-all-error-p readback))
                     (member tag '("X" "Y" "Х" "У"))
                     (or (wcmatch readback "*<*") (wcmatch readback "*%<*")))
              (princ (strcat "\n    [диагностика] ВНИМАНИЕ: атрибут «" tag
                             "» похож на ПОЛЕ (Field) — значение может"
                             " быть перезаписано при следующей регенерации."))
            )
          )
          (vla-Update blk-ref)
        )
        (princ (strcat "\n  ПРЕДУПРЕЖДЕНИЕ: не удалось получить атрибуты блока «"
                       block-name "» даже после Update — проверьте, что блок"
                       " действительно атрибутивный (WBLOCK/BEDIT)."))
      )
      (vlax-vla-object->ename blk-ref)
    )
  )
)

;;; ------------------------------------------------------------
;;; Создаёт параллельный (aligned) размер вдоль сегмента pt1-pt2,
;;; смещённый ПОД полилинию на расстояние offset-dist.
;;;
;;; Оформление размера приведено к образцу:
;;;   - без стрелок на обоих концах;
;;;   - без выносных линий (только сама линия размера, без «усиков»
;;;     вверх к полилинии);
;;;   - высота текста, отступ текста и текстовый стиль назначаются
;;;     явно на объекте размера.
;;;
;;; Встроенный текст размера скрывается (override на пробел),
;;; а значение длины выводится отдельным замаскированным MTEXT:
;;;   - прямо по центру линии размера, если сегмент достаточно
;;;     длинный, чтобы вместить текст;
;;;   - иначе — выносится мультивыноской в сторону (аналогично
;;;     выноскам номеров вершин).
;;;
;;; Аргументы:
;;;   pt1, pt2          — концы сегмента полилинии (точки вершин)
;;;   offset-dist       — расстояние от полилинии до линии размера
;;;   txt-height        — высота текста
;;;   dim-style-name    — размерный стиль (например, «ISO-500 пустой»),
;;;                        импортированный из шаблона проекта
;;;   mleader-style-name— стиль мультивыноски для выноса значения
;;;   dim-text-gap      — отступ текста от линии размера
;;; ------------------------------------------------------------
(defun vl:create-parallel-dim-with-label
       (pt1 pt2 offset-dist txt-height dim-style-name mleader-style-name dim-text-gap
        / acadobj mspace dx dy seg-len perp-x perp-y dim-pt mid-pt
          dim-obj length-str char-count required-width label-ename)

  (vl-load-com)
  (setq acadobj (vlax-get-acad-object))
  (setq mspace  (vla-get-modelspace (vla-get-activedocument acadobj)))

  ;; --- Вектор сегмента и его длина ---
  (setq dx (- (car pt2) (car pt1)))
  (setq dy (- (cadr pt2) (cadr pt1)))
  (setq seg-len (sqrt (+ (* dx dx) (* dy dy))))

  ;; --- Перпендикуляр к сегменту (нормированный) ---
  (if (> seg-len 1e-9)
    (progn
      (setq perp-x (/ (- dy) seg-len))
      (setq perp-y (/ dx seg-len))
      ;; Если перпендикуляр направлен вверх — разворачиваем на 180°,
      ;; чтобы линия размера всегда была ПОД полилинией
      (if (> perp-y 0.0)
        (progn (setq perp-x (- perp-x)) (setq perp-y (- perp-y)))
      )
    )
    ;; Вырожденный (нулевой) сегмент — перпендикуляр "вниз" по умолчанию
    (progn (setq perp-x 0.0) (setq perp-y -1.0))
  )

  ;; --- Средняя точка сегмента ---
  (setq mid-pt
    (list (/ (+ (car pt1) (car pt2)) 2.0)
          (/ (+ (cadr pt1) (cadr pt2)) 2.0)
          0.0)
  )

  ;; --- Точка, задающая положение линии размера (смещена вниз) ---
  (setq dim-pt
    (list
      (+ (car mid-pt) (* perp-x offset-dist))
      (+ (cadr mid-pt) (* perp-y offset-dist))
      0.0
    )
  )

  ;; --- Создаём параллельный (aligned) размер ---
  (setq dim-obj
    (vla-AddDimAligned
      mspace
      (vlax-3d-point pt1)
      (vlax-3d-point pt2)
      (vlax-3d-point dim-pt)
    )
  )

  (if (null dim-obj)
    (progn
      (princ "\n  ОШИБКА: не удалось создать параллельный размер.")
      nil
    )
    (progn
      ;; --- Назначаем размерный стиль «ISO-500 пустой», импортированный
      ;;     из шаблона проекта: он уже содержит нужное оформление
      ;;     (без стрелок, без выносных линий, текстовый стиль Д-431,
      ;;     высота, отступ и т.д.) — вручную ничего задавать не нужно ---
      (vl-catch-all-apply '(lambda () (vla-put-StyleName dim-obj dim-style-name)))

      ;; --- Формируем строку значения длины (только для оценки длины) ---
      (setq length-str (vl:fmt-length seg-len))
      (setq char-count (strlen length-str))

      ;; --- Оцениваем, помещается ли текст на линии размера ---
      ;; Приблизительная ширина одного символа ~ 0.6 * высота текста
      (setq required-width (* char-count txt-height 0.6))

      (if (>= seg-len required-width)
        ;; --- Сегмент достаточно длинный: оставляем встроенный текст
        ;;     размера как есть — заливка (в т.ч. «Фон») берётся из
        ;;     назначенного размерного стиля, вручную не переопределяем ---
        (progn
          (setq label-ename (vlax-vla-object->ename dim-obj))
        )
        ;; --- Сегмент короче требуемой ширины: скрываем встроенный текст
        ;;     размера (override на пробел) и выносим значение мультивыноской ---
        (progn
          (vl-catch-all-apply '(lambda () (vla-put-TextOverride dim-obj " ")))
          (princ (strcat "\n  Сегмент короче подписи (" length-str
                         ") — значение вынесено мультивыноской."))
          (setq label-ename
            (vl:create-mleader
              mid-pt                                             ; Острие на полилинии
              (list (car mid-pt) (+ (cadr mid-pt) (* txt-height 2.0)) 0.0) ; Точка полки
              (list (+ (car mid-pt) (* txt-height 2.0)) (+ (cadr mid-pt) (* txt-height 2.0)) 0.0) ; Точка текста
              length-str
              mleader-style-name
              (* txt-height 1.5)
            )
          )
        )
      )
      label-ename
    )
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

;;; ------------------------------------------------------------
;;; Экспортирует таблицу координат в CSV-файл (открывается в Excel).
;;; Разделитель ";" — корректно распознаётся Excel с русской локалью.
;;;
;;; Аргумент data-list — список записей вида (idx x-str y-str)
;;; ------------------------------------------------------------
(defun vl:export-csv (data-list csv-path / f row)
  (setq f (open csv-path "w"))
  (if (null f)
    (progn
      (princ (strcat "\n  ОШИБКА: не удалось создать файл " csv-path))
      nil
    )
    (progn
      (write-line "№;X;Y" f)
      (foreach row data-list
        (write-line
          (strcat (itoa (car row)) ";" (cadr row) ";" (caddr row))
          f
        )
      )
      (close f)
      (princ (strcat "\n  Файл CSV сохранён: " csv-path))
      T
    )
  )
)

;;; ------------------------------------------------------------
;;; Экспортирует таблицу координат в RTF-файл с настоящей таблицей
;;; (открывается в Word без COM-автоматизации).
;;;
;;; Аргумент data-list — список записей вида (idx x-str y-str)
;;; ------------------------------------------------------------
(defun vl:export-rtf (data-list rtf-path / f row col1 col2 col3 row-def)
  (setq f (open rtf-path "w"))
  (if (null f)
    (progn
      (princ (strcat "\n  ОШИБКА: не удалось создать файл " rtf-path))
      nil
    )
    (progn
      (setq col1 1200)
      (setq col2 3600)
      (setq col3 6000)

      (setq row-def
        (strcat
          "\\trowd\\trgaph108\\trleft0"
          "\\clbrdrt\\brdrs\\brdrw10\\clbrdrl\\brdrs\\brdrw10"
          "\\clbrdrb\\brdrs\\brdrw10\\clbrdrr\\brdrs\\brdrw10\\cellx" (itoa col1)
          "\\clbrdrt\\brdrs\\brdrw10\\clbrdrl\\brdrs\\brdrw10"
          "\\clbrdrb\\brdrs\\brdrw10\\clbrdrr\\brdrs\\brdrw10\\cellx" (itoa col2)
          "\\clbrdrt\\brdrs\\brdrw10\\clbrdrl\\brdrs\\brdrw10"
          "\\clbrdrb\\brdrs\\brdrw10\\clbrdrr\\brdrs\\brdrw10\\cellx" (itoa col3)
        )
      )

      (write-line "{\\rtf1\\ansi\\ansicpg1251\\deff0" f)
      (write-line "{\\fonttbl{\\f0 Arial;}}" f)
      (write-line "\\f0\\fs24" f)
      (write-line "{\\b Таблица координат вершин полилинии}\\par\\par" f)

      (write-line row-def f)
      (write-line "\\intbl\\qc\\b №\\cell\\qc X\\cell\\qc Y\\cell\\row\\b0" f)

      (foreach row data-list
        (write-line row-def f)
        (write-line
          (strcat
            "\\intbl\\qc " (itoa (car row)) "\\cell"
            "\\qc " (cadr row) "\\cell"
            "\\qc " (caddr row) "\\cell\\row"
          )
          f
        )
      )

      (write-line "}" f)
      (close f)
      (princ (strcat "\n  Файл RTF (Word) сохранён: " rtf-path))
      T
    )
  )
)

;;; ------------------------------------------------------------
;;; Запрашивает у пользователя, нужно ли экспортировать таблицу
;;; координат, и в каком формате (Excel / Word / Оба / Нет).
;;;
;;; Аргумент data-list — список записей вида (idx x-str y-str)
;;; ------------------------------------------------------------
(defun vl:offer-export (data-list / choice base-path)
  (initget "Excel Word Оба Нет")
  (setq choice
    (getkword
      "\nЭкспортировать таблицу координат? [Excel/Word/Оба/Нет] <Нет>: "
    )
  )
  (if (null choice) (setq choice "Нет"))

  (cond
    ((= choice "Нет")
     (princ "\n  Экспорт пропущен.")
    )
    (T
     (setq base-path
       (getfiled "Укажите имя файла для экспорта" "координаты" "csv" 1)
     )
     (if (null base-path)
       (princ "\n  Экспорт отменён пользователем.")
       (progn
         (if (wcmatch (strcase base-path) "*.CSV")
           (setq base-path (substr base-path 1 (- (strlen base-path) 4)))
         )
         (cond
           ((= choice "Excel")
            (vl:export-csv data-list (strcat base-path ".csv"))
           )
           ((= choice "Word")
            (vl:export-rtf data-list (strcat base-path ".rtf"))
           )
           ((= choice "Оба")
            (vl:export-csv data-list (strcat base-path ".csv"))
            (vl:export-rtf data-list (strcat base-path ".rtf"))
           )
         )
       )
     )
    )
  )
)

;;; ============================================================
;;; ГЛАВНАЯ КОМАНДА: VERTEXLEADERS
;;; Запускается через команду AutoCAD: VERTEXLEADERS
;;; ============================================================
(defun c:VERTEXLEADERS (/ sel ename vertices pt-count idx pt
                          x-coord y-coord coord-text
                          mleader-style-name text-style-name actual-mleader-style
                          template-path export-data
                          txt-height land-length
                          pt-land pt-text
                          ml-ename num-ename
                          num-text num-ins-pt
                          dim-offset dim-text-gap seg-count seg-idx seg-pt1 seg-pt2
                          dim-style-name actual-dim-style-name
                          old-osmode old-cmdecho start-choice old-error-handler
                          output-mode coord-block-name block-scale)

  ;; --- Инициализация ---
  (vl-load-com)
  (setq mleader-style-name "Координаты")   ; желаемое имя стиля MLEADER
  (setq text-style-name    "Д-431")        ; имя текстового стиля
  (setq dim-style-name     "ISO-500 пустой COORD") ; желаемое имя размерного стиля
  (setq coord-block-name "XY")   ; имя блока — как в шаблоне ++РАМКА++.dwg
  ;; Масштаб вставки блока — отвечает только за геометрию/пропорции
  ;; блока (линии, рамку и т.п.), НЕ за высоту текста атрибутов:
  ;; высота текста X/Y всегда принудительно приравнивается к
  ;; txt-height внутри vl:insert-coord-block, независимо от этого
  ;; масштаба. Обычно 1.0 достаточно; меняйте, только если сама
  ;; геометрия блока (не текст) выглядит слишком крупной/мелкой.
  (setq block-scale 1.0)

  ;; ВАЖНО: укажите здесь реальный путь к вашему файлу-шаблону
  (setq template-path "D:/Projects/Lisp/LISP/++РАМКА++.dwg")

  (setq old-osmode  (getvar "OSMODE"))
  (setq old-cmdecho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (setvar "OSMODE"  0)

  ;; --- Блок обработки ошибок ---
  ;; ВАЖНО: сохраняем прежний обработчик *error* и ОБЯЗАТЕЛЬНО
  ;; возвращаем его обратно внутри нового обработчика. Иначе после
  ;; завершения команды в системе остаётся наш кастомный *error*,
  ;; который ссылается на локальные переменные old-osmode/old-cmdecho -
  ;; а они после выхода из функции становятся nil, и следующая же
  ;; ошибка в ЛЮБОЙ другой команде приведёт к вторичному сбою
  ;; (setvar с аргументом nil).
  (setq old-error-handler *error*)
  (defun *error* (msg)
    (setvar "OSMODE"  old-osmode)
    (setvar "CMDECHO" old-cmdecho)
    (setq *error* old-error-handler)   ; возвращаем прежний обработчик
    (if (not (wcmatch (strcase msg) "*CANCEL*,*EXIT*,*QUIT*"))
      (princ (strcat "\n  ОШИБКА: " msg))
    )
    (princ "\n  Команда отменена.")
    (princ)
  )

  (princ "\n=== Простановка координатных выносок и размеров по полилинии ===")
  (princ "\n  Команда: VERTEXLEADERS")

  ;; --- Проверяем текстовый стиль ---
  (vl:ensure-text-style text-style-name)

  ;; --- Проверяем / импортируем стиль мультивыноски из шаблона ---
  (if (vl:ensure-mleader-style-from-template template-path mleader-style-name)
    (setq actual-mleader-style mleader-style-name)
    (progn
      (princ "\n  Использую «Standard» как базу — параметры оформления")
      (princ "\n  назначаются вручную каждому объекту.")
      (setq actual-mleader-style "Standard")
    )
  )

  ;; --- Проверяем / импортируем размерный стиль из шаблона ---
  (if (vl:ensure-dimstyle-from-template template-path dim-style-name)
    (setq actual-dim-style-name dim-style-name)
    (progn
      (princ "\n  Использую «Standard» как базовый размерный стиль.")
      (setq actual-dim-style-name "Standard")
    )
  )

  ;; --- Запрашиваем выбор полилинии ---
  (setq sel (entsel "\nВыберите полилинию: "))

  (if (null sel)
    (progn
      (princ "\n  Выбор отменён пользователем.")
      (setvar "OSMODE"  old-osmode)
      (setvar "CMDECHO" old-cmdecho)
      (setq *error* old-error-handler)
      (princ)
      (exit)
    )
  )

  (setq ename (car sel))

  (if (and *VL:LAST-POLY-ENAME* (not (entget *VL:LAST-POLY-ENAME*)))
    (progn
      (setq *VL:LAST-VERTEX-NUM* nil)
      (setq *VL:ALL-EXPORT-DATA* '())
      (princ "\n Предыдущая полилиния была удалена. Счетчик сброшен.")
    )
  )

  (if (not (member (cdr (assoc 0 (entget ename)))
                   '("LWPOLYLINE" "POLYLINE" "3DPOLYLINE")))
    (progn
      (princ (strcat "\n  ОШИБКА: выбранный объект не является полилинией (тип: "
                     (cdr (assoc 0 (entget ename))) ")."))
      (setvar "OSMODE"  old-osmode)
      (setvar "CMDECHO" old-cmdecho)
      (setq *error* old-error-handler)
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
      (setq *error* old-error-handler)
      (princ)
      (exit)
    )
  )

  (setq pt-count (length vertices))
  (princ (strcat "\n  Найдено вершин: " (itoa pt-count)))

  ;; --- Способ вывода координат: мультивыноска или блок ---
  (initget "Мультивыноска Блок")
  (setq output-mode
    (getkword
      "\nСпособ вывода координат? [Мультивыноска/Блок] <Мультивыноска>: "
    )
  )
  (if (null output-mode) (setq output-mode "Мультивыноска"))
  (princ (strcat "\n  Выбран способ: " output-mode))

  ;; --- Если выбран режим "Блок" — проверяем/импортируем блок ОДИН РАЗ,
  ;;     до начала цикла по вершинам (а не на каждой вершине) ---
  (if (= output-mode "Блок")
    (vl:ensure-coord-block-from-template template-path coord-block-name)
  )

  ;; --- Параметры оформления ---
  (setq txt-height  1.0)
  (setq land-length (* txt-height 1.5))
  (setq dim-offset    0.7)   ; расстояние от полилинии до линии размера
  (setq dim-text-gap  0.5)   ; отступ текста от линии размера

  ;; --- Список для накопления данных экспорта: (idx x-str y-str) ---
  (setq export-data '())

  ;; ------------------------------------------------------------
  ;; Определяем, с какого номера начинать нумерацию вершин ЭТОЙ
  ;; полилинии.
  ;;
  ;; Если это первая полилиния в сеансе (счётчик ещё не установлен) —
  ;; начинаем с 1 без вопросов. Если в сеансе уже была обработана
  ;; хотя бы одна полилиния — спрашиваем пользователя явно.
  ;; ------------------------------------------------------------
  (if (and *VL:LAST-VERTEX-NUM* (> *VL:LAST-VERTEX-NUM* 0))
    (progn
      (initget "Продолжить Заново")
      (setq start-choice
        (getkword
          (strcat
            "\nПродолжить нумерацию с №" (itoa (1+ *VL:LAST-VERTEX-NUM*))
            " или начать заново с №1? [Продолжить/Заново] <Продолжить>: "
          )
        )
      )
      (if (or (null start-choice) (= start-choice "Продолжить"))
        (setq idx (1+ *VL:LAST-VERTEX-NUM*))
        (progn
          (setq idx 1)
          ;; Если пользователь решил начать нумерацию заново, старые
          ;; накопленные данные экспорта (с прежними номерами вершин)
          ;; тоже сбрасываем - иначе в таблице появятся дублирующиеся
          ;; номера, указывающие на разные координаты.
          (setq *VL:EXPORT-DATA* nil)
        )
      )
    )
    ;; Первая полилиния в сеансе - нумерация всегда с 1
    (setq idx 1)
  )
  (princ (strcat "\n  Нумерация вершин начнётся с №" (itoa idx) "."))

  ;; --- ПРОВЕРКА НА УДАЛЕНИЕ ---
  (if (and *VL:LAST-POLY-ENAME* (not (entget *VL:LAST-POLY-ENAME*)))
    (progn
      (setq *VL:LAST-VERTEX-NUM* nil)
      (setq *VL:ALL-EXPORT-DATA* '())
      (princ "\n Предыдущая полилиния была удалена. Сброс истории.")
    )
  )

;; ============================================================
  ;; ЧАСТЬ 1: координатные выноски и номера
  ;; ============================================================
  (foreach pt vertices
    (princ (strcat "\n  Обработка вершины №" (itoa idx) "..."))
    (setq x-coord (car pt))
    (setq y-coord (cadr pt))

    ;; Накапливаем данные в ГЛОБАЛЬНЫЙ список
    (setq *VL:ALL-EXPORT-DATA*
      (append *VL:ALL-EXPORT-DATA*
        (list (list idx (vl:fmt-coord x-coord) (vl:fmt-coord y-coord)))
      )
    )

    (setq coord-text (strcat "X= " (vl:fmt-coord x-coord) "\\PY= " (vl:fmt-coord y-coord)))

    (setq pt-land (list (+ x-coord land-length) (+ y-coord (* txt-height 0.8)) 0.0))
    (setq pt-text (list (+ (car pt-land) land-length) (cadr pt-land) 0.0))

    (if (= output-mode "Мультивыноска")
      (vl:create-mleader (list x-coord y-coord 0.0) pt-land pt-text coord-text actual-mleader-style land-length)
      (vl:insert-coord-block
        (list x-coord y-coord 0.0)   ; точка вставки блока = точка вершины полилинии
        coord-block-name
        (vl:fmt-coord x-coord)       ; уже содержит знак +/- — префикс "X=" рисует сам блок
        (vl:fmt-coord y-coord)       ; уже содержит знак +/- — префикс "Y=" рисует сам блок
        block-scale
        txt-height                   ; высота текста атрибутов = высота текста мультивыноски
      )
    )

    (vl:create-mtext-masked (list x-coord y-coord 0.0) (itoa idx) txt-height 0.0 1 text-style-name)

    (setq idx (1+ idx))
  )

  ;; Запоминаем состояние
  (setq *VL:LAST-POLY-ENAME* ename)
  (setq *VL:LAST-VERTEX-NUM* (1- idx))

;; ============================================================
  ;; ЧАСТЬ 2: Построение размеров по каждому сегменту
  ;; ============================================================
  (princ "\n\nПроставляю размеры по сегментам...")
  (setq seg-count (1- pt-count))
  (setq seg-idx 0)

  (repeat seg-count
    (vl:create-parallel-dim-with-label
      (nth seg-idx vertices)
      (nth (1+ seg-idx) vertices)
      dim-offset
      1.0
      actual-dim-style-name
      actual-mleader-style
      dim-text-gap
    )
    (setq seg-idx (1+ seg-idx))
  )

  (if (vl:polyline-closed-p ename)
    (vl:create-parallel-dim-with-label
      (last vertices)
      (car vertices)
      dim-offset
      1.0
      actual-dim-style-name
      actual-mleader-style
      dim-text-gap
    )
  )
  (princ "\nРазмеры построены.")

  ;; --- ЭКСПОРТ ВСЕХ ДАННЫХ ---
  (vl:offer-export *VL:ALL-EXPORT-DATA*)

  ;; Восстановление системных переменных
  (setvar "OSMODE" old-osmode)
  (setvar "CMDECHO" old-cmdecho)
  (setq *error* old-error-handler)

  (command "_.REGEN")
  (princ "\n=== VERTEXLEADERS завершена ===")
  (princ)
) ;; Это закрывающая скобка всей функции c:VERTEXLEADERS

;;; ============================================================
;;; ДОПОЛНИТЕЛЬНАЯ УТИЛИТА: RESETVERTEXNUM
;;; Сбрасывает сквозной счётчик нумерации вершин (см.
;;; *VL:LAST-VERTEX-NUM*), не требуя перезапуска AutoCAD.
;;; После сброса следующий запуск VERTEXLEADERS начнёт нумерацию
;;; с №1 без вопроса "Продолжить/Заново".
;;; ============================================================
(defun c:RESETVERTEXNUM ()
  (setq *VL:LAST-VERTEX-NUM* nil)
  (setq *VL:ALL-EXPORT-DATA* '())
  (setq *VL:LAST-POLY-ENAME* nil)
  (princ "\nСчётчик и накопленные данные сброшены.")
  (princ)
)


;;; ============================================================
;;; ДОПОЛНИТЕЛЬНАЯ УТИЛИТА: CHECKSTYLE
;;; ============================================================
(defun c:CHECKSTYLE (/ mleader-style-name text-style-name template-path)
  (setq mleader-style-name "Координаты")
  (setq text-style-name    "Д-431")
  (setq template-path "D:/Projects/Lisp/LISP/++РАМКА++.dwg")

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
(princ "\n|    VERTEXLEADERS  — координаты + размеры + экспорт |")
(princ "\n|    CHECKSTYLE     — проверить/импортировать стили  |")
(princ "\n|    RESETVERTEXNUM — сбросить счётчик нумерации     |")
(princ "\n+---------------------------------------------------+")
(princ)

;;; ============================================================
;;; Конец файла vertex_leaders.lsp
;;; ============================================================