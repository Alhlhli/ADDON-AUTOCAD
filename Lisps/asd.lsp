;; ===================================================================
;; Draw lengths/angles, then create two tables for coordinates and polygon info
;; رسم الأطوال والزوايا، ثم إنشاء جدولين للإحداثيات ومعلومات المضلع
;; 
;; Original Author: Amer Alhlhli 
;; Email: alhlhli@gmail.com   +966504667646
;; 
;; Enhanced by:   AI - Safe Table Creation & Corrected Dimension Logic
;; Development Date: 2025-07-31
;; Modification1: Angles are drawn inside the polygon.
;; Modification2: Fixed 'butlast' error. Angles are drawn inside the polygon with a specific offset.
;; ===================================================================

(defun c:asd ( / *error* old_vars point-inside-polygon-p mid adoc spc dim sel d ts i lw enx pl lwn enxn plni_outer plni_inner plnom pl_prev pl_next poly_obj poly_area poly_perimeter point_num polyline_points polyline_points_list current_polyline_points j pt table_ins_pt coordsTable infoTable info_table_ins_pt row_idx col_idx num_rows my-butlast)

  ;; تحميل مكتبة Visual LISP
  (vl-load-com)

  ;; ===================================================================
  ;; ==== بداية الدوال المساعدة ====
  ;; ===================================================================

  ;; دالة لإزالة العنصر الأخير من القائمة (بديل لـ butlast)
  ;; تم إضافتها لإصلاح خطأ "no function definition: BUTLAST"
  (defun my-butlast (lst)
    (reverse (cdr (reverse lst)))
  )

  ;; دالة معالجة الأخطاء المحسنة
  (defun *error* ( msg )
    (if adoc (vla-endundomark adoc))
    (if old_vars
      (progn
        (mapcar 'setvar '(dimtxsty dimjust dimtad dimtih dimupt dimtix DIMFXLon DIMFXL dimtofl dimaunit dimlunit DIMCLRD DIMCLRE DIMLTYPE dimadec dimdec dimasz) old_vars)
      )
    )
    (if (and msg (/= msg "Function cancelled") (/= msg "quit / exit abort"))
      (prompt (strcat "\nError: " msg " / خطأ: " msg))
    )
    (princ)
  )

  ;; دالة محسنة للتحقق من وجود النقطة داخل البولي لاين
  (defun point-inside-polygon-p ( pt polygon / count i j n x y xi yi xj yj)
    (setq count 0 n (length polygon) x (car pt) y (cadr pt) j (1- n) i 0)
    (while (< i n)
      (setq xi (car (nth i polygon)) yi (cadr (nth i polygon)) xj (car (nth j polygon)) yj (cadr (nth j polygon)))
      (if (and (or (and (< yi y) (>= yj y)) (and (< yj y) (>= yi y))) (< (+ xi (* (/ (- y yi) (- yj yi)) (- xj xi))) x))
        (setq count (1+ count)))
      (setq j i i (1+ i)))
    (= (rem count 2) 1)
  )

  ;; دالة حساب نقطة المنتصف
  (defun mid ( p1 p2 ) (mapcar (function (lambda ( a b ) (/ (+ a b) 2.0))) p1 p2))

  ;; ===================================================================
  ;; ==== بداية الكود الرئيسي ====
  ;; ===================================================================

  ;; بدء عملية التراجع
  (vla-startundomark (setq adoc (vla-get-activedocument (vlax-get-acad-object))))
  (setq spc (vla-get-block (vla-get-activelayout adoc)))

  ;; حفظ الإعدادات الحالية
  (setq old_vars (mapcar 'getvar '(dimtxsty dimjust dimtad dimtih dimupt dimtix DIMFXLon DIMFXL dimtofl dimaunit dimlunit DIMCLRD DIMCLRE DIMLTYPE dimadec dimdec dimasz)))

  ;; إنشاء أو الحصول على نمط التسطير
  (if (not (tblsearch "DIMSTYLE" "3mer")) 
    (setq dim (vla-add (vla-get-dimstyles adoc) "3mer")) 
    (setq dim (vla-item (vla-get-dimstyles adoc) "3mer"))
  )
  (vla-put-activedimstyle adoc dim)

  ;; طلب تحديد البولي لاين المغلق
  (prompt "\nSelect closed polygon(s) / اختر بولي لاين مغلق")
  (setq sel (ssget '((0 . "LWPOLYLINE") (-4 . "&=") (70 . 1))))

  (if (not sel) 
    (progn 
      (prompt "\nNo closed polyline selected. / لم يتم تحديد أي بولي لاين مغلق") 
      (*error* nil) 
      (exit)
    )
  )

  ;; طلب مسافة الإزاحة
  (initget 7)
  (setq d (getdist "\nEnter offset distance for dimensions: / أدخل مسافة الإزاحة للأبعاد: "))
  (if (not d) 
    (progn 
      (prompt "\nInvalid distance. / مسافة غير صالحة") 
      (*error* nil) 
      (exit)
    )
  )
  
  (setq ts (/ d 2.0))

  ;; ضبط إعدادات التسطير
  (mapcar 'setvar '(dimtxsty dimjust dimtad dimtih dimupt dimtix DIMFXLon dimtofl dimaunit dimlunit DIMCLRD DIMCLRE DIMLTYPE dimadec dimdec)
                   '("Standard" 0 0 0 0 1 1 1 1 2 8 8 "CENTER2" 0 2))
  (setvar 'DIMFXL ts)
  (setvar 'dimasz (/ ts 2.0))
  
  (if (tblsearch "STYLE" "Standard") 
    (vla-put-Height (vlax-ename->vla-object (tblobjname "STYLE" "Standard")) ts)
  )
  (vla-copyfrom dim adoc)

  ;; ===================================================================
  ;; ==== بداية منطق رسم الأبعاد المٌصحح ====
  ;; ===================================================================
  (repeat (setq i (sslength sel))
    (setq lw (ssname sel (setq i (1- i))))
    (setq enx (entget lw))
    (setq pl (mapcar (function (lambda (x) (trans (list (car x) (cadr x) (cdr (assoc 38 enx))) lw 1))) 
                     (mapcar 'cdr (vl-remove-if-not (function (lambda (x) (= (car x) 10))) enx))))

    ;; --- إنشاء نقاط الإزاحة الخارجية لأبعاد الأطوال ---
    (vla-offset (vlax-ename->vla-object lw) d) 
    (setq lwn (entlast))
    (setq enxn (entget lwn))
    (setq plni_outer (mapcar (function (lambda (x) (trans (list (car x) (cadr x) (cdr (assoc 38 enxn))) lwn 1))) 
                       (mapcar 'cdr (vl-remove-if-not (function (lambda (x) (= (car x) 10))) enxn))))
    (if (point-inside-polygon-p (car plni_outer) pl) ; إذا كانت الإزاحة للداخل، أعدها للخارج
      (progn 
        (entdel lwn) 
        (vla-offset (vlax-ename->vla-object lw) (- d)) 
        (setq lwn (entlast))
        (setq enxn (entget lwn))
        (setq plni_outer (mapcar (function (lambda (x) (trans (list (car x) (cadr x) (cdr (assoc 38 enxn))) lwn 1))) 
                           (mapcar 'cdr (vl-remove-if-not (function (lambda (x) (= (car x) 10))) enxn))))
      )
    )
    (entdel lwn) ; حذف المضلع الخارجي المؤقت

    ;; --- إنشاء نقاط الإزاحة الداخلية لأبعاد الزوايا ---
    (vla-offset (vlax-ename->vla-object lw) (- d)) ; محاولة الإزاحة للداخل أولاً
    (setq lwn (entlast))
    (setq enxn (entget lwn))
    (setq plni_inner (mapcar (function (lambda (x) (trans (list (car x) (cadr x) (cdr (assoc 38 enxn))) lwn 1))) 
                       (mapcar 'cdr (vl-remove-if-not (function (lambda (x) (= (car x) 10))) enxn))))
    (if (not (point-inside-polygon-p (car plni_inner) pl)) ; إذا لم تكن الإزاحة للداخل، أعدها للداخل
      (progn 
        (entdel lwn) 
        (vla-offset (vlax-ename->vla-object lw) d) 
        (setq lwn (entlast))
        (setq enxn (entget lwn))
        (setq plni_inner (mapcar (function (lambda (x) (trans (list (car x) (cadr x) (cdr (assoc 38 enxn))) lwn 1))) 
                           (mapcar 'cdr (vl-remove-if-not (function (lambda (x) (= (car x) 10))) enxn))))
      )
    )
    (entdel lwn) ; حذف المضلع الداخلي المؤقت

    ;; --- إنشاء الأبعاد ---

    ;; إنشاء نقاط موقع نص أبعاد الأطوال
    (setq plnom (mapcar (function (lambda (a b) (mid a b))) plni_outer (append (cdr plni_outer) (list (car plni_outer)))))
    
    ;; إضافة أبعاد الأطوال
    (mapcar (function (lambda (a b c) 
              (vla-addDimAligned spc (vlax-3d-point a) (vlax-3d-point b) (vlax-3d-point c))
            )) 
            pl (append (cdr pl) (list (car pl))) plnom)
    
    ;; تجهيز القوائم لأبعاد الزوايا بالطريقة الموثوقة
    (setq pl_prev (cons (last pl) (my-butlast pl))) ; ** تم التصحيح هنا **
    (setq pl_next (append (cdr pl) (list (car pl))))

    ;; إضافة أبعاد الزوايا (للداخل)
    ;; يتم استخدام رؤوس المضلع الداخلي (plni_inner) لتحديد موقع قوس البعد
    (mapcar (function (lambda (angle_vertex prev_vertex next_vertex arc_location) 
              (vla-AddDim3PointAngular spc 
                                     (vlax-3d-point angle_vertex) 
                                     (vlax-3d-point prev_vertex) 
                                     (vlax-3d-point next_vertex) 
                                     (vlax-3d-point arc_location))
            )) 
            pl          ; الرؤوس التي تقاس عندها الزوايا
            pl_prev     ; الرأس السابق لكل زاوية
            pl_next     ; الرأس التالي لكل زاوية
            plni_inner  ; الرأس الداخلي المقابل لتحديد موقع القوس
    )
  )
  ;; ===================================================================
  ;; ==== نهاية منطق رسم الأبعاد المٌصحح ====
  ;; ===================================================================
  
  (prompt (strcat "\nSuccessfully created dimensions for " (itoa (sslength sel)) " polyline(s). / تم إنشاء الأبعاد بنجاح لـ " (itoa (sslength sel)) " عنصر."))

  ;; =================================================================
  ;; ==== بداية كود إنشاء الجداول وترقيم النقاط (النسخة الآمنة) ====
  ;; =================================================================
  
  ;; الحصول على معلومات المضلع
  (setq poly_obj (vlax-ename->vla-object lw))
  (setq poly_area (vla-get-Area poly_obj))
  (setq poly_perimeter (vla-get-Length poly_obj))
  
  ;; استخراج نقاط المضلع
  (setq point_num 1)
  (setq polyline_points (vlax-get-property poly_obj 'Coordinates))
  (setq polyline_points_list (vlax-safearray->list (vlax-variant-value polyline_points)))
  (setq current_polyline_points nil)
  (setq j 0)
  
  ;; تحويل الإحداثيات إلى قائمة نقاط
  (while (< j (length polyline_points_list))
    (setq current_polyline_points (cons (list (nth j polyline_points_list) (nth (+ j 1) polyline_points_list)) current_polyline_points))
    (setq j (+ j 2))
  )
  (setq current_polyline_points (reverse current_polyline_points))

  ;; إضافة أرقام النقاط
  (foreach pt current_polyline_points
    (vla-AddText spc (itoa point_num) (vlax-3d-point (car pt) (cadr pt) 0.0) ts)
    (setq point_num (1+ point_num))
  )

  ;; طلب نقطة إدراج الجدول
  (setq table_ins_pt (getpoint "\nSelect coordinates table insertion point / حدد نقطة إدراج جدول الإحداثيات: "))
  
  (if table_ins_pt
    (progn
      (prompt "\nCreating coordinates table... / إنشاء جدول الإحداثيات...")
      
      ;; ==============================================
      ;; === إنشاء جدول الإحداثيات (بطريقة آمنة) ===
      ;; ==============================================
      (setq coordsTable nil)
      (if (not (vl-catch-all-error-p 
                (setq coordsTable 
                  (vl-catch-all-apply 'vla-AddTable 
                    (list spc 
                          (vlax-3d-point table_ins_pt) 
                          (+ 2 (length current_polyline_points)) 
                          3 
                          (* ts 2.0) 
                          (* ts 8.0))))))
        (progn
          (prompt "\nCoordinates table created successfully!")
          
          ;; تعيين النصوص الأساسية
          (vla-SetText coordsTable 0 0 "احداثيات النقاط")
          (vla-SetText coordsTable 1 0 "رقم النقطة")
          (vla-SetText coordsTable 1 1 "الاحداثيات الشرقية")
          (vla-SetText coordsTable 1 2 "الاحداثيات الشمالية")

          ;; تعبئة بيانات النقاط
          (setq row_idx 2 i 0)
          (while (< i (length current_polyline_points))
            (setq pt (nth i current_polyline_points))
            (vla-SetText coordsTable row_idx 0 (itoa (+ i 1)))
            (vla-SetText coordsTable row_idx 1 (rtos (car pt) 2 3))
            (vla-SetText coordsTable row_idx 2 (rtos (cadr pt) 2 3))
            (setq i (1+ i) row_idx (1+ row_idx))
          )
          
          ;; تطبيق تنسيق أساسي آمن
          (vl-catch-all-apply 'vla-SetAlignment (list coordsTable -1 -1 acMiddleCenter))
          (vl-catch-all-apply 'vla-MergeCells (list coordsTable 0 0 0 2))
        )
        (prompt "\nError creating coordinates table!")
      )
      
      (prompt "\nCreating polygon info table... / إنشاء جدول معلومات المضلع...")
      
      ;; ==============================================
      ;; === إنشاء جدول معلومات المضلع (بجانب جدول الإحداثيات) ===
      ;; ==============================================
      ;; حساب المسافة بناءً على عرض الجدول الأول
      (setq table_width (* ts 22)) ; عرض جدول الإحداثيات تقريباً
      (setq info_table_ins_pt (polar table_ins_pt 0.0 (+ table_width (* ts 5.0)))) ; إلى اليمين مع مسافة
      (setq infoTable nil)
      
      (if (not (vl-catch-all-error-p 
                (setq infoTable 
                  (vl-catch-all-apply 'vla-AddTable 
                    (list spc 
                          (vlax-3d-point info_table_ins_pt) 
                          3 
                          2 
                          (* ts 5.0) 
                          (* ts 11.0))))))
        (progn
          (prompt "\nPolygon info table created successfully!")
          
          ;; تعيين النصوص
          (vla-SetText infoTable 0 0 "معلومات المضلع")
          (vla-SetText infoTable 1 0 "المحيط")
          (vla-SetText infoTable 1 1 "المساحة")
          (vla-SetText infoTable 2 0 (strcat (rtos poly_perimeter 2 3) " m"))
          (vla-SetText infoTable 2 1 (strcat (rtos poly_area 2 3) " m²"))
          
          ;; تطبيق تنسيق أساسي آمن
          (vl-catch-all-apply 'vla-SetAlignment (list infoTable -1 -1 acMiddleCenter))
          (vl-catch-all-apply 'vla-MergeCells (list infoTable 0 0 0 1))
          
          (prompt "\nBoth tables created successfully! / تم إنشاء كلا الجدولين بنجاح!")
        )
        (prompt "\nError creating polygon info table!")
      )
    )
    (prompt "\nTable insertion cancelled. / تم إلغاء إدراج الجداول.")
  )

  (*error* nil)
  (princ)
)
