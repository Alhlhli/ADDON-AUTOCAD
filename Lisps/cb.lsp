;; ============================================================
;; Command: CB
;;
;; Selected objects                     -> Color 8
;; Contents of selected blocks (deep)   -> Layer 0 + ByBlock
;; Text / attributes / leaders          -> ByBlock
;; Hatch foreground and background      -> ByBlock
;;
;; Notes:
;; - A layer itself cannot be "ByBlock".  Objects inside a block
;;   are moved to Layer 0 so that the block reference controls them.
;; - Xref block definitions are skipped because they are read-only.
;; ============================================================

(vl-load-com)

(defun c:CB
  (/ *error* *processed-blocks* acad doc undo-open ss i ent data typ
     make-byblock-color remove-all-dxf strip-mtext-colors
     set-property-safe set-complex-colors set-hatch-background
     set-mleader-dxf-colors change-entity process-sub-entities
     process-block-def block-is-editable)

  ;; ACI 0 is ByBlock.  AcCmColor method 193 is also ByBlock.
  (defun make-byblock-color (/ color-object)
    (setq color-object
      (vla-GetInterfaceObject
        acad
        (strcat "AutoCAD.AcCmColor." (substr (getvar "ACADVER") 1 2))))
    (vla-put-ColorMethod color-object 193)
    color-object
  )

  ;; Remove every occurrence of a DXF code, not only the first one.
  (defun remove-all-dxf (code lst)
    (vl-remove-if '(lambda (item) (= (car item) code)) lst)
  )

  ;; Remove MTEXT inline foreground-color overrides: \C...; and \c...;
  ;; These overrides otherwise remain visible even when entity color is ByBlock.
  (defun strip-mtext-colors (txt / marker pos semi)
    (if txt
      (progn
        (foreach marker '("\\C" "\\c")
          (while (and marker (setq pos (vl-string-search marker txt)))
            (if (setq semi (vl-string-search ";" txt (+ pos 2)))
              (setq txt
                (strcat
                  (substr txt 1 pos)
                  (substr txt (+ semi 2))))
              ;; Malformed trailing color code: leave it unchanged.
              (setq pos nil marker nil)
            )
          )
        )
      )
    )
    txt
  )

  (defun set-property-safe (obj prop value)
    (if (vlax-property-available-p obj prop T)
      (vl-catch-all-apply 'vlax-put-property (list obj prop value))
    )
  )

  ;; Colors stored as special properties rather than the entity's DXF 62.
  (defun set-complex-colors (ent col / obj color-object)
    (setq obj (vlax-ename->vla-object ent))

    ;; Old LEADER and DIMENSION families can inherit colors from their styles.
    (set-property-safe obj 'DimensionLineColor col)
    (set-property-safe obj 'ExtensionLineColor col)
    (set-property-safe obj 'TextColor col)

    ;; MLeader LeaderLineColor is an AcCmColor object.
    (if (vlax-property-available-p obj 'LeaderLineColor T)
      (progn
        (setq color-object (make-byblock-color))
        (vl-catch-all-apply
          'vlax-put-property
          (list obj 'LeaderLineColor color-object))
        (vlax-release-object color-object)
      )
    )
  )

  ;; A hatch background is a separate AcCmColor property.  Setting it here
  ;; also handles backgrounds stored in HATCHBACKGROUNDCOLOR xdata.
  (defun set-hatch-background (ent / obj color-object)
    (setq obj (vlax-ename->vla-object ent))
    (setq color-object (make-byblock-color))
    (if (vlax-property-available-p obj 'BackgroundColor T)
      (progn
        (vl-catch-all-apply
          'vlax-put-property
          (list obj 'BackgroundColor color-object))
      )
    )
    ;; For gradient hatches these are the two visible foreground colors.
    (if (vlax-property-available-p obj 'GradientColor1 T)
      (vl-catch-all-apply
        'vlax-put-property
        (list obj 'GradientColor1 color-object))
    )
    (if (vlax-property-available-p obj 'GradientColor2 T)
      (vl-catch-all-apply
        'vlax-put-property
        (list obj 'GradientColor2 color-object))
    )
    (vlax-release-object color-object)
  )

  ;; MULTILEADER stores redundant packed colors in its context and common
  ;; data.  The packed 32-bit value below is AcCmEntityColor ByBlock.
  (defun set-mleader-dxf-colors (lst / out context-depth item code val)
    (setq out nil context-depth 0)
    (foreach item lst
      (setq code (car item) val (cdr item))

      (cond
        ((and (= code 300) (= val "CONTEXT_DATA{"))
          (setq context-depth 1))
        ((and (> context-depth 0)
              (member code '(302 304))
              (= (type val) 'STR)
              (wcmatch val "*{"))
          (setq context-depth (1+ context-depth)))
        ((and (> context-depth 0) (= code 301) (= val "}"))
          (setq context-depth (1- context-depth)))
      )

      (cond
        ;; Context text color and context block-content color.
        ((and (= context-depth 1) (member code '(90 93)))
          (setq item (cons code -1056964608)))

        ;; Enable entity-level overrides for leader-line, text, and block color
        ;; so an explicit MLeaderStyle color cannot win over ByBlock.
        ((and (= context-depth 0) (= code 90))
          (setq item (cons code (logior val 2 32768 1048576))))

        ;; Common leader-line, text, and block-content colors.
        ((and (= context-depth 0) (member code '(91 92 93)))
          (setq item (cons code -1056964608)))

        ;; MLeader's embedded MTEXT contents.
        ((and (= context-depth 1) (= code 304))
          (setq item (cons code (strip-mtext-colors val))))
      )

      (setq out (cons item out))
    )
    (reverse out)
  )

  ;; col = 0 for ByBlock, or 8 for the final selected object.
  ;; to-layer-zero is used only for objects contained in block definitions.
  (defun change-entity (ent col to-layer-zero / data typ item new-data)
    (if (setq data (entget ent '("ACAD" "HATCHBACKGROUNDCOLOR")))
      (progn
        (setq typ (cdr (assoc 0 data)))

        ;; Remove entity TrueColor, color-book name, and transparency override.
        ;; Transparency is deliberately preserved (DXF 440); it is not a color.
        (setq data (remove-all-dxf 420 data))
        (setq data (remove-all-dxf 430 data))

        (if (assoc 62 data)
          (setq data (subst (cons 62 col) (assoc 62 data) data))
          (setq data (append data (list (cons 62 col))))
        )

        (if to-layer-zero
          (if (assoc 8 data)
            (setq data (subst '(8 . "0") (assoc 8 data) data))
            (setq data (append data '((8 . "0"))))
          )
        )

        ;; Strip inline MTEXT color overrides from text-like objects.
        (if (member typ '("MTEXT" "ATTRIB" "ATTDEF"))
          (setq data
            (mapcar
              '(lambda (x)
                (if (member (car x) '(1 3))
                  (cons (car x) (strip-mtext-colors (cdr x)))
                  x))
              data))
        )

        (if (= typ "MULTILEADER")
          (setq data (set-mleader-dxf-colors data))
        )

        (if (entmod data)
          (progn
            (if (= col 0) (set-complex-colors ent col))
            (if (member typ '("HATCH" "MPOLYGON"))
              (set-hatch-background ent))
            (entupd ent)
          )
        )
      )
    )
  )

  ;; ATTRIBs belonging to INSERTs and VERTEX entities belonging to POLYLINEs.
  (defun process-sub-entities (main-ent to-layer-zero / sub-ent sub-data sub-type)
    (if (= (cdr (assoc 66 (entget main-ent))) 1)
      (progn
        (setq sub-ent (entnext main-ent))
        (while
          (and sub-ent
               (setq sub-data (entget sub-ent))
               (/= (setq sub-type (cdr (assoc 0 sub-data))) "SEQEND"))
          (change-entity sub-ent 0 to-layer-zero)
          (setq sub-ent (entnext sub-ent))
        )
      )
    )
  )

  (defun block-is-editable (block-ent / flags)
    (setq flags (cdr (assoc 70 (entget block-ent))))
    ;; Bit 4 = xref; bit 8 = xref overlay; bit 16 = externally dependent.
    (= 0 (logand (if flags flags 0) (+ 4 8 16)))
  )

  ;; Recursively process block definitions at any nesting depth.
  (defun process-block-def (block-name / key block-ent sub-ent sub-data sub-type)
    (setq key (strcase block-name))
    (if (not (member key *processed-blocks*))
      (progn
        (setq *processed-blocks* (cons key *processed-blocks*))
        (if (and (setq block-ent (tblobjname "BLOCK" block-name))
                 (block-is-editable block-ent))
          (progn
            (setq sub-ent (entnext block-ent))
            (while
              (and sub-ent
                   (setq sub-data (entget sub-ent))
                   (/= (setq sub-type (cdr (assoc 0 sub-data))) "ENDBLK"))

              (change-entity sub-ent 0 T)
              (process-sub-entities sub-ent T)

              (if (= sub-type "INSERT")
                (process-block-def (cdr (assoc 2 sub-data)))
              )
              (setq sub-ent (entnext sub-ent))
            )
          )
        )
      )
    )
  )

  (setq acad (vlax-get-acad-object)
        doc  (vla-get-ActiveDocument acad)
        undo-open nil)

  (defun *error* (message)
    (if undo-open
      (progn
        (vla-EndUndoMark doc)
        (setq undo-open nil))
    )
    (if (and message
             (not (wcmatch (strcase message) "*CANCEL*,*QUIT*,*EXIT*")))
      (princ (strcat "\nCB error: " message))
    )
    (princ)
  )

  (princ
    "\nSelect objects: selected = Color 8; nested contents = Layer 0 + ByBlock: ")

  (if (setq ss (ssget "_:L"))
    (progn
      (vla-StartUndoMark doc)
      (setq undo-open T
            *processed-blocks* nil
            i 0)

      (while (< i (sslength ss))
        (setq ent  (ssname ss i)
              data (entget ent)
              typ  (cdr (assoc 0 data)))

        ;; First make all nested definitions inherit from their references.
        (if (= typ "INSERT")
          (process-block-def (cdr (assoc 2 data)))
        )

        ;; Attributes/vertices directly owned by the selected entity inherit it.
        (process-sub-entities ent nil)

        ;; Last operation by design: selected outer object becomes Color 8.
        (change-entity ent 8 nil)
        (setq i (1+ i))
      )

      (vla-Regen doc 1)
      (vla-EndUndoMark doc)
      (setq undo-open nil)
      (princ
        "\nDone: selected objects = Color 8; all nested contents = Layer 0 + ByBlock.")
    )
    (princ "\nNo objects selected.")
  )
  (princ)
)
