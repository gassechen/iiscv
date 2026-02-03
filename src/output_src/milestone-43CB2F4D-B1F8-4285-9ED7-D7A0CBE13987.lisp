;;; Human Commit: TEST 1
;;; UUID: 43CB2F4D-B1F8-4285-9ED7-D7A0CBE13987
;;; Timestamp: 3979110379


(DEFUN PROCESAR-DATOS (INPUT)
  "Usa el sensor."
  (LET ((V (LEER-SENSOR INPUT)))
    (IF (> V 500)
        :ERROR
        :OK)))

(DEFUN LEER-SENSOR (ID) "Redefinición" (* ID 2))
