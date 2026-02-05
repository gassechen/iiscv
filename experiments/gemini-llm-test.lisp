;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;; GEMINI BINDS  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Función auxiliar para extraer solo el código Lisp de la respuesta de Gemini
(defun extract-lisp-code (response)
  "Extrae el primer bloque de código Lisp de una respuesta de texto."
  (let* ((start (search "```lisp" response)))
    (when start
      (let* ((code-start (+ start 7))
             (end (search "```" response :start2 code-start)))
        (when end
          (subseq response code-start end))))))


;; Funciones auxiliares más robustas para extraer código
(defun extract-code-any-markdown (response)
  "Extrae código de cualquier bloque markdown (```lisp, ```common-lisp, ```)."
  (let* ((start (search "```" response)))
    (when start
      (let* ((lang-start (+ start 3))
            (newline-pos (position #\Newline response :start lang-start))
            (code-start (when newline-pos (+ newline-pos 1)))
            (end (search "```" response :start2 (or code-start lang-start))))
        (when (and code-start end)
          (subseq response code-start end))))))

(defun extract-plain-code (response)
  "Intenta evaluar la respuesta como si fuera código Lisp plano."
  (handler-case
      (progn
        (read-from-string response)
        response)
    (error () nil)))


(defun start-ai-programmer-debug (initial-task)
  "Versión corregida: Evalúa todas las formas y registra cada una en IISCV."
  (format t "~%=== Iniciando Programador IA (MODO DEBUG ROBUSTO) ===~%")
  (funcall *correction-attempt-counter* :action :increment)
  
  (let* ((full-prompt (format nil "~A~%~%TASK:~%~A" *iiscv-system-prompt* initial-task))
         (gemini-response (uiop:run-program `("gemini" "-p" ,full-prompt) 
                                            :output :string 
                                            :error-output :string)))
    
    (format t "--- RESPUESTA CRUDA DE GEMINI ---~%~A~%--------------------------------~%" gemini-response)
    
    (let* ((sanitized-code (sanitize-gemini-response gemini-response)))
      (unless sanitized-code
        (format t "Error: No se pudo extraer o sanear el código Lisp.~%")
        (return-from start-ai-programmer-debug nil))
      
      (format t "--- CÓDIGO SANEADO PARA EVALUAR ---~%~A~%-----------------------------------~%" sanitized-code)
      
      (handler-case
          (progn
            (format t "> Evaluando en imagen viva...~%")
            ;; Usamos el string directamente con el Reader Loop
            (with-input-from-string (s sanitized-code)
              (loop for form = (read s nil :eof)
                    until (eq form :eof)
                    do (let ((result (eval form)))
                         (format t "~% Evaluando forma: ~S -> ~A~%" form result)
                         ;; IMPORTANTE: Registramos cada forma en el historial
                         (make-atomic-commit form))))
            ;; Una vez evaluado todo, disparamos la auditoría
            (check-violations))
        
        ;; Corregido: 'e' es la variable que captura el error
        (error (e)
          (format t "~%¡ERROR EN EVALUACIÓN! ~A~%Tipo: ~A~%" e (type-of e)))))))


(defun check-violations ()
  "Revisa si hay violaciones y, si las hay, inicia un ciclo de corrección."
  (when *audit-violations*

    (let ((max-attempts 10)) 
	   
        
        ;; --- Condición de salida ---
        (when (>= (funcall *correction-attempt-counter* :action :get) max-attempts)
          (format t "~%=== LÍMITE DE INTENTOS ALCANZADO (~A). Deteniendo. ===~%" max-attempts)
          (funcall *correction-attempt-counter* :action :reset)
          (return-from check-violations nil)))

    
    
    (let* ((first-violation (caar *audit-violations*)) ; Obtiene la primera violación como string
           (start-pos (search "found in '" first-violation))
           (end-pos (when start-pos (search "'" first-violation :start2 (+ start-pos 10))))
           (function-name (when (and start-pos end-pos)
                            (string-upcase (subseq first-violation (+ start-pos 10) end-pos))))
           (function-symbol (when function-name (find-symbol function-name :iiscv))))
      
      (when (and function-name function-symbol)
        (let* ((fully-qualified-name (format nil "~A::~A" (package-name (symbol-package function-symbol)) (symbol-name function-symbol)))
               (source-code (get-source-form fully-qualified-name))
               (violation-messages (mapcar #'car *audit-violations*))) ; <-- OBTENER LOS MENSAJES
          
          (when source-code
            ;; --- CORRECCIÓN CLAVE EN EL PROMPT ---
            (let ((correction-prompt (format nil "The following Common Lisp function has these specific quality violations:

Violations:
~{~A~^~%~}

Please fix the function to address these violations and provide only the corrected Lisp code.

Function to fix:
```lisp
~A
```"
                                              violation-messages
                                              source-code)))
              (format t "~%> Violaciones detectadas. Iniciando corrección automática para ~A...~%" function-name)
              (format t "> Enviando a Gemini el siguiente prompt de corrección:~%~A~%" correction-prompt)
	      
              (start-ai-programmer-debug correction-prompt)
	      )))))))



(defun sanitize-gemini-response (response)
  "Limpia la respuesta de Gemini eliminando formas no deseadas como (in-package ...)."
  (let* ((code-block (or (extract-lisp-code response)
                          (extract-code-any-markdown response)
                          (extract-plain-code response))))
    (when code-block
      ;; Usa una regex para encontrar y eliminar cualquier forma (in-package ...)
      (cl-ppcre:regex-replace-all "\\(\\s*in-package\\s*[^)]+\\)\\s*" code-block ""))))




(defvar *correction-attempt-counter*
  (let ((count 0))
    (lambda (&key (action :get))
      "Closure para gestionar el contador de intentos de corrección.
       ACTION puede ser :GET, :INCREMENT o :RESET."
      (case action
        (:get count)
        (:increment (incf count))
        (:reset (setf count 0))
        (otherwise
	 (error "Acción inválida para el contador: ~A. Usa :GET, :INCREMENT o :RESET."
		action))))))

