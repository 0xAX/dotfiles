(defun run-gemini-api-key-script ()
  "Run the Gemini API key script, supply password, and return output as string."
  (interactive)
  (let ((password (read-passwd "Enter password: "))
        (output "")
        (done nil))
    (let ((proc
           (make-process
            :name "gemini-api-key"
            :buffer nil
            :command '("/home/alex/bin/llm/gemini-api-key")
            :noquery t
            :filter (lambda (proc string)
                      (setq output (concat output string))
                      (when (string-match "Enter the password for kuleshovmail@gmail.com at my.1password.com:" string)
                        (process-send-string proc (concat password "\n"))))
            :sentinel (lambda (_proc event)
                        (when (string= event "finished\n")
                          (setq done t))))))
      (while (not done)
        (accept-process-output proc 0.1))
      (car (last (split-string output "\n" t))))))

(defun start-gemini ()
  "Start GPTEL with gemini"
  (interactive)
  (setq gemini-api-key (run-gemini-api-key-script))
  (setq
   gptel-model 'gemini-2.5-flash-preview-04-17
   gptel-backend (gptel-make-gemini "Gemini"
                   :key gemin-api-key
                   :stream t)))
