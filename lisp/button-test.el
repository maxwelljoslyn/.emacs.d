(let ((my-b-t (define-button-type 'pressable
                 'action '(lambda (button) (message (format "The button has been pressed")))
                 'follow-link t
                 'help-echo "Click On Me"
                 'help-args "testing")))
  (progn
    (insert "\n\n")
    (insert-text-button "ding-dong" :type my-b-t)
    (previous-line)
    (kill-line)))

(let* ((onpress-fun (lambda (button) (message (format "El botón ha primado"))))
       (my-b-t (define-button-type 'pressable
                 'action onpress-fun
                 'follow-link t
                 'help-echo "Me primé, por favor"
                 'help-args "testing")))
  (progn
    (insert "\n\n")
    (insert-text-button "ding-dong" :type my-b-t)
    (previous-line)
    (kill-line)))
