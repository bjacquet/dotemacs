;;; 1. Avaliar funcoes
(defun set-nsb-english ()
  (setenv "CREWS_LANGUAGE" "ENGLISH"))

(defun set-nsb-norwegian ()
  (setenv "CREWS_LANGUAGE" "NORWEGIAN"))

(defun set-no-dictionaries ()
  (setenv "CREWS_LANGUAGE" "NONE"))

;;; 2. Copiar tudo de novo da siscog (avaliar)
(make-siscog "y:/siscog")

;;; 3. Compilacao, Imagens e Distribuicao da NSB (TUDO & INGLES + NORUEGUES)
(install-crews-image "-co nsb 
                      -pu  
                      -cid data-manager-win  
                      -cid data-manager-st-win  
                      -pu  
                      -ci scheduler-win  
                      -ci roster-win  
                      -ci allocator-win  
                      -ci scheduler-st-win  
                      -ci work-recorder-win  
                      -pu  
                      -f set-nsb-norwegian  
                      -cd scheduler-win  
                      -cd roster-win  
                      -cd allocator-win  
                      -cd work-recorder-win  
                      -pu  
                      -f set-no-dictionaries  
                      -cid application-controller-db-win  
                      -f set-nsb-english
                      -fd")

;;; Depois de aumentar o heap
(install-crews-image "-co nsb -pu -f set-nsb-norwegian -cd scheduler-st-win -f set-nsb-english")

(install-crews-image "-co nsb -fd")

;;; 4. Só compilação
(install-crews-image "-co nsb -pu -c data-manager-win -c data-manager-st-win -c scheduler-win -c roster-win -c scheduler-st-win -c allocator-win -c work-recorder-win -pu -f set-no-dictionaries -c application-controller-db-win -f set-nsb-english -pu")


;;; 5. Só compilação e images
(install-crews-image "-co nsb -pu -ci data-manager-win -ci data-manager-st-win -pu -ci scheduler-win -ci roster-win -ci scheduler-st-win -ci allocator-win -ci work-recorder-win -ci application-controller-db-win")


;;; SISCOG


(install-crews-image "-co siscog -pu -ci application-controller-db-win -pu -ci data-manager-win -ci data-manager-st-win -pu -ci scheduler-win -ci roster-win -ci scheduler-st-win -ci allocator-win -ci recorder-win -ci work-recorder-win")

(install-crews-image "-co siscog -pu -c data-manager-win -c data-manager-st-win -c scheduler-win -c roster-win -c scheduler-st-win -c allocator-win -c recorder-win -c work-recorder-win")

(install-crews-image "-co siscog -pu")

(install-crews-image "-co siscog -c application-controller-db-win -c data-manager-win -c data-manager-st-win -c scheduler-win -c roster-win -c scheduler-st-win -c allocator-win -c recorder-win -c work-recorder-win")

(install-crews-image "-co siscog -pu -ci application-controller-db-win -ci data-manager-win -ci data-manager-st-win -pu -ci scheduler-win -ci roster-win -ci scheduler-st-win -ci allocator-win -ci recorder-win -ci work-recorder-win")

(install-crews-image "-co siscog -pu -cid application-controller-db-win -cid data-manager-win -cid data-manager-st-win -pu -cid scheduler-win -cid roster-win -cid scheduler-st-win -cid allocator-win -cid recorder-win -cid work-recorder-win")



;;; VR

(install-crews-image "-co vr -c application-controller-db-win -ci data-manager-win -ci data-manager-st-win -c scheduler-win -c roster-win -c scheduler-st-win -c allocator-win")
(install-crews-image "-co vr -ci roster-win ")

