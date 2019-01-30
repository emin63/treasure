

;;(makunbound 'treasure-map-hydras) ;; uncomment when testing for reloading

(defvar treasure-map-hydras 
  '(("q" (message "quit") "quit hydra" :exit t :color blue))
  "List of hydras to setup the treasure map for.

This should be a list of lists where each sub-list is of the form
expected by defhydras. The simplest form is (KEY CMD MSG) where KEY
is the key that triggers the hydra, CMD is the command to run, and
MSG is the message to show next to KEY.

You can then call treasure-bind-master-hydra to create a master hydra
to access all hydras you have added to treasure-map-hydras.

NOTE: It is probably best if you only modify this using the
treasure-add-hydra function for safety. See docs for the
treasure-add-hydra and treasure-bind-master-hydra for details."
  )

(defun treasure-add-hydra (hlist)
  "Add a hydra to the treasure-map-hydras variable.

    The various components are as follows:

      KEY:   String key to use for the map.
      HYDRA: A hydra to call for the map.
      REST:  Additional parts of a hydra
      "
  (if (member (car hlist) (mapcar (lambda (x) (car x)) treasure-map-hydras))
      (error (format "Key %s already defined in treasure-map-hydras"
		     (car hlist)))
    (setq treasure-map-hydras (append treasure-map-hydras
				      (list hlist))))
  )

(defhydra hydra-treasure-map-help (:color teal :columns 1)
  "Show help for hydra treasure map

  The treasure package is designed to make it easier for you to find things.
  You can define verbose hydras which are easy to call so you can quickly
  and easily search for useful things.
  "
  ("b" hydra-treasure-map/body "Back to top of hydra-treasure-map")
  ("e" (treasure-show-example) "show example hydra definition")
  ("h" treasure-describe-hydras "tell me more about hydras")
  ("q" (message "quit") "quit" :color blue)
  )

(defun treasure-show-example ()
  "Show an example of what a hydra looks like"
  (message "A hydra is a special function provided by the hydra package

  You can define a hydra via something like

    (defhydra example-hydra
      \"Example hydra definition\"
      (\"a\" (message \"you pressed a\") \"function for the 'a' key\")
      (\"b\" (message \"you pressed b\") \"function for the 'b' key\")
      (\"q\" (message \"quitting example hydra\") \"quit this hydra\")
    )

      "
	   ))



(defun treasure-find (my-map cmds)
  "Look in MY-MAP to find CMDS and extract them into a list.

This function takes a keyamp as MY-MAP and a list, CMDS, consisting
of either string characters like \"g\" or symbols like 'grep indicating
commands you want to extract out.

Matches from CMDS which are found in my-map are put into the output
list in the form (key cmd doc) where key is the key in my-map to
run the cmd and doc is the first line of its docstring.

This is useful as an intermediate tool to extract valuable commands
from a keymap.
  "
  (let ((results nil))
    (map-keymap
     (lambda (key target)
       (if (or (and (symbolp target)
		    (member
		     (if (integerp key) (format "%c" key) key)
		     cmds))
	       (member target cmds))
	   (progn
	     (setq results
		   (cons (list
			  (if (integerp key)
			      (format "%c" key) key) target
			  (nth 0 (split-string (documentation target) "\n")))
			 results))
	     )
	 ;;(message "skip key=%s target=%s" key target)
	 )
       ) my-map)
    results)
  )

;; if do-add is a character we add the new hydra to treasure-for-hydra
;; with the given character binding
(defun treasure-for-hydra (name my-map cmds &optional do-add hydra-help)
  (let* ((hydra-map (treasure-find my-map cmds))
	 (hydra-body `(defhydra ,name (:color teal :columns 1)
			,(format "Define a treasure for hydra %s" name)
			,@(mapcar (lambda (x) x) hydra-map))))
    (eval hydra-body)
    (if do-add 
	(treasure-add-hydra (list do-add (intern (format "%s/body" name)) 
				  (if hydra-help hydra-help (format "hydra for %s"name))))
      )
    )
  )


(defun treasure-setup (key cmd &optional help)
  (list key cmd
	(if help help
	  (nth 0 (split-string (documentation cmd) "\n"))))
  )

  
;; if do-add is a character we add the new hydra to treasure-for-hydra
;; with the given character binding
(defun treasure-make-hoard (name hydra-data &optional hydra-help do-add)
  (let* ((hydra-info
	  (mapcar
	   (lambda (item) (apply 'treasure-setup item))
	   hydra-data))
	 (hydra-body
	  `(defhydra ,name (:color teal :columns 1)
	     ,(format (if hydra-help hydra-help
			"Define a treasure for hydra %s" name))
	     ,@(mapcar (lambda (x) x) hydra-info))))
   ;; (message hydra-info)
    (eval hydra-body)
    (if do-add 
	(treasure-add-hydra
	 (list do-add (intern (format "%s/body" name)) 
	       (if hydra-help hydra-help (format "hydra for %s"name))))
      )
    )
  )


(defun treasure-bind-master-hydra (&optional noaddquit)
  "Bind the master hydra.

This will use eval to create hydra-treasure-map/body
"
  (let ((clean-hdata treasure-map-hydras)
	)
    (if (not noaddquit)
	(if (member "q" (mapcar 'car clean-hdata))
	    (error "Cannot add quit command; treasure-map-hydras has 'q' cmd")
	  (setq clean-hdata
		(append clean-hdata
			'(("q" (message "Exiting hydra")
			   "quit hydra" :color blue))))))
    (message "noqddquit=%s hdata=%s" noaddquit clean-hdata)
    (eval `(defhydra hydra-treasure-map (:color teal :columns 1)
	     "Top-level hydra to show your hydras.
"
	     ,@(mapcar (lambda (x) x) clean-hdata)))
    )
  )

(provide 'treasure)
