


;; FIXME:  Use the following to get first doc line of a symbol
;; (nth 0 (split-string (documentation 'quoted-insert) "\n"))
;; FIXME: can lookup through keymap with something like
;; (map-keymap (lambda (a b) (message "a is %s and b is %s of type %s with doc %s" a b (type-of b) (nth 0 (split-string (documentation 'quoted-insert) "\n")))) my-mode-map)

(makunbound 'treasure-map-hydras) ;; uncomment when testing for reloading

(defvar treasure-map-hydras 
  '(("q" (message "quit") "quit hydra" :exit t :color blue))
  "List of hydras to setup the treasure map for.

This should be a list of lists where each sub-list is of the form
expected by defhydras. The simplest form is (KEY CMD MSG) where KEY
is the key that triggers the hydra, CMD is the command to run, and
MSG is the message to show next to KEY.

You should generally makes sure there is a quit command in this list.
For example, the default starting value is

    ((\"q\" (message \"quiting hydra\") \"quit hydra\" :exit t :color blue))

which makes sure that the \"q\" shows the hint \"quit hydra\" and
prints the \"quitting hydra\" message when pressed while exiting.

It is probably best if you only modify this using the treasure-add-hydra
function for safety. See docs for the treasure-add-hydra function for
details."
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
  "Look in MY-MAP to find cmds and make a hydra
  "
  (let ((fixme nil))
    (map-keymap (lambda (key target) 
		  (if (and (symbolp target) (member (format "%c" key) cmds))
		      (progn (setq fixme (cons 
					  (list (format "%c" key) target
						(nth 0 (split-string (documentation target) "\n")))
					  fixme))
			     )
		    ;;   (message "key is %s and target is %s with doc %s" key target
		    ;;     (nth 0 (split-string (documentation 'quoted-insert) "\n")))
		    ;; (message "skip %s %s" key target)
		    )
		  ) my-map)
    fixme)
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

(treasure-for-hydra 'hydra-test-fixme my-mode-map 
		    (list "a" "g" "f" "h") "o" "Example dynamic hydra")

;;    (treasure-add-hydra 
;;	'(";" hydra-test-fixme/body "FIXME test dynamic hydra"))
(treasure-add-hydra 
 '("h" hydra-treasure-map-help/body "help on hydras"))
(treasure-add-hydra 
 '("t" hydra-treasure-map-help/body "help on hydras again"))

(eval `(defhydra hydra-treasure-map (:color teal :columns 1)
	 "Fast links by projects stuff"
	 ,@(mapcar (lambda (x) x) treasure-map-hydras)))

(provide 'treasure)
