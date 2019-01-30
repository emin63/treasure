

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

An error is raised if the given KEY is already spoken for in
the treasure-map-hydras variable.
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

      ")
  )


(defun treasure-find (my-map cmds)
  "Look in MY-MAP to find CMDS and extract them into a list.

This function takes a keyamp as MY-MAP and a list, CMDS, consisting
of either string characters like \"g\" or symbols like 'grep indicating
commands you want to extract out.

Matches from CMDS which are found in my-map are put into the output
list in the form (key cmd doc) where key is the key in my-map to
run the cmd and doc is the first line of its docstring.

This is useful as an intermediate tool to extract valuable commands
from a keymap. For example, you can do something like

(treasure-make-hoard 'hydra-my-help
  (treasure-find help-map (list \"g\" 'where-is \"f\" \"k\" \"a\"))
)

where the second line is using treasure-find to extract out the desired
commands from help-map and the first line is calling treasure-make-hoard
to actually make the hydra with those commands.
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

(defun treasure-setup (key cmd &optional help)
  "Setup treasure to show help if it was not given.

Usually hydras want a line like (KEY CMD HELP). But it is nice
to be able to just provide (KEY CMD) and automatically lookup HELP
from the doc string in CMD. By calling this as

  (treasure KEY CMD HELP)

where HELP is not nil you will simply get back (KEY CMD HELP).

But if HELP is nil, then we return (KEY CMD DOC) where DOC is
the first line of the doc string in CMD.
"
  (list key cmd
	(if help help
	  (nth 0 (split-string (documentation cmd) "\n"))))
  )

  
;; if do-add is a character we add the new hydra to treasure-map-hydras
;; with the given character binding
(defun treasure-make-hoard (name hydra-data
				 &optional hydra-help do-add no-top)
  "Make a hydra to watch over a hoard of useful commands.

The following are the arguments:

  NAME:        A string name for the new hydra. Note that based on hydra
               convention, you should call the hydra as name/body.
  HYDRA-DATA:  A list of lists used to define the body of the hydra.
               Each sub-list of HYDRA-DATA should be of the form
               (KEY CMD HELP) providing the key to type to execute CMD
               which is shown with the doc string HELP. If the HELP is
               nil, the treasure-setup will try to look it up for you.

Optional Arguments:

  HYDRA-HELP:  Optional string doc for the hydra we create.
  DO-ADD:      If this is a string key, then we call treasure-add-hydra
               with DO-ADD as the key and the new hydra so you don't
               have to manually do that.
  NO-TOP:      If this is nil we add an entry to take the hydra back
               to the top hydra-treasure-map/body. If this is non-nil
               we do not add a top element.
"
  (let* ((hydra-info-base
	  (mapcar
	   (lambda (item) (apply 'treasure-setup item))
	   hydra-data))
	 (hydra-info (if no-top hydra-info-base
		       (append hydra-info-base
			       '(("!" hydra-treasure-map/body
				  "Back to top-level hydra-treasure-map")))))
	 (hydra-body
	  `(defhydra ,name (:color teal :columns 1)
	     ,(format (if hydra-help hydra-help
			"Define a treasure for hydra %s"
			(if (symbolp name) (symbol-name name) name)))
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

This will use eval to create hydra-treasure-map/body. It is intended
that you call treasure-bind-master-hydra once you have added all
your hydras to the treasure-map-hydras variable by calling
the treasure-add-hydra function to create the master hydra.

You can then bind hydra-treasure-map/body to the desired key and
access all your hydras through that.

If the optional noaddquit is provided, then we will not add a quit
command to the master hydra
"
  (let ((clean-hdata treasure-map-hydras)
	)
    (if (not noaddquit)
	(if (member "q" (mapcar 'car clean-hdata))
	    (error "Cannot add quit command; treasure-map-hydras has 'q' cmd")
	  (setq clean-hdata
		(append clean-hdata
			'(("q" (message "Exiting hydra")
			   "quit hydra"))))))
    (treasure-make-hoard 'hydra-treasure-map clean-hdata
			 "Top-level hydra to show your hydras.
"
			 nil 't
			 )
    )
  )


(provide 'treasure)
