(asdf:defsystem #:cypress
  :depends-on (:xelf)
  :components ((:file "package")
	       (:file "world" :depends-on ("package"))
	       (:file "mission" :depends-on ("world"))
	       (:file "gumps" :depends-on ("mission"))
	       (:file "dialogue" :depends-on ("gumps"))
	       (:file "monk" :depends-on ("dialogue"))
	       (:file "cypress" :depends-on ("monk"))))
