(asdf:defsystem #:cypress
  :depends-on (:xelf)
  :components ((:file "package")
	       (:file "mission" :depends-on ("package"))
	       (:file "meadow" :depends-on ("mission"))
	       (:file "dialogue" :depends-on ("meadow"))
	       (:file "monk" :depends-on ("dialogue"))
	       (:file "cypress" :depends-on ("monk"))))
