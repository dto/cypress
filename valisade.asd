(asdf:defsystem #:f0rest
  :depends-on (:blocky)
  :components ((:file "package")
	       (:file "mission" :depends-on ("package"))
	       (:file "meadow" :depends-on ("mission"))
	       (:file "dialogue" :depends-on ("meadow"))
	       (:file "monk" :depends-on ("dialogue"))
	       (:file "f0rest" :depends-on ("monk"))))
