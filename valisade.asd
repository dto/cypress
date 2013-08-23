(asdf:defsystem #:valisade
  :depends-on (:blocky)
  :components ((:file "package")
	       (:file "mission" :depends-on ("package"))
	       (:file "meadow" :depends-on ("mission"))
	       (:file "dialogue" :depends-on ("meadow"))
	       (:file "monk" :depends-on ("dialogue"))
	       (:file "valisade" :depends-on ("monk"))))
