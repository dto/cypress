(asdf:defsystem #:valisade
  :depends-on (:blocky)
  :components ((:file "package")
	       (:file "meadow" :depends-on ("package"))
	       (:file "dialogue" :depends-on ("meadow"))
	       (:file "monk" :depends-on ("dialogue"))
	       (:file "valisade" :depends-on ("monk"))))
