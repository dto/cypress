(asdf:defsystem #:valisade
  :depends-on (:blocky)
  :components ((:file "package")
	       (:file "meadow" :depends-on ("package"))
	       (:file "monk" :depends-on ("meadow"))
	       (:file "valisade" :depends-on ("monk"))))
