(asdf:defsystem "signal"
    :class :package-inferred-system
    :defsystem-depends-on (:asdf-package-system)
    :description "Translator for SIGNAL language"
    :version "0.0.1"
    :author "Sokolovskyi Bohdan kv-73"
    :depends-on ("signal/core/all")
    :in-order-to ((test-op (load-op "signal/tests/all")))
    :perform (test-op (o c) (uiop:symbol-call :signal/tests/all :test-suite)))
