(defsystem test-weakly-depends-on
  :weakly-depends-on (does-not-exist)
  :if-component-dep-fails :ignore
  :components ((:file "file1")))

