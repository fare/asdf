(defsystem test-weakly-depends-on-present
  :weakly-depends-on (file3-only)
  :if-component-dep-fails :ignore
  :components ((:file "file1")))

