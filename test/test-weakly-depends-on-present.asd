(defsystem test-weakly-depends-on-present
  :weakly-depends-on (file3-only)
  :components ((:file "file1")))

