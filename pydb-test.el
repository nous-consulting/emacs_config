; -*- emacs-lisp -*-
(load-file "./elk-test.el")
(load-file "./pydb.el")

(defun regexp-test (location-str file-str)
  "Test to see that location-str matches gud-pydb-marker-regexp"
  (assert-equal 0 (string-match gud-pydb-marker-regexp location-str))
  (assert-equal file-str
		(substring location-str
			   (match-beginning gud-pydb-marker-regexp-file-group) 
			   (match-end gud-pydb-marker-regexp-file-group)))
)
(deftest "pydb-marker-regexp-test"

  (regexp-test 
   "(e:\\sources\\capfilterscanner\\capanalyzer.py:3):  <module>"
   "e:\\sources\\capfilterscanner\\capanalyzer.py"
   )
  (regexp-test 
   "(e:\\Documents and Settings\\jsmith\\My Documents\\cpanalyzer test.py:3):  <module>"
   "e:\\Documents and Settings\\jsmith\\My Documents\\cpanalyzer test.py"
   )  
  (regexp-test 
   "(/tmp/gcd.py:29):  gcd"
   "/tmp/gcd.py"
   )
  (regexp-test 
   "(/tmp/gcd.py:29)"
   "/tmp/gcd.py"
   )
)

(defun position-regexp-test (location-str file-str line-str)
  "Test to see that location-str matches position-regexp-test with the correct
file and line submatches."
  (assert-equal 0 (string-match pydb-position-re location-str))
  (assert-equal file-str (match-string pydb-marker-regexp-file-group
                                       location-str))
  (assert-equal line-str (match-string pydb-marker-regexp-line-group
                                       location-str))
  )
(deftest "pydb-position-re-test"

  (position-regexp-test 
   "(e:\\sources\\capfilterscanner\\capanalyzer.py:3):  <module>\n"
   "e:\\sources\\capfilterscanner\\capanalyzer.py" "3"
   )
  (position-regexp-test 
   "(e:\\Documents and Settings\\jsmith\\My Documents\\cpanalyzer test.py:3):  <module>\n"
   "e:\\Documents and Settings\\jsmith\\My Documents\\cpanalyzer test.py" "3"
   )  
  (position-regexp-test 
   "(/tmp/gcd.py:29):  gcd\n"
   "/tmp/gcd.py" "29"
   )
  (position-regexp-test 
   "(/tmp/gcd.py:29)\n"
   "/tmp/gcd.py" "29"
   )
)  
   
(build-suite "pydb-suite" "pydb-marker-regexp-test" "pydb-position-re-test")
(run-elk-test "pydb-suite"
              "test regular expression used in tracking lines")

