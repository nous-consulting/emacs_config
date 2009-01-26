;;; Commentary:

;; ------------------------------------------------------------------------
;; Copyright (C) 2006 Jens Peter Secher
;;
;; This software is provided 'as-is', without any expressed or implied
;; warranty.  In no event will the authors be held liable for any
;; damages arising from the use of this software.
;;
;; Permission is granted to anyone to use this software for any
;; purpose, including commercial applications, and to alter it and
;; redistribute it freely, subject to the following restrictions:
;;
;; 1. The origin of this software must not be misrepresented; you must
;;    not claim that you wrote the original software.  If you use this
;;    software in a product, an acknowledgment in the product
;;    documentation would be appreciated but is not required.
;; 2. Altered source versions must be plainly marked as such, and must
;;    not be misrepresented as being the original software.
;; 3. This notice may not be removed or altered from any source
;;    distribution.
;; ------------------------------------------------------------------------

;; This is haxe-mode, an Emacs major mode for the haXe programming
;; language (http://haxe.org).

;; haxe-mode is built on top of the excellent cc-mode, inspired by the
;; guide http://cc-mode.sourceforge.net/derived-mode-ex.el.

;; haxe-mode is not part of GNU Emacs.

;;; Versions:
;;
;;    0.1.0 - Initial release.
;;

;;; Notes:

;; Until I find the right way to fix various small flaws, you need to
;; insert the following in your .emacs file:
;;
;; (require 'cc-mode)
;; (require 'haxe-mode)
;; (defconst my-haxe-style
;;   '((c-offsets-alist . ((case-label . +)
;;                         (topmost-intro-cont . 0))))
;;   "My haXe Programming Style")
;; (add-hook 'haxe-mode-hook (function (lambda () (c-add-style "haxe" my-haxe-style t))))
;;


;;; Code:

(require 'cc-mode)
(require 'cc-fonts)
(require 'cc-langs)
(require 'cc-bytecomp)

;; "The language constants are needed when compiling."
(eval-when-compile
 (let ((load-path
        (if (and (boundp 'byte-compile-dest-file)
                 (stringp byte-compile-dest-file))
            (cons (file-name-directory byte-compile-dest-file) load-path)
          load-path)))
   (load "cc-mode" nil t)
   (load "cc-fonts" nil t)
   (load "cc-langs" nil t)
   (load "cc-bytecomp" nil t)
   ))

(eval-and-compile
 ;; Tell the language constant system about haXe and base it on C++.
 (c-add-language 'haxe-mode 'c++-mode))


;;; Lexer-level syntax (identifiers, tokens etc).

;; There are dots in fully qualified identifiers, as in Java.
(c-lang-defconst c-identifier-ops
 haxe (c-lang-const c-identifier-ops java))

;; No other operators in identifiers.
(c-lang-defconst c-after-id-concat-ops
 haxe nil)

;; No backslash escaped newlines inside string literals.
(c-lang-defconst c-string-escaped-newlines
 haxe nil)

;;; Conditional compilation.

;; No strings in conditional compilation.
(c-lang-defconst c-cpp-message-directives
 haxe nil)

;; No file name in angle brackets or quotes in conditional compilation.
(c-lang-defconst c-cpp-include-directives
 haxe nil)

;; No macro definition in conditional compilation.
(c-lang-defconst c-opt-cpp-macro-define
 haxe nil)

;; Conditional compilation directives followed by expressions.
(c-lang-defconst c-cpp-expr-directives
 haxe '("if" "else"))

;; No functions in conditional compilation.
(c-lang-defconst c-cpp-expr-functions
 haxe nil)

;; Assignment operators.
(c-lang-defconst c-assignment-operators
 haxe (append
       ;; The usual Java assignment operators.
       (c-lang-const c-assignment-operators java)
       ;; haXe operators.
       '("<<=" ">>=")))

;; haXe operators.
(c-lang-defconst c-operators
 haxe `(
        ;; Preprocessor.
        (prefix "#")
        ;; Standard operators.
        ,@(c-lang-const c-identifier-ops)
        ;; Generics.
        (postfix-if-paren "<" ">")
        ;; Postfix.
        (left-assoc "." "->")
        (postfix "++" "--" "[" "]" "(" ")")
        ;; Unary.
        (prefix "++" "--" "+" "-" "!" "~" "new")
        ;; Multiplicative.
        (left-assoc "*" "/" "%")
        ;; Additive.
        (left-assoc "+" "-")
        ;; Shift.
        (left-assoc "<<" ">>" ">>>")
        ;; Relational.
        (left-assoc "<" ">" "<=" ">=")
        ;; Iteration.
        (left-assoc "...")
        ;; Equality.
        (left-assoc "==" "!=" "===" "!==")
        ;; Bitwise and.
        (left-assoc "&")
        ;; Bitwise exclusive or.
        (left-assoc "^")
        ;; Bitwise or.
        (left-assoc "|")
        ;; Logical and.
        (left-assoc "&&")
        ;; Logical or.
        (left-assoc "||")
        ;; Assignment.
        (right-assoc ,@(c-lang-const c-assignment-operators))
        ;; Exception.
        (prefix "throw")
        ;; Sequence.
        (left-assoc ",")))

;; No overloading.
(c-lang-defconst c-overloadable-operators
 haxe nil)
(c-lang-defconst c-opt-op-identitier-prefix
 haxe nil)

;; Punctuation or parenthesis that have uses other than as expression operators.
(c-lang-defconst c-other-op-syntax-tokens
 haxe '("{" "}" "(" ")" "[" "]" ";" ":" "," "/*" "*/" "//"))

;; The characters that should be considered to bound statements.
(c-lang-defconst c-stmt-delim-chars
 haxe "^;{}")
(c-lang-defconst c-stmt-delim-chars-with-comma
 haxe "^;,{}")


;;; Syntactic analysis ("virtual semicolons").
; TODO I might need to define c-at-vsemi-p-fn to handle typedefs.

;;; Keywords.

;; Types are pretty simle in haXe.
(c-lang-defconst c-primitive-type-kwds
  haxe '( "Dynamic" "Void" "Bool" "Int" "Float" "String" "Array" ))
; I will treat types below since they all start with capital letters.

;; Function declarations begin with "function" and variables with "var".  There
;; is no special keyword list for that in CC Mode, but treating it as a
;; type prefix works fairly well.
(c-lang-defconst c-primitive-type-prefix-kwds
 haxe '( "function" "var" ))
(c-lang-defconst c-type-prefix-kwds
 haxe nil)
(c-lang-defconst c-type-modifier-kwds
 haxe nil)

;; Type-introduction is straight forward in haXe.
(c-lang-defconst c-class-decl-kwds
 haxe '( "class" "interface" "enum" "typedef" ))
(c-lang-defconst c-brace-list-decl-kwds
 haxe nil)
(c-lang-defconst c-other-block-decl-kwds
 haxe nil)

;; TODO: here we might want to have Flash directives?
(c-lang-defconst c-decl-hangon-kwds
 haxe nil)

;; Only one definition modifier.
(c-lang-defconst c-modifier-kwds
 haxe '( "static" ))

;; Access protection label keywords in classes.
(c-lang-defconst c-protection-kwds
 haxe '( "private" "public" ))

;; No way to combine declarations with definitions in haXe.
(c-lang-defconst c-block-decls-with-vars
 haxe nil)

(c-lang-defconst c-postfix-decl-spec-kwds
 haxe nil)

(c-lang-defconst c-nonsymbol-sexp-kwds
 haxe nil)

(c-lang-defconst c-type-list-kwds
 haxe '( "extends" "implements" ))

(c-lang-defconst c-ref-list-kwds
 haxe '( "import" "package" ))

(c-lang-defconst c-colon-type-list-kwds
 haxe nil)

(c-lang-defconst c-paren-nontype-kwds
 haxe nil)

(c-lang-defconst c-paren-type-kwds
 haxe nil)

(c-lang-defconst c-<>-arglist-kwds
 haxe nil)

(c-lang-defconst c-simple-stmt-kwds
 haxe '("break" "continue" "return"))

;; No semicolons in for.
(c-lang-defconst c-paren-stmt-kwds
 haxe nil)

(c-lang-defconst c-asm-stmt-kwds
 haxe nil)

;; No gotos.
(c-lang-defconst c-before-label-kwds
 haxe nil)

(c-lang-defconst c-constant-kwds
 haxe '( "true" "false" "null" "default" ))

(c-lang-defconst c-primary-expr-kwds
 haxe '( "this" "super" ))

;; Untyped blocks.
(c-lang-defconst c-inexpr-brace-list-kwds
 haxe '( "untyped" ))

;; No bitfields.
(c-lang-defconst c-bitfield-kwds
 haxe nil)

(c-lang-defconst c-other-kwds
 haxe '( "in" ))

;; No labels.
(c-lang-defconst c-recognize-colon-labels
 haxe nil)

(c-lang-defconst c-opt-friend-key
 haxe nil)

;; No three-dots in type lists.
(c-lang-defconst c-opt-type-suffix-key
 haxe nil)

;; All identifiers starting with a capital letter are types.
(c-lang-defconst c-cpp-matchers
  haxe (append
        (c-lang-const c-cpp-matchers c)
        '(("\\<\\([A-Z][A-Za-z_]*\\)\\>" 1 font-lock-type-face))))

;; Fontification degrees.
(defconst haxe-font-lock-keywords-1 (c-lang-const c-matchers-1 haxe)
 "Minimal highlighting for haxe mode.")
(defconst haxe-font-lock-keywords-2 (c-lang-const c-matchers-2 haxe)
 "Fast normal highlighting for haxe mode.")
(defconst haxe-font-lock-keywords-3 (c-lang-const c-matchers-3 haxe)
 "Accurate normal highlighting for haxe mode.")
(defvar haxe-font-lock-keywords haxe-font-lock-keywords-3
 "Default expressions to highlight in haxe mode.")
(defun haxe-font-lock-keywords-1 ()
 (c-compose-keywords-list haxe-font-lock-keywords-1))
(defun haxe-font-lock-keywords-2 ()
 (c-compose-keywords-list haxe-font-lock-keywords-2))
(defun haxe-font-lock-keywords-3 ()
 (c-compose-keywords-list haxe-font-lock-keywords-3))
(defun haxe-font-lock-keywords ()
 (c-compose-keywords-list haxe-font-lock-keywords))

(defvar haxe-mode-syntax-table nil
 "Syntax table used in HaXe mode buffers.")
(or haxe-mode-syntax-table
   (setq haxe-mode-syntax-table
         (funcall (c-lang-const c-make-mode-syntax-table haxe))))

(defvar haxe-mode-abbrev-table nil
 "Abbreviation table used in haxe mode buffers.")
(c-define-abbrev-table 'haxe-mode-abbrev-table
 ;; Keywords that, if they occur first on a line, might alter the
 ;; syntactic context, and which therefore should trigger
 ;; reindentation when they are completed.
 '(("else" "else" c-electric-continued-statement 0)
   ("while" "while" c-electric-continued-statement 0)
   ("catch" "catch" c-electric-continued-statement 0)))

(defvar haxe-mode-map ()
 "Keymap used in haxe mode buffers.")
(if haxe-mode-map
   nil
 (setq haxe-mode-map (c-make-inherited-keymap)))

(add-to-list 'auto-mode-alist '("\\.hx\\'" . haxe-mode))

(defcustom haxe-mode-hook nil
  "*Hook called by `haxe-mode'."
  :type 'hook
  :group 'c)

(defun haxe-mode ()
 "Major mode for editing haXe code.

The hook `c-mode-common-hook' is run with no args at mode
initialization, then `haxe-mode-hook'.

Key bindings:
\\{haxe-mode-map}"
 (interactive)
 (kill-all-local-variables)
 (c-initialize-cc-mode t)
 (set-syntax-table haxe-mode-syntax-table)
 (setq major-mode 'haxe-mode
       mode-name "haXe"
       local-abbrev-table haxe-mode-abbrev-table
       abbrev-mode t)
 (use-local-map haxe-mode-map)
 ;; `c-init-language-vars' is a macro that is expanded at compile
 ;; time to a large `setq' with all the language variables and their
 ;; customized values for our language.
 (c-init-language-vars haxe-mode)
 ;; `c-common-init' initializes most of the components of a CC Mode
 ;; buffer, including setup of the mode menu, font-lock, etc.
 ;; There's also a lower level routine `c-basic-common-init' that
 ;; only makes the necessary initialization to get the syntactic
 ;; analysis and similar things working.
 (c-common-init 'haxe-mode)
 ;(easy-menu-add haxe-menu)
 (run-hooks 'c-mode-common-hook 'haxe-mode-hook)
 (c-update-modeline))

(provide 'haxe-mode)

;;; haxe-modex.el ends here.