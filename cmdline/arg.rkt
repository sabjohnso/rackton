#lang rackton

;; rackton/cmdline/arg — argument information (cmdliner's Arg.info).
;;
;; OCaml uses optional labelled arguments to build an `Arg.info`;
;; Rackton has none, so an `ArgInfo` is a record (`struct`) built by
;; `arg-info` with defaults, then refined left-to-right by the field
;; updaters `info-doc` / `info-docv` / `info-env` / `info-docs`.
;;
;;   (info-doc "Be loud" (arg-info (list "v" "verbose")))

(provide (struct-out EnvInfo)
         (struct-out ArgInfo)
         (data-out ArgKind)
         (struct-out ArgDecl)
         arg-info info-doc info-docv info-env info-docs)

;; ----- argument kind -----------------------------------------------
;; What an argument consumes, so eval can derive the parser's option
;; specs (a flag takes no value, an option does, a positional is read by
;; index).  Set by the Arg constructor; carried in a Term's declarations.

(data ArgKind KFlag KOpt KPos)

(struct ArgDecl
  [info : ArgInfo]
  [kind : ArgKind])

;; ----- environment-variable backing for an argument ----------------

(struct EnvInfo
  [var  : String]      ; the variable name
  [doc  : String]      ; man text for the ENVIRONMENT section
  [docs : String])     ; section the variable is documented under

;; ----- argument information ----------------------------------------
;; names is empty for a positional argument.

(struct ArgInfo
  [names : (List String)]
  [doc   : String]
  [docv  : (Maybe String)]   ; value placeholder; None ⇒ take it from the Conv
  [env   : (Maybe EnvInfo)]
  [docs  : String])          ; man section (default "OPTIONS")

;; names → a default ArgInfo.
(: arg-info (-> (List String) ArgInfo))
(define (arg-info names) (ArgInfo names "" None None "OPTIONS"))

;; ----- field updaters ----------------------------------------------
;; `update` resolves a field by its (globally unique) name; ArgInfo and
;; EnvInfo share `doc`/`docs`, so we rebuild positionally instead —
;; robust to any other struct reusing those field names.

(: info-doc (-> String ArgInfo ArgInfo))
(define (info-doc d i)
  (match i [(ArgInfo n _ dv e ds) (ArgInfo n d dv e ds)]))

(: info-docv (-> String ArgInfo ArgInfo))
(define (info-docv v i)
  (match i [(ArgInfo n d _ e ds) (ArgInfo n d (Some v) e ds)]))

(: info-env (-> EnvInfo ArgInfo ArgInfo))
(define (info-env e i)
  (match i [(ArgInfo n d dv _ ds) (ArgInfo n d dv (Some e) ds)]))

(: info-docs (-> String ArgInfo ArgInfo))
(define (info-docs s i)
  (match i [(ArgInfo n d dv e _) (ArgInfo n d dv e s)]))
