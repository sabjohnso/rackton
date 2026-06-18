#lang rackton

;; Fixture for promoted-kinds-cross-module-test.rkt.  A library whose
;; DataKinds-promoted constructors (TInt / TBool of kind Ty, SEmpty /
;; SCons of kind Stack) index a parameterised type Mem :: Stack -> Ty ->
;; *.  An importing module must keep those promoted kinds so its kind
;; checker enforces a promoted index — see ISSUES.org "Imported promoted
;; constructor loses its kind".

(provide (data-out Ty) (data-out Stack) (data-out Mem)
         (data-out PList) (data-out PStack))

(data Ty TInt TBool)
(data Stack SEmpty (SCons Ty Stack))

(data (Mem g a)                         ; Mem :: Stack -> Ty -> *
  (MZ : (Mem (SCons a g) a))
  (MS : (-> (Mem g a) (Mem (SCons b g) a))))

;; A PROMOTED PARAMETERISED datatype: PList :: (PList k) for any kind k.
;; Its kind-polymorphic constructors (PNil/PCons, carrying `kapp` kind
;; schemes) must round-trip through the sidecar so an importer can use
;; (PList Ty) as an index.
(data (PList a) PNil (PCons a (PList a)))

(data (PStack g)                        ; PStack :: (PList Ty) -> *
  (PSEmpty : (PStack PNil))
  (PSPush  : (-> (PStack g) (PStack (PCons TInt g)))))
