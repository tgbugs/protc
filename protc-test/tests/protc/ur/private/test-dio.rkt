#lang protc/ur
; (module->exports 'tests/protc/ur/private/test-dio)
(protc:output "DIO mouse" #:prov (hyp: 'dio-mouse)
  (protc:aspect "type identifier" (protc:invariant "JAX:380050"))
  (protc:aspect "mass" (protc:parameter* (param:quantity 52 (unit 'grams))))
  (protc:input "mouse"                                                      
    (protc:aspect "age" (protc:parameter* (param:quantity 6 (unit 'weeks)))))
  (protc:input "mouse diet"                                                  
    (protc:aspect "ad libitum" (protc:parameter* (bool #t)))
    (protc:aspect "type identifier" (protc:invariant "D12392"))))
