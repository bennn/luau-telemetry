echo "deleting everything in out/ ..."
echo "=== before"
ls out/
rm -f out/*.*
echo "=== after"

PLTSTDERR="error info@luau" racket code/prepare.rkt
PLTSTDERR="error info@luau" racket code/prepare.rkt --mode size-distro
PLTSTDERR="error info@luau" racket code/prepare.rkt --mode modswitch
PLTSTDERR="error info@luau" racket code/prepare.rkt --mode divide-sessions
PLTSTDERR="error info@luau" racket code/prepare.rkt --mode session-query
PLTSTDERR="error info@luau" racket code/prepare.rkt --mode session-fold
PLTSTDERR="error info@luau" racket code/prepare.rkt --mode count-te-editrange 
PLTSTDERR="error info@luau" racket code/prepare.rkt --mode aggregate-te

racket code/row-distribution.rkt
racket code/size-distribution.rkt
racket code/sdupdate.rkt
racket code/downgrade-count.rkt
racket code/error-by-mode.rkt
racket code/type-error-survival.rkt
racket code/type-error-count.rkt
racket code/error-count.rkt

echo "DONE BUILDING, next step = compile pdf outside docker"
