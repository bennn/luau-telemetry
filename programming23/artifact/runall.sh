echo "deleting everything in out/ ..."
echo "=== before"
ls out/
rm -f out/*.*
echo "=== after"
echo "=== rebuilding ..."

echo "TIME $(date +%F_%T)"
PLTSTDERR="error info@luau" racket code/prepare.rkt
echo "TIME $(date +%F_%T)"
PLTSTDERR="error info@luau" racket code/prepare.rkt --mode size-distro
echo "TIME $(date +%F_%T)"
PLTSTDERR="error info@luau" racket code/prepare.rkt --mode modswitch
echo "TIME $(date +%F_%T)"
PLTSTDERR="error info@luau" racket code/prepare.rkt --mode divide-sessions
echo "TIME $(date +%F_%T)"
PLTSTDERR="error info@luau" racket code/prepare.rkt --mode session-query
echo "TIME $(date +%F_%T)"
PLTSTDERR="error info@luau" racket code/prepare.rkt --mode session-fold
echo "TIME $(date +%F_%T)"
PLTSTDERR="error info@luau" racket code/prepare.rkt --mode count-te-editrange 
echo "TIME $(date +%F_%T)"
PLTSTDERR="error info@luau" racket code/prepare.rkt --mode aggregate-te

echo "TIME $(date +%F_%T)"
racket code/row-distribution.rkt
echo "TIME $(date +%F_%T)"
racket code/sdupdate.rkt
echo "TIME $(date +%F_%T)"
racket code/size-distribution.rkt
echo "TIME $(date +%F_%T)"
racket code/downgrade-count.rkt
echo "TIME $(date +%F_%T)"
racket code/error-by-mode.rkt
echo "TIME $(date +%F_%T)"
racket code/type-error-survival.rkt
echo "TIME $(date +%F_%T)"
racket code/type-error-count.rkt
echo "TIME $(date +%F_%T)"
racket code/error-count.rkt
echo "TIME $(date +%F_%T)"

echo "\nDONE BUILDING, next step = compile pdf outside docker"
