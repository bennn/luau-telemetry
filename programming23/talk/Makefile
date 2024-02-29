TALK=talk.ss
RACO=/Applications/Racket\ v8.10/bin/raco

pict: compiletalk
	@${RACO} pict ${TALK}

compiletalk:
	@${RACO} make -v ${TALK}

show: compiletalk
	@${RACO} slideshow --widescreen ${TALK}

pdf: compiletalk
	@${RACO} slideshow --widescreen --condense -x ${TALK}

