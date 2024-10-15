SRCS := $(patsubst %org,%html,$(wildcard src/*.org))

web: $(SRCS)

ins: web
	mv src/*.html cnt/

%.html: %.org
	emacs --batch --load web.el $<\
              --eval '(progn (outline-show-all)\
                             (font-lock-flush)\
                             (font-lock-fontify-buffer)\
                             (org-html-export-to-html))'
	-wtidy -m $@
