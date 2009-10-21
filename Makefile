all: server

debug:
	ghci \
		SlackStack/Util.hs \
		SlackStack/Handlers.hs

fcgi:
	ghc --make -threaded \
		-o slackstack.fcgi \
		SlackStack/FCGI.hs \
		SlackStack/Util.hs \
		SlackStack/Handlers.hs \

server:
	ghc --make -threaded \
		-o slackstack \
		SlackStack/Main.hs \
		SlackStack/Util.hs \
		SlackStack/Handlers.hs \
