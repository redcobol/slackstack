all: server

fcgi:
	ghc --make -threaded \
		-o slackstack.fcgi \
		SlackStack/FCGI.hs \
		SlackStack/Model.hs \
		SlackStack/Util.hs \
		SlackStack/Handlers.hs \

server:
	ghc --make -threaded \
		-o slackstack \
		SlackStack/Main.hs \
		SlackStack/Model.hs \
		SlackStack/Util.hs \
		SlackStack/Handlers.hs \
