all: server

debug:
	ghci \
		SlackStack/Handlers.hs \
		SlackStack/Handlers/Auth.hs \
		SlackStack/Handlers/Admin.hs \
		SlackStack/Util.hs \

fcgi:
	ghc --make -threaded \
		-o slackstack.fcgi \
		SlackStack/FCGI.hs \
		SlackStack/Util.hs \
		SlackStack/Handlers.hs \
		SlackStack/Handlers/Auth.hs \
		SlackStack/Handlers/Admin.hs \

cgi:
	ghc --make -threaded \
		-o slackstack.cgi \
		SlackStack/CGI.hs \
		SlackStack/Util.hs \
		SlackStack/Handlers.hs \
		SlackStack/Handlers/Auth.hs \
		SlackStack/Handlers/Admin.hs \

server:
	ghc --make -threaded \
		-o slackstack \
		SlackStack/Main.hs \
		SlackStack/Util.hs \
		SlackStack/Handlers.hs \
		SlackStack/Handlers/Auth.hs \
		SlackStack/Handlers/Admin.hs \
