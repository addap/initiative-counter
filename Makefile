# https://www.fpcomplete.com/blog/2015/05/haskell-web-server-in-5mb
default: run

hello:
	@stack build
	@strip .stack-work/install/x86_64-linux-tinfo6/1f856a9bf91d2b4cea92f6f32ed0d616e1c63576f8b0fdb18cc5f1de70643cf7/8.6.5/bin/initiative-counter

image: | hello
	@docker build -t registry.gitlab.com/addapp/initiative-counter/initiative-counter:latest .

run: | image
	@docker run --rm -p 8023:8023 --name initiative-counter -i -t haskell-initiative-counter:latest

clean:
	@rm -rf hello

.PHONY: default image run
