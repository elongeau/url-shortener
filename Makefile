mongo:
	docker compose -f mongo-stack.yml up -d

run:
	@stack --no-nix run

.PHONY: test
test: 
	@stack --no-nix test --file-watch
