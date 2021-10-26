db-start:
	docker compose -f mongo-stack.yml up -d

db-stop:
	docker compose -f mongo-stack.yml down

run:
	@stack --no-nix run

.PHONY: test
test: 
	@stack --no-nix test --file-watch
