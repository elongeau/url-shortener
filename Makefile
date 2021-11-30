mongo:
	docker compose -f mongo-stack.yml up -d

run:
	@stack --no-nix run

.PHONY: test
test: mongo
	@ghcid --command="stack ghci url-shortener:lib url-shortener:test:spec  --ghci-options=-fobject-code" --test "main"
