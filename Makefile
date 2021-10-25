db-start:
	docker compose -f mongo-stack.yml up -d

db-stop:
	docker compose -f mongo-stack.yml down

run:
	cabal run url-shortener