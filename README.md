# URL Shortener

Coding environment is provided by [nixkell](https://github.com/pwm/nixkell).
The project [issue-wanted](https://github.com/kowainik/issue-wanted) was a great source of inspiration and also the post [Three layer Haskell cake](https://www.parsonsmatt.org/2018/03/22/three_layer_haskell_cake.html) by Matt Parsons.

# Shortener algorithm

I used the blogpost [System Design : Scalable URL shortener service like TinyURL](https://medium.com/@sandeep4.verma/system-design-scalable-url-shortener-service-like-tinyurl-106f30f23a82) for the shortener algorithm and some reflexion about this kind of service

# Running the application

The configuration is loaded from environment variables, see the `.envrc` for the details (don't forget to [`direnv allow .`](https://direnv.net/)).

You need docker to start the MongoDB used by the application, use the `Makefile`:

- `db-start`/`db-stop` to start/stop the local MongoDB
- `run` to start the application
- `test` to build and test the application

# Interacting with the application

## Send an URL to shorten

```shell
curl --request POST \
     --url http://localhost:8080/shorten \
     --header 'Content-Type: application/json' \
     --data '{
  "raw": "http://example.com/shortener-url"
  }'
```

You'll get a result of this from

```json
{
  "url": "http://localhost:8080/sMSNdzb"
}
```

You can then paste this URL in your browser and get redirected to the original URL.

# Architecture

The application used the [ReaderT pattern](https://www.fpcomplete.com/blog/2017/06/readert-design-pattern/) mixed with the [Handle pattern](https://jaspervdj.be/posts/2018-03-08-handle-pattern.html).

# Project structure

- `app`: contains the executable
- `src`: contains the project sources, structured around 3 modules:
  - `Core.hs`: the business model and logic
  - `Handlers.hs`: the API of the application
  - `Infra.hs`: the glue that join all the parts
- `test`: tests of the application
