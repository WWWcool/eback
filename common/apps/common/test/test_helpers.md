test helpers
=====

Памятка для тестирования сервера

- запуск сервера 
    `rebar3 release`
    `./_build/default/rel/common/bin/common foreground`

- curl запрос для добавления простейшей структуры данных
 
curl --header "Content-Type: application/json" \
--request POST \
--data '{"user":{"id":"123"}}' \
http://localhost:8080/123

- получение созданной структуры
    `http://localhost:8080/123`
