# Compilaton and testing
FROM erlang:21.0 as util

COPY ./ /build
WORKDIR /build

RUN rebar3 compile
#RUN rebar3 do eunit, ct

RUN rm -rf _build/default/rel
RUN rebar3 release

# Make an app image
FROM erlang:21.0

WORKDIR /opt/otp21
COPY --from=util /build/_build/default/rel/otp21 ./

EXPOSE 4369
EXPOSE 8080

CMD bin/otp21 foreground
