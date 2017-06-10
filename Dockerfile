FROM erlang:latest

MAINTAINER "Benjamin Schubert <ben.c.schubert@gmail.com>"


ADD . /srv/sentibot

WORKDIR /srv/sentibot

RUN rebar3 release

CMD ["/srv/sentibot/_build/default/rel/sentibot/bin/sentibot", "foreground"]
