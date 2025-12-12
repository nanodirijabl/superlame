ARG OTP_VERSION

FROM docker.io/library/erlang:${OTP_VERSION} AS builder
SHELL ["/bin/bash", "-o", "pipefail", "-c"]

RUN mkdir /build
WORKDIR /build

# NOTE Detach deps layer cache from the rest of the files
COPY rebar.lock /build/
RUN rebar3 get-deps

COPY . /build/
RUN rebar3 release --all

FROM docker.io/library/erlang:${OTP_VERSION}-slim

ARG USER_NAME=superlame
ARG USER_UID=1001
ARG USER_GID=$USER_UID

ENV RELX_REPLACE_OS_VARS=true
ENV ERL_DIST_PORT=31337
ENV CHARSET=UTF-8
ENV LANG=C.UTF-8

COPY --from=builder /build/_build/default/rel/ /app/

RUN groupadd --gid ${USER_GID} ${USER_NAME} && \
    useradd --uid ${USER_UID} --gid ${USER_GID} -M ${USER_NAME} && \
    chown -R ${USER_UID}:${USER_GID} /app

USER ${USER_NAME}

ENTRYPOINT []
CMD []
