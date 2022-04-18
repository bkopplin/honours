FROM ubuntu:20.04

WORKDIR /eneo

# Set timezone
ENV DEBIAN_FRONTEND=noninteractive
ENV TZ=Europe/London
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone

# Install Dependencies
RUN apt-get update -y &&\
	apt-get install -y curl

# Install erlang
RUN curl https://packages.erlang-solutions.com/erlang/debian/pool/esl-erlang_24.2.1-1~ubuntu~focal_amd64.deb -o /erlang.deb &&\
	 apt-get install -y /erlang.deb &&\
	 rm /erlang.deb

# Install rebar3
RUN curl https://s3.amazonaws.com/rebar3/rebar3 -o /opt/rebar3 &&\
	chmod u+x /opt/rebar3 &&\
	echo "export PATH=/opt:$PATH >> /root/.bashrc"
