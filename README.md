webmachine
==========

This project began at [Basho](http://basho.com), the creators and
maintainers of Riak. Due to the importance of webmachine to the
broader Erlang community, a new organization was formed. Please
contact [@seancribbs](http://github.com/seancribbs) to get involved.

### Overview

[![Build Status](https://travis-ci.org/webmachine/webmachine.svg?branch=develop)](https://travis-ci.org/webmachine/webmachine)

Webmachine is an application layer that adds HTTP semantic awareness
on top of the excellent bit-pushing and HTTP syntax-management
provided by mochiweb, and provides a simple and clean way to connect
that to your application's behavior.

More information is available
[here](https://github.com/webmachine/webmachine/wiki). You can also
read past blog posts about Webmachine
[here](http://basho.com/tag/webmachine/).

### Development

Webmachine is a [rebar3](http://rebar3.org) project.

Running all tests and dialyzer is as easy as

```
make all
```

However, if you'd like to run them separately:

* EUnit: `rebar3 eunit`
* Dialyzer: `rebar3 dialyzer`


If you don't have `rebar3`, you should get it. If you don't want to,
it's downloaded as part of `make all`

### Quick Start

A [rebar3](http://rebar3.org) template is provided for users quickly
and easily create a new `webmachine` application.

```
$ mkdir -p ~/.config/rebar3/templates
$ git clone git://github.com/webmachine/webmachine-rebar3-template.git ~/.config/rebar3/templates
$ rebar3 new webmachine your_app_here
```

Once a new application has been created it can be built and started.

```
$ cd your_app_here
$ rebar3 release
$ _build/default/rel/your_app_here/bin/your_app_here console
```

The application will be available at [http://localhost:8080](http://localhost:8080).

To learn more continue reading [here](https://github.com/webmachine/webmachine/wiki).

### Contributing

   We encourage contributions to `webmachine` from the community.

   1) Fork the `webmachine` repository on [Github](https://github.com/webmachine/webmachine).
   
   2) Clone your fork or add the remote if you already have a clone of
   the repository.

```
git clone git@github.com:yourusername/webmachine.git
```

or

```
git remote add mine git@github.com:yourusername/webmachine.git
```

   3) Create a topic branch for your change.

```
git checkout -b some-topic-branch
```

   4) Make your change and commit. Use a clear and descriptive commit
      message, spanning multiple lines if detailed explanation is
      needed.
      
   5) Push to your fork of the repository and then send a pull-request
      through Github.

```
git push mine some-topic-branch
```

   6) A community maintainer will review your pull request and merge
      it into the main repository or send you feedback.
