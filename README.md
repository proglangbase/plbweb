# plbweb

proglangbase website

*in a POSIX shell you can enter:*


### `./plbweb_ctl`  
  Used to start/stop the `plbweb` server. Like a standard Linux service script, 
  it supports `start`, `stop`, `restart`, and `status` commands.
  - Requires Erlang/OTP commands are found in PATH.
  - Requires `plbcom/bin/sh` is found in PATH.
  - `. ./setenv` will install Erlang/OTP and `plbcom` locally, if necessary,
    and modify PATH accordingly.
     

### `./run`
  - requires Erlang/OTP `epmd` and `erl`
  - runs the default implementation
  - at URL [http://localhost:8088](http://localhost:8088)
  - in an Erlang node named 'plbweb'
  - idempotent if already running
  - remains in the Erlang shell
  - `Ctrl-G<Enter> q<Enter>` to leave the shell
  - or `q().<Enter>` to shutdown the Erlang node

## implementations

- [BEAM](https://en.wikipedia.org/wiki/BEAM_(Erlang_virtual_machine))
  1. [Erlang](https://github.com/erlang/otp) **WIP**
  2. [Elixir](https://github.com/elixir-lang/elixir) *TODO*
  3. [Gleam](https://github.com/gleam-lang/gleam) *TODO*
  4. [LFE](https://github.com/lfe/lfe) *TODO*

## references

- [Awesome Erlang Web Frameworks](https://project-awesome.org/drobakowski/awesome-erlang#web-frameworks)
- [Erlang Forums: Which webserver do you use?](https://erlangforums.com/t/which-webserver-do-you-use/1911)
- [Languages on the BEAM](https://github.com/llaisdy/beam_languages#34-languages-on-the-beam)
