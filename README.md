# Ignition

Ignition is a polyglot Vagrantfile generator.

Clone, build, throw it on your path.

```
ignition postgres haskell
```

Peruse your shiny new Vagrantfile in your current directory. Currently `base postgres haskell elixir java clojure ruby node elm` are available, with base installed by default. Multiple may be specified. Running on windows assumes Hyper-V and SMB, virtualbox otherwise.

Parsing args for things like box and memory are on the TODO list.
