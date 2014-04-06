scalasca
========

A static code analysis plugin for the Scala compiler.

Compiling
--------------

Use `sbt` and `package` to compile the plugin.

Testing
-----------

In order to run the unit tests, use SBT's `testOnly`. Standalone tests can be run using `src/standaloneTest`. As a first example, you can run ScalaSCA on ScalaSCA's source itself, using the `src/autoCheck` script.

Run
------

 Once the plugin has been compiled, it can be used as an additional phase for the compiler: `scalac -Xplugin:target/scala-XXX/scalasca_XXX-0.1.jar`, where `XXX` stands for the current scala version.

Improvements
---------------

- `BlockConstantPropagation` casts to `String` and `Int` in `Constant val built from other constants`. Any clean quasiquotish solution for this (not being able to be lifted to `Any`)?
- `BlockConstantPropagation` does not consider arbitrary methods

Currently checked
-------------------------

```scala

```