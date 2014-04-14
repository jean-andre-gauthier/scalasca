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

Summary
-------------------------

ScalaSCA has four warning levels, that are displayed in their respective colours, if your terminal supports them:
- Notice (white): for rules of any kind, that do not check the "correctness" of the code. General static analyses (e.g. constant propagation) typically fall into this category
- Warning (yellow): generally used when a rule discovered a potential bug (medium probability)
- Severe warning (dark yellow): used when a rule discovered a potential bug (high probability)
- Fatal (red): used when a rule discovered a piece of code that will crash at runtime, if executed

| Rule Error Code | Description | Category |
| ---------- | ----------- | -------- |
| [ASS_USELESS_VAR_ASSIGNMENT](doc/ASS_USELESS_VAR_ASSIGNMENT.md) | Re-assignment to var has no effect | Bad Practice |
| [MEM_MISSING_RESOURCE_CLOSING](doc/MEM_MISSING_RESOURCE_CLOSING.md) | Resource not freed after usage | Bad Practice |
| [CLS_PUBLIC_MUTABLE_FIELDS](doc/CLS_PUBLIC_MUTABLE_FIELDS.md) | Public vars in classes/objects/traits | Bad Practice |
| [DEC_UNUSED_DATA](doc/DEC_UNUSED_DATA.md) | Declared but unused val/var | Bad Practice |
| [ARI_DIV_BY_ZERO](doc/ARI_DIV_BY_ZERO.md) | Divison by 0 | Fatal |
| [GEN_UNUSED_CODE_REMOVAL](doc/GEN_UNUSED_CODE_REMOVAL.md) | Removes unused code | General |
| [GEN_BLOCK_CONST_PROP](doc/GEN_BLOCK_CONST_PROP.md) | Propagates constant values | General |


Improvements
---------------

- `BlockConstantPropagation` casts to `String` and `Int` in `Constant val built from other constants`. Any clean quasiquotish solution for this (not being able to be lifted to `Any`)?
- `BlockConstantPropagation` does not consider arbitrary methods