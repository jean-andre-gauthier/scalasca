MEM_MISSING_RESOURCE_CLOSING
============================

*DOES*:
- Flag any call to openMethodName for which there exists at least one execution path where closeMethodName is not called.

*DOES NOT*:
- Handle exceptions (yet)