ASS_USELESS_VAR_ASSIGNMENT
==========================

*DOES*:
- Find re-assignments to local vars, after which ones the var in question is not used as an rvalue anymore

*DOES NOT*:
- Handle class/object/trait fields