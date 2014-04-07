DEC_UNUSED_DATA
===============

*DOES*:
- Find vars and vals that are declared, but never used.
- Find vars/vals that are only written to or reassigned, without being evaluated in an expression

*DOES NOT*:
- Find such vars/vals, when they are used in assignments to vars/vals that are themselves not used
- Handle class/object/trait fields