Data Structure
==============
I always use List as data structure to hold various kind of objects.
I don't deny that it's not always the best choice according to the usage made of the collection,
but this rule provide the following benefits :
 - Every List of things is sorted. Generally speaking, this is a requirement in this software.
 - A sorted List is algorithms friendly : if input is sorted, then output can be sorted without much extra processing.
 - Forced sorted things enforce conventions, so it's easier to write code and tests in a consistant manners.

Immutability
============
Not a single line makes use of a mutable variable. This piece of software is a playground for me. I'm new
to functional programming and I want to prove myself that immutability can be a rule of thumb for developping software.
I know, sometimes `var`s are so tempting ...
