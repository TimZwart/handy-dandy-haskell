# handy-dandy-haskell

When I'm coding in any language, some things just annoy me. Then I make a library which adrresses those lacks. It wont go down in history but it will go down on my code. The functions concern lists, association lists and Strings.

like:

!!! a replacement for !! giving a more detailed error message.

lookUp something which gets an entry in an alist without giving you a Maybe. and gives a useful error message if it cant

something which checks whether a list has a certain number of elements (so you dont have to go through the entire list like with length)

replace functions for lists and alists

something to convert a list to a 2-tuple

parse-to-alist functions for CSV and a single level of depth JSON object

and last-but-not-least a map variant which also feeds the current index as you traverse the list. 