lexical\_uuid.erl
=================

UUIDs that are byte-ordered lamport clocks (timestamp, worker\_id). Much simpler than type-1 UUID’s crappy, weirdo layout.

I've also got a rubby implementation here: http://github.com/jamesgolick/lexical\_uuid

# Usage

    1> application:start(lexical_uuid).
    ok
    2> lexical_uuid:get().
    <<0,0,12,20,236,86,12,151,66,162,70,139,128,79,14,154>>
    3> lexical_uuid:get_string().
    "00000c14-ec61-2531-42a2-468b804f0e9a"
    4> lexical_uuid:get_binary_string().
    <<"00000c14-ec69-a267-42a2-468b804f0e9a">>

# Credits

  * Idea: Coda Hale ([@coda](http://twitter.com/coda)). Originally seen in his (now defunct) http://github.com/codahale/cassie project.
  * fnv1a: Cliff Moon ([@moonpolysoft](http://twitter.com/moonpolysoft)). https://github.com/cliffmoon/ef_examples/blob/master/src/fnv_offset.erl
  * fnv1a tests: Jake Douglas ([@jakedouglas](http://twitter.com/jakedouglas)). https://raw.github.com/jakedouglas/fnv-ruby/master/test/fnv_test.rb

# Copyright
  
Copyright (c) 2010 James Golick, BitLove Inc except fnv1a.erl. See LICENSE for details.
