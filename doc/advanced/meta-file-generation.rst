META File Generation
====================

Dune uses ``META`` files from the `findlib library
manager <http://projects.camlcity.org/projects/findlib.html>`__ in order
to interoperate with the rest of the world when installing libraries. It's
able to generate them automatically. However, for the rare cases
where you would need a specific ``META`` file, or to ease the transition
of a project to Dune, it is allowed to write/generate a specific
one.

In order to do that, write or setup a rule to generate a
``META.<package>.template`` file in the same directory as the
``<package>.opam`` file. Dune will generate a ``META.<package>``
file from the ``META.<package>.template`` file by replacing lines of
the form ``# DUNE_GEN`` with the contents of the ``META`` it would
normally generate.

For instance if you want to extend the ``META`` file generated by
Dune, you can write the following ``META.foo.template`` file:

.. code::

   # DUNE_GEN
   blah = "..."

