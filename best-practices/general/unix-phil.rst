.. _unix-phil:

The Unix Philosophy
-------------------

When we develop software in a community we must consider that our work is not just for ourselves but is expected to be
used, adapted and (even) improved by others in that community. Much of the Unix philosophy [Raymond]_ is worth
considering when developing in such a context:

* Rule of Modularity: Write simple parts connected by clean interfaces.
* Rule of Clarity: Clarity is better than cleverness.
* Rule of Composition: Design programs to be connected to other programs.
* Rule of Separation: Separate policy from mechanism; separate interfaces from engines.
* Rule of Simplicity: Design for simplicity; add complexity only where you must.
* Rule of Parsimony: Write a big program only when it is clear by demonstration that nothing else will do.
* Rule of Transparency: Design for visibility to make inspection and debugging easier.
* Rule of Robustness: Robustness is the child of transparency and simplicity.
* Rule of Representation: Fold knowledge into data so program logic can be stupid and robust.
* Rule of Least Surprise: In interface design, always do the least surprising thing.
* Rule of Silence: When a program has nothing surprising to say, it should say nothing.
* Rule of Repair: When you must fail, fail noisily and as soon as possible.
* Rule of Economy: Programmer time is expensive; conserve it in preference to machine time.
* Rule of Generation: Avoid hand-hacking; write programs to write programs when you can.
* Rule of Optimization: Prototype before polishing. Get it working before you optimize it.
* Rule of Diversity: Distrust all claims for “one true way”.
* Rule of Extensibility: Design for the future, because it will be here sooner than you think.

If you're new to Unix, these principles are worth some meditation (and I would recommend reading the fuller descriptions
of the `programming "rules" derived from the Unix philosophy <http://www.faqs.org/docs/artu/ch01s06.html>`_)

.. [Raymond] The Art of Unix Programming, Eric Steven Raymond http://www.faqs.org/docs/artu/index.html