.. _ci:

Continuous Integration
----------------------

Continuous Integration (CI) is a development practice that requires developers to integrate code into a shared
repository several times a day. Each check-in is then verified by an automated build, allowing teams to detect problems
early.

Our weapon of choice is `GitLab CI <https://about.gitlab.com/features/gitlab-ci-cd/>`_, the main reason being
controlling the servers where the tests are run allows us to customise their configuration. Of course you can get
similar services on `GitHub <https://github.com/>`_, `Travis CI <https://travis-ci.org/>`_ being the most popular (and
completely for free if you are open source).

Gamification
^^^^^^^^^^^^

One of the big advantages of the automated CI is that it helps to gamify the development experience. Having all your
unit tests pass or having 100% code coverage for your tests (and getting a little badge to appear notifying you), gives
a (small) feeling of accomplishment and can be a good motivator to write better merge requests,