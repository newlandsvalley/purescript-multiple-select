purescript-multiple-select
==========================

A Pux module that provides an autonomous multiple-select drop-down list.  Items (which must be strings) can be selected individually from the dropdown and then appear immediately below it.  Each item thus selected has a remove button associated with it which then allows the choice to be reverted. A default external css file is provided which can be amended according to taste.

The design is largely taken from [here](http://www.suumit.com/projects/bsmSelect/examples/example_results.php)

to build the module
-------------------

   bower install

   pulp build


to build the example
--------------------

   bower install

   ./buildExample.sh

   and then navigate to /dist/index.html   