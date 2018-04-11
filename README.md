# ps-nav

Purescript single page web application to solve navigation calculations

N.B.  This is partially working:

1. It is only partially implemented.  It is up to the same standard as the Haskell library it was based on (<https://github.com/ScottSedgwick/libNavigation>).
2. Those parts that are implemented have unit tests and work correctly.
3. The user interface is not done at all, and still looks like an Address Book tutorial.

## Install the Tools (if required)

These instructions are for a Mac:

* Install Homebrew:
  * /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
* Install Node: 
  * brew install node
* Install Bower:
  * npm install -g bower

## Initial setup

bower install

npm install

## Build Application

npm run pack

Open html/index.html in your browser.

## Tests

pulp test

## Source material

Most of the source material comes from documents obtainable from <http://shipofficer.com/>.

Not all the chapters are relevant to convert to formulae, so some are omitted, e.g. 01, 02, 07.

* Horizon.purs and HorizonTests.purs come from 03.-Distance-of-Horizon.pdf
* TODO : 04.-Position-Lines.pdf
* ParallelSailing.purs and ParallelSailingTests.purs come from 05.-Parallel-Sailing.pdf
* PlaneSailing.purs and PlaneSailingTests.purs come from 06.-Plane-Sailing.pdf
* TODO : 08.-Mercator-Sailing.pdf
* GreatCircle.purs and GreatCircleTests.purs come from 10.-Great-Circle-Sailing.pdf
* TODO : as required from 11 to 27.
