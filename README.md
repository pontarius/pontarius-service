Pontarius Service
=================

This is a work in progress of a service that will manage the configurations, credentials, and local storage, of Pontarius users. It will expose its API overthe D-BUS inter-process communication protocol. This enables Pontarius to be integrated with other applications, such as [FreeDesktop.org](http://www.freedesktop.org/wiki/) environments.

Building Instructions
---------------------

    $ sudo apt-get install c2hs libgpgme11-dev
    $ git submodule init
    $ git submodule update
    $ cabal sandbox init
    $ cabal sandbox add-source pontarius-gpg
    $ cabal sandbox add-source pontarius-xmpp-e2e
    $ cabal sandbox add-source d-bus
    $ cabal install

Copyright & License
-------------------

Copyright © Jon Kristensen, 2014-2015.

Licensed under the GNU Affero General Public License, Version 3; see LICENSE for more information.

Contact
-------

Please refer to [Jon Kristensen's contact details](http://www.jonkri.com/contact/).
