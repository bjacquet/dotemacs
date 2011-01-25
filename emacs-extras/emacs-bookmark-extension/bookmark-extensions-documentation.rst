================================
 BookmarExtension Documentation
================================

Bookmarking a web page from Firefox in Emacs bookmarks
======================================================

Setting up firefox
------------------

Adding a new protocol handler to Firefox 3.5.*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Adding a new protocol to firefox 3 is not as so easy as it was in
firefox versions < 3.

1) Manually by editing files
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In addition to setting the protocols handlers in user.js or pref.js,
you will have to modify also the complex mimeType.rdf file.

Step1: add the protocol handler
-------------------------------

The protocol we are adding is emacsbookmark.
It will be use to record your current page in firefox to Standard
emacs bookmarks.

Create a new file user.js in $HOME/.mozilla/xxxxx.default
Add following lines to this file:

user_pref("network.protocol-handler.app.emacsbookmark", "/PATH/TO/YOUR/emacsbookmark");
user_pref("network.protocol-handler.external.emacsbookmark", true);

Where /PATH/TO/YOUR/emacsbookmark is the application we associate to
the new protocol emacsbookmark.

Here it is the script that we will use to send info about our web page
to Emacs.

Step2: Create a bookmarklet
---------------------------

Go to the bookmark bar in firefox, right click on it and choose add a
new bookmark:

In name, add: emacsbookmark
In address, add:

javascript:location.href='emacsbookmark://' + location.href + '::emacsbookmark::' + escape(document.title)

Step3: Modify the mimeTypes.rdf file
------------------------------------

First save the original mimeTypes.rdf file.

Then just after the 3 first lines:


.. <?xml version="1.0"?>
.. <RDF:RDF xmlns:NC="http://home.netscape.com/NC-rdf#"
..          xmlns:RDF="http://www.w3.org/1999/02/22-rdf-syntax-ns#">


Add the following lines:


.. <!-- Emacsbookmark Section -->
..   <RDF:Description RDF:about="urn:scheme:externalApplication:emacsbookmark"
..                    NC:prettyName="emacsbookmark"
..                    NC:path="/home/thierry/bin/emacsbookmark" />

..   <RDF:Description RDF:about="urn:scheme:emacsbookmark"
..                    NC:value="emacsbookmark">
..     <NC:handlerProp RDF:resource="urn:scheme:handler:emacsbookmark"/>
..   </RDF:Description>

..   <RDF:Description RDF:about="urn:handler:local:/home/thierry/bin/emacsbookmark"
..                    NC:prettyName="emacsbookmark"
..                    NC:path="/home/thierry/bin/emacsbookmark" />

..   <RDF:Seq RDF:about="urn:schemes:root">  
..     <RDF:li RDF:resource="urn:scheme:emacsbookmark"/>
..   </RDF:Seq>

..   <RDF:Description RDF:about="urn:scheme:handler:emacsbookmark"
..                    NC:alwaysAsk="false">
..     <NC:externalApplication RDF:resource="urn:scheme:externalApplication:emacsbookmark"/>
..     <NC:possibleApplication RDF:resource="urn:handler:local:/home/thierry/bin/emacsbookmark"/>
..   </RDF:Description>
.. <!-- End Emacsbookmark Section -->


Be sure nxml-mode is turned on, it should show you "(nxlml valid)" in
the mode-line.

2) Automatic install of a protocol to firefox
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Install firefox-protocol.el:

(require 'firefox-protocol)

Facultative: (needed if you use firefox for the first time)
Launch firefox, that will create a new directory if you use firefox
for the first time.
Exit firefox.

Now from Emacs run:

M-x firefox-protocol-installer-install

Start Firefox and setup your bookmarklet as described before in Step2.


Install the script emacsbookmark
================================

Copy the file emacsbookmark somewhere in your PATH.
Be sure to make it executable with:

chmod +x emacsbookmark

Install the elisp code
======================

Put file bookmark-firefox-handler.el in your elisp directory.
Then add to your .emacs:

(require 'bookmark-firefox-handler)

Load the file bookmark-firefox-handler or eval the code above or
restart emacs.

Launch or restart Firefox
=========================

Open a page somewhere, now click on the bookmarklet emacsbookmark, 
and go back to emacs [1].

Note: If you didn't start server in emacs, you will not be able to use
      emacsclient.

Say yes (y) and you will have your page bookmarked in your
Emacs Bookmarks.
 
[1] If you use stumpwm, you should be able to raise emacs automaticly
    when emacsclient is called from external applications.
    (I will add doc soon as the stumpwm wiki is down actually)
