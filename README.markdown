# Bugzilla mode #

A mode to interface with Bugzilla and other bug tracking systems from within Emacs. Currently somewhat working bug trackers are:

- [Bugzilla](https://www.bugzilla.org/)
- [Rally](https://www.rallydev.com/)

## Requirements ##
* [json.el](http://cvs.savannah.gnu.org/viewvc/*checkout*/emacs/lisp/json.el?root=emacs)
* Recent release of Bugzilla, probably at least 3.6
* JSON-RPC mode enabled for Bugzilla

## Installing ##
* call `make` to generate autoloads
* Edit your ~/.emacs or ~/.emacs.d/init.el:

        (load-file "/path/to/bz-mode/bz.el")

* `M-x customize-group RET bz` and adjust at least the the list of instances and the name of the default instance. The instance plist configuration could look like this:

        (:foo (:url "https://foo.example")
         :bar (:url "https://bar.example" :authinfo "~/.netrc")
         :rally (:api-key "_yourapikey" :type "rally"))

* Store your credentials in authinfo format in `~/.authinfo`, or specify a database location with the `:authinfo` property
* For using Rally, generate [API keys](https://rally1.rallydev.com/login/accounts/index.html#/keys) if your subscription allows it, and set the `:api-key property`. With no API key configured username/password from authinfo are used as well.

## Functions ##
### bz-login / bz-logout###
Some Bugzilla instances require explicit login. Use those functions to login/logout if that's the case for your installation.
### bz-bug ###
Open a single bug (Bugzilla only)
### bz-search ###
Search for bugs. The behavior is different, depending on the bug tracker used.

Bugzilla supports
* free form text query
* key-value (e.g. "component:Test")
* the name of a named search

Rally supports
* free form text query
* complex query in Rally query syntax

Both Bugzilla and Rally directly open a bug view instead of a list if the search term matches a bug identifier, or the search only returned a single result.
### bz-search-multiple ###
Search for bugs with multiple criteria. Same query format as bz-search (Bugzilla only)
### bz-stored-bugs ###
Open a list of locally stored bugs

## Key bindings for bz-modes ##
### bz-list-mode ###
* u - execute query again
* RET - show single bug
* q - kill buffer

### bz-bug-mode ###
* RET - open attachment with browse-url function
* b - open bug in default browser
* c - add comment
* d - download attachment with w3m-download
* r - remember the bug in a locally stored list
* u - execute query again
* q - kill buffer

### bz-comment-mode ###
* C-c C-c - commit comment

## Proxy ##

HTTPS proxy support in emacs has been broken for quite a while, for details read [bug 11788](https://debbugs.gnu.org/cgi/bugreport.cgi?bug=11788). As a result, doing something like the following will at best lead to interesting results if your bug tracker is only reachable via https for authenticated operations (which it should be):

        (setq url-proy-services
              '(("no_proxy" . "^\\((localhost\\|10.*\\)")
                ("http" . "a.proxy.example")
                ("https" . "a.proxy.example")))

There are multiple possible workarounds, some of them are documented here in order of desirability.

### Patch url ###

If you're using emacs 25 you can just grab the patch from git (`2d1a6054b161bd1055d4feb11c8c5ac95543f5db`) and apply it. If you're using emacs 24 or older you'd need to spend some effort to back-port the patch.

### Use portforwarding ###

If your proxy allows using `CONNECT`, and you have a suitable shell host available you can use this to forward a local port to Rally, bypassing the whole proxy mess. An example entry for `~/.ssh/config` could look like this:

        Host rally-forward
            ProxyCommand /usr/bin/connect-proxy -H a.proxy.example:8080 a.shellhost.example 443
            LocalForward 9900 rally1.rallydev.com:443

Additionally `/etc/hosts` needs `rally1.rallydev.com` added after `127.0.0.1` to have it resolve to localhost, and the URL bz uses to access Rally needs to be adjusted to include the locally bound port:

        (setq bz-rally-url "https://rally1.rallydev.com:9900/slm/webservice/v2.0/")

After starting a SSH connection (`ssh rally-forward`) you should be able to use bz without issues.

### Use a proxy aware TLS program ###

OpenSSL's s_client [gained proxy support in trunk](https://rt.openssl.org/Ticket/Display.html?id=2651&user=guest&pass=guest). Assuming your network allows host resolution it might be possible to use this as workaround:

        ;; disable builtin gnutls
        (if (fboundp 'gnutls-available-p)
            (fmakunbound 'gnutls-available-p))

        ;; set openssl compiled from trunk as tls-program
        (setf tls-program '("openssl-trunk s_client -connect %h:%p -proxy a.proxy.example:8080 -ign_eof"))

Note that this will bypass the whole noproxy logic, so if you're using tls in the local network without proxy as well this will break things.
