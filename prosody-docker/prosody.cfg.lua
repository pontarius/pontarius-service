daemonize=false

modules_enabled = {
		-- Generally required
			"roster"; -- Allow users to have a roster. Recommended ;)
			"saslauth"; -- Authentication for clients and servers. Recommended if you want to log in.
			"tls"; -- Add support for secure TLS on c2s/s2s connections
			"dialback"; -- s2s dialback support
		  	"disco"; -- Service discovery

		-- Not essential, but recommended
			-- "private"; -- Private XML storage (for room bookmarks, etc.)
			-- "vcard"; -- Allow users to set vCards

		-- Nice to have
			"legacyauth"; -- Legacy authentication. Only used by some old clients and bots.
			"version"; -- Replies to server version requests
		  	"uptime"; -- Report how long server has been running
		  	"time"; -- Let others know the time here on this server
		  	"ping"; -- Replies to XMPP pings with pongs
			--"register"; -- Allow users to register on this server using a client and change passwords
			-- Other specific functionality
			-- "posix"; -- POSIX functionality, sends server to background, enables syslog, etc.
		  	--"console"; -- telnet to port 5582 (needs console_enabled = true)
			--"bosh"; -- Enable BOSH clients, aka "Jabber over HTTP"
			--"httpserver"; -- Serve static files from a directory over HTTP
		  };

modules_disabled = { "s2s" }

allow_registration = false;


ssl = {
	key = "certs/ca.key";
	certificate = "certs/ca.crt";
	}

VirtualHost "localhost"
VirtualHost "test.pontarius.org"
