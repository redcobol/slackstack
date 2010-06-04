<!DOCTYPE html
    PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
    "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" lang="en-US" xml:lang="en-US">
<head>
    <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
    $if(title)$
        <title>$ title $ :: $ blogTitle $</title>
    $else$
        <title>$ blogTitle $</title>
    $endif$
    <link rel="StyleSheet" type="text/css"
        href="$ blogRoot $/stylesheets/slack.css" />
    <link rel="icon" href="$ blogRoot $/images/favicon.png" type="image/png" />
    <link rel="alternate" type="application/rss+xml"
        href="/rss" title="$ blogTitle $" />
    <!-- being able to type "substack.net" into the openID login box is great -->
    <link rel="openid.server" href="http://www.myopenid.com/server" />
    <link rel="openid.delegate" href="http://substack.myopenid.com/" />
    <link rel="openid2.local_id" href="http://substack.myopenid.com" />
    <link rel="openid2.provider" href="http://www.myopenid.com/server" />
    <meta http-equiv="X-XRDS-Location"
        content="http://www.myopenid.com/xrds?username=substack.myopenid.com"
    />
    
    <script type="text/javascript">
        // google analytics stuff
        var gaJsHost = (("https:" == document.location.protocol) ? "https://ssl." : "http://www.");
        document.write(unescape("%3Cscript src='" + gaJsHost
            + "google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E"
        ));
    </script>
    <script type="text/javascript">
        try {
            var pageTracker = _gat._getTracker("UA-15464999-1");
            pageTracker._trackPageview();
        } catch(err) {}
    </script>
</head>
<body>
    $ header() $
    
    <div id="content">
        $ content $
    </div>
    
    $ footer() $
</body>
</html>
