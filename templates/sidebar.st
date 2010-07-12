<div id="sidebar">
    $if(isAuthed)$
        <p id="openid">
            authenticated as
            <br />
            <a href="$identity$">$identity$</a>
            <br />
            [ <a href="$ blogRoot $/logout/$ sessionID $">logout</a> ]
        </p>
    $else$
        <form id="openid" action="openid" method="post">
            <p id="openid">Sign in with <a href="#">openid</a></p>
            <input type="text" name="identity" />
            <input type="submit" value ="&lambda;" />
        </form>
    $endif$
    
    <div id="sidebar-links">
    
    $if(isRoot)$
        <p><a href="$ blogroot $/admin/$ it $">admin</a></p>
    $endif$
    
    <p><a href="$ blogroot $/about">about</a></p>
    <p><a href="http://github.com/substack">code</a></p>
    
    $categories:{
        <p><a href="$ blogroot $/category/$ it $">$ it $</a></p>
    }$
    
    <p>
    <a href="/rss"
    ><img src="/images/feed.png" width="20" height="20" alt="feed"></a>
    <a href="/rss">rss</a>
    </p>
    
    <hr />
    
    <div id="friends">
        <p>Internet Friends</p>
        <p><a href="http://catonmat.net">pkrumins</a></p>
        <p><a href="http://jesusabdullah.github.com/">jesusabdullah</a></p>
        <p><a href="http://podcats.in/training">Altreus</a></p>
        <p><a href="http://simcop2387.info/">simcop2387</a></p>
        <p><a href="http://f00li5h.pin21.com/">f00li5h</a></p>
    </div>

    </div>
</div>
