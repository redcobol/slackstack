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
    
    <dl>
    $if(isRoot)$
        <dt>
            <a href="$ blogroot $/admin/$ it $">admin</a>
        </dt>
    $endif$
    <dt>
        <a href="$ blogroot $/about">about</a>
    </dt>
    $categories:{
        <dt>
            <a href="$ blogroot $/category/$ it $">$ it $</a>
        </dt>
    }$
    </dl>
</div>
