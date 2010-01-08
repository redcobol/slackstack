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
    
    <br />
    
    $if(isRoot)$
        <a href="$ blogroot $/admin/$ it $">admin</a><br />
    $endif$
    
    <a href="$ blogroot $/about">about</a><br />
    <a href="http://github.com/substack">code</a><br />
    
    $categories:{
        <a href="$ blogroot $/category/$ it $">$ it $</a><br />
    }$
</div>
