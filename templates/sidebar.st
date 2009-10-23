<div id="sidebar">
    $if(authed)$
        <p id="openid">$openid$</p>
    $else$
        <form id="openid" action="openid" method="post">
            <p id="openid">Sign in with <a href="#">openid</a></p>
            <input type="text" name="identity" />
            <input type="submit" value ="&lambda;" />
        </form>
    $endif$
    
    <dl>
    $categories:{
        <dt>
            <a href="$ blogRoot $/category/$ it $">$ it $</a>
        </dt>
    }$
    </dl>
</div>
