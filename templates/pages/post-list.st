$posts:{
    <div class="post">
        <h1><a
            href="$ blogRoot $/posts/$ it.('id') $/$ it.('uri-title') $"
        >$ it.('title') $</a></h1>
        <h2>$ it.('timestamp') $</h2>
        $ it.('body') $
    </div>
}$
