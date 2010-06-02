$posts:{
    <div class="post">
        <div>
            <span class="title"><a
                href="$ blogRoot $/posts/$ it.('id') $/$ it.('uri-title') $"
            >$ it.('title') $</a></span>
            <span class="timestamp">[ $ it.('timestamp') $ UTC ]</span>
            $if(isRoot)$
            <span class="admin-post-controls">
                [
                <a href="$ blogroot $/admin/edit-post/$ it.('id') $">edit</a>
                    |
                <a
                    href="$ blogroot $/admin/remove-post/$
                        it.('id') $/$ sessionID $"
                    onclick="return confirm('Remove this post?')"
                >remove</a>
                ]
            </span>
            $endif$
        </div>
        <hr />
        $ it.('body') $
        
        $if(it.('next_id'))$
            next!
        $endif$
        $if(it.('prev_id'))$
            prev!
        $endif$
    </div>
}$
