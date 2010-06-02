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
        <hr>
        $ it.('body') $
        
        <hr>
        <p class="page-links">
        
        $if(it.('next_id'))$
            $if(single)$
                <a href="/posts/$ it.('next_id') $"
                >&lt;--- $ it.('next_title') $</a> |
            $else$
                <a href="/browse/$ it.('next_timestamp') $">&lt;--- newer</a> |
            $endif$
        $endif$
        
        $if(it.('prev_id'))$
            $if(single)$
                | <a href="/posts/$ it.('prev_id') $"
                >$ it.('prev_title') $ ---&gt;</a>
            $else$
                | <a href="/browse/$ it.('prev_timestamp') $">older ---&gt;</a>
            $endif$
        $endif$
        
        </p>
    </div>
}$
