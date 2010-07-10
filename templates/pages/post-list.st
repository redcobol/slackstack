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
        <div id="page-links">
        
            $if(it.('next_id'))$
                <div>
                    $if(single)$
                        <a href="/posts/$ it.('next_id') $">
                            <img src="/images/left.png"
                                width="141" height="77">
                        </a>
                        <br>
                        <a href="/posts/$ it.('next_id') $">
                            <span>$ it.('next_title') $</span></a>
                        </a>
                    $else$
                        <a href="/browse/$ it.('next_timestamp') $">
                            <img src="/images/left.png"
                                width="141" height="77">
                        </a>
                        <br>
                        <a href="/browse/$ it.('next_timestamp') $">
                            <span>newer</span>
                        </a>
                    $endif$
                </div>
            $endif$
            
            $if(it.('prev_id'))$
                $if(single)$
                    <div>
                        <a href="/posts/$ it.('prev_id') $">
                            <img src="/images/right.png"
                                width="133" height="79">
                        </a>
                        <br>
                        <a href="/posts/$ it.('prev_id') $">
                            <span>$ it.('prev_title') $</span>
                        </a>
                    </div>
                $else$
                    <div>
                        <a href="/browse/$ it.('prev_timestamp') $">
                            <img src="/images/right.png"
                                width="133" height="79">
                        </a>
                        <br>
                        <a href="/browse/$ it.('prev_timestamp') $">
                            <span>older</span>
                        </a>
                    </div>
                $endif$
            $endif$
            
            $if(it.('next_id'))$
                <div class="spacer"></div>
            $elseif(it.('prev_id'))$
                <div class="spacer"></div>
            $endif$
        
        </div>
    </div>
}$
