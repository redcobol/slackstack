<form action="$ blogRoot $/admin/edit-post" method="post">
    <h1>Edit Post</h1>
    <input name="postID" type="hidden" value="$ postID $" />
    <p>
        <input name="title" class="posttext" type="text" value="$ postTitle $" />
    </p>
    <p>
        <textarea name="body" class="postbox">$ postBody $</textarea>
    </p>
    <input type="submit" value="submit" />
</form>
