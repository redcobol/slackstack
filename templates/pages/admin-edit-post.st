<form action="$ blogRoot $/admin/edit-post" method="post">
    <h1>Edit Post</h1>
    <input name="postID" type="hidden" value="$ postID $" />
    
    <p><label for="title">title</label></p>
    <p><input name="title" class="posttext" type="text" value="$ postTitle $" /></p>
    
    <p><label for="description">description</label></p>
    <p><textarea name="description" class="descbox">$ postDesc $</textarea></p>
    
    <p><label for="body">body</label></p>
    <p><textarea name="body" class="postbox">$ postBody $</textarea></p>
    
    <input type="submit" value="submit" />
</form>
