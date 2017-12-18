<!DOCTYPE html>
<html>
  <body>

    <?php

  if ($_FILES['program']['error'] > 0)
  {
    echo 'Problem: ';
    switch ($_FILES['program']['error'])
    {
      case 1:	echo 'File exceeded upload_max_filesize';
	  			break;
      case 2:	echo 'File exceeded max_file_size';
	  			break;
      case 3:	echo 'File only partially uploaded';
	  			break;
      case 4:	echo 'No file uploaded';
	  			break;
      case 6:   echo 'Cannot upload file: No temp directory specified.';
	  			break;
      case 7:   echo 'Upload failed: Cannot write to disk.';
	  			break;
    }
    exit;
  }

    system('echo ^<htlm^> Program: ^<b^>' . $_FILES["program"]["name"] . '^</b^>^<br /^> false ^</html^> > html\justification.html');

echo "<div style='white-space: pre-wrap'>";

system('echo: ' . $_POST["query"] . ' > query | ..\casp   -w ' . $_FILES["program"]["tmp_name"] . ' query' );
      
echo "</div>";

system('start html\justification.html');

      ?>


  </body>
</html>

