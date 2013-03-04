<?php
session_start();
var_dump($_POST['markasdone_button']);
?>

<?php
session_start();
include_once 'apicaller.php';

$apicaller = new ApiCaller('APP001', '28e336ac6c9423d946ba02d19c6a2632', 'http://localhost/simpletodo_api/');

// A completer

exit();
?>